library(tidyverse)
library(here)
library(rstan)
library(stringr)
library(tidybayes)
library(posterior)


# Load data ---------------------------------------------------------------

dat <- read_csv(here("data", "final-standings-men-2024.csv"))
race_order <- dat %>%
  dplyr::select(-c("Place", "Name", "Scored Points", "Total Points")) %>%
  colnames()
name_order <- dat %>%
  distinct(Place, Name) %>%
  mutate(Name = str_to_lower(Name)) %>%
  mutate(Name = str_replace_all(Name, " ", "_")) %>%
  pull(Name)

ldat <- dat %>%
  dplyr::select(-c("Total Points", "Scored Points")) %>%
  pivot_longer(cols = `Sea Otter Classic`:`Big Sugar Gravel`, names_to = "race")

places <- ldat %>%
  dplyr::select(-Place) %>%
  group_by(race) %>%
  arrange(race, -value) %>%
  mutate(finish = 1:n()) %>%
  ungroup() %>%
  dplyr::select(name = Name, race, finish)

mod_dat_fct <- places %>%
  mutate(name = str_to_lower(name)) %>%
  mutate(name = str_replace_all(name, " ", "_")) %>%
  mutate(name = fct(name, levels = name_order)) %>%
  mutate(race = fct(race, levels = race_order)) %>%
  arrange(name, race)

mod_dat <- mod_dat_fct %>%
  mutate(name = as.numeric(name),
         race = as.numeric(race))

n_racers <- length(unique(mod_dat$name))
n_ranks <- max(mod_dat$finish)
n_races <- length(unique(mod_dat$race))
ranks <- mod_dat %>%
  pivot_wider(names_from = race, values_from = finish) %>%
  dplyr::select(-name) %>%
  as.matrix()
reward <- rev(c(1:25, seq(27, 35, by = 2)))

# Prepare data for Stan
data_list <- list(
  N = n_racers,
  K = n_ranks,
  R = n_races,
  rank = ranks,
  reward = reward
)

# Fit the model
fit <- stan(
  file = here("stan", "base-model.stan"),  # Path to the updated Stan model
  data = data_list,
  iter = 2000,
  chains = 1
)

# Extract results
print(fit, pars = c("points"))

draws <- fit %>% 
  as_draws_df() %>%
  as_tibble() %>%
  pivot_longer(cols = -c(".chain", ".iteration", ".draw")) %>%
  filter(str_detect(name, "points")) %>%
  filter(str_detect(name, "cut", negate = TRUE)) %>%
  mutate(
    racer = str_extract(name, "(?<=\\[)\\d+"),
    race = str_extract(name, "(?<=,)\\d+(?=\\])")
  ) %>%
  dplyr::select(-name) %>%
  group_by(racer, .draw) %>%
  arrange(race) %>%
  mutate(value = cumsum(value)) %>%
  ungroup() 

# Todo: incorporate drop race...
win_probs <- draws %>%
  group_by(.draw, race) %>%
  mutate(leader = max(value)) %>%
  ungroup() %>%
  group_by(.draw) %>%
  mutate(dlead = value - leader,
         rraces = 6-as.numeric(race),
         cancatch = abs(dlead) <= rraces*35,
         isleader = value == leader) %>%
  filter((cancatch == FALSE | isleader == TRUE) & value == leader) %>%
  filter(race == max(as.numeric(race))) %>% 
  group_by(.draw) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  group_by(racer) %>%
  summarise(n = n()/1000) %>%
  arrange(as.numeric(racer)) %>%
  mutate(name = name_order[1:nrow(.)]) %>%
  mutate(name = fct(name, levels = name_order))

ggplot(data = win_probs, aes(x = "test", y = name, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

# %>%
#   group_by(racer, race) %>%
#   summarise(mid = median(value),
#             lo = quantile(value, 0.025),
#             hi = quantile(value, 0.975),
#             .groups = "drop") %>%
#   ggplot(aes(x = as.numeric(race), y = mid, color = racer, fill = racer)) +
#   geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.5, color = NA) +
#   geom_line() +
#   facet_wrap(~racer)

