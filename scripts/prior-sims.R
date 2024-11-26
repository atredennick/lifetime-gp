# Load required packages
library(rstan)
library(here)
library(tidyverse)

# Simulate data for 8 races
N <- 25  # Number of participants
K <- 25  # Number of ranks
R <- 8   # Number of races
true_skill <- matrix(rnorm(N), nrow=N, ncol=R)  # Initialize the skill matrix with R columns
ranks <- matrix(NA, nrow=N, ncol=R)  # Placeholder for ranks

# Simulate race results with skill progression
for (r in 1:R) {
  ranks[, r] <- rank(-true_skill[, r])  # Rank based on skill (lower rank is better)
  if (r < R) {
    true_skill[, r + 1] <- true_skill[, r] + rnorm(N, mean=0, sd=0.1)  # Skill evolves for next race
  }
}

reward <- c(100, 80, 70, 60, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5, rep(1, 11))

# Prepare data for Stan
data_list <- list(
  N = N,
  K = K,
  R = R,
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
