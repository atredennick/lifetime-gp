data {
  int<lower=1> N;           // Number of participants
  int<lower=1> K;           // Number of possible ranks (same as number of participants)
  int<lower=1> R;           // Number of races (e.g., 8)
  int<lower=1, upper=K> rank[N, R]; // Observed ranks for each participant in each race
}

parameters {
  vector[N] skill[R];        // Latent skill levels for each participant in each race
  real<lower=0> sigma;       // Noise in the ranking process
  ordered[K-1] cutpoints;    // Cutpoints to define ranking boundaries
  real<lower=0> delta;       // Skill change rate between races
}

model {
  // Priors
  skill[, 1] ~ normal(0, 1);   // Initial skill for each participant in the first race
  for (r in 2:R) {
    skill[, r] ~ normal(skill[, r-1], delta);  // Skill evolves over time (race to race)
  }
  sigma ~ cauchy(0, 2);        // Prior for noise in the ranking process
  cutpoints ~ normal(0, 1);    // Prior for cutpoints

  // Likelihood
  for (r in 1:R) {
    for (n in 1:N) {
      rank[n, r] ~ ordered_logistic(skill[n, r], cutpoints / sigma); // Ranking model
    }
  }
}

generated quantities {
  int<lower=1, upper=K> predicted_rank[N, R]; // Predicted ranks for each race and participant
  for (r in 1:R) {
    for (n in 1:N) {
      predicted_rank[n, r] = ordered_logistic_rng(skill[n, r], cutpoints / sigma);
    }
  }
}
