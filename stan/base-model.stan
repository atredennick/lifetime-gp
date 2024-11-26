data {
  int<lower=1> N;          // Number of participants (e.g., 25)
  int<lower=1> K;          // Number of possible ranks (e.g., 25)
  int<lower=1, upper=K> rank[N]; // Observed rank for each participant
}

parameters {
  vector[N] skill;          // Latent skill levels for each participant
  real<lower=0> sigma;      // Noise in the ranking process
  ordered[K-1] cutpoints;   // Cutpoints to define ranking boundaries
}

model {
  // Priors
  skill ~ normal(0, 1);     // Prior for latent skills
  sigma ~ cauchy(0, 2);     // Prior for noise
  cutpoints ~ normal(0, 1); // Prior for cutpoints

  // Likelihood
  for (n in 1:N) {
    rank[n] ~ ordered_logistic(skill[n], cutpoints / sigma);
  }
}

generated quantities {
  int<lower=1, upper=K> predicted_rank[N]; // Predicted ranks
  for (n in 1:N) {
    predicted_rank[n] = ordered_logistic_rng(skill[n], cutpoints / sigma);
  }
}
