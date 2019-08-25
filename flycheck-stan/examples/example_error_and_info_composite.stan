# The Eight Schools example with non-centered parametrization.
# https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
  int<lower=0> J;
  vector[J] y;
  vector<lower=0>[J] sigma;
}
parameters {
  rear mu;
  // The parser stops at the above line.
  // Any issues below are not detected.
  real<lower=0> tau;
  // This undefined K is not detected.
  vector[K] eta;
}
transformed parameters {
  vector[J] theta;
  theta <- mu + tau * eta;
}
model {
  increment_log_prob(normal_log(mu, 0, 10));
  target += cauchy_log(tau, 0, 10);
  eta ~ normal(0, 1); # implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}
