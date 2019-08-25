// The Eight Schools example with non-centered parametrization.
// https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html
data {
  int<lower=0> J;
  vector[J] y;
  vector<lower=0>[J] sigma;
}
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  mu ~ normal(0, 10);
  tau ~ cauchy(0, 10);
  eta ~ normal(0, 1); // implies theta ~ normal(mu, tau)
  y ~ normal(theta, sigma);
}
