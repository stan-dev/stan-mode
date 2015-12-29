/*
A file for testing Stan syntax highlighting.

This is a completely invalid model, but is useful for testing syntax highlighting
*/
# also a comment
// also a comment
functions {
  void f1(void a, real b) {
    return 1 / a;
  }
  real f2(int a, vector b, real c) {
    return a + b + c;
  }
}
data {
  // valid names
  real abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_abc;
  real a;
  real a3;
  real Sigma;
  real my_cpp_style_variable;
  real myCamelCaseVariable;
  // invalid names
  int a__;
  int 1a;
  int _a;
  // C++ reserved
  real public;
  // Stan reserved
  real var;
  real fvar;
  real STAN_MAJOR;

  // all types should be highlighed
  int alpha;
  real bravo;
  vector[1] charlie;
  ordered[1] delta;
  positive_ordered[3] echo;
  simplex[1] foxtrot;
  row_vector[1] golf;
  matrix[1, 1] hotel;
  corr_matrix[3] india;
  cov_matrix[3] juliette;
  cholesky_factor_cov[3] kilo;
  cholesky_factor_corr[3] lima;

  // ranges;
  real<lower=-1,upper=1> mike;
  real<lower=0> november;
  real<upper=0> oscar;
  // arrays
  real papa[1]
  real quebec[1, 1];
  real romeo[1][1];
}
transformed data {
  real sierra;
  sierra <- 1 + 1;
}
parameters {
  real tango;
}
transformed parameters {
  real uniform;
  uniform <- 1 / tango;
}
model {
  real foo;
  int bar;
  // valid integer literals
  bar <- 0;
  bar <- 1;
  bar <- -1;
  bar <- 256;
  bar <- -127098;
  // valid real literals
  foo <- 0.0;
  foo <- 1.0;
  foo <- 3.14;
  foo <- -217.9387;
  foo <- 2.7e3;
  foo <- -2E-5;

  // sampling distributions
  y ~ normal_log(alpha, beta);  
  foo <- normal_log(y, alpha, beta);
  foo <- normal_cdf(y, alpha, beta);
  foo <- normal_cdf_log(y, alpha, beta);  
  foo <- normal_ccdf_log(y, alpha, beta);

  // truncation
  alpha ~ normal(0, 1) T[-0.5, 0.5];
  
  // control structures
  for (i in 1:10) {
    tmp <- tmp + 1;
  }
  while (tmp < 5.0) {
    tmp <- tmp + 1;
  }
  if (tmp > 0) {
    tmp <- tmp + 1;
  } else if (tmp < 0) {
    tmp <- tmp + 1;
  } else {
    tmp <- tmp + 1;
  }

  // operators
  foo || foo;
  foo && foo;
  foo == foo;
  foo != foo;
  foo < foo;
  foo <= foo;
  foo > foo;
  foo >= foo;
  foo + foo;
  foo - foo;
  foo * foo;
  foo / foo;
  foo % foo;
  foo .* foo;
  foo ./ foo;
  ! foo;
  - foo;
  + foo;
  foo ^ foo;
  foo ';

  // lp__ should be highlighted
  lp__ <- lp__ + normal_log(alpha, 0, 1);
  // normal_log as a function
  increment_log_prob(normal_log(alpha, 0, 1));

  // ODE
  y_hat <- integrate_ode(sho, y0, t0, ts, theta, x_r, x_i);
  
  // print and reject statements
  print("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_~@#$%^&*`'-+={}[].,;: ");
  print("Hello, world!");
  print("");
  reject("rejected!");
  

}
generated quantities {
  real baz;
  // sampling
  baz <- normal_rng(y, alpha, beta);
}
