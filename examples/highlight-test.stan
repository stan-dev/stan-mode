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
  // valid name
  int abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_abc;
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
  // valid integer literal
  bar <- 0;
  bar <- 123;
  // valid real literal
  foo <- 0.23497;
  foo <- 1234.56789;
  foo <- .6;
  foo <- 7e8;
  foo <- 2.0e9;

  // sampling distributions
  y <- normal_log(alpha, beta);  
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
