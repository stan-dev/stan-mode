;;; Compiled snippets and support files for `stan-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("~" "~ ${1:$$(yas-choose-value stan-distribution-list)}($0);" "~ ..." nil nil nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("data" "data {\n  $0\n}\n" "data{...}" nil
			("Blocks")
			nil nil nil nil)
		       ("generated" "generated quantities {\n  $0\n}\n" "generated quantities{...}" nil
			("Blocks")
			nil nil nil nil)
		       ("model" "model {\n  $0\n}\n" "model{...}" nil
			("Blocks")
			nil nil nil nil)
		       ("param" "parameters {\n  $0\n}\n" "parameters{...}" nil
			("Blocks")
			nil nil nil nil)
		       ("program" "data {\n  $0\n}\ntransformed data {\n}\nparameters {\n}\ntransformed parameters {\n}\nmodel {\n}\ngenerated quantities {\n}" "data{...} ... " nil
			("Blocks")
			nil nil nil nil)
		       ("tdata" "transformed data {\n  $0\n}\n" "data{...}" nil
			("Blocks")
			nil nil nil nil)
		       ("tpar" "transformed parameters {\n  $0\n}\n" "transformed parameters{...}" nil
			("Blocks")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("elif" "else if (${1:condition}) {\n    $0\n}" "else if (...) { ... }" nil
			("Conditionals")
			nil nil nil nil)
		       ("for" "for (${1:i} in ${2:1}:${3:N}) {\n    $0\n}\n" "for (...; ...; ...) { ... }" nil
			("Conditionals")
			nil nil nil nil)
		       ("if" "if (${1:condition}) {\n    $0\n}" "if (...) { ... }" nil
			("Conditionals")
			nil nil nil nil)
		       ("else" "else (${1:condition}) {\n    $0\n}" "else (...) { ... }" nil
			("Conditionals")
			nil nil nil nil)
		       ("while" "while (${1:condition}) {\n    $0\n}" "while (...) { ... }" nil
			("Conditionals")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("<" "<lower=${1:0},upper=${2:1}>$0" "<lower=..., upper=...>" nil
			("Range Constraints")
			nil nil nil nil)
		       ("<" "<lower=${1:0}>$0" "<lower=...>" nil
			("Range Constraints")
			nil nil nil nil)
		       ("<" "<upper=${1:0}>$0" "<upper=...>" nil
			("Range Constraints")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("cholesky_factor_cov" "cholesky_factor_cov[${1:expression}${2:${3:, ${4:expression}}}] ${5:variable}${6:[${7:dims}]};\n$0" "cholesky_factor_cov[] ... ;" nil
			("Types")
			nil nil nil nil)
		       ("corr_matrix" "corr_matrix[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "corr_matrix[] ... ;" nil
			("Types")
			nil nil nil nil)
		       ("cov_matrix" "cov_matrix[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "cov_matrix[] ... ;" nil
			("Types")
			nil nil nil nil)
		       ("int" "int${1:<${2:lower=...,upper=...}>} ${3:variable}${4:[${5:dims}]};\n$0\n" "int ... ;" nil
			("Types")
			nil nil nil nil)
		       ("matrix" "matrix{1:<${2:lower=...,upper=...}>}[$3, $4] ${5:variable}${6:[${7:dims}]};\n$0" "matrix[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("ordered" "ordered[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "ordered[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("pordered" "positive_ordered[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "positive_ordered[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("real" "real${1:<${2:lower=...,upper=...}>} ${3:variable}${4:[${5:dims}]};\n$0" "real ...;" nil
			("Types")
			nil nil nil nil)
		       ("rvector" "row_vector${1:<${2:lower=...,upper=...}>}[${3:expression}] ${4:variable}${5:[${6:dims}]};\n$0" "row_vector[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("simplex" "simplex[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "simplex" nil
			("Types")
			nil nil nil nil)
		       ("uvector" "unit_vector[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "unit_vector[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("vector" "vector${1:<${2:lower=...,upper=...}>}[${3:expression}] ${4:variable}${5:[${6:dims}]};\n$0" "vector[] ...;" nil
			("Types")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("bernoulli" "bernoulli(${1:reals theta})$0\n" "bernoulli(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("bernoulli_logit" "bernoulli_logit(${1:reals alpha})$0\n" "bernoulli_logit(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("beta_binomial" "beta_binomial(${1:ints N}, ${2:reals alpha}, ${3:reals beta})$0\n" "beta_binomial(ints, reals, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("beta" "beta(${1:reals alpha}, ${2:reals beta})$0\n" "beta(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("binomial" "binomial(${1:ints N}, ${2:reals theta})$0\n" "binomial(ints, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("binomial_logit" "binomial_logit(${1:ints N}, ${2:reals alpha})$0\n" "binomial_logit(ints, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("categorical" "categorical(${1:vector theta})$0\n" "categorical(vector)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("categorical_logit" "categorical_logit(${1:vector beta})$0\n" "categorical_logit(vector)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("cauchy" "cauchy(${1:reals mu}, ${2:reals sigma})$0\n" "cauchy(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("chi_square" "chi_square(${1:reals nu})$0\n" "chi_square(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("dirichlet" "dirichlet(${1:vector alpha})$0\n" "dirichlet(vector)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("double_exponential" "double_exponential(${1:reals mu}, ${2:reals sigma})$0\n" "double_exponential(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("exp_mod_normal" "exp_mod_normal(${1:reals mu}, ${2:reals sigma}, ${3:reals lambda})$0\n" "exp_mod_normal(reals, reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("exponential" "exponential(${1:reals beta})$0\n" "exponential(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("gamma" "gamma(${1:reals alpha}, ${2:reals beta})$0\n" "gamma(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("gaussian_dlm_obs" "gaussian_dlm_obs(${1:matrix F}, ${2:matrix G}, ${3:matrix V}, ${4:matrix W}, ${5:vector m0}, ${6:matrix C0})$0\n" "gaussian_dlm_obs(matrix, matrix, matrix, matrix, vector, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("gumbel" "gumbel(${1:reals mu}, ${2:reals beta})$0\n" "gumbel(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("hypergeometric" "hypergeometric(${1:int N}, ${2:int a}, ${3:int b})$0\n" "hypergeometric(int, int, int)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("inv_chi_square" "inv_chi_square(${1:reals nu})$0\n" "inv_chi_square(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("inv_gamma" "inv_gamma(${1:reals alpha}, ${2:reals beta})$0\n" "inv_gamma(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("inv_wishart" "inv_wishart(${1:real nu}, ${2:matrix Sigma})$0\n" "inv_wishart(real, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("lkj_corr" "lkj_corr(${1:real eta})$0\n" "lkj_corr(real)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("lkj_cov" "lkj_cov(${1:vector mu}, ${2:vector sigma}, ${3:real eta})$0\n" "lkj_cov(vector, vector, real)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("logistic" "logistic(${1:reals mu}, ${2:reals sigma})$0\n" "logistic(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("lognormal" "lognormal(${1:reals mu}, ${2:reals sigma})$0\n" "lognormal(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_normal_cholesky" "multi_normal_cholesky(${1:vector mu}, ${2:matrix L})$0\n" "multi_normal_cholesky(vector, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_normal" "multi_normal(${1:vector mu}, ${2:matrix Sigma})$0\n" "multi_normal(vector, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_normal_prec" "multi_normal_prec(${1:vector mu}, ${2:matrix Omega})$0\n" "multi_normal_prec(vector, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_student_t" "multi_student_t(${1:real nu}, ${2:vector mu}, ${3:matrix Sigma})$0\n" "multi_student_t(real, vector, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multinomial" "multinomial(${1:vector theta}, ${2:int N})$0\n" "multinomial(vector, int)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("neg_binomial" "neg_binomial(${1:reals alpha}, ${2:reals beta})$0\n" "neg_binomial(reals, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("normal" "normal(${1:reals mu}, ${2:reals sigma})$0\n" "normal(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("ordered_logistic" "ordered_logistic(${1:real eta}, ${2:vector c})$0\n" "ordered_logistic(real, vector)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("pareto" "pareto(${1:reals y_min}, ${2:reals alpha})$0\n" "pareto(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("poisson" "poisson(${1:reals lambda})$0\n" "poisson(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("poisson_log" "poisson_log(${1:reals alpha})$0\n" "poisson_log(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("rayleigh" "rayleigh(${1:reals sigma})$0\n" "rayleigh(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("scaled_inv_chi_square" "scaled_inv_chi_square(${1:reals nu}, ${2:reals sigma})$0\n" "scaled_inv_chi_square(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("skew_normal" "skew_normal(${1:reals mu}, ${2:reals sigma}, ${3:reals alpha})$0\n" "skew_normal(reals, reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("student_t" "student_t(${1:reals nu}, ${2:reals mu}, ${3:reals sigma})$0\n" "student_t(reals, reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("uniform" "uniform(${1:reals alpha}, ${2:reals beta})$0\n" "uniform(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("von_mises" "von_mises(${1:reals mu}, ${2:reals kappa})$0\n" "von_mises(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("weibull" "weibull(${1:reals alpha}, ${2:reals sigma})$0\n" "weibull(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("wishart" "wishart(${1:real nu}, ${2:matrix Sigma})$0\n" "wishart(real, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("Phi" "Phi(${1:real x})$0\n" "Phi(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("Phi_approx" "Phi_approx(${1:real x})$0\n" "Phi_approx(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("abs" "abs(${1:int x})$0\n" "abs(int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Absolute Functions")
			nil nil nil nil)
		       ("abs" "abs(${1:real x})$0\n" "abs(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("acos" "acos(${1:real x})$0\n" "acos(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("acosh" "acosh(${1:real x})$0\n" "acosh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("asin" "asin(${1:real x})$0\n" "asin(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("asinh" "asinh(${1:real x})$0\n" "asinh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("atan" "atan(${1:real x})$0\n" "atan(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("atan2" "atan2(${1:real x}, ${2:real y})$0\n" "atan2(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("atanh" "atanh(${1:real x})$0\n" "atanh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("bernoulli_ccdf_log" "bernoulli_ccdf_log(${1:ints y}, ${2:reals theta})$0\n" "bernoulli_ccdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_cdf" "bernoulli_cdf(${1:ints y}, ${2:reals theta})$0\n" "bernoulli_cdf(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_cdf_log" "bernoulli_cdf_log(${1:ints y}, ${2:reals theta})$0\n" "bernoulli_cdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_log" "bernoulli_log(${1:ints y}, ${2:reals theta})$0\n" "bernoulli_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_logit_log" "bernoulli_logit_log(${1:ints y}, ${2:reals alpha})$0\n" "bernoulli_logit_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution, Logit Parameterization")
			nil nil nil nil)
		       ("bernoulli_rng" "bernoulli_rng(${1:real theta})$0\n" "bernoulli_rng(real)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bessel_first_kind" "bessel_first_kind(${1:real v}, ${2:real z})$0\n" "bessel_first_kind(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("bessel_second_kind" "bessel_second_kind(${1:real v}, ${2:real z})$0\n" "bessel_second_kind(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("beta_binomial_ccdf_log" "beta_binomial_ccdf_log(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0\n" "beta_binomial_ccdf_log(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_cdf" "beta_binomial_cdf(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0\n" "beta_binomial_cdf(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_cdf_log" "beta_binomial_cdf_log(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0\n" "beta_binomial_cdf_log(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_log" "beta_binomial_log(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0\n" "beta_binomial_log(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_rng" "beta_binomial_rng(${1:int N}, ${2:real alpha}, ${3:real beta})$0\n" "beta_binomial_rng(int, real, real)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_ccdf_log" "beta_ccdf_log(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0\n" "beta_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_cdf" "beta_cdf(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0\n" "beta_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_cdf_log" "beta_cdf_log(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0\n" "beta_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_log" "beta_log(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0\n" "beta_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_rng" "beta_rng(${1:real alpha}, ${2:real beta})$0\n" "beta_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("binary_log_loss" "binary_log_loss(${1:int y}, ${2:real y_hat})$0\n" "binary_log_loss(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("binomial_ccdf_log" "binomial_ccdf_log(${1:ints n}, ${2:ints N}, ${3:reals theta})$0\n" "binomial_ccdf_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_cdf" "binomial_cdf(${1:ints n}, ${2:ints N}, ${3:reals theta})$0\n" "binomial_cdf(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_cdf_log" "binomial_cdf_log(${1:ints n}, ${2:ints N}, ${3:reals theta})$0\n" "binomial_cdf_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_coefficient_log" "binomial_coefficient_log(${1:real x}, ${2:real y})$0\n" "binomial_coefficient_log(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("binomial_log" "binomial_log(${1:ints n}, ${2:ints N}, ${3:reals theta})$0\n" "binomial_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_logit_log" "binomial_logit_log(${1:ints n}, ${2:ints N}, ${3:reals alpha})$0\n" "binomial_logit_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution, Logit Parameterization")
			nil nil nil nil)
		       ("binomial_rng" "binomial_rng(${1:int N}, ${2:real theta})$0\n" "binomial_rng(int, real)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("block" "block(${1:matrix x}, ${2:int i}, ${3:int j}, ${4:int n_rows}, ${5:int n_cols})$0\n" "block(matrix, int, int, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("categorical_log" "categorical_log(${1:ints y}, ${2:vector theta})$0\n" "categorical_log(ints, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Categorical Distribution")
			nil nil nil nil)
		       ("categorical_logit_log" "categorical_logit_log(${1:ints y}, ${2:vector beta})$0\n" "categorical_logit_log(ints, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Categorical Distribution")
			nil nil nil nil)
		       ("categorical_rng" "categorical_rng(${1:vector theta})$0\n" "categorical_rng(vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Categorical Distribution")
			nil nil nil nil)
		       ("cauchy_ccdf_log" "cauchy_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "cauchy_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_cdf" "cauchy_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "cauchy_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_cdf_log" "cauchy_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "cauchy_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_log" "cauchy_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "cauchy_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_rng" "cauchy_rng(${1:real mu}, ${2:real sigma})$0\n" "cauchy_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cbrt" "cbrt(${1:real x})$0\n" "cbrt(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("ceil" "ceil(${1:real x})$0\n" "ceil(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("chi_square_ccdf_log" "chi_square_ccdf_log(${1:reals y}, ${2:reals nu})$0\n" "chi_square_ccdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_cdf" "chi_square_cdf(${1:reals y}, ${2:reals nu})$0\n" "chi_square_cdf(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_cdf_log" "chi_square_cdf_log(${1:reals y}, ${2:reals nu})$0\n" "chi_square_cdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_log" "chi_square_log(${1:reals y}, ${2:reals nu})$0\n" "chi_square_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_rng" "chi_square_rng(${1:real nu})$0\n" "chi_square_rng(real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("cholesky_decompose" "cholesky_decompose(${1:matrix A})$0\n" "cholesky_decompose(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("col" "col(${1:matrix x}, ${2:int n})$0\n" "col(matrix, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("cols" "cols(${1:matrix x})$0\n" "cols(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("cols" "cols(${1:row_vector x})$0\n" "cols(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("cols" "cols(${1:vector x})$0\n" "cols(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:matrix x}, ${2:matrix y})$0\n" "columns_dot_product(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:row_vector x}, ${2:row_vector y})$0\n" "columns_dot_product(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:vector x}, ${2:vector y})$0\n" "columns_dot_product(vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:matrix x})$0\n" "columns_dot_self(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:row_vector x})$0\n" "columns_dot_self(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:vector x})$0\n" "columns_dot_self(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cos" "cos(${1:real x})$0\n" "cos(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("cosh" "cosh(${1:real x})$0\n" "cosh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("crossprod" "crossprod(${1:matrix x})$0\n" "crossprod(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:real[] x})$0\n" "cumulative_sum(real[])" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:row_vector rv})$0\n" "cumulative_sum(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:vector v})$0\n" "cumulative_sum(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("determinant" "determinant(${1:matrix A})$0\n" "determinant(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("diag_matrix" "diag_matrix(${1:vector x})$0\n" "diag_matrix(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:matrix m}, ${2:row_vector rv})$0\n" "diag_post_multiply(matrix, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:matrix m}, ${2:vector v})$0\n" "diag_post_multiply(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:row_vector rv}, ${2:matrix m})$0\n" "diag_pre_multiply(row_vector, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:vector v}, ${2:matrix m})$0\n" "diag_pre_multiply(vector, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diagonal" "diagonal(${1:matrix x})$0\n" "diagonal(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("dims" "dims(${1:T x})$0\n" "dims(T)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Size and Dimension Function")
			nil nil nil nil)
		       ("dirichlet_log" "dirichlet_log(${1:vector theta}, ${2:vector alpha})$0\n" "dirichlet_log(vector, vector)" nil
			("Functions" "Continuous Distributions" "Simplex Distributions" "Dirichlet Distribution")
			nil nil nil nil)
		       ("dirichlet_rng" "dirichlet_rng(${1:vector alpha})$0\n" "dirichlet_rng(vector)" nil
			("Functions" "Continuous Distributions" "Simplex Distributions" "Dirichlet Distribution")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:row_vector x}, ${2:row_vector y})$0\n" "dot_product(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:row_vector x}, ${2:vector y})$0\n" "dot_product(row_vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:vector x}, ${2:row_vector y})$0\n" "dot_product(vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:vector x}, ${2:vector y})$0\n" "dot_product(vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_self" "dot_self(${1:row_vector x})$0\n" "dot_self(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_self" "dot_self(${1:vector x})$0\n" "dot_self(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("double_exponential_ccdf_log" "double_exponential_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "double_exponential_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_cdf" "double_exponential_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "double_exponential_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_cdf_log" "double_exponential_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "double_exponential_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_log" "double_exponential_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "double_exponential_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_rng" "double_exponential_rng(${1:real mu}, ${2:real sigma})$0\n" "double_exponential_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("e" "e()$0\n" "e()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("eigenvalues_sym" "eigenvalues_sym(${1:matrix A})$0\n" "eigenvalues_sym(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("eigenvectors_sym" "eigenvectors_sym(${1:matrix A})$0\n" "eigenvectors_sym(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("erf" "erf(${1:real x})$0\n" "erf(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("erfc" "erfc(${1:real x})$0\n" "erfc(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("exp" "exp(${1:matrix x})$0\n" "exp(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("exp" "exp(${1:real x})$0\n" "exp(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("exp" "exp(${1:row_vector x})$0\n" "exp(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("exp" "exp(${1:vector x})$0\n" "exp(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("exp2" "exp2(${1:real x})$0\n" "exp2(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("exp_mod_normal_ccdf_log" "exp_mod_normal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0\n" "exp_mod_normal_ccdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_cdf" "exp_mod_normal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0\n" "exp_mod_normal_cdf(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_cdf_log" "exp_mod_normal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0\n" "exp_mod_normal_cdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_log" "exp_mod_normal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0\n" "exp_mod_normal_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_rng" "exp_mod_normal_rng(${1:real mu}, ${2:real sigma}, ${3:real lambda})$0\n" "exp_mod_normal_rng(real, real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("expm1" "expm1(${1:real x})$0\n" "expm1(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("exponential_ccdf_log" "exponential_ccdf_log(${1:reals y}, ${2:reals beta})$0\n" "exponential_ccdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_cdf" "exponential_cdf(${1:reals y}, ${2:reals beta})$0\n" "exponential_cdf(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_cdf_log" "exponential_cdf_log(${1:reals y}, ${2:reals beta})$0\n" "exponential_cdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_log" "exponential_log(${1:reals y}, ${2:reals beta})$0\n" "exponential_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_rng" "exponential_rng(${1:real beta})$0\n" "exponential_rng(real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("fabs" "fabs(${1:real x})$0\n" "fabs(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("falling_factorial" "falling_factorial(${1:real x}, ${2:real n})$0\n" "falling_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("fdim" "fdim(${1:real x}, ${2:real y})$0\n" "fdim(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("floor" "floor(${1:real x})$0\n" "floor(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("fma" "fma(${1:real x}, ${2:real y}, ${3:real z})$0\n" "fma(real, real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("fmax" "fmax(${1:real x}, ${2:real y})$0\n" "fmax(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("fmin" "fmin(${1:real x}, ${2:real y})$0\n" "fmin(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("fmod" "fmod(${1:real x}, ${2:real y})$0\n" "fmod(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("gamma_ccdf_log" "gamma_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "gamma_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_cdf" "gamma_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "gamma_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_cdf_log" "gamma_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "gamma_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_log" "gamma_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "gamma_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_p" "gamma_p(${1:real a}, ${2:real z})$0\n" "gamma_p(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("gamma_q" "gamma_q(${1:real a}, ${2:real z})$0\n" "gamma_q(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("gamma_rng" "gamma_rng(${1:real alpha}, ${2:real beta})$0\n" "gamma_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gaussian_dlm_obs_log" "gaussian_dlm_obs_log(${1:vector y}, ${2:matrix F}, ${3:matrix G}, ${4:matrix V}, ${5:matrix W}, ${6:vector m0}, ${7:matrix C0})$0\n" "gaussian_dlm_obs_log(vector, matrix, matrix, matrix, matrix, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Gaussian Dynamic Linear Models")
			nil nil nil nil)
		       ("gumbel_ccdf_log" "gumbel_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0\n" "gumbel_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_cdf" "gumbel_cdf(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0\n" "gumbel_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_cdf_log" "gumbel_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0\n" "gumbel_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_log" "gumbel_log(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0\n" "gumbel_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_rng" "gumbel_rng(${1:real mu}, ${2:real beta})$0\n" "gumbel_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("head" "head(${1:T[] sv}, ${2:int n})$0\n" "head(T[], int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("head" "head(${1:row_vector rv}, ${2:int n})$0\n" "head(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("head" "head(${1:vector v}, ${2:int n})$0\n" "head(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("hypergeometric_log" "hypergeometric_log(${1:int n}, ${2:int N}, ${3:int a}, ${4:int b})$0\n" "hypergeometric_log(int, int, int, int)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Hypergeometric Distribution")
			nil nil nil nil)
		       ("hypergeometric_rng" "hypergeometric_rng(${1:int N}, ${2:real a}, ${3:real b})$0\n" "hypergeometric_rng(int, real, real)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Hypergeometric Distribution")
			nil nil nil nil)
		       ("hypot" "hypot(${1:real x}, ${2:real y})$0\n" "hypot(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("if_else" "if_else(${1:int cond}, ${2:real x}, ${3:real y})$0\n" "if_else(int, real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Logical Functions")
			nil nil nil nil)
		       ("increment_log_prob" "increment_log_prob(${1:real lp})$0\n" "increment_log_prob(real)" nil
			("Functions" "Built-In Functions" "Void Functions" "Increment Log Probability")
			nil nil nil nil)
		       ("int_step" "int_step(${1:int x})$0\n" "int_step(int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Absolute Functions")
			nil nil nil nil)
		       ("int_step" "int_step(${1:real x})$0\n" "int_step(real)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Absolute Functions")
			nil nil nil nil)
		       ("inv" "inv(${1:real x})$0\n" "inv(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("inv_chi_square_ccdf_log" "inv_chi_square_ccdf_log(${1:reals y}, ${2:reals nu})$0\n" "inv_chi_square_ccdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_cdf" "inv_chi_square_cdf(${1:reals y}, ${2:reals nu})$0\n" "inv_chi_square_cdf(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_cdf_log" "inv_chi_square_cdf_log(${1:reals y}, ${2:reals nu})$0\n" "inv_chi_square_cdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_log" "inv_chi_square_log(${1:reals y}, ${2:reals nu})$0\n" "inv_chi_square_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_rng" "inv_chi_square_rng(${1:real nu})$0\n" "inv_chi_square_rng(real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_cloglog" "inv_cloglog(${1:real y})$0\n" "inv_cloglog(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Link Functions")
			nil nil nil nil)
		       ("inv_gamma_ccdf_log" "inv_gamma_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "inv_gamma_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_cdf" "inv_gamma_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "inv_gamma_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_cdf_log" "inv_gamma_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "inv_gamma_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_log" "inv_gamma_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "inv_gamma_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_rng" "inv_gamma_rng(${1:real alpha}, ${2:real beta})$0\n" "inv_gamma_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_logit" "inv_logit(${1:real y})$0\n" "inv_logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Link Functions")
			nil nil nil nil)
		       ("inv_sqrt" "inv_sqrt(${1:real x})$0\n" "inv_sqrt(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("inv_square" "inv_square(${1:real x})$0\n" "inv_square(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("inv_wishart_log" "inv_wishart_log(${1:matrix W}, ${2:real nu}, ${3:matrix Sigma})$0\n" "inv_wishart_log(matrix, real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Inverse Wishart Distribution")
			nil nil nil nil)
		       ("inv_wishart_rng" "inv_wishart_rng(${1:real nu}, ${2:matrix Sigma})$0\n" "inv_wishart_rng(real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Inverse Wishart Distribution")
			nil nil nil nil)
		       ("inverse" "inverse(${1:matrix A})$0\n" "inverse(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("inverse_spd" "inverse_spd(${1:matrix A})$0\n" "inverse_spd(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("lbeta" "lbeta(${1:real alpha}, ${2:real beta})$0\n" "lbeta(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("lgamma" "lgamma(${1:real x})$0\n" "lgamma(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("lkj_corr_log" "lkj_corr_log(${1:matrix y}, ${2:real eta})$0\n" "lkj_corr_log(matrix, real)" nil
			("Functions" "Continuous Distributions" "Correlation Matrix Distributions" "LKJ Correlation Distribution")
			nil nil nil nil)
		       ("lkj_corr_rng" "lkj_corr_rng(${1:int K}, ${2:real eta})$0\n" "lkj_corr_rng(int, real)" nil
			("Functions" "Continuous Distributions" "Correlation Matrix Distributions" "LKJ Correlation Distribution")
			nil nil nil nil)
		       ("lkj_cov_log" "lkj_cov_log(${1:matrix W}, ${2:vector mu}, ${3:vector sigma}, ${4:real eta})$0\n" "lkj_cov_log(matrix, vector, vector, real)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "LKJ Covariance Distribution")
			nil nil nil nil)
		       ("lmgamma" "lmgamma(${1:int n}, ${2:real x})$0\n" "lmgamma(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("log" "log(${1:matrix x})$0\n" "log(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("log" "log(${1:real x})$0\n" "log(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("log" "log(${1:row_vector x})$0\n" "log(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("log" "log(${1:vector x})$0\n" "log(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("log10" "log10(${1:real x})$0\n" "log10(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("log10" "log10()$0\n" "log10()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("log1m" "log1m(${1:real x})$0\n" "log1m(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1m_exp" "log1m_exp(${1:real x})$0\n" "log1m_exp(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1m_inv_logit" "log1m_inv_logit(${1:real x})$0\n" "log1m_inv_logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1p" "log1p(${1:real x})$0\n" "log1p(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1p_exp" "log1p_exp(${1:real x})$0\n" "log1p_exp(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log2" "log2(${1:real x})$0\n" "log2(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("log2" "log2()$0\n" "log2()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("log_determinant" "log_determinant(${1:matrix A})$0\n" "log_determinant(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("log_diff_exp" "log_diff_exp(${1:real x}, ${2:real y})$0\n" "log_diff_exp(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log_falling_factorial" "log_falling_factorial(${1:real x}, ${2:real n})$0\n" "log_falling_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("log_inv_logit" "log_inv_logit(${1:real x})$0\n" "log_inv_logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log_rising_factorial" "log_rising_factorial(${1:real x}, ${2:real n})$0\n" "log_rising_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("log_softmax" "log_softmax(${1:vector x})$0\n" "log_softmax(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Special Matrix Functions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:matrix x})$0\n" "log_sum_exp(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:real x}, ${2:real y})$0\n" "log_sum_exp(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:real x[]})$0\n" "log_sum_exp(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:row_vector x})$0\n" "log_sum_exp(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:vector x})$0\n" "log_sum_exp(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("logistic_ccdf_log" "logistic_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "logistic_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_cdf" "logistic_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "logistic_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_cdf_log" "logistic_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "logistic_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_log" "logistic_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "logistic_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_rng" "logistic_rng(${1:real mu}, ${2:real sigma})$0\n" "logistic_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logit" "logit(${1:real x})$0\n" "logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Link Functions")
			nil nil nil nil)
		       ("lognormal_ccdf_log" "lognormal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "lognormal_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_cdf" "lognormal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "lognormal_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_cdf_log" "lognormal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "lognormal_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_log" "lognormal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "lognormal_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_rng" "lognormal_rng(${1:real mu}, ${2:real beta})$0\n" "lognormal_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("machine_precision" "machine_precision()$0\n" "machine_precision()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("max" "max(${1:int x}, ${2:int y})$0\n" "max(int, int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Bound Functions")
			nil nil nil nil)
		       ("max" "max(${1:int x[]})$0\n" "max(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:matrix x})$0\n" "max(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:real x[]})$0\n" "max(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:row_vector x})$0\n" "max(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:vector x})$0\n" "max(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("mdivide_left_tri_low" "mdivide_left_tri_low(${1:matrix a}, ${2:matrix b})$0\n" "mdivide_left_tri_low(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mdivide_left_tri_low" "mdivide_left_tri_low(${1:matrix a}, ${2:vector b})$0\n" "mdivide_left_tri_low(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mdivide_right_tri_low" "mdivide_right_tri_low(${1:matrix b}, ${2:matrix a})$0\n" "mdivide_right_tri_low(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mdivide_right_tri_low" "mdivide_right_tri_low(${1:row_vector b}, ${2:matrix a})$0\n" "mdivide_right_tri_low(row_vector, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mean" "mean(${1:matrix x})$0\n" "mean(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("mean" "mean(${1:real x[]})$0\n" "mean(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("mean" "mean(${1:row_vector x})$0\n" "mean(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("mean" "mean(${1:vector x})$0\n" "mean(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:int x}, ${2:int y})$0\n" "min(int, int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Bound Functions")
			nil nil nil nil)
		       ("min" "min(${1:int x[]})$0\n" "min(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:matrix x})$0\n" "min(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:real x[]})$0\n" "min(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:row_vector x})$0\n" "min(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:vector x})$0\n" "min(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("modified_bessel_first_kind" "modified_bessel_first_kind(${1:real v}, ${2:real z})$0\n" "modified_bessel_first_kind(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("modified_bessel_second_kind" "modified_bessel_second_kind(${1:real v}, ${2:real z})$0\n" "modified_bessel_second_kind(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("multi_normal_cholesky_log" "multi_normal_cholesky_log(${1:vector y}, ${2:vector mu}, ${3:matrix L})$0\n" "multi_normal_cholesky_log(vector, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution, Cholesky Parameterization")
			nil nil nil nil)
		       ("multi_normal_log" "multi_normal_log(${1:vector y}, ${2:vector mu}, ${3:matrix Sigma})$0\n" "multi_normal_log(vector, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution")
			nil nil nil nil)
		       ("multi_normal_prec_log" "multi_normal_prec_log(${1:vector y}, ${2:vector mu}, ${3:matrix Omega})$0\n" "multi_normal_prec_log(vector, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution, Precision Parameterization")
			nil nil nil nil)
		       ("multi_normal_rng" "multi_normal_rng(${1:vector mu}, ${2:matrix Sigma})$0\n" "multi_normal_rng(vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution")
			nil nil nil nil)
		       ("multi_student_t_log" "multi_student_t_log(${1:vector y}, ${2:real nu}, ${3:vector mu}, ${4:matrix Sigma})$0\n" "multi_student_t_log(vector, real, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Student-t Distribution")
			nil nil nil nil)
		       ("multi_student_t_rng" "multi_student_t_rng(${1:real nu}, ${2:vector mu}, ${3:matrix Sigma})$0\n" "multi_student_t_rng(real, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Student-t Distribution")
			nil nil nil nil)
		       ("multinomial_log" "multinomial_log(${1:int[] y}, ${2:vector theta}, ${3:int N})$0\n" "multinomial_log(int[], vector, int)" nil
			("Functions" "Discrete Distributions" "Multivariate Discrete Distributions" "Multinomial Distribution")
			nil nil nil nil)
		       ("multinomial_rng" "multinomial_rng(${1:vector theta}, ${2:int N})$0\n" "multinomial_rng(vector, int)" nil
			("Functions" "Discrete Distributions" "Multivariate Discrete Distributions" "Multinomial Distribution")
			nil nil nil nil)
		       ("multiply_log" "multiply_log(${1:real x}, ${2:real y})$0\n" "multiply_log(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("multiply_lower_tri_self_transpose" "multiply_lower_tri_self_transpose(${1:matrix x})$0\n" "multiply_lower_tri_self_transpose(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("neg_binomial_ccdf_log" "neg_binomial_ccdf_log(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0\n" "neg_binomial_ccdf_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_cdf" "neg_binomial_cdf(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0\n" "neg_binomial_cdf(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_cdf_log" "neg_binomial_cdf_log(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0\n" "neg_binomial_cdf_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_log" "neg_binomial_log(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0\n" "neg_binomial_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_rng" "neg_binomial_rng(${1:real alpha}, ${2:real beta})$0\n" "neg_binomial_rng(real, real)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("negative_infinity" "negative_infinity()$0\n" "negative_infinity()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("normal_ccdf_log" "normal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "normal_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_cdf" "normal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "normal_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_cdf_log" "normal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "normal_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_log" "normal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0\n" "normal_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_rng" "normal_rng(${1:real mu}, ${2:real sigma})$0\n" "normal_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("not_a_number" "not_a_number()$0\n" "not_a_number()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("ordered_logistic_log" "ordered_logistic_log(${1:int k}, ${2:real eta}, ${3:vector c})$0\n" "ordered_logistic_log(int, real, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Ordered Logistic Distribution")
			nil nil nil nil)
		       ("ordered_logistic_rng" "ordered_logistic_rng(${1:real eta}, ${2:vector c})$0\n" "ordered_logistic_rng(real, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Ordered Logistic Distribution")
			nil nil nil nil)
		       ("owens_t" "owens_t(${1:real h}, ${2:real a})$0\n" "owens_t(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("pareto_ccdf_log" "pareto_ccdf_log(${1:reals y}, ${2:reals y_min}, ${3:reals alpha})$0\n" "pareto_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_cdf" "pareto_cdf(${1:reals y}, ${2:reals y_min}, ${3:reals alpha})$0\n" "pareto_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_cdf_log" "pareto_cdf_log(${1:reals y}, ${2:reals y_min}, ${3:reals alpha})$0\n" "pareto_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_log" "pareto_log(${1:reals y}, ${2:reals y_min}, ${3:reals alpha})$0\n" "pareto_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_rng" "pareto_rng(${1:real y_min}, ${2:real alpha})$0\n" "pareto_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pi" "pi()$0\n" "pi()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("poisson_ccdf_log" "poisson_ccdf_log(${1:ints n}, ${2:reals lambda})$0\n" "poisson_ccdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_cdf" "poisson_cdf(${1:ints n}, ${2:reals lambda})$0\n" "poisson_cdf(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_cdf_log" "poisson_cdf_log(${1:ints n}, ${2:reals lambda})$0\n" "poisson_cdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_log" "poisson_log(${1:ints n}, ${2:reals lambda})$0\n" "poisson_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_log_log" "poisson_log_log(${1:ints n}, ${2:reals alpha})$0\n" "poisson_log_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution, Log Parameterization")
			nil nil nil nil)
		       ("poisson_rng" "poisson_rng(${1:real lambda})$0\n" "poisson_rng(real)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("positive_infinity" "positive_infinity()$0\n" "positive_infinity()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("pow" "pow(${1:real x}, ${2:real y})$0\n" "pow(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("prod" "prod(${1:int x[]})$0\n" "prod(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:matrix x})$0\n" "prod(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:real x[]})$0\n" "prod(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:row_vector x})$0\n" "prod(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:vector x})$0\n" "prod(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("quad_form" "quad_form(${1:matrix A}, ${2:matrix B})$0\n" "quad_form(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("quad_form" "quad_form(${1:matrix A}, ${2:vector B})$0\n" "quad_form(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rank" "rank(${1:int[] v}, ${2:int s})$0\n" "rank(int[], int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("rank" "rank(${1:real[] v}, ${2:int s})$0\n" "rank(real[], int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("rank" "rank(${1:row_vector v}, ${2:int s})$0\n" "rank(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("rank" "rank(${1:vector v}, ${2:int s})$0\n" "rank(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("rayleigh_ccdf_log" "rayleigh_ccdf_log(${1:real y}, ${2:real sigma})$0\n" "rayleigh_ccdf_log(real, real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_cdf" "rayleigh_cdf(${1:real y}, ${2:real sigma})$0\n" "rayleigh_cdf(real, real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_cdf_log" "rayleigh_cdf_log(${1:real y}, ${2:real sigma})$0\n" "rayleigh_cdf_log(real, real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_log" "rayleigh_log(${1:reals y}, ${2:reals sigma})$0\n" "rayleigh_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_rng" "rayleigh_rng(${1:real sigma})$0\n" "rayleigh_rng(real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:T x}, ${2:int k}, ${3:int m}, ${4:int n})$0\n" "rep_array(T, int, int, int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Broadcasting")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:T x}, ${2:int m}, ${3:int n})$0\n" "rep_array(T, int, int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Broadcasting")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:T x}, ${2:int n})$0\n" "rep_array(T, int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Broadcasting")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:real x}, ${2:int m}, ${3:int n})$0\n" "rep_matrix(real, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:row_vector rv}, ${2:int m})$0\n" "rep_matrix(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:vector v}, ${2:int n})$0\n" "rep_matrix(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_row_vector" "rep_row_vector(${1:real x}, ${2:int n})$0\n" "rep_row_vector(real, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_vector" "rep_vector(${1:real x}, ${2:int m})$0\n" "rep_vector(real, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rising_factorial" "rising_factorial(${1:real x}, ${2:real n})$0\n" "rising_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("round" "round(${1:real x})$0\n" "round(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("row" "row(${1:matrix x}, ${2:int m})$0\n" "row(matrix, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("rows" "rows(${1:matrix x})$0\n" "rows(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("rows" "rows(${1:row_vector x})$0\n" "rows(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("rows" "rows(${1:vector x})$0\n" "rows(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:matrix x}, ${2:matrix y})$0\n" "rows_dot_product(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:row_vector x}, ${2:row_vector y})$0\n" "rows_dot_product(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:vector x}, ${2:vector y})$0\n" "rows_dot_product(vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:matrix x})$0\n" "rows_dot_self(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:row_vector x})$0\n" "rows_dot_self(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:vector x})$0\n" "rows_dot_self(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("scaled_inv_chi_square_ccdf_log" "scaled_inv_chi_square_ccdf_log(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0\n" "scaled_inv_chi_square_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_cdf" "scaled_inv_chi_square_cdf(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0\n" "scaled_inv_chi_square_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_cdf_log" "scaled_inv_chi_square_cdf_log(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0\n" "scaled_inv_chi_square_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_log" "scaled_inv_chi_square_log(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0\n" "scaled_inv_chi_square_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_rng" "scaled_inv_chi_square_rng(${1:real nu}, ${2:real sigma})$0\n" "scaled_inv_chi_square_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("sd" "sd(${1:matrix x})$0\n" "sd(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sd" "sd(${1:real x[]})$0\n" "sd(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("sd" "sd(${1:row_vector x})$0\n" "sd(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sd" "sd(${1:vector x})$0\n" "sd(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("segment" "segment(${1:T[] sv}, ${2:int i}, ${3:int n})$0\n" "segment(T[], int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("segment" "segment(${1:row_vector v}, ${2:int i}, ${3:int n})$0\n" "segment(row_vector, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("segment" "segment(${1:vector v}, ${2:int i}, ${3:int n})$0\n" "segment(vector, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("sin" "sin(${1:real x})$0\n" "sin(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("singular_values" "singular_values(${1:matrix A})$0\n" "singular_values(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("sinh" "sinh(${1:real x})$0\n" "sinh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("size" "size(${1:T[] x})$0\n" "size(T[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Size and Dimension Function")
			nil nil nil nil)
		       ("skew_normal_ccdf_log" "skew_normal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0\n" "skew_normal_ccdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_cdf" "skew_normal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0\n" "skew_normal_cdf(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_cdf_log" "skew_normal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0\n" "skew_normal_cdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_log" "skew_normal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0\n" "skew_normal_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_rng" "skew_normal_rng(${1:real mu}, ${2:real sigma}, ${3:real alpha})$0\n" "skew_normal_rng(real, real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("softmax" "softmax(${1:vector x})$0\n" "softmax(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Special Matrix Functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:int[] v})$0\n" "sort_asc(int[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:real[] v})$0\n" "sort_asc(real[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:row_vector v})$0\n" "sort_asc(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:vector v})$0\n" "sort_asc(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:int[] v})$0\n" "sort_desc(int[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:real[] v})$0\n" "sort_desc(real[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:row_vector v})$0\n" "sort_desc(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:vector v})$0\n" "sort_desc(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sqrt" "sqrt(${1:real x})$0\n" "sqrt(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("sqrt2" "sqrt2()$0\n" "sqrt2()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("square" "square(${1:real x})$0\n" "square(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("step" "step(${1:real x})$0\n" "step(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Logical Functions")
			nil nil nil nil)
		       ("student_t_ccdf_log" "student_t_ccdf_log(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0\n" "student_t_ccdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_cdf" "student_t_cdf(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0\n" "student_t_cdf(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_cdf_log" "student_t_cdf_log(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0\n" "student_t_cdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_log" "student_t_log(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0\n" "student_t_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_rng" "student_t_rng(${1:real nu}, ${2:real mu}, ${3:real sigma})$0\n" "student_t_rng(real, real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("sub_col" "sub_col(${1:matrix x}, ${2:int i}, ${3:int j}, ${4:int n_rows})$0\n" "sub_col(matrix, int, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("sub_row" "sub_row(${1:matrix x}, ${2:int i}, ${3:int j}, ${4:int n_cols})$0\n" "sub_row(matrix, int, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("sum" "sum(${1:int x[]})$0\n" "sum(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:matrix x})$0\n" "sum(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:real x[]})$0\n" "sum(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:row_vector x})$0\n" "sum(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:vector x})$0\n" "sum(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("tail" "tail(${1:T[] sv}, ${2:int n})$0\n" "tail(T[], int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("tail" "tail(${1:row_vector rv}, ${2:int n})$0\n" "tail(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("tail" "tail(${1:vector v}, ${2:int n})$0\n" "tail(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("tan" "tan(${1:real x})$0\n" "tan(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("tanh" "tanh(${1:real x})$0\n" "tanh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("tcrossprod" "tcrossprod(${1:matrix x})$0\n" "tcrossprod(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("tgamma" "tgamma(${1:real x})$0\n" "tgamma(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:matrix m})$0\n" "to_vector(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:row_vector m})$0\n" "to_vector(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("trace" "trace(${1:matrix A})$0\n" "trace(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("trace_gen_quad_form" "trace_gen_quad_form(${1:matrix D}, ${2:matrix A}, ${3:matrix B})$0\n" "trace_gen_quad_form(matrix, matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("trace_quad_form" "trace_quad_form(${1:matrix A}, ${2:matrix B})$0\n" "trace_quad_form(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("trunc" "trunc(${1:real x})$0\n" "trunc(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("uniform_ccdf_log" "uniform_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "uniform_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_cdf" "uniform_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "uniform_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_cdf_log" "uniform_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "uniform_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_log" "uniform_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0\n" "uniform_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_rng" "uniform_rng(${1:real alpha}, ${2:real beta})$0\n" "uniform_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("variance" "variance(${1:matrix x})$0\n" "variance(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("variance" "variance(${1:real x[]})$0\n" "variance(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("variance" "variance(${1:row_vector x})$0\n" "variance(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("variance" "variance(${1:vector x})$0\n" "variance(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("von_mises_log" "von_mises_log(${1:reals y}, ${2:reals mu}, ${3:reals kappa})$0\n" "von_mises_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Circular Distributions" "Von Mises Distribution")
			nil nil nil nil)
		       ("weibull_ccdf_log" "weibull_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0\n" "weibull_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_cdf" "weibull_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0\n" "weibull_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_cdf_log" "weibull_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0\n" "weibull_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_log" "weibull_log(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0\n" "weibull_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_rng" "weibull_rng(${1:real alpha}, ${2:real sigma})$0\n" "weibull_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("wishart_log" "wishart_log(${1:matrix W}, ${2:real nu}, ${3:matrix Sigma})$0\n" "wishart_log(matrix, real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Wishart Distribution")
			nil nil nil nil)
		       ("wishart_rng" "wishart_rng(${1:real nu}, ${2:matrix Sigma})$0\n" "wishart_rng(real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Wishart Distribution")
			nil nil nil nil)))


;;; Do not edit! File generated at Fri Nov 29 16:33:36 2013
