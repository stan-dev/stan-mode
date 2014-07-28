;;; Compiled snippets and support files for `stan-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("cholesky_factor_corr" "cholesky_factor_corr[${1:expression}${2:${3:, ${4:expression}}}] ${5:variable}${6:[${7:dims}]};\n$0" "cholesky_factor_corr[] ... ;" nil
			("Types")
			nil nil nil nil)
		       ("cholesky_factor_cov" "cholesky_factor_cov[${1:expression}${2:${3:, ${4:expression}}}] ${5:variable}${6:[${7:dims}]};\n$0" "cholesky_factor_cov[] ... ;" nil
			("Types")
			nil nil nil nil)
		       ("corr_matrix" "corr_matrix[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "corr_matrix[] ... ;" nil
			("Types")
			nil nil nil nil)
		       ("cov_matrix" "cov_matrix[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "cov_matrix[] ... ;" nil
			("Types")
			nil nil nil nil)
		       ("data" "data {\n  $0\n}\n" "data {...}" nil
			("Blocks")
			nil nil nil nil)
		       ("elif" "else if (${1:condition}) {\n    $0\n}" "else if (...) { ... }" nil
			("Structure")
			nil nil nil nil)
		       ("else" "else {\n    $0\n}" "else { ... }" nil
			("Structure")
			nil nil nil nil)
		       ("for" "for (${1:i} in ${2:1}:${3:N}) {\n    $0\n}\n" "for (...; ...; ...) { ... }" nil
			("Structure")
			nil nil nil nil)
		       ("functions" "functions {\n  $0\n}\n" "functions {...}" nil
			("Blocks")
			nil nil nil nil)
		       ("generated" "generated quantities {\n  $0\n}\n" "generated quantities {...}" nil
			("Blocks")
			nil nil nil nil)
		       ("if" "if (${1:condition}) {\n    $0\n}" "if (...) { ... }" nil
			("Structure")
			nil nil nil nil)
		       ("increment_log_prob" "increment_log_prob(${1:lp});\n$0\n" "increment_log_prob(...);" nil nil nil nil nil nil)
		       ("int" "int${1:<${2:lower=...,upper=...}>} ${3:variable}${4:[${5:dims}]};\n$0\n" "int ... ;" nil
			("Types")
			nil nil nil nil)
		       ("<" "<lower=${1:0}>$0" "<lower=...>" nil
			("Range Constraints")
			nil nil nil nil)
		       ("<" "<lower=${1:0},upper=${2:1}>$0" "<lower=..., upper=...>" nil
			("Range Constraints")
			nil nil nil nil)
		       ("matrix" "matrix{1:<${2:lower=...,upper=...}>}[$3, $4] ${5:variable}${6:[${7:dims}]};\n$0" "matrix[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("model" "model {\n  $0\n}\n" "model {...}" nil
			("Blocks")
			nil nil nil nil)
		       ("ordered" "ordered[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "ordered[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("param" "parameters {\n  $0\n}\n" "parameters {...}" nil
			("Blocks")
			nil nil nil nil)
		       ("pordered" "positive_ordered[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "positive_ordered[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("print" "print($1);\n$0" "print(...)" nil nil nil nil nil nil)
		       ("C-c C-t" "data {\n  $0\n}\ntransformed data {\n}\nparameters {\n}\ntransformed parameters {\n}\nmodel {\n}\ngenerated quantities {\n}" "data {...} ..." nil
			("Blocks")
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
		       ("~" "~ ${1:$$(yas-choose-value stan-distribution-list)};\n$0" "~ distribution(...)" nil nil
			((yas-triggers-in-field 't))
			nil nil nil)
		       ("tdata" "transformed data {\n  $0\n}\n" "transformed data {...}" nil
			("Blocks")
			nil nil nil nil)
		       ("tparam" "transformed parameters {\n  $0\n}\n" "transformed parameters{...}" nil
			("Blocks")
			nil nil nil nil)
		       ("uvector" "unit_vector[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "unit_vector[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("<" "<upper=${1:0}>$0" "<upper=...>" nil
			("Range Constraints")
			nil nil nil nil)
		       ("vector" "vector${1:<${2:lower=...,upper=...}>}[${3:expression}] ${4:variable}${5:[${6:dims}]};\n$0" "vector[] ...;" nil
			("Types")
			nil nil nil nil)
		       ("while" "while (${1:condition}) {\n    $0\n}" "while (...) { ... }" nil
			("Structure")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("bernoulli" "bernoulli(${1:reals theta})$0" "bernoulli(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("bernoulli_logit" "bernoulli_logit(${1:reals alpha})$0" "bernoulli_logit(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("beta_binomial" "beta_binomial(${1:ints N}, ${2:reals alpha}, ${3:reals beta})$0" "beta_binomial(ints, reals, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("beta" "beta(${1:reals alpha}, ${2:reals beta})$0" "beta(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("binomial" "binomial(${1:ints N}, ${2:reals theta})$0" "binomial(ints, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("binomial_logit" "binomial_logit(${1:ints N}, ${2:reals alpha})$0" "binomial_logit(ints, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("categorical" "categorical(${1:vector theta})$0" "categorical(vector)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("categorical_logit" "categorical_logit(${1:vector beta})$0" "categorical_logit(vector)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("cauchy" "cauchy(${1:reals mu}, ${2:reals sigma})$0" "cauchy(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("chi_square" "chi_square(${1:reals nu})$0" "chi_square(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("dirichlet" "dirichlet(${1:vector alpha})$0" "dirichlet(vector)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("double_exponential" "double_exponential(${1:reals mu}, ${2:reals sigma})$0" "double_exponential(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("exp_mod_normal" "exp_mod_normal(${1:reals mu}, ${2:reals sigma}, ${3:reals lambda})$0" "exp_mod_normal(reals, reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("exponential" "exponential(${1:reals beta})$0" "exponential(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("gamma" "gamma(${1:reals alpha}, ${2:reals beta})$0" "gamma(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("gaussian_dlm_obs" "gaussian_dlm_obs(${1:matrix F}, ${2:matrix G}, ${3:matrix V}, ${4:matrix W}, ${5:vector m0}, ${6:matrix C0})$0" "gaussian_dlm_obs(matrix, matrix, matrix, matrix, vector, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("gaussian_dlm_obs" "gaussian_dlm_obs(${1:matrix F}, ${2:matrix G}, ${3:vector V}, ${4:matrix W}, ${5:vector m0}, ${6:matrix C0})$0" "gaussian_dlm_obs(matrix, matrix, vector, matrix, vector, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("gumbel" "gumbel(${1:reals mu}, ${2:reals beta})$0" "gumbel(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("hypergeometric" "hypergeometric(${1:int N}, ${2:int a}, ${3:int b})$0" "hypergeometric(int, int, int)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("inv_chi_square" "inv_chi_square(${1:reals nu})$0" "inv_chi_square(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("inv_gamma" "inv_gamma(${1:reals alpha}, ${2:reals beta})$0" "inv_gamma(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("inv_wishart" "inv_wishart(${1:real nu}, ${2:matrix Sigma})$0" "inv_wishart(real, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("lkj_corr_cholesky" "lkj_corr_cholesky(${1:real eta})$0" "lkj_corr_cholesky(real)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("lkj_corr" "lkj_corr(${1:real eta})$0" "lkj_corr(real)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("lkj_cov" "lkj_cov(${1:vector mu}, ${2:vector sigma}, ${3:real eta})$0" "lkj_cov(vector, vector, real)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("logistic" "logistic(${1:reals mu}, ${2:reals sigma})$0" "logistic(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("lognormal" "lognormal(${1:reals mu}, ${2:reals sigma})$0" "lognormal(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_gp" "multi_gp(${1:matrix Sigma}, ${2:vector w})$0" "multi_gp(matrix, vector)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_normal_cholesky" "multi_normal_cholesky()$0" "multi_normal_cholesky()" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_normal" "multi_normal()$0" "multi_normal()" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_normal_prec" "multi_normal_prec()$0" "multi_normal_prec()" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multi_student_t" "multi_student_t(${1:matrix Sigma})$0" "multi_student_t(matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("multinomial" "multinomial(${1:vector theta}, ${2:int N})$0" "multinomial(vector, int)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("neg_binomial_2" "neg_binomial_2(${1:reals mu}, ${2:reals phi})$0" "neg_binomial_2(reals, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("neg_binomial_2_log" "neg_binomial_2_log(${1:reals eta}, ${2:reals phi})$0" "neg_binomial_2_log(reals, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("neg_binomial" "neg_binomial(${1:reals alpha}, ${2:reals beta})$0" "neg_binomial(reals, reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("normal" "normal(${1:reals mu}, ${2:reals sigma})$0" "normal(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("ordered_logistic" "ordered_logistic(${1:real eta}, ${2:vector c})$0" "ordered_logistic(real, vector)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("pareto" "pareto(${1:reals y}, ${2:reals alpha})$0" "pareto(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("poisson" "poisson(${1:reals lambda})$0" "poisson(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("poisson_log" "poisson_log(${1:reals alpha})$0" "poisson_log(reals)" nil
			("Distributions" "Discrete")
			nil nil nil nil)
		       ("rayleigh" "rayleigh(${1:reals sigma})$0" "rayleigh(reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("scaled_inv_chi_square" "scaled_inv_chi_square(${1:reals nu}, ${2:reals sigma})$0" "scaled_inv_chi_square(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("skew_normal" "skew_normal(${1:reals mu}, ${2:reals sigma}, ${3:reals alpha})$0" "skew_normal(reals, reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("student_t" "student_t(${1:reals nu}, ${2:reals mu}, ${3:reals sigma})$0" "student_t(reals, reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("uniform" "uniform(${1:reals alpha}, ${2:reals beta})$0" "uniform(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("von_mises" "von_mises(${1:reals mu}, ${2:reals kappa})$0" "von_mises(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("weibull" "weibull(${1:reals alpha}, ${2:reals sigma})$0" "weibull(reals, reals)" nil
			("Distributions" "Continuous")
			nil nil nil nil)
		       ("wishart" "wishart(${1:real nu}, ${2:matrix Sigma})$0" "wishart(real, matrix)" nil
			("Distributions" "Continuous")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("Phi" "Phi(${1:real x})$0" "Phi(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("Phi_approx" "Phi_approx(${1:real x})$0" "Phi_approx(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("abs" "abs(${1:int x})$0" "abs(int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Absolute Functions")
			nil nil nil nil)
		       ("abs" "abs(${1:real x})$0" "abs(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("acos" "acos(${1:real x})$0" "acos(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("acosh" "acosh(${1:real x})$0" "acosh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("asin" "asin(${1:real x})$0" "asin(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("asinh" "asinh(${1:real x})$0" "asinh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("atan" "atan(${1:real x})$0" "atan(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("atan2" "atan2(${1:real x}, ${2:real y})$0" "atan2(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("atanh" "atanh(${1:real x})$0" "atanh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("bernoulli_ccdf_log" "bernoulli_ccdf_log(${1:ints y}, ${2:reals theta})$0" "bernoulli_ccdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_cdf" "bernoulli_cdf(${1:ints y}, ${2:reals theta})$0" "bernoulli_cdf(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_cdf_log" "bernoulli_cdf_log(${1:ints y}, ${2:reals theta})$0" "bernoulli_cdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_log" "bernoulli_log(${1:ints y}, ${2:reals theta})$0" "bernoulli_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bernoulli_logit_log" "bernoulli_logit_log(${1:ints y}, ${2:reals alpha})$0" "bernoulli_logit_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution, Logit Parameterization")
			nil nil nil nil)
		       ("bernoulli_rng" "bernoulli_rng(${1:real theta})$0" "bernoulli_rng(real)" nil
			("Functions" "Discrete Distributions" "Binary Distributions" "Bernoulli Distribution")
			nil nil nil nil)
		       ("bessel_first_kind" "bessel_first_kind(${1:int v}, ${2:real z})$0" "bessel_first_kind(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("bessel_second_kind" "bessel_second_kind(${1:int v}, ${2:real z})$0" "bessel_second_kind(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("beta_binomial_ccdf_log" "beta_binomial_ccdf_log(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0" "beta_binomial_ccdf_log(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_cdf" "beta_binomial_cdf(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0" "beta_binomial_cdf(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_cdf_log" "beta_binomial_cdf_log(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0" "beta_binomial_cdf_log(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_log" "beta_binomial_log(${1:ints n}, ${2:ints N}, ${3:reals alpha}, ${4:reals beta})$0" "beta_binomial_log(ints, ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_binomial_rng" "beta_binomial_rng(${1:int N}, ${2:real alpha}, ${3:real beta})$0" "beta_binomial_rng(int, real, real)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Beta-Binomial Distribution")
			nil nil nil nil)
		       ("beta_ccdf_log" "beta_ccdf_log(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0" "beta_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_cdf" "beta_cdf(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0" "beta_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_cdf_log" "beta_cdf_log(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0" "beta_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_log" "beta_log(${1:reals theta}, ${2:reals alpha}, ${3:reals beta})$0" "beta_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("beta_rng" "beta_rng(${1:real alpha}, ${2:real beta})$0" "beta_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Continuous Distributions on [0, 1]" "Beta Distribution")
			nil nil nil nil)
		       ("binary_log_loss" "binary_log_loss(${1:int y}, ${2:real y})$0" "binary_log_loss(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("binomial_ccdf_log" "binomial_ccdf_log(${1:ints n}, ${2:ints N}, ${3:reals theta})$0" "binomial_ccdf_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_cdf" "binomial_cdf(${1:ints n}, ${2:ints N}, ${3:reals theta})$0" "binomial_cdf(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_cdf_log" "binomial_cdf_log(${1:ints n}, ${2:ints N}, ${3:reals theta})$0" "binomial_cdf_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_coefficient_log" "binomial_coefficient_log(${1:real x}, ${2:real y})$0" "binomial_coefficient_log(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("binomial_log" "binomial_log(${1:ints n}, ${2:ints N}, ${3:reals theta})$0" "binomial_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("binomial_logit_log" "binomial_logit_log(${1:ints n}, ${2:ints N}, ${3:reals alpha})$0" "binomial_logit_log(ints, ints, reals)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution, Logit Parameterization")
			nil nil nil nil)
		       ("binomial_rng" "binomial_rng(${1:int N}, ${2:real theta})$0" "binomial_rng(int, real)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Binomial Distribution")
			nil nil nil nil)
		       ("block" "block(${1:matrix x}, ${2:int i}, ${3:int j}, ${4:int n}, ${5:int n})$0" "block(matrix, int, int, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("categorical_log" "categorical_log(${1:ints y}, ${2:vector theta})$0" "categorical_log(ints, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Categorical Distribution")
			nil nil nil nil)
		       ("categorical_logit_log" "categorical_logit_log(${1:ints y}, ${2:vector beta})$0" "categorical_logit_log(ints, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Categorical Distribution")
			nil nil nil nil)
		       ("categorical_rng" "categorical_rng(${1:vector theta})$0" "categorical_rng(vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Categorical Distribution")
			nil nil nil nil)
		       ("cauchy_ccdf_log" "cauchy_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "cauchy_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_cdf" "cauchy_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "cauchy_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_cdf_log" "cauchy_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "cauchy_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_log" "cauchy_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "cauchy_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cauchy_rng" "cauchy_rng(${1:real mu}, ${2:real sigma})$0" "cauchy_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Cauchy Distribution")
			nil nil nil nil)
		       ("cbrt" "cbrt(${1:real x})$0" "cbrt(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("ceil" "ceil(${1:real x})$0" "ceil(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("chi_square_ccdf_log" "chi_square_ccdf_log(${1:reals y}, ${2:reals nu})$0" "chi_square_ccdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_cdf" "chi_square_cdf(${1:reals y}, ${2:reals nu})$0" "chi_square_cdf(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_cdf_log" "chi_square_cdf_log(${1:reals y}, ${2:reals nu})$0" "chi_square_cdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_log" "chi_square_log(${1:reals y}, ${2:reals nu})$0" "chi_square_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("chi_square_rng" "chi_square_rng(${1:real nu})$0" "chi_square_rng(real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Chi-Square Distribution")
			nil nil nil nil)
		       ("cholesky_decompose" "cholesky_decompose(${1:matrix A})$0" "cholesky_decompose(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("col" "col(${1:matrix x}, ${2:int n})$0" "col(matrix, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("cols" "cols(${1:matrix x})$0" "cols(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("cols" "cols(${1:row_vector x})$0" "cols(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("cols" "cols(${1:vector x})$0" "cols(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:matrix x}, ${2:matrix y})$0" "columns_dot_product(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:row_vector x}, ${2:row_vector y})$0" "columns_dot_product(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:vector x}, ${2:vector y})$0" "columns_dot_product(vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:matrix x})$0" "columns_dot_self(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:row_vector x})$0" "columns_dot_self(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:vector x})$0" "columns_dot_self(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cos" "cos(${1:real x})$0" "cos(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("cosh" "cosh(${1:real x})$0" "cosh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("crossprod" "crossprod(${1:matrix x})$0" "crossprod(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:real[] x})$0" "cumulative_sum(real[])" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:row_vector rv})$0" "cumulative_sum(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:vector v})$0" "cumulative_sum(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("determinant" "determinant(${1:matrix A})$0" "determinant(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("diag_matrix" "diag_matrix(${1:vector x})$0" "diag_matrix(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:matrix m}, ${2:row_vector rv})$0" "diag_post_multiply(matrix, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:matrix m}, ${2:vector v})$0" "diag_post_multiply(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:row_vector rv}, ${2:matrix m})$0" "diag_pre_multiply(row_vector, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:vector v}, ${2:matrix m})$0" "diag_pre_multiply(vector, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("diagonal" "diagonal(${1:matrix x})$0" "diagonal(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("digamma" "digamma(${1:real x})$0" "digamma(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("dims" "dims(${1:T x})$0" "dims(T)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Size and Dimension Function")
			nil nil nil nil)
		       ("dirichlet_log" "dirichlet_log(${1:vector theta}, ${2:vector alpha})$0" "dirichlet_log(vector, vector)" nil
			("Functions" "Continuous Distributions" "Simplex Distributions" "Dirichlet Distribution")
			nil nil nil nil)
		       ("dirichlet_rng" "dirichlet_rng(${1:vector alpha})$0" "dirichlet_rng(vector)" nil
			("Functions" "Continuous Distributions" "Simplex Distributions" "Dirichlet Distribution")
			nil nil nil nil)
		       ("distance" "distance(${1:row_vector x}, ${2:row_vector y})$0" "distance(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("distance" "distance(${1:row_vector x}, ${2:vector y})$0" "distance(row_vector, vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("distance" "distance(${1:vector x}, ${2:row_vector y})$0" "distance(vector, row_vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("distance" "distance(${1:vector x}, ${2:vector y})$0" "distance(vector, vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:row_vector x}, ${2:row_vector y})$0" "dot_product(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:row_vector x}, ${2:vector y})$0" "dot_product(row_vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:vector x}, ${2:row_vector y})$0" "dot_product(vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:vector x}, ${2:vector y})$0" "dot_product(vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_self" "dot_self(${1:row_vector x})$0" "dot_self(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("dot_self" "dot_self(${1:vector x})$0" "dot_self(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("double_exponential_ccdf_log" "double_exponential_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "double_exponential_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_cdf" "double_exponential_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "double_exponential_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_cdf_log" "double_exponential_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "double_exponential_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_log" "double_exponential_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "double_exponential_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("double_exponential_rng" "double_exponential_rng(${1:real mu}, ${2:real sigma})$0" "double_exponential_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Double Exponential (Laplace) Distribution")
			nil nil nil nil)
		       ("e" "e()$0" "e()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("eigenvalues_sym" "eigenvalues_sym(${1:matrix A})$0" "eigenvalues_sym(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("eigenvectors_sym" "eigenvectors_sym(${1:matrix A})$0" "eigenvectors_sym(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("erf" "erf(${1:real x})$0" "erf(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("erfc" "erfc(${1:real x})$0" "erfc(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("exp" "exp(${1:matrix x})$0" "exp(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("exp" "exp(${1:real x})$0" "exp(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("exp" "exp(${1:row_vector x})$0" "exp(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("exp" "exp(${1:vector x})$0" "exp(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("exp2" "exp2(${1:real x})$0" "exp2(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("exp_mod_normal_ccdf_log" "exp_mod_normal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0" "exp_mod_normal_ccdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_cdf" "exp_mod_normal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0" "exp_mod_normal_cdf(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_cdf_log" "exp_mod_normal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0" "exp_mod_normal_cdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_log" "exp_mod_normal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals lambda})$0" "exp_mod_normal_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("exp_mod_normal_rng" "exp_mod_normal_rng(${1:real mu}, ${2:real sigma}, ${3:real lambda})$0" "exp_mod_normal_rng(real, real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Exponentially Modified Normal Distribution")
			nil nil nil nil)
		       ("expm1" "expm1(${1:real x})$0" "expm1(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("exponential_ccdf_log" "exponential_ccdf_log(${1:reals y}, ${2:reals beta})$0" "exponential_ccdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_cdf" "exponential_cdf(${1:reals y}, ${2:reals beta})$0" "exponential_cdf(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_cdf_log" "exponential_cdf_log(${1:reals y}, ${2:reals beta})$0" "exponential_cdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_log" "exponential_log(${1:reals y}, ${2:reals beta})$0" "exponential_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("exponential_rng" "exponential_rng(${1:real beta})$0" "exponential_rng(real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Exponential Distribution")
			nil nil nil nil)
		       ("fabs" "fabs(${1:real x})$0" "fabs(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("falling_factorial" "falling_factorial(${1:real x}, ${2:real n})$0" "falling_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("fdim" "fdim(${1:real x}, ${2:real y})$0" "fdim(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("floor" "floor(${1:real x})$0" "floor(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("fma" "fma(${1:real x}, ${2:real y}, ${3:real z})$0" "fma(real, real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("fmax" "fmax(${1:real x}, ${2:real y})$0" "fmax(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("fmin" "fmin(${1:real x}, ${2:real y})$0" "fmin(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("fmod" "fmod(${1:real x}, ${2:real y})$0" "fmod(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("gamma_ccdf_log" "gamma_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "gamma_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_cdf" "gamma_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "gamma_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_cdf_log" "gamma_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "gamma_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_log" "gamma_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "gamma_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gamma_p" "gamma_p(${1:real a}, ${2:real z})$0" "gamma_p(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("gamma_q" "gamma_q(${1:real a}, ${2:real z})$0" "gamma_q(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("gamma_rng" "gamma_rng(${1:real alpha}, ${2:real beta})$0" "gamma_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Gamma Distribution")
			nil nil nil nil)
		       ("gaussian_dlm_obs_log" "gaussian_dlm_obs_log(${1:vector y}, ${2:matrix F}, ${3:matrix G}, ${4:matrix V}, ${5:matrix W}, ${6:vector m0}, ${7:matrix C0})$0" "gaussian_dlm_obs_log(vector, matrix, matrix, matrix, matrix, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Gaussian Dynamic Linear Models")
			nil nil nil nil)
		       ("gaussian_dlm_obs_log" "gaussian_dlm_obs_log(${1:vector y}, ${2:matrix F}, ${3:matrix G}, ${4:vector V}, ${5:matrix W}, ${6:vector m0}, ${7:matrix C0})$0" "gaussian_dlm_obs_log(vector, matrix, matrix, vector, matrix, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Gaussian Dynamic Linear Models")
			nil nil nil nil)
		       ("gumbel_ccdf_log" "gumbel_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0" "gumbel_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_cdf" "gumbel_cdf(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0" "gumbel_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_cdf_log" "gumbel_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0" "gumbel_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_log" "gumbel_log(${1:reals y}, ${2:reals mu}, ${3:reals beta})$0" "gumbel_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("gumbel_rng" "gumbel_rng(${1:real mu}, ${2:real beta})$0" "gumbel_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Gumbel Distribution")
			nil nil nil nil)
		       ("head" "head(${1:T[] sv}, ${2:int n})$0" "head(T[], int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("head" "head(${1:row_vector rv}, ${2:int n})$0" "head(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("head" "head(${1:vector v}, ${2:int n})$0" "head(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("hypergeometric_log" "hypergeometric_log(${1:int n}, ${2:int N}, ${3:int a}, ${4:int b})$0" "hypergeometric_log(int, int, int, int)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Hypergeometric Distribution")
			nil nil nil nil)
		       ("hypergeometric_rng" "hypergeometric_rng(${1:int N}, ${2:real a}, ${3:real b})$0" "hypergeometric_rng(int, real, real)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Hypergeometric Distribution")
			nil nil nil nil)
		       ("hypot" "hypot(${1:real x}, ${2:real y})$0" "hypot(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("if_else" "if_else(${1:int cond}, ${2:real x}, ${3:real y})$0" "if_else(int, real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Logical Functions")
			nil nil nil nil)
		       ("int_step" "int_step(${1:int x})$0" "int_step(int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Absolute Functions")
			nil nil nil nil)
		       ("int_step" "int_step(${1:real x})$0" "int_step(real)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Absolute Functions")
			nil nil nil nil)
		       ("inv" "inv(${1:real x})$0" "inv(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("inv_chi_square_ccdf_log" "inv_chi_square_ccdf_log(${1:reals y}, ${2:reals nu})$0" "inv_chi_square_ccdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_cdf" "inv_chi_square_cdf(${1:reals y}, ${2:reals nu})$0" "inv_chi_square_cdf(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_cdf_log" "inv_chi_square_cdf_log(${1:reals y}, ${2:reals nu})$0" "inv_chi_square_cdf_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_log" "inv_chi_square_log(${1:reals y}, ${2:reals nu})$0" "inv_chi_square_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_chi_square_rng" "inv_chi_square_rng(${1:real nu})$0" "inv_chi_square_rng(real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("inv_cloglog" "inv_cloglog(${1:real y})$0" "inv_cloglog(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Link Functions")
			nil nil nil nil)
		       ("inv_gamma_ccdf_log" "inv_gamma_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "inv_gamma_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_cdf" "inv_gamma_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "inv_gamma_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_cdf_log" "inv_gamma_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "inv_gamma_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_log" "inv_gamma_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "inv_gamma_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_gamma_rng" "inv_gamma_rng(${1:real alpha}, ${2:real beta})$0" "inv_gamma_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Inverse Gamma Distribution")
			nil nil nil nil)
		       ("inv_logit" "inv_logit(${1:real y})$0" "inv_logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Link Functions")
			nil nil nil nil)
		       ("inv_sqrt" "inv_sqrt(${1:real x})$0" "inv_sqrt(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("inv_square" "inv_square(${1:real x})$0" "inv_square(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("inv_wishart_log" "inv_wishart_log(${1:matrix W}, ${2:real nu}, ${3:matrix Sigma})$0" "inv_wishart_log(matrix, real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Inverse Wishart Distribution")
			nil nil nil nil)
		       ("inv_wishart_rng" "inv_wishart_rng(${1:real nu}, ${2:matrix Sigma})$0" "inv_wishart_rng(real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Inverse Wishart Distribution")
			nil nil nil nil)
		       ("inverse" "inverse(${1:matrix A})$0" "inverse(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("inverse_spd" "inverse_spd(${1:matrix A})$0" "inverse_spd(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("lbeta" "lbeta(${1:real alpha}, ${2:real beta})$0" "lbeta(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("lgamma" "lgamma(${1:real x})$0" "lgamma(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("lkj_corr_cholesky_log" "lkj_corr_cholesky_log(${1:matrix L}, ${2:real eta})$0" "lkj_corr_cholesky_log(matrix, real)" nil
			("Functions" "Continuous Distributions" "Cholesky LKJ Correlation Distribution       ")
			nil nil nil nil)
		       ("lkj_corr_cholesky_rng" "lkj_corr_cholesky_rng(${1:int K}, ${2:real eta})$0" "lkj_corr_cholesky_rng(int, real)" nil
			("Functions" "Continuous Distributions" "Cholesky LKJ Correlation Distribution       ")
			nil nil nil nil)
		       ("lkj_corr_log" "lkj_corr_log(${1:matrix y}, ${2:real eta})$0" "lkj_corr_log(matrix, real)" nil
			("Functions" "Continuous Distributions" "Correlation Matrix Distributions" "LKJ Correlation Distribution")
			nil nil nil nil)
		       ("lkj_corr_rng" "lkj_corr_rng(${1:int K}, ${2:real eta})$0" "lkj_corr_rng(int, real)" nil
			("Functions" "Continuous Distributions" "Correlation Matrix Distributions" "LKJ Correlation Distribution")
			nil nil nil nil)
		       ("lkj_cov_log" "lkj_cov_log(${1:matrix W}, ${2:vector mu}, ${3:vector sigma}, ${4:real eta})$0" "lkj_cov_log(matrix, vector, vector, real)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "LKJ Covariance Distribution")
			nil nil nil nil)
		       ("lmgamma" "lmgamma(${1:int n}, ${2:real x})$0" "lmgamma(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("log" "log(${1:matrix x})$0" "log(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("log" "log(${1:real x})$0" "log(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("log" "log(${1:row_vector x})$0" "log(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("log" "log(${1:vector x})$0" "log(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("log10" "log10(${1:real x})$0" "log10(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("log10" "log10()$0" "log10()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("log1m" "log1m(${1:real x})$0" "log1m(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1m_exp" "log1m_exp(${1:real x})$0" "log1m_exp(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1m_inv_logit" "log1m_inv_logit(${1:real x})$0" "log1m_inv_logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1p" "log1p(${1:real x})$0" "log1p(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log1p_exp" "log1p_exp(${1:real x})$0" "log1p_exp(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log2" "log2(${1:real x})$0" "log2(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("log2" "log2()$0" "log2()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("log_determinant" "log_determinant(${1:matrix A})$0" "log_determinant(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("log_diff_exp" "log_diff_exp(${1:real x}, ${2:real y})$0" "log_diff_exp(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log_falling_factorial" "log_falling_factorial(${1:real x}, ${2:real n})$0" "log_falling_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("log_inv_logit" "log_inv_logit(${1:real x})$0" "log_inv_logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log_rising_factorial" "log_rising_factorial(${1:real x}, ${2:real n})$0" "log_rising_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("log_softmax" "log_softmax(${1:vector x})$0" "log_softmax(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Special Matrix Functions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:matrix x})$0" "log_sum_exp(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:real x}, ${2:real y})$0" "log_sum_exp(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:real x})$0" "log_sum_exp(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:row_vector x})$0" "log_sum_exp(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:vector x})$0" "log_sum_exp(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("logistic_ccdf_log" "logistic_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "logistic_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_cdf" "logistic_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "logistic_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_cdf_log" "logistic_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "logistic_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_log" "logistic_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "logistic_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logistic_rng" "logistic_rng(${1:real mu}, ${2:real sigma})$0" "logistic_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Logistic Distribution")
			nil nil nil nil)
		       ("logit" "logit(${1:real x})$0" "logit(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Link Functions")
			nil nil nil nil)
		       ("lognormal_ccdf_log" "lognormal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "lognormal_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_cdf" "lognormal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "lognormal_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_cdf_log" "lognormal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "lognormal_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_log" "lognormal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "lognormal_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("lognormal_rng" "lognormal_rng(${1:real mu}, ${2:real beta})$0" "lognormal_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Lognormal Distribution")
			nil nil nil nil)
		       ("machine_precision" "machine_precision()$0" "machine_precision()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("max" "max(${1:int x}, ${2:int y})$0" "max(int, int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Bound Functions")
			nil nil nil nil)
		       ("max" "max(${1:int x})$0" "max(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:matrix x})$0" "max(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:real x})$0" "max(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:row_vector x})$0" "max(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("max" "max(${1:vector x})$0" "max(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("mdivide_left_tri_low" "mdivide_left_tri_low(${1:matrix a}, ${2:matrix b})$0" "mdivide_left_tri_low(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mdivide_left_tri_low" "mdivide_left_tri_low(${1:matrix a}, ${2:vector b})$0" "mdivide_left_tri_low(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mdivide_right_tri_low" "mdivide_right_tri_low(${1:matrix b}, ${2:matrix a})$0" "mdivide_right_tri_low(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mdivide_right_tri_low" "mdivide_right_tri_low(${1:row_vector b}, ${2:matrix a})$0" "mdivide_right_tri_low(row_vector, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("mean" "mean(${1:matrix x})$0" "mean(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("mean" "mean(${1:real x})$0" "mean(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("mean" "mean(${1:row_vector x})$0" "mean(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("mean" "mean(${1:vector x})$0" "mean(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:int x}, ${2:int y})$0" "min(int, int)" nil
			("Functions" "Built-In Functions" "Integer-Valued Basic Functions" "Bound Functions")
			nil nil nil nil)
		       ("min" "min(${1:int x})$0" "min(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:matrix x})$0" "min(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:real x})$0" "min(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:row_vector x})$0" "min(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("min" "min(${1:vector x})$0" "min(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("modified_bessel_first_kind" "modified_bessel_first_kind(${1:int v}, ${2:real z})$0" "modified_bessel_first_kind(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("modified_bessel_second_kind" "modified_bessel_second_kind(${1:int v}, ${2:real z})$0" "modified_bessel_second_kind(int, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("multi_gp_log" "multi_gp_log(${1:vector y}, ${2:matrix Sigma}, ${3:vector w})$0" "multi_gp_log(vector, matrix, vector)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Gaussian Process Distribution    ")
			nil nil nil nil)
		       ("multi_normal_cholesky_log" "multi_normal_cholesky_log(${1:matrix L})$0" "multi_normal_cholesky_log(matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution, Cholesky Parameterization")
			nil nil nil nil)
		       ("multi_normal_cholesky_rng" "multi_normal_cholesky_rng(${1:vector mu}, ${2:matrix L})$0" "multi_normal_cholesky_rng(vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution, Cholesky Parameterization")
			nil nil nil nil)
		       ("multi_normal_log" "multi_normal_log(${1:matrix Sigma})$0" "multi_normal_log(matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution")
			nil nil nil nil)
		       ("multi_normal_prec_log" "multi_normal_prec_log(${1:matrix Omega})$0" "multi_normal_prec_log(matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution, Precision Parameterization")
			nil nil nil nil)
		       ("multi_normal_rng" "multi_normal_rng(${1:vector mu}, ${2:matrix Sigma})$0" "multi_normal_rng(vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Normal Distribution")
			nil nil nil nil)
		       ("multi_student_t_log" "multi_student_t_log(${1:real nu}, ${2:matrix Sigma})$0" "multi_student_t_log(real, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Student-t Distribution")
			nil nil nil nil)
		       ("multi_student_t_rng" "multi_student_t_rng(${1:real nu}, ${2:vector mu}, ${3:matrix Sigma})$0" "multi_student_t_rng(real, vector, matrix)" nil
			("Functions" "Continuous Distributions" "Distributions over Unbounded Vectors" "Multivariate Student-t Distribution")
			nil nil nil nil)
		       ("multinomial_log" "multinomial_log(${1:int[] y}, ${2:vector theta}, ${3:int N})$0" "multinomial_log(int[], vector, int)" nil
			("Functions" "Discrete Distributions" "Multivariate Discrete Distributions" "Multinomial Distribution")
			nil nil nil nil)
		       ("multinomial_rng" "multinomial_rng(${1:vector theta}, ${2:int N})$0" "multinomial_rng(vector, int)" nil
			("Functions" "Discrete Distributions" "Multivariate Discrete Distributions" "Multinomial Distribution")
			nil nil nil nil)
		       ("multiply_log" "multiply_log(${1:real x}, ${2:real y})$0" "multiply_log(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Composed Functions")
			nil nil nil nil)
		       ("multiply_lower_tri_self_transpose" "multiply_lower_tri_self_transpose(${1:matrix x})$0" "multiply_lower_tri_self_transpose(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("neg_binomial_2_log" "neg_binomial_2_log(${1:ints y}, ${2:reals mu}, ${3:reals phi})$0" "neg_binomial_2_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution, alternative parameterization    ")
			nil nil nil nil)
		       ("neg_binomial_2_log_log" "neg_binomial_2_log_log(${1:ints y}, ${2:reals eta}, ${3:reals phi})$0" "neg_binomial_2_log_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution, alternative parameterization    ")
			nil nil nil nil)
		       ("neg_binomial_2_log_rng" "neg_binomial_2_log_rng(${1:real eta}, ${2:real phi})$0" "neg_binomial_2_log_rng(real, real)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution, alternative parameterization    ")
			nil nil nil nil)
		       ("neg_binomial_2_rng" "neg_binomial_2_rng(${1:real mu}, ${2:real phi})$0" "neg_binomial_2_rng(real, real)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution, alternative parameterization    ")
			nil nil nil nil)
		       ("neg_binomial_ccdf_log" "neg_binomial_ccdf_log(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0" "neg_binomial_ccdf_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_cdf" "neg_binomial_cdf(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0" "neg_binomial_cdf(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_cdf_log" "neg_binomial_cdf_log(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0" "neg_binomial_cdf_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_log" "neg_binomial_log(${1:ints n}, ${2:reals alpha}, ${3:reals beta})$0" "neg_binomial_log(ints, reals, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("neg_binomial_rng" "neg_binomial_rng(${1:real alpha}, ${2:real beta})$0" "neg_binomial_rng(real, real)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Negative Binomial Distribution")
			nil nil nil nil)
		       ("negative_infinity" "negative_infinity()$0" "negative_infinity()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("normal_ccdf_log" "normal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "normal_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_cdf" "normal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "normal_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_cdf_log" "normal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "normal_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_log" "normal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma})$0" "normal_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("normal_rng" "normal_rng(${1:real mu}, ${2:real sigma})$0" "normal_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Normal Distribution")
			nil nil nil nil)
		       ("not_a_number" "not_a_number()$0" "not_a_number()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("ordered_logistic_log" "ordered_logistic_log(${1:int k}, ${2:real eta}, ${3:vector c})$0" "ordered_logistic_log(int, real, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Ordered Logistic Distribution")
			nil nil nil nil)
		       ("ordered_logistic_rng" "ordered_logistic_rng(${1:real eta}, ${2:vector c})$0" "ordered_logistic_rng(real, vector)" nil
			("Functions" "Discrete Distributions" "Bounded Discrete Distributions" "Ordered Logistic Distribution")
			nil nil nil nil)
		       ("owens_t" "owens_t(${1:real h}, ${2:real a})$0" "owens_t(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Probability-Related Functions")
			nil nil nil nil)
		       ("pareto_ccdf_log" "pareto_ccdf_log(${1:reals y}, ${2:reals y}, ${3:reals alpha})$0" "pareto_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_cdf" "pareto_cdf(${1:reals y}, ${2:reals y}, ${3:reals alpha})$0" "pareto_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_cdf_log" "pareto_cdf_log(${1:reals y}, ${2:reals y}, ${3:reals alpha})$0" "pareto_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_log" "pareto_log(${1:reals y}, ${2:reals y}, ${3:reals alpha})$0" "pareto_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pareto_rng" "pareto_rng(${1:real y}, ${2:real alpha})$0" "pareto_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Lower-Bounded Probabilities" "Pareto Distribution")
			nil nil nil nil)
		       ("pi" "pi()$0" "pi()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("poisson_ccdf_log" "poisson_ccdf_log(${1:ints n}, ${2:reals lambda})$0" "poisson_ccdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_cdf" "poisson_cdf(${1:ints n}, ${2:reals lambda})$0" "poisson_cdf(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_cdf_log" "poisson_cdf_log(${1:ints n}, ${2:reals lambda})$0" "poisson_cdf_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_log" "poisson_log(${1:ints n}, ${2:reals lambda})$0" "poisson_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("poisson_log_log" "poisson_log_log(${1:ints n}, ${2:reals alpha})$0" "poisson_log_log(ints, reals)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution, Log Parameterization")
			nil nil nil nil)
		       ("poisson_rng" "poisson_rng(${1:real lambda})$0" "poisson_rng(real)" nil
			("Functions" "Discrete Distributions" "Unbounded Discrete Distributions" "Poisson Distribution")
			nil nil nil nil)
		       ("positive_infinity" "positive_infinity()$0" "positive_infinity()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Special Values")
			nil nil nil nil)
		       ("pow" "pow(${1:real x}, ${2:real y})$0" "pow(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("prod" "prod(${1:int x})$0" "prod(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:matrix x})$0" "prod(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:real x})$0" "prod(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:row_vector x})$0" "prod(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("prod" "prod(${1:vector x})$0" "prod(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("qr_Q" "qr_Q(${1:matrix A})$0" "qr_Q(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("qr_R" "qr_R(${1:matrix A})$0" "qr_R(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("quad_form" "quad_form(${1:matrix A}, ${2:matrix B})$0" "quad_form(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("quad_form" "quad_form(${1:matrix A}, ${2:vector B})$0" "quad_form(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("quad_form_diag" "quad_form_diag(${1:matrix m}, ${2:row_vector rv})$0" "quad_form_diag(matrix, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("quad_form_diag" "quad_form_diag(${1:matrix m}, ${2:vector v})$0" "quad_form_diag(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("quad_form_sym" "quad_form_sym(${1:matrix A}, ${2:matrix B})$0" "quad_form_sym(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("quad_form_sym" "quad_form_sym(${1:matrix A}, ${2:vector B})$0" "quad_form_sym(matrix, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rank" "rank(${1:int[] v}, ${2:int s})$0" "rank(int[], int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("rank" "rank(${1:real[] v}, ${2:int s})$0" "rank(real[], int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("rank" "rank(${1:row_vector v}, ${2:int s})$0" "rank(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("rank" "rank(${1:vector v}, ${2:int s})$0" "rank(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("rayleigh_ccdf_log" "rayleigh_ccdf_log(${1:real y}, ${2:real sigma})$0" "rayleigh_ccdf_log(real, real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_cdf" "rayleigh_cdf(${1:real y}, ${2:real sigma})$0" "rayleigh_cdf(real, real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_cdf_log" "rayleigh_cdf_log(${1:real y}, ${2:real sigma})$0" "rayleigh_cdf_log(real, real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_log" "rayleigh_log(${1:reals y}, ${2:reals sigma})$0" "rayleigh_log(reals, reals)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rayleigh_rng" "rayleigh_rng(${1:real sigma})$0" "rayleigh_rng(real)" nil
			("Functions" "Continuous Distributions" "Non-negative Continuous Distributions" "Rayleigh Distribution")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:T x}, ${2:int k}, ${3:int m}, ${4:int n})$0" "rep_array(T, int, int, int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Broadcasting")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:T x}, ${2:int m}, ${3:int n})$0" "rep_array(T, int, int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Broadcasting")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:T x}, ${2:int n})$0" "rep_array(T, int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Broadcasting")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:real x}, ${2:int m}, ${3:int n})$0" "rep_matrix(real, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:row_vector rv}, ${2:int m})$0" "rep_matrix(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:vector v}, ${2:int n})$0" "rep_matrix(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_row_vector" "rep_row_vector(${1:real x}, ${2:int n})$0" "rep_row_vector(real, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rep_vector" "rep_vector(${1:real x}, ${2:int m})$0" "rep_vector(real, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Broadcast Functions")
			nil nil nil nil)
		       ("rising_factorial" "rising_factorial(${1:real x}, ${2:real n})$0" "rising_factorial(real, real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("round" "round(${1:real x})$0" "round(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("row" "row(${1:matrix x}, ${2:int m})$0" "row(matrix, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("rows" "rows(${1:matrix x})$0" "rows(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("rows" "rows(${1:row_vector x})$0" "rows(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("rows" "rows(${1:vector x})$0" "rows(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Integer-Valued Matrix Size Functions")
			nil nil nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:matrix x}, ${2:matrix y})$0" "rows_dot_product(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:row_vector x}, ${2:row_vector y})$0" "rows_dot_product(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:vector x}, ${2:vector y})$0" "rows_dot_product(vector, vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:matrix x})$0" "rows_dot_self(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:row_vector x})$0" "rows_dot_self(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:vector x})$0" "rows_dot_self(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("scaled_inv_chi_square_ccdf_log" "scaled_inv_chi_square_ccdf_log(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0" "scaled_inv_chi_square_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_cdf" "scaled_inv_chi_square_cdf(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0" "scaled_inv_chi_square_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_cdf_log" "scaled_inv_chi_square_cdf_log(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0" "scaled_inv_chi_square_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_log" "scaled_inv_chi_square_log(${1:reals y}, ${2:reals nu}, ${3:reals sigma})$0" "scaled_inv_chi_square_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("scaled_inv_chi_square_rng" "scaled_inv_chi_square_rng(${1:real nu}, ${2:real sigma})$0" "scaled_inv_chi_square_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Scaled Inverse Chi-Square Distribution")
			nil nil nil nil)
		       ("sd" "sd(${1:matrix x})$0" "sd(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sd" "sd(${1:real x})$0" "sd(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("sd" "sd(${1:row_vector x})$0" "sd(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sd" "sd(${1:vector x})$0" "sd(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("segment" "segment(${1:T[] sv}, ${2:int i}, ${3:int n})$0" "segment(T[], int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("segment" "segment(${1:row_vector v}, ${2:int i}, ${3:int n})$0" "segment(row_vector, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("segment" "segment(${1:vector v}, ${2:int i}, ${3:int n})$0" "segment(vector, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("sin" "sin(${1:real x})$0" "sin(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("singular_values" "singular_values(${1:matrix A})$0" "singular_values(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("sinh" "sinh(${1:real x})$0" "sinh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("size" "size(${1:T[] x})$0" "size(T[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Array Size and Dimension Function")
			nil nil nil nil)
		       ("skew_normal_ccdf_log" "skew_normal_ccdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0" "skew_normal_ccdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_cdf" "skew_normal_cdf(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0" "skew_normal_cdf(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_cdf_log" "skew_normal_cdf_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0" "skew_normal_cdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_log" "skew_normal_log(${1:reals y}, ${2:reals mu}, ${3:reals sigma}, ${4:reals alpha})$0" "skew_normal_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("skew_normal_rng" "skew_normal_rng(${1:real mu}, ${2:real sigma}, ${3:real alpha})$0" "skew_normal_rng(real, real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Skew Normal Distribution")
			nil nil nil nil)
		       ("softmax" "softmax(${1:vector x})$0" "softmax(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Special Matrix Functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:int[] v})$0" "sort_asc(int[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:real[] v})$0" "sort_asc(real[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:row_vector v})$0" "sort_asc(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:vector v})$0" "sort_asc(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:int[] v})$0" "sort_desc(int[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:real[] v})$0" "sort_desc(real[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:row_vector v})$0" "sort_desc(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:vector v})$0" "sort_desc(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_asc" "sort_indices_asc(${1:int[] v})$0" "sort_indices_asc(int[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_asc" "sort_indices_asc(${1:real[] v})$0" "sort_indices_asc(real[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_asc" "sort_indices_asc(${1:row_vector v})$0" "sort_indices_asc(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_asc" "sort_indices_asc(${1:vector v})$0" "sort_indices_asc(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_desc" "sort_indices_desc(${1:int[] v})$0" "sort_indices_desc(int[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_desc" "sort_indices_desc(${1:real[] v})$0" "sort_indices_desc(real[])" nil
			("Functions" "Built-In Functions" "Array Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_desc" "sort_indices_desc(${1:row_vector v})$0" "sort_indices_desc(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sort_indices_desc" "sort_indices_desc(${1:vector v})$0" "sort_indices_desc(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Other functions")
			nil nil nil nil)
		       ("sqrt" "sqrt(${1:real x})$0" "sqrt(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("sqrt2" "sqrt2()$0" "sqrt2()" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Mathematical Constants")
			nil nil nil nil)
		       ("square" "square(${1:real x})$0" "square(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Power and Logarithm Functions")
			nil nil nil nil)
		       ("squared_distance" "squared_distance(${1:row_vector x}, ${2:row_vector y})$0" "squared_distance(row_vector, row_vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("squared_distance" "squared_distance(${1:row_vector x}, ${2:vector y})$0" "squared_distance(row_vector, vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("squared_distance" "squared_distance(${1:vector x}, ${2:row_vector y})$0" "squared_distance(vector, row_vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("squared_distance" "squared_distance(${1:vector x}, ${2:vector y})$0" "squared_distance(vector, vector)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("step" "step(${1:real x})$0" "step(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Logical Functions")
			nil nil nil nil)
		       ("student_t_ccdf_log" "student_t_ccdf_log(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0" "student_t_ccdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_cdf" "student_t_cdf(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0" "student_t_cdf(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_cdf_log" "student_t_cdf_log(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0" "student_t_cdf_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_log" "student_t_log(${1:reals y}, ${2:reals nu}, ${3:reals mu}, ${4:reals sigma})$0" "student_t_log(reals, reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("student_t_rng" "student_t_rng(${1:real nu}, ${2:real mu}, ${3:real sigma})$0" "student_t_rng(real, real, real)" nil
			("Functions" "Continuous Distributions" "Unbounded Continuous Distributions" "Student-t Distribution")
			nil nil nil nil)
		       ("sub_col" "sub_col(${1:matrix x}, ${2:int i}, ${3:int j}, ${4:int n})$0" "sub_col(matrix, int, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("sub_row" "sub_row(${1:matrix x}, ${2:int i}, ${3:int j}, ${4:int n})$0" "sub_row(matrix, int, int, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("sum" "sum(${1:int x})$0" "sum(int)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:matrix x})$0" "sum(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:real x})$0" "sum(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:row_vector x})$0" "sum(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("sum" "sum(${1:vector x})$0" "sum(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("tail" "tail(${1:T[] sv}, ${2:int n})$0" "tail(T[], int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("tail" "tail(${1:row_vector rv}, ${2:int n})$0" "tail(row_vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("tail" "tail(${1:vector v}, ${2:int n})$0" "tail(vector, int)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Slice and Package Functions")
			nil nil nil nil)
		       ("tan" "tan(${1:real x})$0" "tan(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Trigonometric Functions")
			nil nil nil nil)
		       ("tanh" "tanh(${1:real x})$0" "tanh(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Hyperbolic Trigonometric Functions")
			nil nil nil nil)
		       ("tcrossprod" "tcrossprod(${1:matrix x})$0" "tcrossprod(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("tgamma" "tgamma(${1:real x})$0" "tgamma(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:int[...] a})$0" "to_array_1d(int[...])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:matrix m})$0" "to_array_1d(matrix)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:real[...] a})$0" "to_array_1d(real[...])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:row_vector v})$0" "to_array_1d(row_vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:vector v})$0" "to_array_1d(vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_array_2d" "to_array_2d(${1:matrix m})$0" "to_array_2d(matrix)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:int[,] a})$0" "to_matrix(int[,])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:matrix m})$0" "to_matrix(matrix)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:real[,] a})$0" "to_matrix(real[,])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:row_vector v})$0" "to_matrix(row_vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:vector v})$0" "to_matrix(vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:int[] a})$0" "to_row_vector(int[])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:matrix m})$0" "to_row_vector(matrix)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:real[] a})$0" "to_row_vector(real[])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:row_vector v})$0" "to_row_vector(row_vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:vector v})$0" "to_row_vector(vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:int[] a})$0" "to_vector(int[])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:matrix m})$0" "to_vector(matrix)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:real[] a})$0" "to_vector(real[])" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:row_vector v})$0" "to_vector(row_vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:vector v})$0" "to_vector(vector)" nil
			("Functions" "Built-In Functions" "Mixed Operations")
			nil nil nil nil)
		       ("trace" "trace(${1:matrix A})$0" "trace(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Linear Algebra Functions and Solvers")
			nil nil nil nil)
		       ("trace_gen_quad_form" "trace_gen_quad_form(${1:matrix D}, ${2:matrix A}, ${3:matrix B})$0" "trace_gen_quad_form(matrix, matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("trace_quad_form" "trace_quad_form(${1:matrix A}, ${2:matrix B})$0" "trace_quad_form(matrix, matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Matrix Arithmetic Operators")
			nil nil nil nil)
		       ("trigamma" "trigamma(${1:real x})$0" "trigamma(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Combinatorial Functions")
			nil nil nil nil)
		       ("trunc" "trunc(${1:real x})$0" "trunc(real)" nil
			("Functions" "Built-In Functions" "Real-Valued Basic Functions" "Step-like Functions")
			nil nil nil nil)
		       ("uniform_ccdf_log" "uniform_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "uniform_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_cdf" "uniform_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "uniform_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_cdf_log" "uniform_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "uniform_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_log" "uniform_log(${1:reals y}, ${2:reals alpha}, ${3:reals beta})$0" "uniform_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("uniform_rng" "uniform_rng(${1:real alpha}, ${2:real beta})$0" "uniform_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Bounded Continuous Probabilities" "Uniform Distribution")
			nil nil nil nil)
		       ("variance" "variance(${1:matrix x})$0" "variance(matrix)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("variance" "variance(${1:real x})$0" "variance(real)" nil
			("Functions" "Built-In Functions" "Array Operations" "Reductions")
			nil nil nil nil)
		       ("variance" "variance(${1:row_vector x})$0" "variance(row_vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("variance" "variance(${1:vector x})$0" "variance(vector)" nil
			("Functions" "Built-In Functions" "Matrix Operations" "Reductions")
			nil nil nil nil)
		       ("von_mises_log" "von_mises_log(${1:reals y}, ${2:reals mu}, ${3:reals kappa})$0" "von_mises_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Circular Distributions" "Von Mises Distribution")
			nil nil nil nil)
		       ("von_mises_rng" "von_mises_rng(${1:reals y}, ${2:reals mu}, ${3:reals kappa})$0" "von_mises_rng(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Circular Distributions" "Von Mises Distribution")
			nil nil nil nil)
		       ("weibull_ccdf_log" "weibull_ccdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0" "weibull_ccdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_cdf" "weibull_cdf(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0" "weibull_cdf(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_cdf_log" "weibull_cdf_log(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0" "weibull_cdf_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_log" "weibull_log(${1:reals y}, ${2:reals alpha}, ${3:reals sigma})$0" "weibull_log(reals, reals, reals)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("weibull_rng" "weibull_rng(${1:real alpha}, ${2:real sigma})$0" "weibull_rng(real, real)" nil
			("Functions" "Continuous Distributions" "Positive Continuous Distributions" "Weibull Distribution")
			nil nil nil nil)
		       ("wishart_log" "wishart_log(${1:matrix W}, ${2:real nu}, ${3:matrix Sigma})$0" "wishart_log(matrix, real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Wishart Distribution")
			nil nil nil nil)
		       ("wishart_rng" "wishart_rng(${1:real nu}, ${2:matrix Sigma})$0" "wishart_rng(real, matrix)" nil
			("Functions" "Continuous Distributions" "Covariance Matrix Distributions" "Wishart Distribution")
			nil nil nil nil)))


