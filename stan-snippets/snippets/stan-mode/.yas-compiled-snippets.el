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
		       ("fun" "${1:return} ${2:name} (${args}) {\n  $0\n}\n" "User-defined function" nil
			("Blocks")
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
		       ("integrate_ode" "integrate_ode(${1:function},${2:y0},${3:t0},${4:t},${5:theta},${6:x_r},${7:x_i});\n$0" "integrate_ode(...);" nil nil nil nil nil nil)
		       ("<" "<lower=${1:0}>$0" "<lower=...>" nil
			("Range Constraints")
			nil nil nil nil)
		       ("<" "<lower=${1:0},upper=${2:1}>$0" "<lower=..., upper=...>" nil
			("Range Constraints")
			nil nil nil nil)
		       ("matrix" "matrix${1:<${2:lower=...,upper=...}>}[$3, $4] ${5:variable}${6:[${7:dims}]};\n$0" "matrix[] ...;" nil
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
		       ("reject" "reject($1);\n$0" "reject(...)" nil nil nil nil nil nil)
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
		     '(("bernoulli" "bernoulli(${1:theta})$0" "bernoulli" nil
			("Distributions")
			nil nil nil nil)
		       ("bernoulli_logit" "bernoulli_logit(${1:alpha})$0" "bernoulli_logit" nil
			("Distributions")
			nil nil nil nil)
		       ("beta" "beta(${1:alpha}, ${2:beta})$0" "beta" nil
			("Distributions")
			nil nil nil nil)
		       ("beta_binomial" "beta_binomial(${1:N}, ${2:alpha}, ${3:beta})$0" "beta_binomial" nil
			("Distributions")
			nil nil nil nil)
		       ("binomial" "binomial(${1:N}, ${2:theta})$0" "binomial" nil
			("Distributions")
			nil nil nil nil)
		       ("binomial_logit" "binomial_logit(${1:N}, ${2:alpha})$0" "binomial_logit" nil
			("Distributions")
			nil nil nil nil)
		       ("categorical" "categorical(${1:theta})$0" "categorical" nil
			("Distributions")
			nil nil nil nil)
		       ("categorical_logit" "categorical_logit(${1:beta})$0" "categorical_logit" nil
			("Distributions")
			nil nil nil nil)
		       ("cauchy" "cauchy(${1:mu}, ${2:sigma})$0" "cauchy" nil
			("Distributions")
			nil nil nil nil)
		       ("chi_square" "chi_square(${1:nu})$0" "chi_square" nil
			("Distributions")
			nil nil nil nil)
		       ("dirichlet" "dirichlet(${1:alpha})$0" "dirichlet" nil
			("Distributions")
			nil nil nil nil)
		       ("double_exponential" "double_exponential(${1:mu}, ${2:sigma})$0" "double_exponential" nil
			("Distributions")
			nil nil nil nil)
		       ("exp_mod_normal" "exp_mod_normal(${1:mu}, ${2:sigma}, ${3:lambda})$0" "exp_mod_normal" nil
			("Distributions")
			nil nil nil nil)
		       ("exponential" "exponential(${1:beta})$0" "exponential" nil
			("Distributions")
			nil nil nil nil)
		       ("frechet" "frechet(${1:alpha}, ${2:sigma})$0" "frechet" nil
			("Distributions")
			nil nil nil nil)
		       ("gamma" "gamma(${1:alpha}, ${2:beta})$0" "gamma" nil
			("Distributions")
			nil nil nil nil)
		       ("gaussian_dlm_obs" "gaussian_dlm_obs(${1:F}, ${2:G}, ${3:V}, ${4:W}, ${5:m0}, ${6:C0})$0" "gaussian_dlm_obs" nil
			("Distributions")
			nil nil nil nil)
		       ("gumbel" "gumbel(${1:mu}, ${2:beta})$0" "gumbel" nil
			("Distributions")
			nil nil nil nil)
		       ("hypergeometric" "hypergeometric(${1:N}, ${2:a}, ${3:b})$0" "hypergeometric" nil
			("Distributions")
			nil nil nil nil)
		       ("inv_chi_square" "inv_chi_square(${1:nu})$0" "inv_chi_square" nil
			("Distributions")
			nil nil nil nil)
		       ("inv_gamma" "inv_gamma(${1:alpha}, ${2:beta})$0" "inv_gamma" nil
			("Distributions")
			nil nil nil nil)
		       ("inv_wishart" "inv_wishart(${1:nu}, ${2:Sigma})$0" "inv_wishart" nil
			("Distributions")
			nil nil nil nil)
		       ("lkj_corr" "lkj_corr(${1:eta})$0" "lkj_corr" nil
			("Distributions")
			nil nil nil nil)
		       ("lkj_corr_cholesky" "lkj_corr_cholesky(${1:eta})$0" "lkj_corr_cholesky" nil
			("Distributions")
			nil nil nil nil)
		       ("logistic" "logistic(${1:mu}, ${2:sigma})$0" "logistic" nil
			("Distributions")
			nil nil nil nil)
		       ("lognormal" "lognormal(${1:mu}, ${2:sigma})$0" "lognormal" nil
			("Distributions")
			nil nil nil nil)
		       ("multi_gp" "multi_gp(${1:Sigma}, ${2:w})$0" "multi_gp" nil
			("Distributions")
			nil nil nil nil)
		       ("multi_gp_cholesky" "multi_gp_cholesky(${1:L}, ${2:w})$0" "multi_gp_cholesky" nil
			("Distributions")
			nil nil nil nil)
		       ("multi_normal" "multi_normal()$0" "multi_normal" nil
			("Distributions")
			nil nil nil nil)
		       ("multi_normal_cholesky" "multi_normal_cholesky()$0" "multi_normal_cholesky" nil
			("Distributions")
			nil nil nil nil)
		       ("multi_normal_prec" "multi_normal_prec()$0" "multi_normal_prec" nil
			("Distributions")
			nil nil nil nil)
		       ("multi_student_t" "multi_student_t(${1:Sigma})$0" "multi_student_t" nil
			("Distributions")
			nil nil nil nil)
		       ("multinomial" "multinomial(${1:theta})$0" "multinomial" nil
			("Distributions")
			nil nil nil nil)
		       ("neg_binomial" "neg_binomial(${1:alpha}, ${2:beta})$0" "neg_binomial" nil
			("Distributions")
			nil nil nil nil)
		       ("neg_binomial_2" "neg_binomial_2(${1:mu}, ${2:phi})$0" "neg_binomial_2" nil
			("Distributions")
			nil nil nil nil)
		       ("neg_binomial_2_log" "neg_binomial_2_log(${1:eta}, ${2:phi})$0" "neg_binomial_2_log" nil
			("Distributions")
			nil nil nil nil)
		       ("normal" "normal(${1:mu}, ${2:sigma})$0" "normal" nil
			("Distributions")
			nil nil nil nil)
		       ("ordered_logistic" "ordered_logistic(${1:eta}, ${2:c})$0" "ordered_logistic" nil
			("Distributions")
			nil nil nil nil)
		       ("pareto" "pareto(${1:y_min}, ${2:alpha})$0" "pareto" nil
			("Distributions")
			nil nil nil nil)
		       ("pareto_type_2" "pareto_type_2(${1:mu}, ${2:lambda}, ${3:alpha})$0" "pareto_type_2" nil
			("Distributions")
			nil nil nil nil)
		       ("poisson" "poisson(${1:lambda})$0" "poisson" nil
			("Distributions")
			nil nil nil nil)
		       ("poisson_log" "poisson_log(${1:alpha})$0" "poisson_log" nil
			("Distributions")
			nil nil nil nil)
		       ("rayleigh" "rayleigh(${1:sigma})$0" "rayleigh" nil
			("Distributions")
			nil nil nil nil)
		       ("scaled_inv_chi_square" "scaled_inv_chi_square(${1:nu}, ${2:sigma})$0" "scaled_inv_chi_square" nil
			("Distributions")
			nil nil nil nil)
		       ("skew_normal" "skew_normal(${1:mu}, ${2:sigma}, ${3:alpha})$0" "skew_normal" nil
			("Distributions")
			nil nil nil nil)
		       ("student_t" "student_t(${1:nu}, ${2:mu}, ${3:sigma})$0" "student_t" nil
			("Distributions")
			nil nil nil nil)
		       ("uniform" "uniform(${1:alpha}, ${2:beta})$0" "uniform" nil
			("Distributions")
			nil nil nil nil)
		       ("von_mises" "von_mises(${1:mu}, ${2:kappa})$0" "von_mises" nil
			("Distributions")
			nil nil nil nil)
		       ("weibull" "weibull(${1:alpha}, ${2:sigma})$0" "weibull" nil
			("Distributions")
			nil nil nil nil)
		       ("wiener" "wiener(${1:alpha}, ${2:tau}, ${3:beta}, ${4:delta})$0" "wiener" nil
			("Distributions")
			nil nil nil nil)
		       ("wishart" "wishart(${1:nu}, ${2:Sigma})$0" "wishart" nil
			("Distributions")
			nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("Phi" "Phi(${1:x})$0" "Phi" nil
			("Functions")
			nil nil nil nil)
		       ("Phi_approx" "Phi_approx(${1:x})$0" "Phi_approx" nil
			("Functions")
			nil nil nil nil)
		       ("abs" "abs(${1:x})$0" "abs" nil
			("Functions")
			nil nil nil nil)
		       ("acos" "acos(${1:x})$0" "acos" nil
			("Functions")
			nil nil nil nil)
		       ("acosh" "acosh(${1:x})$0" "acosh" nil
			("Functions")
			nil nil nil nil)
		       ("append_col" "append_col(${1:x}, ${2:y})$0" "append_col" nil
			("Functions")
			nil nil nil nil)
		       ("append_row" "append_row(${1:x}, ${2:y})$0" "append_row" nil
			("Functions")
			nil nil nil nil)
		       ("asin" "asin(${1:x})$0" "asin" nil
			("Functions")
			nil nil nil nil)
		       ("asinh" "asinh(${1:x})$0" "asinh" nil
			("Functions")
			nil nil nil nil)
		       ("atan" "atan(${1:x})$0" "atan" nil
			("Functions")
			nil nil nil nil)
		       ("atan2" "atan2(${1:x}, ${2:y})$0" "atan2" nil
			("Functions")
			nil nil nil nil)
		       ("atanh" "atanh(${1:x})$0" "atanh" nil
			("Functions")
			nil nil nil nil)
		       ("bernoulli_ccdf_log" "bernoulli_ccdf_log(${1:y}, ${2:theta})$0" "bernoulli_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("bernoulli_cdf" "bernoulli_cdf(${1:y}, ${2:theta})$0" "bernoulli_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("bernoulli_cdf_log" "bernoulli_cdf_log(${1:y}, ${2:theta})$0" "bernoulli_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("bernoulli_log" "bernoulli_log(${1:y}, ${2:theta})$0" "bernoulli_log" nil
			("Functions")
			nil nil nil nil)
		       ("bernoulli_logit_log" "bernoulli_logit_log(${1:y}, ${2:alpha})$0" "bernoulli_logit_log" nil
			("Functions")
			nil nil nil nil)
		       ("bernoulli_rng" "bernoulli_rng(${1:theta})$0" "bernoulli_rng" nil
			("Functions")
			nil nil nil nil)
		       ("bessel_first_kind" "bessel_first_kind(${1:v}, ${2:x})$0" "bessel_first_kind" nil
			("Functions")
			nil nil nil nil)
		       ("bessel_second_kind" "bessel_second_kind(${1:v}, ${2:x})$0" "bessel_second_kind" nil
			("Functions")
			nil nil nil nil)
		       ("beta_binomial_ccdf_log" "beta_binomial_ccdf_log(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("beta_binomial_cdf" "beta_binomial_cdf(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("beta_binomial_cdf_log" "beta_binomial_cdf_log(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("beta_binomial_log" "beta_binomial_log(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_log" nil
			("Functions")
			nil nil nil nil)
		       ("beta_binomial_rng" "beta_binomial_rng(${1:N}, ${2:alpha}, ${3:beta})$0" "beta_binomial_rng" nil
			("Functions")
			nil nil nil nil)
		       ("beta_ccdf_log" "beta_ccdf_log(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("beta_cdf" "beta_cdf(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("beta_cdf_log" "beta_cdf_log(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("beta_log" "beta_log(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_log" nil
			("Functions")
			nil nil nil nil)
		       ("beta_rng" "beta_rng(${1:alpha}, ${2:beta})$0" "beta_rng" nil
			("Functions")
			nil nil nil nil)
		       ("binary_log_loss" "binary_log_loss(${1:y}, ${2:y_hat})$0" "binary_log_loss" nil
			("Functions")
			nil nil nil nil)
		       ("binomial_ccdf_log" "binomial_ccdf_log(${1:n}, ${2:N}, ${3:theta})$0" "binomial_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("binomial_cdf" "binomial_cdf(${1:n}, ${2:N}, ${3:theta})$0" "binomial_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("binomial_cdf_log" "binomial_cdf_log(${1:n}, ${2:N}, ${3:theta})$0" "binomial_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("binomial_coefficient_log" "binomial_coefficient_log(${1:x}, ${2:y})$0" "binomial_coefficient_log" nil
			("Functions")
			nil nil nil nil)
		       ("binomial_log" "binomial_log(${1:n}, ${2:N}, ${3:theta})$0" "binomial_log" nil
			("Functions")
			nil nil nil nil)
		       ("binomial_logit_log" "binomial_logit_log(${1:n}, ${2:N}, ${3:alpha})$0" "binomial_logit_log" nil
			("Functions")
			nil nil nil nil)
		       ("binomial_rng" "binomial_rng(${1:N}, ${2:theta})$0" "binomial_rng" nil
			("Functions")
			nil nil nil nil)
		       ("block" "block(${1:x}, ${2:i}, ${3:j}, ${4:n_rows}, ${5:n_cols})$0" "block" nil
			("Functions")
			nil nil nil nil)
		       ("categorical_log" "categorical_log(${1:y}, ${2:theta})$0" "categorical_log" nil
			("Functions")
			nil nil nil nil)
		       ("categorical_logit_log" "categorical_logit_log(${1:y}, ${2:beta})$0" "categorical_logit_log" nil
			("Functions")
			nil nil nil nil)
		       ("categorical_rng" "categorical_rng(${1:theta})$0" "categorical_rng" nil
			("Functions")
			nil nil nil nil)
		       ("cauchy_ccdf_log" "cauchy_ccdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("cauchy_cdf" "cauchy_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("cauchy_cdf_log" "cauchy_cdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("cauchy_log" "cauchy_log(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_log" nil
			("Functions")
			nil nil nil nil)
		       ("cauchy_rng" "cauchy_rng(${1:mu}, ${2:sigma})$0" "cauchy_rng" nil
			("Functions")
			nil nil nil nil)
		       ("cbrt" "cbrt(${1:x})$0" "cbrt" nil
			("Functions")
			nil nil nil nil)
		       ("ceil" "ceil(${1:x})$0" "ceil" nil
			("Functions")
			nil nil nil nil)
		       ("chi_square_ccdf_log" "chi_square_ccdf_log(${1:y}, ${2:nu})$0" "chi_square_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("chi_square_cdf" "chi_square_cdf(${1:y}, ${2:nu})$0" "chi_square_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("chi_square_cdf_log" "chi_square_cdf_log(${1:y}, ${2:nu})$0" "chi_square_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("chi_square_log" "chi_square_log(${1:y}, ${2:nu})$0" "chi_square_log" nil
			("Functions")
			nil nil nil nil)
		       ("chi_square_rng" "chi_square_rng(${1:nu})$0" "chi_square_rng" nil
			("Functions")
			nil nil nil nil)
		       ("cholesky_decompose" "cholesky_decompose(${1:A})$0" "cholesky_decompose" nil
			("Functions")
			nil nil nil nil)
		       ("col" "col(${1:x}, ${2:n})$0" "col" nil
			("Functions")
			nil nil nil nil)
		       ("cols" "cols(${1:x})$0" "cols" nil
			("Functions")
			nil nil nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:x}, ${2:y})$0" "columns_dot_product" nil
			("Functions")
			nil nil nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:x})$0" "columns_dot_self" nil
			("Functions")
			nil nil nil nil)
		       ("cos" "cos(${1:x})$0" "cos" nil
			("Functions")
			nil nil nil nil)
		       ("cosh" "cosh(${1:x})$0" "cosh" nil
			("Functions")
			nil nil nil nil)
		       ("crossprod" "crossprod(${1:x})$0" "crossprod" nil
			("Functions")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:rv})$0" "cumulative_sum" nil
			("Functions")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:v})$0" "cumulative_sum" nil
			("Functions")
			nil nil nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:x})$0" "cumulative_sum" nil
			("Functions")
			nil nil nil nil)
		       ("determinant" "determinant(${1:A})$0" "determinant" nil
			("Functions")
			nil nil nil nil)
		       ("diag_matrix" "diag_matrix(${1:x})$0" "diag_matrix" nil
			("Functions")
			nil nil nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:m}, ${2:rv})$0" "diag_post_multiply" nil
			("Functions")
			nil nil nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:m}, ${2:v})$0" "diag_post_multiply" nil
			("Functions")
			nil nil nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:rv}, ${2:m})$0" "diag_pre_multiply" nil
			("Functions")
			nil nil nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:v}, ${2:m})$0" "diag_pre_multiply" nil
			("Functions")
			nil nil nil nil)
		       ("diagonal" "diagonal(${1:x})$0" "diagonal" nil
			("Functions")
			nil nil nil nil)
		       ("digamma" "digamma(${1:x})$0" "digamma" nil
			("Functions")
			nil nil nil nil)
		       ("dims" "dims(${1:x})$0" "dims" nil
			("Functions")
			nil nil nil nil)
		       ("dirichlet_log" "dirichlet_log(${1:theta}, ${2:alpha})$0" "dirichlet_log" nil
			("Functions")
			nil nil nil nil)
		       ("dirichlet_rng" "dirichlet_rng(${1:alpha})$0" "dirichlet_rng" nil
			("Functions")
			nil nil nil nil)
		       ("distance" "distance(${1:x}, ${2:y})$0" "distance" nil
			("Functions")
			nil nil nil nil)
		       ("dot_product" "dot_product(${1:x}, ${2:y})$0" "dot_product" nil
			("Functions")
			nil nil nil nil)
		       ("dot_self" "dot_self(${1:x})$0" "dot_self" nil
			("Functions")
			nil nil nil nil)
		       ("double_exponential_ccdf_log" "double_exponential_ccdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("double_exponential_cdf" "double_exponential_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("double_exponential_cdf_log" "double_exponential_cdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("double_exponential_log" "double_exponential_log(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_log" nil
			("Functions")
			nil nil nil nil)
		       ("double_exponential_rng" "double_exponential_rng(${1:mu}, ${2:sigma})$0" "double_exponential_rng" nil
			("Functions")
			nil nil nil nil)
		       ("e" "e()$0" "e" nil
			("Functions")
			nil nil nil nil)
		       ("eigenvalues_sym" "eigenvalues_sym(${1:A})$0" "eigenvalues_sym" nil
			("Functions")
			nil nil nil nil)
		       ("eigenvectors_sym" "eigenvectors_sym(${1:A})$0" "eigenvectors_sym" nil
			("Functions")
			nil nil nil nil)
		       ("erf" "erf(${1:x})$0" "erf" nil
			("Functions")
			nil nil nil nil)
		       ("erfc" "erfc(${1:x})$0" "erfc" nil
			("Functions")
			nil nil nil nil)
		       ("exp" "exp(${1:x})$0" "exp" nil
			("Functions")
			nil nil nil nil)
		       ("exp2" "exp2(${1:x})$0" "exp2" nil
			("Functions")
			nil nil nil nil)
		       ("exp_mod_normal_ccdf_log" "exp_mod_normal_ccdf_log(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("exp_mod_normal_cdf" "exp_mod_normal_cdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("exp_mod_normal_cdf_log" "exp_mod_normal_cdf_log(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("exp_mod_normal_log" "exp_mod_normal_log(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_log" nil
			("Functions")
			nil nil nil nil)
		       ("exp_mod_normal_rng" "exp_mod_normal_rng(${1:mu}, ${2:sigma}, ${3:lambda})$0" "exp_mod_normal_rng" nil
			("Functions")
			nil nil nil nil)
		       ("expm1" "expm1(${1:x})$0" "expm1" nil
			("Functions")
			nil nil nil nil)
		       ("exponential_ccdf_log" "exponential_ccdf_log(${1:y}, ${2:beta})$0" "exponential_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("exponential_cdf" "exponential_cdf(${1:y}, ${2:beta})$0" "exponential_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("exponential_cdf_log" "exponential_cdf_log(${1:y}, ${2:beta})$0" "exponential_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("exponential_log" "exponential_log(${1:y}, ${2:beta})$0" "exponential_log" nil
			("Functions")
			nil nil nil nil)
		       ("exponential_rng" "exponential_rng(${1:beta})$0" "exponential_rng" nil
			("Functions")
			nil nil nil nil)
		       ("fabs" "fabs(${1:x})$0" "fabs" nil
			("Functions")
			nil nil nil nil)
		       ("falling_factorial" "falling_factorial(${1:x}, ${2:n})$0" "falling_factorial" nil
			("Functions")
			nil nil nil nil)
		       ("fdim" "fdim(${1:x}, ${2:y})$0" "fdim" nil
			("Functions")
			nil nil nil nil)
		       ("floor" "floor(${1:x})$0" "floor" nil
			("Functions")
			nil nil nil nil)
		       ("fma" "fma(${1:x}, ${2:y}, ${3:z})$0" "fma" nil
			("Functions")
			nil nil nil nil)
		       ("fmax" "fmax(${1:x}, ${2:y})$0" "fmax" nil
			("Functions")
			nil nil nil nil)
		       ("fmin" "fmin(${1:x}, ${2:y})$0" "fmin" nil
			("Functions")
			nil nil nil nil)
		       ("fmod" "fmod(${1:x}, ${2:y})$0" "fmod" nil
			("Functions")
			nil nil nil nil)
		       ("frechet_ccdf_log" "frechet_ccdf_log(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("frechet_cdf" "frechet_cdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("frechet_cdf_log" "frechet_cdf_log(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("frechet_log" "frechet_log(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_log" nil
			("Functions")
			nil nil nil nil)
		       ("frechet_rng" "frechet_rng(${1:alpha}, ${2:sigma})$0" "frechet_rng" nil
			("Functions")
			nil nil nil nil)
		       ("gamma_ccdf_log" "gamma_ccdf_log(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("gamma_cdf" "gamma_cdf(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("gamma_cdf_log" "gamma_cdf_log(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("gamma_log" "gamma_log(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_log" nil
			("Functions")
			nil nil nil nil)
		       ("gamma_p" "gamma_p(${1:a}, ${2:z})$0" "gamma_p" nil
			("Functions")
			nil nil nil nil)
		       ("gamma_q" "gamma_q(${1:a}, ${2:z})$0" "gamma_q" nil
			("Functions")
			nil nil nil nil)
		       ("gamma_rng" "gamma_rng(${1:alpha}, ${2:beta})$0" "gamma_rng" nil
			("Functions")
			nil nil nil nil)
		       ("gaussian_dlm_obs_log" "gaussian_dlm_obs_log(${1:y}, ${2:F}, ${3:G}, ${4:V}, ${5:W}, ${6:m0}, ${7:C0})$0" "gaussian_dlm_obs_log" nil
			("Functions")
			nil nil nil nil)
		       ("get_lp" "get_lp()$0" "get_lp" nil
			("Functions")
			nil nil nil nil)
		       ("gumbel_ccdf_log" "gumbel_ccdf_log(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("gumbel_cdf" "gumbel_cdf(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("gumbel_cdf_log" "gumbel_cdf_log(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("gumbel_log" "gumbel_log(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_log" nil
			("Functions")
			nil nil nil nil)
		       ("gumbel_rng" "gumbel_rng(${1:mu}, ${2:beta})$0" "gumbel_rng" nil
			("Functions")
			nil nil nil nil)
		       ("head" "head(${1:rv}, ${2:n})$0" "head" nil
			("Functions")
			nil nil nil nil)
		       ("head" "head(${1:sv}, ${2:n})$0" "head" nil
			("Functions")
			nil nil nil nil)
		       ("head" "head(${1:v}, ${2:n})$0" "head" nil
			("Functions")
			nil nil nil nil)
		       ("hypergeometric_log" "hypergeometric_log(${1:n}, ${2:N}, ${3:a}, ${4:b})$0" "hypergeometric_log" nil
			("Functions")
			nil nil nil nil)
		       ("hypergeometric_rng" "hypergeometric_rng(${1:N}, ${2:a}, ${3:b})$0" "hypergeometric_rng" nil
			("Functions")
			nil nil nil nil)
		       ("hypot" "hypot(${1:x}, ${2:y})$0" "hypot" nil
			("Functions")
			nil nil nil nil)
		       ("if_else" "if_else(${1:cond}, ${2:x}, ${3:y})$0" "if_else" nil
			("Functions")
			nil nil nil nil)
		       ("int_step" "int_step(${1:x})$0" "int_step" nil
			("Functions")
			nil nil nil nil)
		       ("inv" "inv(${1:x})$0" "inv" nil
			("Functions")
			nil nil nil nil)
		       ("inv_chi_square_ccdf_log" "inv_chi_square_ccdf_log(${1:y}, ${2:nu})$0" "inv_chi_square_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("inv_chi_square_cdf" "inv_chi_square_cdf(${1:y}, ${2:nu})$0" "inv_chi_square_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("inv_chi_square_cdf_log" "inv_chi_square_cdf_log(${1:y}, ${2:nu})$0" "inv_chi_square_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("inv_chi_square_log" "inv_chi_square_log(${1:y}, ${2:nu})$0" "inv_chi_square_log" nil
			("Functions")
			nil nil nil nil)
		       ("inv_chi_square_rng" "inv_chi_square_rng(${1:nu})$0" "inv_chi_square_rng" nil
			("Functions")
			nil nil nil nil)
		       ("inv_cloglog" "inv_cloglog(${1:y})$0" "inv_cloglog" nil
			("Functions")
			nil nil nil nil)
		       ("inv_gamma_ccdf_log" "inv_gamma_ccdf_log(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("inv_gamma_cdf" "inv_gamma_cdf(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("inv_gamma_cdf_log" "inv_gamma_cdf_log(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("inv_gamma_log" "inv_gamma_log(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_log" nil
			("Functions")
			nil nil nil nil)
		       ("inv_gamma_rng" "inv_gamma_rng(${1:alpha}, ${2:beta})$0" "inv_gamma_rng" nil
			("Functions")
			nil nil nil nil)
		       ("inv_logit" "inv_logit(${1:y})$0" "inv_logit" nil
			("Functions")
			nil nil nil nil)
		       ("inv_sqrt" "inv_sqrt(${1:x})$0" "inv_sqrt" nil
			("Functions")
			nil nil nil nil)
		       ("inv_square" "inv_square(${1:x})$0" "inv_square" nil
			("Functions")
			nil nil nil nil)
		       ("inv_wishart_log" "inv_wishart_log(${1:W}, ${2:nu}, ${3:Sigma})$0" "inv_wishart_log" nil
			("Functions")
			nil nil nil nil)
		       ("inv_wishart_rng" "inv_wishart_rng(${1:nu}, ${2:Sigma})$0" "inv_wishart_rng" nil
			("Functions")
			nil nil nil nil)
		       ("inverse" "inverse(${1:A})$0" "inverse" nil
			("Functions")
			nil nil nil nil)
		       ("inverse_spd" "inverse_spd(${1:A})$0" "inverse_spd" nil
			("Functions")
			nil nil nil nil)
		       ("is_inf" "is_inf(${1:x})$0" "is_inf" nil
			("Functions")
			nil nil nil nil)
		       ("is_nan" "is_nan(${1:x})$0" "is_nan" nil
			("Functions")
			nil nil nil nil)
		       ("lbeta" "lbeta(${1:alpha}, ${2:beta})$0" "lbeta" nil
			("Functions")
			nil nil nil nil)
		       ("lgamma" "lgamma(${1:x})$0" "lgamma" nil
			("Functions")
			nil nil nil nil)
		       ("lkj_corr_cholesky_log" "lkj_corr_cholesky_log(${1:L}, ${2:eta})$0" "lkj_corr_cholesky_log" nil
			("Functions")
			nil nil nil nil)
		       ("lkj_corr_cholesky_rng" "lkj_corr_cholesky_rng(${1:K}, ${2:eta})$0" "lkj_corr_cholesky_rng" nil
			("Functions")
			nil nil nil nil)
		       ("lkj_corr_log" "lkj_corr_log(${1:y}, ${2:eta})$0" "lkj_corr_log" nil
			("Functions")
			nil nil nil nil)
		       ("lkj_corr_rng" "lkj_corr_rng(${1:K}, ${2:eta})$0" "lkj_corr_rng" nil
			("Functions")
			nil nil nil nil)
		       ("lmgamma" "lmgamma(${1:n}, ${2:x})$0" "lmgamma" nil
			("Functions")
			nil nil nil nil)
		       ("log" "log(${1:x})$0" "log" nil
			("Functions")
			nil nil nil nil)
		       ("log10" "log10()$0" "log10" nil
			("Functions")
			nil nil nil nil)
		       ("log10" "log10(${1:x})$0" "log10" nil
			("Functions")
			nil nil nil nil)
		       ("log1m" "log1m(${1:x})$0" "log1m" nil
			("Functions")
			nil nil nil nil)
		       ("log1m_exp" "log1m_exp(${1:x})$0" "log1m_exp" nil
			("Functions")
			nil nil nil nil)
		       ("log1m_inv_logit" "log1m_inv_logit(${1:x})$0" "log1m_inv_logit" nil
			("Functions")
			nil nil nil nil)
		       ("log1p" "log1p(${1:x})$0" "log1p" nil
			("Functions")
			nil nil nil nil)
		       ("log1p_exp" "log1p_exp(${1:x})$0" "log1p_exp" nil
			("Functions")
			nil nil nil nil)
		       ("log2" "log2()$0" "log2" nil
			("Functions")
			nil nil nil nil)
		       ("log2" "log2(${1:x})$0" "log2" nil
			("Functions")
			nil nil nil nil)
		       ("log_determinant" "log_determinant(${1:A})$0" "log_determinant" nil
			("Functions")
			nil nil nil nil)
		       ("log_diff_exp" "log_diff_exp(${1:x}, ${2:y})$0" "log_diff_exp" nil
			("Functions")
			nil nil nil nil)
		       ("log_falling_factorial" "log_falling_factorial(${1:x}, ${2:n})$0" "log_falling_factorial" nil
			("Functions")
			nil nil nil nil)
		       ("log_inv_logit" "log_inv_logit(${1:x})$0" "log_inv_logit" nil
			("Functions")
			nil nil nil nil)
		       ("log_mix" "log_mix(${1:theta}, ${2:lp1}, ${3:lp2})$0" "log_mix" nil
			("Functions")
			nil nil nil nil)
		       ("log_rising_factorial" "log_rising_factorial(${1:x}, ${2:n})$0" "log_rising_factorial" nil
			("Functions")
			nil nil nil nil)
		       ("log_softmax" "log_softmax(${1:x})$0" "log_softmax" nil
			("Functions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:x})$0" "log_sum_exp" nil
			("Functions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:x}, ${2:y})$0" "log_sum_exp" nil
			("Functions")
			nil nil nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:x})$0" "log_sum_exp" nil
			("Functions")
			nil nil nil nil)
		       ("logistic_ccdf_log" "logistic_ccdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("logistic_cdf" "logistic_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("logistic_cdf_log" "logistic_cdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("logistic_log" "logistic_log(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_log" nil
			("Functions")
			nil nil nil nil)
		       ("logistic_rng" "logistic_rng(${1:mu}, ${2:sigma})$0" "logistic_rng" nil
			("Functions")
			nil nil nil nil)
		       ("logit" "logit(${1:x})$0" "logit" nil
			("Functions")
			nil nil nil nil)
		       ("lognormal_ccdf_log" "lognormal_ccdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("lognormal_cdf" "lognormal_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("lognormal_cdf_log" "lognormal_cdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("lognormal_log" "lognormal_log(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_log" nil
			("Functions")
			nil nil nil nil)
		       ("lognormal_rng" "lognormal_rng(${1:mu}, ${2:beta})$0" "lognormal_rng" nil
			("Functions")
			nil nil nil nil)
		       ("machine_precision" "machine_precision()$0" "machine_precision" nil
			("Functions")
			nil nil nil nil)
		       ("max" "max(${1:x})$0" "max" nil
			("Functions")
			nil nil nil nil)
		       ("max" "max(${1:x}, ${2:y})$0" "max" nil
			("Functions")
			nil nil nil nil)
		       ("max" "max(${1:x})$0" "max" nil
			("Functions")
			nil nil nil nil)
		       ("mdivide_left_tri_low" "mdivide_left_tri_low(${1:A}, ${2:B})$0" "mdivide_left_tri_low" nil
			("Functions")
			nil nil nil nil)
		       ("mdivide_right_tri_low" "mdivide_right_tri_low(${1:B}, ${2:A})$0" "mdivide_right_tri_low" nil
			("Functions")
			nil nil nil nil)
		       ("mean" "mean(${1:x})$0" "mean" nil
			("Functions")
			nil nil nil nil)
		       ("mean" "mean(${1:x})$0" "mean" nil
			("Functions")
			nil nil nil nil)
		       ("min" "min(${1:x})$0" "min" nil
			("Functions")
			nil nil nil nil)
		       ("min" "min(${1:x}, ${2:y})$0" "min" nil
			("Functions")
			nil nil nil nil)
		       ("min" "min(${1:x})$0" "min" nil
			("Functions")
			nil nil nil nil)
		       ("modified_bessel_first_kind" "modified_bessel_first_kind(${1:v}, ${2:z})$0" "modified_bessel_first_kind" nil
			("Functions")
			nil nil nil nil)
		       ("modified_bessel_second_kind" "modified_bessel_second_kind(${1:v}, ${2:z})$0" "modified_bessel_second_kind" nil
			("Functions")
			nil nil nil nil)
		       ("multi_gp_cholesky_log" "multi_gp_cholesky_log(${1:y}, ${2:L}, ${3:w})$0" "multi_gp_cholesky_log" nil
			("Functions")
			nil nil nil nil)
		       ("multi_gp_log" "multi_gp_log(${1:y}, ${2:Sigma}, ${3:w})$0" "multi_gp_log" nil
			("Functions")
			nil nil nil nil)
		       ("multi_normal_cholesky_log" "multi_normal_cholesky_log(${1:L})$0" "multi_normal_cholesky_log" nil
			("Functions")
			nil nil nil nil)
		       ("multi_normal_cholesky_rng" "multi_normal_cholesky_rng(${1:mu}, ${2:L})$0" "multi_normal_cholesky_rng" nil
			("Functions")
			nil nil nil nil)
		       ("multi_normal_log" "multi_normal_log(${1:Sigma})$0" "multi_normal_log" nil
			("Functions")
			nil nil nil nil)
		       ("multi_normal_prec_log" "multi_normal_prec_log(${1:Omega})$0" "multi_normal_prec_log" nil
			("Functions")
			nil nil nil nil)
		       ("multi_normal_rng" "multi_normal_rng(${1:mu}, ${2:Sigma})$0" "multi_normal_rng" nil
			("Functions")
			nil nil nil nil)
		       ("multi_student_t_log" "multi_student_t_log(${1:nu}, ${2:Sigma})$0" "multi_student_t_log" nil
			("Functions")
			nil nil nil nil)
		       ("multi_student_t_rng" "multi_student_t_rng(${1:nu}, ${2:mu}, ${3:Sigma})$0" "multi_student_t_rng" nil
			("Functions")
			nil nil nil nil)
		       ("multinomial_log" "multinomial_log(${1:y}, ${2:theta})$0" "multinomial_log" nil
			("Functions")
			nil nil nil nil)
		       ("multinomial_rng" "multinomial_rng(${1:theta}, ${2:N})$0" "multinomial_rng" nil
			("Functions")
			nil nil nil nil)
		       ("multiply_log" "multiply_log(${1:x}, ${2:y})$0" "multiply_log" nil
			("Functions")
			nil nil nil nil)
		       ("multiply_lower_tri_self_transpose" "multiply_lower_tri_self_transpose(${1:x})$0" "multiply_lower_tri_self_transpose" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_2_ccdf_log" "neg_binomial_2_ccdf_log(${1:n}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_2_cdf" "neg_binomial_2_cdf(${1:n}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_2_cdf_log" "neg_binomial_2_cdf_log(${1:n}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_2_log" "neg_binomial_2_log(${1:y}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_log" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_2_log_log" "neg_binomial_2_log_log(${1:y}, ${2:eta}, ${3:phi})$0" "neg_binomial_2_log_log" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_2_log_rng" "neg_binomial_2_log_rng(${1:eta}, ${2:phi})$0" "neg_binomial_2_log_rng" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_2_rng" "neg_binomial_2_rng(${1:mu}, ${2:phi})$0" "neg_binomial_2_rng" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_ccdf_log" "neg_binomial_ccdf_log(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_cdf" "neg_binomial_cdf(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_cdf_log" "neg_binomial_cdf_log(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_log" "neg_binomial_log(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_log" nil
			("Functions")
			nil nil nil nil)
		       ("neg_binomial_rng" "neg_binomial_rng(${1:alpha}, ${2:beta})$0" "neg_binomial_rng" nil
			("Functions")
			nil nil nil nil)
		       ("negative_infinity" "negative_infinity()$0" "negative_infinity" nil
			("Functions")
			nil nil nil nil)
		       ("normal_ccdf_log" "normal_ccdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("normal_cdf" "normal_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("normal_cdf_log" "normal_cdf_log(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("normal_log" "normal_log(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_log" nil
			("Functions")
			nil nil nil nil)
		       ("normal_rng" "normal_rng(${1:mu}, ${2:sigma})$0" "normal_rng" nil
			("Functions")
			nil nil nil nil)
		       ("not_a_number" "not_a_number()$0" "not_a_number" nil
			("Functions")
			nil nil nil nil)
		       ("num_elements" "num_elements(${1:x})$0" "num_elements" nil
			("Functions")
			nil nil nil nil)
		       ("ordered_logistic_log" "ordered_logistic_log(${1:k}, ${2:eta}, ${3:c})$0" "ordered_logistic_log" nil
			("Functions")
			nil nil nil nil)
		       ("ordered_logistic_rng" "ordered_logistic_rng(${1:eta}, ${2:c})$0" "ordered_logistic_rng" nil
			("Functions")
			nil nil nil nil)
		       ("owens_t" "owens_t(${1:h}, ${2:a})$0" "owens_t" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_ccdf_log" "pareto_ccdf_log(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_cdf" "pareto_cdf(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_cdf_log" "pareto_cdf_log(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_log" "pareto_log(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_log" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_rng" "pareto_rng(${1:y_min}, ${2:alpha})$0" "pareto_rng" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_type_2_ccdf_log" "pareto_type_2_ccdf_log(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_type_2_cdf" "pareto_type_2_cdf(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_type_2_cdf_log" "pareto_type_2_cdf_log(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_type_2_log" "pareto_type_2_log(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_log" nil
			("Functions")
			nil nil nil nil)
		       ("pareto_type_2_rng" "pareto_type_2_rng(${1:mu}, ${2:lambda}, ${3:alpha})$0" "pareto_type_2_rng" nil
			("Functions")
			nil nil nil nil)
		       ("pi" "pi()$0" "pi" nil
			("Functions")
			nil nil nil nil)
		       ("poisson_ccdf_log" "poisson_ccdf_log(${1:n}, ${2:lambda})$0" "poisson_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("poisson_cdf" "poisson_cdf(${1:n}, ${2:lambda})$0" "poisson_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("poisson_cdf_log" "poisson_cdf_log(${1:n}, ${2:lambda})$0" "poisson_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("poisson_log" "poisson_log(${1:n}, ${2:lambda})$0" "poisson_log" nil
			("Functions")
			nil nil nil nil)
		       ("poisson_log_log" "poisson_log_log(${1:n}, ${2:alpha})$0" "poisson_log_log" nil
			("Functions")
			nil nil nil nil)
		       ("poisson_log_rng" "poisson_log_rng(${1:alpha})$0" "poisson_log_rng" nil
			("Functions")
			nil nil nil nil)
		       ("poisson_rng" "poisson_rng(${1:lambda})$0" "poisson_rng" nil
			("Functions")
			nil nil nil nil)
		       ("positive_infinity" "positive_infinity()$0" "positive_infinity" nil
			("Functions")
			nil nil nil nil)
		       ("pow" "pow(${1:x}, ${2:y})$0" "pow" nil
			("Functions")
			nil nil nil nil)
		       ("prod" "prod(${1:x})$0" "prod" nil
			("Functions")
			nil nil nil nil)
		       ("prod" "prod(${1:x})$0" "prod" nil
			("Functions")
			nil nil nil nil)
		       ("qr_Q" "qr_Q(${1:A})$0" "qr_Q" nil
			("Functions")
			nil nil nil nil)
		       ("qr_R" "qr_R(${1:A})$0" "qr_R" nil
			("Functions")
			nil nil nil nil)
		       ("quad_form" "quad_form(${1:A}, ${2:B})$0" "quad_form" nil
			("Functions")
			nil nil nil nil)
		       ("quad_form_diag" "quad_form_diag(${1:m}, ${2:rv})$0" "quad_form_diag" nil
			("Functions")
			nil nil nil nil)
		       ("quad_form_diag" "quad_form_diag(${1:m}, ${2:v})$0" "quad_form_diag" nil
			("Functions")
			nil nil nil nil)
		       ("quad_form_sym" "quad_form_sym(${1:A}, ${2:B})$0" "quad_form_sym" nil
			("Functions")
			nil nil nil nil)
		       ("rank" "rank(${1:v}, ${2:s})$0" "rank" nil
			("Functions")
			nil nil nil nil)
		       ("rayleigh_ccdf_log" "rayleigh_ccdf_log(${1:y}, ${2:sigma})$0" "rayleigh_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("rayleigh_cdf" "rayleigh_cdf(${1:y}, ${2:sigma})$0" "rayleigh_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("rayleigh_cdf_log" "rayleigh_cdf_log(${1:y}, ${2:sigma})$0" "rayleigh_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("rayleigh_log" "rayleigh_log(${1:y}, ${2:sigma})$0" "rayleigh_log" nil
			("Functions")
			nil nil nil nil)
		       ("rayleigh_rng" "rayleigh_rng(${1:sigma})$0" "rayleigh_rng" nil
			("Functions")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:x}, ${2:k}, ${3:m}, ${4:n})$0" "rep_array" nil
			("Functions")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:x}, ${2:m}, ${3:n})$0" "rep_array" nil
			("Functions")
			nil nil nil nil)
		       ("rep_array" "rep_array(${1:x}, ${2:n})$0" "rep_array" nil
			("Functions")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:rv}, ${2:m})$0" "rep_matrix" nil
			("Functions")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:v}, ${2:n})$0" "rep_matrix" nil
			("Functions")
			nil nil nil nil)
		       ("rep_matrix" "rep_matrix(${1:x}, ${2:m}, ${3:n})$0" "rep_matrix" nil
			("Functions")
			nil nil nil nil)
		       ("rep_row_vector" "rep_row_vector(${1:x}, ${2:n})$0" "rep_row_vector" nil
			("Functions")
			nil nil nil nil)
		       ("rep_vector" "rep_vector(${1:x}, ${2:m})$0" "rep_vector" nil
			("Functions")
			nil nil nil nil)
		       ("rising_factorial" "rising_factorial(${1:x}, ${2:n})$0" "rising_factorial" nil
			("Functions")
			nil nil nil nil)
		       ("round" "round(${1:x})$0" "round" nil
			("Functions")
			nil nil nil nil)
		       ("row" "row(${1:x}, ${2:m})$0" "row" nil
			("Functions")
			nil nil nil nil)
		       ("rows" "rows(${1:x})$0" "rows" nil
			("Functions")
			nil nil nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:x}, ${2:y})$0" "rows_dot_product" nil
			("Functions")
			nil nil nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:x})$0" "rows_dot_self" nil
			("Functions")
			nil nil nil nil)
		       ("scaled_inv_chi_square_ccdf_log" "scaled_inv_chi_square_ccdf_log(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("scaled_inv_chi_square_cdf" "scaled_inv_chi_square_cdf(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("scaled_inv_chi_square_cdf_log" "scaled_inv_chi_square_cdf_log(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("scaled_inv_chi_square_log" "scaled_inv_chi_square_log(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_log" nil
			("Functions")
			nil nil nil nil)
		       ("scaled_inv_chi_square_rng" "scaled_inv_chi_square_rng(${1:nu}, ${2:sigma})$0" "scaled_inv_chi_square_rng" nil
			("Functions")
			nil nil nil nil)
		       ("sd" "sd(${1:x})$0" "sd" nil
			("Functions")
			nil nil nil nil)
		       ("sd" "sd(${1:x})$0" "sd" nil
			("Functions")
			nil nil nil nil)
		       ("segment" "segment(${1:sv}, ${2:i}, ${3:n})$0" "segment" nil
			("Functions")
			nil nil nil nil)
		       ("segment" "segment(${1:v}, ${2:i}, ${3:n})$0" "segment" nil
			("Functions")
			nil nil nil nil)
		       ("sin" "sin(${1:x})$0" "sin" nil
			("Functions")
			nil nil nil nil)
		       ("singular_values" "singular_values(${1:A})$0" "singular_values" nil
			("Functions")
			nil nil nil nil)
		       ("sinh" "sinh(${1:x})$0" "sinh" nil
			("Functions")
			nil nil nil nil)
		       ("size" "size(${1:x})$0" "size" nil
			("Functions")
			nil nil nil nil)
		       ("skew_normal_ccdf_log" "skew_normal_ccdf_log(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("skew_normal_cdf" "skew_normal_cdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("skew_normal_cdf_log" "skew_normal_cdf_log(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("skew_normal_log" "skew_normal_log(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_log" nil
			("Functions")
			nil nil nil nil)
		       ("skew_normal_rng" "skew_normal_rng(${1:mu}, ${2:sigma}, ${3:alpha})$0" "skew_normal_rng" nil
			("Functions")
			nil nil nil nil)
		       ("softmax" "softmax(${1:x})$0" "softmax" nil
			("Functions")
			nil nil nil nil)
		       ("sort_asc" "sort_asc(${1:v})$0" "sort_asc" nil
			("Functions")
			nil nil nil nil)
		       ("sort_desc" "sort_desc(${1:v})$0" "sort_desc" nil
			("Functions")
			nil nil nil nil)
		       ("sort_indices_asc" "sort_indices_asc(${1:v})$0" "sort_indices_asc" nil
			("Functions")
			nil nil nil nil)
		       ("sort_indices_desc" "sort_indices_desc(${1:v})$0" "sort_indices_desc" nil
			("Functions")
			nil nil nil nil)
		       ("sqrt" "sqrt(${1:x})$0" "sqrt" nil
			("Functions")
			nil nil nil nil)
		       ("sqrt2" "sqrt2()$0" "sqrt2" nil
			("Functions")
			nil nil nil nil)
		       ("square" "square(${1:x})$0" "square" nil
			("Functions")
			nil nil nil nil)
		       ("squared_distance" "squared_distance(${1:x}, ${2:y})$0" "squared_distance" nil
			("Functions")
			nil nil nil nil)
		       ("squared_distance" "squared_distance(${1:x}, ${2:y})$0" "squared_distance" nil
			("Functions")
			nil nil nil nil)
		       ("step" "step(${1:x})$0" "step" nil
			("Functions")
			nil nil nil nil)
		       ("student_t_ccdf_log" "student_t_ccdf_log(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("student_t_cdf" "student_t_cdf(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("student_t_cdf_log" "student_t_cdf_log(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("student_t_log" "student_t_log(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_log" nil
			("Functions")
			nil nil nil nil)
		       ("student_t_rng" "student_t_rng(${1:nu}, ${2:mu}, ${3:sigma})$0" "student_t_rng" nil
			("Functions")
			nil nil nil nil)
		       ("sub_col" "sub_col(${1:x}, ${2:i}, ${3:j}, ${4:n_rows})$0" "sub_col" nil
			("Functions")
			nil nil nil nil)
		       ("sub_row" "sub_row(${1:x}, ${2:i}, ${3:j}, ${4:n_cols})$0" "sub_row" nil
			("Functions")
			nil nil nil nil)
		       ("sum" "sum(${1:x})$0" "sum" nil
			("Functions")
			nil nil nil nil)
		       ("sum" "sum(${1:x})$0" "sum" nil
			("Functions")
			nil nil nil nil)
		       ("tail" "tail(${1:rv}, ${2:n})$0" "tail" nil
			("Functions")
			nil nil nil nil)
		       ("tail" "tail(${1:sv}, ${2:n})$0" "tail" nil
			("Functions")
			nil nil nil nil)
		       ("tail" "tail(${1:v}, ${2:n})$0" "tail" nil
			("Functions")
			nil nil nil nil)
		       ("tan" "tan(${1:x})$0" "tan" nil
			("Functions")
			nil nil nil nil)
		       ("tanh" "tanh(${1:x})$0" "tanh" nil
			("Functions")
			nil nil nil nil)
		       ("tcrossprod" "tcrossprod(${1:x})$0" "tcrossprod" nil
			("Functions")
			nil nil nil nil)
		       ("tgamma" "tgamma(${1:x})$0" "tgamma" nil
			("Functions")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:a})$0" "to_array_1d" nil
			("Functions")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:m})$0" "to_array_1d" nil
			("Functions")
			nil nil nil nil)
		       ("to_array_1d" "to_array_1d(${1:v})$0" "to_array_1d" nil
			("Functions")
			nil nil nil nil)
		       ("to_array_2d" "to_array_2d(${1:m})$0" "to_array_2d" nil
			("Functions")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:a})$0" "to_matrix" nil
			("Functions")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:m})$0" "to_matrix" nil
			("Functions")
			nil nil nil nil)
		       ("to_matrix" "to_matrix(${1:v})$0" "to_matrix" nil
			("Functions")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:a})$0" "to_row_vector" nil
			("Functions")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:m})$0" "to_row_vector" nil
			("Functions")
			nil nil nil nil)
		       ("to_row_vector" "to_row_vector(${1:v})$0" "to_row_vector" nil
			("Functions")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:a})$0" "to_vector" nil
			("Functions")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:m})$0" "to_vector" nil
			("Functions")
			nil nil nil nil)
		       ("to_vector" "to_vector(${1:v})$0" "to_vector" nil
			("Functions")
			nil nil nil nil)
		       ("trace" "trace(${1:A})$0" "trace" nil
			("Functions")
			nil nil nil nil)
		       ("trace_gen_quad_form" "trace_gen_quad_form(${1:D}, ${2:A}, ${3:B})$0" "trace_gen_quad_form" nil
			("Functions")
			nil nil nil nil)
		       ("trace_quad_form" "trace_quad_form(${1:A}, ${2:B})$0" "trace_quad_form" nil
			("Functions")
			nil nil nil nil)
		       ("trigamma" "trigamma(${1:x})$0" "trigamma" nil
			("Functions")
			nil nil nil nil)
		       ("trunc" "trunc(${1:x})$0" "trunc" nil
			("Functions")
			nil nil nil nil)
		       ("uniform_ccdf_log" "uniform_ccdf_log(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("uniform_cdf" "uniform_cdf(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("uniform_cdf_log" "uniform_cdf_log(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("uniform_log" "uniform_log(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_log" nil
			("Functions")
			nil nil nil nil)
		       ("uniform_rng" "uniform_rng(${1:alpha}, ${2:beta})$0" "uniform_rng" nil
			("Functions")
			nil nil nil nil)
		       ("variance" "variance(${1:x})$0" "variance" nil
			("Functions")
			nil nil nil nil)
		       ("variance" "variance(${1:x})$0" "variance" nil
			("Functions")
			nil nil nil nil)
		       ("von_mises_log" "von_mises_log(${1:y}, ${2:mu}, ${3:kappa})$0" "von_mises_log" nil
			("Functions")
			nil nil nil nil)
		       ("von_mises_rng" "von_mises_rng(${1:mu}, ${2:kappa})$0" "von_mises_rng" nil
			("Functions")
			nil nil nil nil)
		       ("weibull_ccdf_log" "weibull_ccdf_log(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_ccdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("weibull_cdf" "weibull_cdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_cdf" nil
			("Functions")
			nil nil nil nil)
		       ("weibull_cdf_log" "weibull_cdf_log(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_cdf_log" nil
			("Functions")
			nil nil nil nil)
		       ("weibull_log" "weibull_log(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_log" nil
			("Functions")
			nil nil nil nil)
		       ("weibull_rng" "weibull_rng(${1:alpha}, ${2:sigma})$0" "weibull_rng" nil
			("Functions")
			nil nil nil nil)
		       ("wiener_log" "wiener_log(${1:y}, ${2:alpha}, ${3:tau}, ${4:beta}, ${5:delta})$0" "wiener_log" nil
			("Functions")
			nil nil nil nil)
		       ("wishart_log" "wishart_log(${1:W}, ${2:nu}, ${3:Sigma})$0" "wishart_log" nil
			("Functions")
			nil nil nil nil)
		       ("wishart_rng" "wishart_rng(${1:nu}, ${2:Sigma})$0" "wishart_rng" nil
			("Functions")
			nil nil nil nil)))


