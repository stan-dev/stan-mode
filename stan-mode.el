;; Stan mode for v1.2.0 of the language

(defvar stan-blocks
  '("data" "transformed data" 
    "parameters" "transformed parameters" 
    "model" "generated quantities")
  "Model blocks in Stan")
(defvar stan-types
  '("int" "real" "vector" "ordered" "positive_ordered" "simplex" "row_vector" 
    "matrix" "corr_matrix" "cov_matrix")
  "Types in Stan")
(defvar stan-type-bounds
  '("lower" "upper")
  "Types in Stan")
(defvar stan-distributions
  '("bernoulli" "bernoulli_logit" "beta_binomial" 
    "beta" "binomial" "categorical" "cauchy" "chi_square" "dirichlet"
    "double_exponential" "exponential" "gamma" "hypergeometric" 
    "inv_chi_square" "inv_gamma" 
    "inv_wishart" "lkj_corr_cholesky" "lkj_corr" "lkj_cov"
    "logistic" "lognormal" 
    "multi_normal_cholesky" "multi_normal" "multi_student_t"
    "multinomial" "neg_binomial" "normal" 
    "ordered_logistic"
    "pareto" "poisson" "poisson_log" "scaled_inv_chi_square" 
    "student_t" "uniform"
    "weibull" "wishart")
  "Distributions in Stan (see index)")
(defvar stan-cdfs
  '("bernoulli_cdf" "beta_binomial_cdf" "beta_cdf" "binomial_cdf" 
    "exponential_cdf" "inv_chi_square_cdf" "inv_gamma_cdf" "logistic_cdf" 
    "lognormal_cdf" "neg_binomial_cdf" "normal_cdf" "pareto_cdf" 
    "poisson_cdf" "scaled_inv_chi_square_cdf" "student_t_cdf")
  "CDFs in Stan (see index)")
(defvar stan-functions
  '("Phi" "Phi_approx" "abs" "acos" "acosh" "asin"
    "asinh" "atan" "atan2" "atanh" "binary_log_loss"
    "binomial_coefficient_log" "block" "cbrt" "ceil" "cholesky_decompose" 
    "col" "cols" "cos" "cosh" "crossprod" "cumulative_sum" "determinant" 
    "diag_matrix" "diag_post_multiply" "diag_pre_multiply" "diagonal" 
    "dims" "dot_product" "dot_self" "e" 
    "eigenvalues_sym" "eigenvectors_sym" "epsilon"
    "erf" "erfc" "exp" "exp2" "expm1" 
    "fabs" "fdim" "floor" "fma" "fmax" 
    "fmin" "fmod" "hypot" "if_else" 
    "int_step" "inv_cloglog" "inv_logit" "inverse" "lbeta" 
    "lgamma" "lmgamma" "log" "log10" "log1m" "log1m_inv_logit"
    "log1p" "log1p_exp" "log2" "log_determinant" "log_inv_logit"
    "log_sum_exp" "logit" 
    "max" "mdivide_left_tri_low" "mdivide_right_tri_low" "mean" 
    "min" "multiply_log" "multiply_lower_tri_self_transpose" 
    "negative_epsilon" "negative_infinity"
    "not_a_number" "pi" "positive_infinity" "pow" "prod" 
    "round" "row" "rows" "sd" "sin" "singular_values" 
    "sinh" "size" "softmax" "sqrt" "sqrt2" 
    "square" "step" "sum" "tan" "tanh" 
    "tcrossprod" "tgamma" "trace" "trunc" "variance")
  "Functions in Stan (excluding distributions and cdfs)")
(defvar stan-keywords
  '("for")
  "Keywords in Stan")

(defvar stan-font-lock-defaults
  `((
     ;; Stan model blocks. Look for start of line before and open brace after.
     ( ,(concat "^[[:space:]]*\\(" (regexp-opt stan-blocks 'words) "\\)\\s-*{") 1 font-lock-keyword-face)
     ;; Stan types. Look for it to come after the start of a line or semicolon.
     ( ,(concat "\\(^\\|;\\)\\s-*" (regexp-opt stan-types 'words)) 2 font-lock-type-face)
     ;; Stan type bounds. Look for them after either '<' or ','
     ( ,(concat "\\(<\\|,\\)\\s-*" (regexp-opt stan-type-bounds 'words)) 2 font-lock-constant-face)
     ;; Stan variables. Look for it to come after the types, anything, then '>'
     ( ,(concat "\\(^\\|;\\)\\s-*" (regexp-opt stan-types 'words) "\\s-*\\(\\w*\\)") 3 font-lock-variable-name-face)
     ( ,(concat "\\(^\\|;\\)\\s-*" (regexp-opt stan-types 'words) "<.*>\\s-*\\(\\w*\\)") 3 font-lock-variable-name-face)
     ;; Stan functions.
     ( ,(regexp-opt stan-functions 'words) . font-lock-builtin-face)
     ;; Stan distributions. Look for distribution after '~'
     ( ,(concat "~[[:space:]]*" (regexp-opt stan-distributions 'words)) 1 font-lock-builtin-face)
     ;; Stan distributions. Look for distribution_log after '<-'
     ( ,(concat "<-\\s-*\\(\\<" (regexp-opt stan-distributions) "_log\\>\\)") 1 font-lock-builtin-face)
     ;; Stan distributions. Look for cdfs after '<-'
     ( ,(concat "<-[[:space:]]*" (regexp-opt stan-cdfs 'words)) 1 font-lock-builtin-face)
     ;; Stan keywords.
     ( ,(regexp-opt stan-keywords 'words) . font-lock-keyword-face)

     ))
  "Stan-mode font lock defaults"
  )

(define-derived-mode 
  stan-mode   ; variant
  c++-mode    ; parent
  "stan-mode" ; name
  "Stan mode is a mode for editing Stan models" ; doc string
  ;; keyword-args
  ;; body
  ;; comments: '#', '//', or '/*' '*/' pair
  (modify-syntax-entry ?# "< b" stan-mode-syntax-table)
  (modify-syntax-entry ?_ "w" stan-mode-syntax-table)
  (modify-syntax-entry ?\' "_" stan-mode-syntax-table)
  (setq font-lock-defaults stan-font-lock-defaults)   ; stan fonts
  )


(add-to-list 'auto-mode-alist 
	     '("\\.stan\\'" . stan-mode))

(provide 'stan-mode)
