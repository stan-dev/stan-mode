;;; Compiled snippets and support files for `stan-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("while" "while (${1:condition}) {\n    $0\n}" "while (...) { ... }" nil
			("Structure")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/while.yasnippet" nil nil)
		       ("vector" "vector${1:<${2:lower=...,upper=...}>}[${3:expression}] ${4:variable}${5:[${6:dims}]};\n$0" "vector[] ...;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/vector.yasnippet" nil nil)
		       ("<" "<upper=${1:0}>$0" "<upper=...>" nil
			("Range Constraints")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/upper.yasnippet" nil nil)
		       ("uvector" "unit_vector[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "unit_vector[] ...;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/unit_vector.yasnippet" nil nil)
		       ("tparam" "transformed parameters {\n  $0\n}\n" "transformed parameters{...}" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/transformed_parameters.yasnippet" nil nil)
		       ("tdata" "transformed data {\n  $0\n}\n" "transformed data {...}" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/transformed_data.yasnippet" nil nil)
		       ("target" "target += $0;\n" "target += ..." nil nil nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/target.yasnippet" nil nil)
		       ("simplex" "simplex[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "simplex" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/simplex.yasnippet" nil nil)
		       ("rvector" "row_vector${1:<${2:lower=...,upper=...}>}[${3:expression}] ${4:variable}${5:[${6:dims}]};\n$0" "row_vector[] ...;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/row_vector.yasnippet" nil nil)
		       ("reject" "reject($1);\n$0" "reject(...)" nil nil nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/reject.yasnippet" nil nil)
		       ("real" "real${1:<${2:lower=...,upper=...}>} ${3:variable}${4:[${5:dims}]};\n$0" "real ...;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/real.yasnippet" nil nil)
		       ("C-c C-t" "data {\n  $0\n}\ntransformed data {\n}\nparameters {\n}\ntransformed parameters {\n}\nmodel {\n}\ngenerated quantities {\n}" "data {...} ..." nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/program.yasnippet" nil nil)
		       ("print" "print($1);\n$0" "print(...)" nil nil nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/print.yasnippet" nil nil)
		       ("pordered" "positive_ordered[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "positive_ordered[] ...;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/positive_ordered.yasnippet" nil nil)
		       ("param" "parameters {\n  $0\n}\n" "parameters {...}" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/parameters.yasnippet" nil nil)
		       ("ordered" "ordered[${1:dim}] ${2:variable}${3:[${4:dims}]};\n$0" "ordered[] ...;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/ordered.yasnippet" nil nil)
		       ("model" "model {\n  $0\n}\n" "model {...}" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/model.yasnippet" nil nil)
		       ("matrix" "matrix${1:<${2:lower=...,upper=...}>}[$3, $4] ${5:variable}${6:[${7:dims}]};\n$0" "matrix[] ...;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/matrix.yasnippet" nil nil)
		       ("<" "<lower=${1:0},upper=${2:1}>$0" "<lower=..., upper=...>" nil
			("Range Constraints")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/lower_upper.yasnippet" nil nil)
		       ("<" "<lower=${1:0}>$0" "<lower=...>" nil
			("Range Constraints")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/lower.yasnippet" nil nil)
		       ("integrate_ode_rk45" "integrate_ode_rk45(${1:function}, ${2:y0}, ${3:t0},${4:t}, ${5:theta}, ${6:x_r}, ${7:x_i}${8:${9:real_tol}, ${10:abs_tol}, ${11:max_num_steps}});\n$0" "integrate_ode_rk45(...);" nil nil nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/integrate_ode_rk45.yasnippet" nil nil)
		       ("integrate_ode_bdf" "integrate_ode_bdf(${1:function}, ${2:y0}, ${3:t0},${4:t}, ${5:theta}, ${6:x_r}, ${7:x_i}${8:${9:real_tol}, ${10:abs_tol}, ${11:max_num_steps}});\n$0" "integrate_ode_bdf(...);" nil nil nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/integrate_ode_bdf.yasnippet" nil nil)
		       ("int" "int${1:<${2:lower=...,upper=...}>} ${3:variable}${4:[${5:dims}]};\n$0\n" "int ... ;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/int.yasnippet" nil nil)
		       ("if" "if (${1:condition}) {\n    $0\n}" "if (...) { ... }" nil
			("Structure")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/if.yasnippet" nil nil)
		       ("generated" "generated quantities {\n  $0\n}\n" "generated quantities {...}" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/generated_quantities.yasnippet" nil nil)
		       ("functions" "functions {\n  $0\n}\n" "functions {...}" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions.yasnippet" nil nil)
		       ("fun" "${1:return} ${2:name} (${args}) {\n  $0\n}\n" "User-defined function" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/fun.yasnippet" nil nil)
		       ("for" "for (${1:i} in ${2:1}:${3:N}) {\n    $0\n}\n" "for (...; ...; ...) { ... }" nil
			("Structure")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/for.yasnippet" nil nil)
		       ("else" "else {\n    $0\n}" "else { ... }" nil
			("Structure")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/else.yasnippet" nil nil)
		       ("elif" "else if (${1:condition}) {\n    $0\n}" "else if (...) { ... }" nil
			("Structure")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/elif.yasnippet" nil nil)
		       ("data" "data {\n  $0\n}\n" "data {...}" nil
			("Blocks")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/data.yasnippet" nil nil)
		       ("cov_matrix" "cov_matrix[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "cov_matrix[] ... ;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/cov_matrix.yasnippet" nil nil)
		       ("corr_matrix" "corr_matrix[${1:expression}] ${2:variable}${3:[${4:dims}]};\n$0" "corr_matrix[] ... ;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/corr_matrix.yasnippet" nil nil)
		       ("cholesky_factor_cov" "cholesky_factor_cov[${1:expression}${2:${3:, ${4:expression}}}] ${5:variable}${6:[${7:dims}]};\n$0" "cholesky_factor_cov[] ... ;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/cholesky_factor_cov.yasnippet" nil nil)
		       ("cholesky_factor_corr" "cholesky_factor_corr[${1:expression}${2:${3:, ${4:expression}}}] ${5:variable}${6:[${7:dims}]};\n$0" "cholesky_factor_corr[] ... ;" nil
			("Types")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/cholesky_factor_corr.yasnippet" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'stan-mode
		     '(("wishart_rng" "wishart_rng(${1:nu}, ${2:Sigma})$0" "wishart_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/wishart_rng(nu,Sigma).yasnippet" nil nil)
		       ("wishart_lpdf" "wishart_lpdf(${1:W}, ${2:nu}, ${3:Sigma})$0" "wishart_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/wishart_lpdf(W,nu,Sigma).yasnippet" nil nil)
		       ("wiener_lpdf" "wiener_lpdf(${1:y}, ${2:alpha}, ${3:tau}, ${4:beta}, ${5:delta})$0" "wiener_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/wiener_lpdf(y,alpha,tau,beta,delta).yasnippet" nil nil)
		       ("weibull_rng" "weibull_rng(${1:alpha}, ${2:sigma})$0" "weibull_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/weibull_rng(alpha,sigma).yasnippet" nil nil)
		       ("weibull_lpdf" "weibull_lpdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/weibull_lpdf(y,alpha,sigma).yasnippet" nil nil)
		       ("weibull_lcdf" "weibull_lcdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/weibull_lcdf(y,alpha,sigma).yasnippet" nil nil)
		       ("weibull_lccdf" "weibull_lccdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/weibull_lccdf(y,alpha,sigma).yasnippet" nil nil)
		       ("weibull_cdf" "weibull_cdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "weibull_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/weibull_cdf(y,alpha,sigma).yasnippet" nil nil)
		       ("von_mises_rng" "von_mises_rng(${1:mu}, ${2:kappa})$0" "von_mises_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/von_mises_rng(mu,kappa).yasnippet" nil nil)
		       ("von_mises_lpdf" "von_mises_lpdf(${1:y}, ${2:mu}, ${3:kappa})$0" "von_mises_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/von_mises_lpdf(y,mu,kappa).yasnippet" nil nil)
		       ("variance" "variance(${1:x})$0" "variance" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/variance(x[]).yasnippet" nil nil)
		       ("variance" "variance(${1:x})$0" "variance" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/variance(x).yasnippet" nil nil)
		       ("uniform_rng" "uniform_rng(${1:alpha}, ${2:beta})$0" "uniform_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/uniform_rng(alpha,beta).yasnippet" nil nil)
		       ("uniform_lpdf" "uniform_lpdf(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/uniform_lpdf(y,alpha,beta).yasnippet" nil nil)
		       ("uniform_lcdf" "uniform_lcdf(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/uniform_lcdf(y,alpha,beta).yasnippet" nil nil)
		       ("uniform_lccdf" "uniform_lccdf(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/uniform_lccdf(y,alpha,beta).yasnippet" nil nil)
		       ("uniform_cdf" "uniform_cdf(${1:y}, ${2:alpha}, ${3:beta})$0" "uniform_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/uniform_cdf(y,alpha,beta).yasnippet" nil nil)
		       ("trunc" "trunc(${1:x})$0" "trunc" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/trunc(x).yasnippet" nil nil)
		       ("trigamma" "trigamma(${1:x})$0" "trigamma" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/trigamma(x).yasnippet" nil nil)
		       ("trace_quad_form" "trace_quad_form(${1:A}, ${2:B})$0" "trace_quad_form" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/trace_quad_form(A,B).yasnippet" nil nil)
		       ("trace_gen_quad_form" "trace_gen_quad_form(${1:D}, ${2:A}, ${3:B})$0" "trace_gen_quad_form" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/trace_gen_quad_form(D,A,B).yasnippet" nil nil)
		       ("trace" "trace(${1:A})$0" "trace" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/trace(A).yasnippet" nil nil)
		       ("to_vector" "to_vector(${1:v})$0" "to_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_vector(v).yasnippet" nil nil)
		       ("to_vector" "to_vector(${1:m})$0" "to_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_vector(m).yasnippet" nil nil)
		       ("to_vector" "to_vector(${1:a})$0" "to_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_vector(a).yasnippet" nil nil)
		       ("to_row_vector" "to_row_vector(${1:v})$0" "to_row_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_row_vector(v).yasnippet" nil nil)
		       ("to_row_vector" "to_row_vector(${1:m})$0" "to_row_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_row_vector(m).yasnippet" nil nil)
		       ("to_row_vector" "to_row_vector(${1:a})$0" "to_row_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_row_vector(a).yasnippet" nil nil)
		       ("to_matrix" "to_matrix(${1:v})$0" "to_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_matrix(v).yasnippet" nil nil)
		       ("to_matrix" "to_matrix(${1:m})$0" "to_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_matrix(m).yasnippet" nil nil)
		       ("to_matrix" "to_matrix(${1:a})$0" "to_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_matrix(a).yasnippet" nil nil)
		       ("to_array_2d" "to_array_2d(${1:m})$0" "to_array_2d" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_array_2d(m).yasnippet" nil nil)
		       ("to_array_1d" "to_array_1d(${1:v})$0" "to_array_1d" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_array_1d(v).yasnippet" nil nil)
		       ("to_array_1d" "to_array_1d(${1:m})$0" "to_array_1d" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_array_1d(m).yasnippet" nil nil)
		       ("to_array_1d" "to_array_1d(${1:a})$0" "to_array_1d" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/to_array_1d(a).yasnippet" nil nil)
		       ("tgamma" "tgamma(${1:x})$0" "tgamma" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/tgamma(x).yasnippet" nil nil)
		       ("tcrossprod" "tcrossprod(${1:x})$0" "tcrossprod" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/tcrossprod(x).yasnippet" nil nil)
		       ("target" "target()$0" "target" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/target().yasnippet" nil nil)
		       ("tanh" "tanh(${1:x})$0" "tanh" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/tanh(x).yasnippet" nil nil)
		       ("tan" "tan(${1:x})$0" "tan" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/tan(x).yasnippet" nil nil)
		       ("tail" "tail(${1:v}, ${2:n})$0" "tail" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/tail(v,n).yasnippet" nil nil)
		       ("tail" "tail(${1:sv}, ${2:n})$0" "tail" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/tail(sv,n).yasnippet" nil nil)
		       ("tail" "tail(${1:rv}, ${2:n})$0" "tail" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/tail(rv,n).yasnippet" nil nil)
		       ("sum" "sum(${1:x})$0" "sum" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sum(x[]).yasnippet" nil nil)
		       ("sum" "sum(${1:x})$0" "sum" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sum(x).yasnippet" nil nil)
		       ("sub_row" "sub_row(${1:x}, ${2:i}, ${3:j}, ${4:n_cols})$0" "sub_row" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sub_row(x,i,j,n_cols).yasnippet" nil nil)
		       ("sub_col" "sub_col(${1:x}, ${2:i}, ${3:j}, ${4:n_rows})$0" "sub_col" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sub_col(x,i,j,n_rows).yasnippet" nil nil)
		       ("student_t_rng" "student_t_rng(${1:nu}, ${2:mu}, ${3:sigma})$0" "student_t_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/student_t_rng(nu,mu,sigma).yasnippet" nil nil)
		       ("student_t_lpdf" "student_t_lpdf(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/student_t_lpdf(y,nu,mu,sigma).yasnippet" nil nil)
		       ("student_t_lcdf" "student_t_lcdf(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/student_t_lcdf(y,nu,mu,sigma).yasnippet" nil nil)
		       ("student_t_lccdf" "student_t_lccdf(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/student_t_lccdf(y,nu,mu,sigma).yasnippet" nil nil)
		       ("student_t_cdf" "student_t_cdf(${1:y}, ${2:nu}, ${3:mu}, ${4:sigma})$0" "student_t_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/student_t_cdf(y,nu,mu,sigma).yasnippet" nil nil)
		       ("step" "step(${1:x})$0" "step" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/step(x).yasnippet" nil nil)
		       ("squared_distance" "squared_distance(${1:x}, ${2:y})$0" "squared_distance" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/squared_distance(x,y[]).yasnippet" nil nil)
		       ("squared_distance" "squared_distance(${1:x}, ${2:y})$0" "squared_distance" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/squared_distance(x,y).yasnippet" nil nil)
		       ("square" "square(${1:x})$0" "square" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/square(x).yasnippet" nil nil)
		       ("sqrt2" "sqrt2()$0" "sqrt2" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sqrt2().yasnippet" nil nil)
		       ("sqrt" "sqrt(${1:x})$0" "sqrt" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sqrt(x).yasnippet" nil nil)
		       ("sort_indices_desc" "sort_indices_desc(${1:v})$0" "sort_indices_desc" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sort_indices_desc(v).yasnippet" nil nil)
		       ("sort_indices_asc" "sort_indices_asc(${1:v})$0" "sort_indices_asc" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sort_indices_asc(v).yasnippet" nil nil)
		       ("sort_desc" "sort_desc(${1:v})$0" "sort_desc" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sort_desc(v).yasnippet" nil nil)
		       ("sort_asc" "sort_asc(${1:v})$0" "sort_asc" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sort_asc(v).yasnippet" nil nil)
		       ("softmax" "softmax(${1:x})$0" "softmax" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/softmax(x).yasnippet" nil nil)
		       ("skew_normal_rng" "skew_normal_rng(${1:mu}, ${2:sigma}, ${3:alpha})$0" "skew_normal_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/skew_normal_rng(mu,sigma,alpha).yasnippet" nil nil)
		       ("skew_normal_lpdf" "skew_normal_lpdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/skew_normal_lpdf(y,mu,sigma,alpha).yasnippet" nil nil)
		       ("skew_normal_lcdf" "skew_normal_lcdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/skew_normal_lcdf(y,mu,sigma,alpha).yasnippet" nil nil)
		       ("skew_normal_lccdf" "skew_normal_lccdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/skew_normal_lccdf(y,mu,sigma,alpha).yasnippet" nil nil)
		       ("skew_normal_cdf" "skew_normal_cdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:alpha})$0" "skew_normal_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/skew_normal_cdf(y,mu,sigma,alpha).yasnippet" nil nil)
		       ("size" "size(${1:x})$0" "size" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/size(x).yasnippet" nil nil)
		       ("sinh" "sinh(${1:x})$0" "sinh" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sinh(x).yasnippet" nil nil)
		       ("singular_values" "singular_values(${1:A})$0" "singular_values" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/singular_values(A).yasnippet" nil nil)
		       ("sin" "sin(${1:x})$0" "sin" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sin(x).yasnippet" nil nil)
		       ("segment" "segment(${1:v}, ${2:i}, ${3:n})$0" "segment" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/segment(v,i,n).yasnippet" nil nil)
		       ("segment" "segment(${1:sv}, ${2:i}, ${3:n})$0" "segment" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/segment(sv,i,n).yasnippet" nil nil)
		       ("segment" "segment(${1:rv}, ${2:i}, ${3:n})$0" "segment" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/segment(rv,i,n).yasnippet" nil nil)
		       ("sd" "sd(${1:x})$0" "sd" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sd(x[]).yasnippet" nil nil)
		       ("sd" "sd(${1:x})$0" "sd" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/sd(x).yasnippet" nil nil)
		       ("scaled_inv_chi_square_rng" "scaled_inv_chi_square_rng(${1:nu}, ${2:sigma})$0" "scaled_inv_chi_square_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/scaled_inv_chi_square_rng(nu,sigma).yasnippet" nil nil)
		       ("scaled_inv_chi_square_lpdf" "scaled_inv_chi_square_lpdf(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/scaled_inv_chi_square_lpdf(y,nu,sigma).yasnippet" nil nil)
		       ("scaled_inv_chi_square_lcdf" "scaled_inv_chi_square_lcdf(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/scaled_inv_chi_square_lcdf(y,nu,sigma).yasnippet" nil nil)
		       ("scaled_inv_chi_square_lccdf" "scaled_inv_chi_square_lccdf(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/scaled_inv_chi_square_lccdf(y,nu,sigma).yasnippet" nil nil)
		       ("scaled_inv_chi_square_cdf" "scaled_inv_chi_square_cdf(${1:y}, ${2:nu}, ${3:sigma})$0" "scaled_inv_chi_square_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/scaled_inv_chi_square_cdf(y,nu,sigma).yasnippet" nil nil)
		       ("rows_dot_self" "rows_dot_self(${1:x})$0" "rows_dot_self" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rows_dot_self(x).yasnippet" nil nil)
		       ("rows_dot_product" "rows_dot_product(${1:x}, ${2:y})$0" "rows_dot_product" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rows_dot_product(x,y).yasnippet" nil nil)
		       ("rows" "rows(${1:x})$0" "rows" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rows(x).yasnippet" nil nil)
		       ("row" "row(${1:x}, ${2:m})$0" "row" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/row(x,m).yasnippet" nil nil)
		       ("round" "round(${1:x})$0" "round" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/round(x).yasnippet" nil nil)
		       ("rising_factorial" "rising_factorial(${1:x}, ${2:n})$0" "rising_factorial" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rising_factorial(x,n).yasnippet" nil nil)
		       ("rep_vector" "rep_vector(${1:x}, ${2:m})$0" "rep_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_vector(x,m).yasnippet" nil nil)
		       ("rep_row_vector" "rep_row_vector(${1:x}, ${2:n})$0" "rep_row_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_row_vector(x,n).yasnippet" nil nil)
		       ("rep_matrix" "rep_matrix(${1:x}, ${2:m}, ${3:n})$0" "rep_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_matrix(x,m,n).yasnippet" nil nil)
		       ("rep_matrix" "rep_matrix(${1:v}, ${2:n})$0" "rep_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_matrix(v,n).yasnippet" nil nil)
		       ("rep_matrix" "rep_matrix(${1:rv}, ${2:m})$0" "rep_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_matrix(rv,m).yasnippet" nil nil)
		       ("rep_array" "rep_array(${1:x}, ${2:n})$0" "rep_array" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_array(x,n).yasnippet" nil nil)
		       ("rep_array" "rep_array(${1:x}, ${2:m}, ${3:n})$0" "rep_array" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_array(x,m,n).yasnippet" nil nil)
		       ("rep_array" "rep_array(${1:x}, ${2:k}, ${3:m}, ${4:n})$0" "rep_array" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rep_array(x,k,m,n).yasnippet" nil nil)
		       ("rayleigh_rng" "rayleigh_rng(${1:sigma})$0" "rayleigh_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rayleigh_rng(sigma).yasnippet" nil nil)
		       ("rayleigh_lpdf" "rayleigh_lpdf(${1:y}, ${2:sigma})$0" "rayleigh_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rayleigh_lpdf(y,sigma).yasnippet" nil nil)
		       ("rayleigh_lcdf" "rayleigh_lcdf(${1:y}, ${2:sigma})$0" "rayleigh_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rayleigh_lcdf(y,sigma).yasnippet" nil nil)
		       ("rayleigh_lccdf" "rayleigh_lccdf(${1:y}, ${2:sigma})$0" "rayleigh_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rayleigh_lccdf(y,sigma).yasnippet" nil nil)
		       ("rayleigh_cdf" "rayleigh_cdf(${1:y}, ${2:sigma})$0" "rayleigh_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rayleigh_cdf(y,sigma).yasnippet" nil nil)
		       ("rank" "rank(${1:v}, ${2:s})$0" "rank" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/rank(v,s).yasnippet" nil nil)
		       ("quad_form_sym" "quad_form_sym(${1:A}, ${2:B})$0" "quad_form_sym" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/quad_form_sym(A,B).yasnippet" nil nil)
		       ("quad_form_diag" "quad_form_diag(${1:m}, ${2:v})$0" "quad_form_diag" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/quad_form_diag(m,v).yasnippet" nil nil)
		       ("quad_form_diag" "quad_form_diag(${1:m}, ${2:rv})$0" "quad_form_diag" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/quad_form_diag(m,rv).yasnippet" nil nil)
		       ("quad_form" "quad_form(${1:A}, ${2:B})$0" "quad_form" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/quad_form(A,B).yasnippet" nil nil)
		       ("qr_R" "qr_R(${1:A})$0" "qr_R" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/qr_R(A).yasnippet" nil nil)
		       ("qr_Q" "qr_Q(${1:A})$0" "qr_Q" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/qr_Q(A).yasnippet" nil nil)
		       ("prod" "prod(${1:x})$0" "prod" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/prod(x[]).yasnippet" nil nil)
		       ("prod" "prod(${1:x})$0" "prod" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/prod(x).yasnippet" nil nil)
		       ("print" "print(${1:x1}, ${2:...}, ${3:xN})$0" "print" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/print(x1,...,xN).yasnippet" nil nil)
		       ("pow" "pow(${1:x}, ${2:y})$0" "pow" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pow(x,y).yasnippet" nil nil)
		       ("positive_infinity" "positive_infinity()$0" "positive_infinity" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/positive_infinity().yasnippet" nil nil)
		       ("poisson_rng" "poisson_rng(${1:lambda})$0" "poisson_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/poisson_rng(lambda).yasnippet" nil nil)
		       ("poisson_lpmf" "poisson_lpmf(${1:n}, ${2:lambda})$0" "poisson_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/poisson_lpmf(n,lambda).yasnippet" nil nil)
		       ("poisson_log_rng" "poisson_log_rng(${1:alpha})$0" "poisson_log_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/poisson_log_rng(alpha).yasnippet" nil nil)
		       ("poisson_log_lpmf" "poisson_log_lpmf(${1:n}, ${2:alpha})$0" "poisson_log_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/poisson_log_lpmf(n,alpha).yasnippet" nil nil)
		       ("poisson_lcdf" "poisson_lcdf(${1:n}, ${2:lambda})$0" "poisson_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/poisson_lcdf(n,lambda).yasnippet" nil nil)
		       ("poisson_lccdf" "poisson_lccdf(${1:n}, ${2:lambda})$0" "poisson_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/poisson_lccdf(n,lambda).yasnippet" nil nil)
		       ("poisson_cdf" "poisson_cdf(${1:n}, ${2:lambda})$0" "poisson_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/poisson_cdf(n,lambda).yasnippet" nil nil)
		       ("pi" "pi()$0" "pi" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pi().yasnippet" nil nil)
		       ("pareto_type_2_rng" "pareto_type_2_rng(${1:mu}, ${2:lambda}, ${3:alpha})$0" "pareto_type_2_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_type_2_rng(mu,lambda,alpha).yasnippet" nil nil)
		       ("pareto_type_2_lpdf" "pareto_type_2_lpdf(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_type_2_lpdf(y,mu,lambda,alpha).yasnippet" nil nil)
		       ("pareto_type_2_lcdf" "pareto_type_2_lcdf(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_type_2_lcdf(y,mu,lambda,alpha).yasnippet" nil nil)
		       ("pareto_type_2_lccdf" "pareto_type_2_lccdf(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_type_2_lccdf(y,mu,lambda,alpha).yasnippet" nil nil)
		       ("pareto_type_2_cdf" "pareto_type_2_cdf(${1:y}, ${2:mu}, ${3:lambda}, ${4:alpha})$0" "pareto_type_2_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_type_2_cdf(y,mu,lambda,alpha).yasnippet" nil nil)
		       ("pareto_rng" "pareto_rng(${1:y_min}, ${2:alpha})$0" "pareto_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_rng(y_min,alpha).yasnippet" nil nil)
		       ("pareto_lpdf" "pareto_lpdf(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_lpdf(y,y_min,alpha).yasnippet" nil nil)
		       ("pareto_lcdf" "pareto_lcdf(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_lcdf(y,y_min,alpha).yasnippet" nil nil)
		       ("pareto_lccdf" "pareto_lccdf(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_lccdf(y,y_min,alpha).yasnippet" nil nil)
		       ("pareto_cdf" "pareto_cdf(${1:y}, ${2:y_min}, ${3:alpha})$0" "pareto_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/pareto_cdf(y,y_min,alpha).yasnippet" nil nil)
		       ("owens_t" "owens_t(${1:h}, ${2:a})$0" "owens_t" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/owens_t(h,a).yasnippet" nil nil)
		       ("ordered_logistic_rng" "ordered_logistic_rng(${1:eta}, ${2:c})$0" "ordered_logistic_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/ordered_logistic_rng(eta,c).yasnippet" nil nil)
		       ("ordered_logistic_lpmf" "ordered_logistic_lpmf(${1:k}, ${2:eta}, ${3:c})$0" "ordered_logistic_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/ordered_logistic_lpmf(k,eta,c).yasnippet" nil nil)
		       ("num_elements" "num_elements(${1:x})$0" "num_elements" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/num_elements(x).yasnippet" nil nil)
		       ("not_a_number" "not_a_number()$0" "not_a_number" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/not_a_number().yasnippet" nil nil)
		       ("normal_rng" "normal_rng(${1:mu}, ${2:sigma})$0" "normal_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/normal_rng(mu,sigma).yasnippet" nil nil)
		       ("normal_lpdf" "normal_lpdf(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/normal_lpdf(y,mu,sigma).yasnippet" nil nil)
		       ("normal_lcdf" "normal_lcdf(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/normal_lcdf(y,mu,sigma).yasnippet" nil nil)
		       ("normal_lccdf" "normal_lccdf(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/normal_lccdf(y,mu,sigma).yasnippet" nil nil)
		       ("normal_cdf" "normal_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "normal_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/normal_cdf(y,mu,sigma).yasnippet" nil nil)
		       ("negative_infinity" "negative_infinity()$0" "negative_infinity" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/negative_infinity().yasnippet" nil nil)
		       ("neg_binomial_rng" "neg_binomial_rng(${1:alpha}, ${2:beta})$0" "neg_binomial_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_rng(alpha,beta).yasnippet" nil nil)
		       ("neg_binomial_lpmf" "neg_binomial_lpmf(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_lpmf(n,alpha,beta).yasnippet" nil nil)
		       ("neg_binomial_lcdf" "neg_binomial_lcdf(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_lcdf(n,alpha,beta).yasnippet" nil nil)
		       ("neg_binomial_lccdf" "neg_binomial_lccdf(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_lccdf(n,alpha,beta).yasnippet" nil nil)
		       ("neg_binomial_cdf" "neg_binomial_cdf(${1:n}, ${2:alpha}, ${3:beta})$0" "neg_binomial_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_cdf(n,alpha,beta).yasnippet" nil nil)
		       ("neg_binomial_2_rng" "neg_binomial_2_rng(${1:mu}, ${2:phi})$0" "neg_binomial_2_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_2_rng(mu,phi).yasnippet" nil nil)
		       ("neg_binomial_2_lpmf" "neg_binomial_2_lpmf(${1:y}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_2_lpmf(y,mu,phi).yasnippet" nil nil)
		       ("neg_binomial_2_log_rng" "neg_binomial_2_log_rng(${1:eta}, ${2:phi})$0" "neg_binomial_2_log_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_2_log_rng(eta,phi).yasnippet" nil nil)
		       ("neg_binomial_2_log_lpmf" "neg_binomial_2_log_lpmf(${1:y}, ${2:eta}, ${3:phi})$0" "neg_binomial_2_log_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_2_log_lpmf(y,eta,phi).yasnippet" nil nil)
		       ("neg_binomial_2_lcdf" "neg_binomial_2_lcdf(${1:n}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_2_lcdf(n,mu,phi).yasnippet" nil nil)
		       ("neg_binomial_2_lccdf" "neg_binomial_2_lccdf(${1:n}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_2_lccdf(n,mu,phi).yasnippet" nil nil)
		       ("neg_binomial_2_cdf" "neg_binomial_2_cdf(${1:n}, ${2:mu}, ${3:phi})$0" "neg_binomial_2_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/neg_binomial_2_cdf(n,mu,phi).yasnippet" nil nil)
		       ("multiply_lower_tri_self_transpose" "multiply_lower_tri_self_transpose(${1:x})$0" "multiply_lower_tri_self_transpose" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multiply_lower_tri_self_transpose(x).yasnippet" nil nil)
		       ("multinomial_rng" "multinomial_rng(${1:theta}, ${2:N})$0" "multinomial_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multinomial_rng(theta,N).yasnippet" nil nil)
		       ("multinomial_lpmf" "multinomial_lpmf(${1:y}, ${2:theta})$0" "multinomial_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multinomial_lpmf(y,theta).yasnippet" nil nil)
		       ("multi_student_t_rng" "multi_student_t_rng(${1:nu}, ${2:mu}, ${3:Sigma})$0" "multi_student_t_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_student_t_rng(nu,mu,Sigma).yasnippet" nil nil)
		       ("multi_student_t_lpdf" "multi_student_t_lpdf(${1:y}, ${2:nu}, ${3:mu}, ${4:Sigma})$0" "multi_student_t_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_student_t_lpdf(y,nu,mu,Sigma).yasnippet" nil nil)
		       ("multi_normal_rng" "multi_normal_rng(${1:mu}, ${2:Sigma})$0" "multi_normal_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_normal_rng(mu,Sigma).yasnippet" nil nil)
		       ("multi_normal_prec_lpdf" "multi_normal_prec_lpdf(${1:y}, ${2:mu}, ${3:Omega})$0" "multi_normal_prec_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_normal_prec_lpdf(y,mu,Omega).yasnippet" nil nil)
		       ("multi_normal_lpdf" "multi_normal_lpdf(${1:y}, ${2:mu}, ${3:Sigma})$0" "multi_normal_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_normal_lpdf(y,mu,Sigma).yasnippet" nil nil)
		       ("multi_normal_cholesky_rng" "multi_normal_cholesky_rng(${1:mu}, ${2:L})$0" "multi_normal_cholesky_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_normal_cholesky_rng(mu,L).yasnippet" nil nil)
		       ("multi_normal_cholesky_lpdf" "multi_normal_cholesky_lpdf(${1:y}, ${2:mu}, ${3:L})$0" "multi_normal_cholesky_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_normal_cholesky_lpdf(y,mu,L).yasnippet" nil nil)
		       ("multi_gp_lpdf" "multi_gp_lpdf(${1:y}, ${2:Sigma}, ${3:w})$0" "multi_gp_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_gp_lpdf(y,Sigma,w).yasnippet" nil nil)
		       ("multi_gp_cholesky_lpdf" "multi_gp_cholesky_lpdf(${1:y}, ${2:L}, ${3:w})$0" "multi_gp_cholesky_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/multi_gp_cholesky_lpdf(y,L,w).yasnippet" nil nil)
		       ("modified_bessel_second_kind" "modified_bessel_second_kind(${1:v}, ${2:z})$0" "modified_bessel_second_kind" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/modified_bessel_second_kind(v,z).yasnippet" nil nil)
		       ("modified_bessel_first_kind" "modified_bessel_first_kind(${1:v}, ${2:z})$0" "modified_bessel_first_kind" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/modified_bessel_first_kind(v,z).yasnippet" nil nil)
		       ("min" "min(${1:x})$0" "min" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/min(x[]).yasnippet" nil nil)
		       ("min" "min(${1:x}, ${2:y})$0" "min" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/min(x,y).yasnippet" nil nil)
		       ("min" "min(${1:x})$0" "min" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/min(x).yasnippet" nil nil)
		       ("mean" "mean(${1:x})$0" "mean" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/mean(x[]).yasnippet" nil nil)
		       ("mean" "mean(${1:x})$0" "mean" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/mean(x).yasnippet" nil nil)
		       ("mdivide_right_tri_low" "mdivide_right_tri_low(${1:B}, ${2:A})$0" "mdivide_right_tri_low" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/mdivide_right_tri_low(b,A).yasnippet" nil nil)
		       ("mdivide_right_spd" "mdivide_right_spd(${1:b}, ${2:A})$0" "mdivide_right_spd" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/mdivide_right_spd(B,A).yasnippet" nil nil)
		       ("mdivide_left_tri_low" "mdivide_left_tri_low(${1:A}, ${2:b})$0" "mdivide_left_tri_low" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/mdivide_left_tri_low(A,B).yasnippet" nil nil)
		       ("mdivide_left_spd" "mdivide_left_spd(${1:A}, ${2:b})$0" "mdivide_left_spd" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/mdivide_left_spd(A,B).yasnippet" nil nil)
		       ("max" "max(${1:x})$0" "max" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/max(x[]).yasnippet" nil nil)
		       ("max" "max(${1:x}, ${2:y})$0" "max" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/max(x,y).yasnippet" nil nil)
		       ("max" "max(${1:x})$0" "max" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/max(x).yasnippet" nil nil)
		       ("machine_precision" "machine_precision()$0" "machine_precision" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/machine_precision().yasnippet" nil nil)
		       ("lognormal_rng" "lognormal_rng(${1:mu}, ${2:beta})$0" "lognormal_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lognormal_rng(mu,beta).yasnippet" nil nil)
		       ("lognormal_lpdf" "lognormal_lpdf(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lognormal_lpdf(y,mu,sigma).yasnippet" nil nil)
		       ("lognormal_lcdf" "lognormal_lcdf(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lognormal_lcdf(y,mu,sigma).yasnippet" nil nil)
		       ("lognormal_lccdf" "lognormal_lccdf(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lognormal_lccdf(y,mu,sigma).yasnippet" nil nil)
		       ("lognormal_cdf" "lognormal_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "lognormal_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lognormal_cdf(y,mu,sigma).yasnippet" nil nil)
		       ("logit" "logit(${1:x})$0" "logit" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/logit(x).yasnippet" nil nil)
		       ("logistic_rng" "logistic_rng(${1:mu}, ${2:sigma})$0" "logistic_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/logistic_rng(mu,sigma).yasnippet" nil nil)
		       ("logistic_lpdf" "logistic_lpdf(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/logistic_lpdf(y,mu,sigma).yasnippet" nil nil)
		       ("logistic_lcdf" "logistic_lcdf(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/logistic_lcdf(y,mu,sigma).yasnippet" nil nil)
		       ("logistic_lccdf" "logistic_lccdf(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/logistic_lccdf(y,mu,sigma).yasnippet" nil nil)
		       ("logistic_cdf" "logistic_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "logistic_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/logistic_cdf(y,mu,sigma).yasnippet" nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:x})$0" "log_sum_exp" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_sum_exp(x[]).yasnippet" nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:x}, ${2:y})$0" "log_sum_exp" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_sum_exp(x,y).yasnippet" nil nil)
		       ("log_sum_exp" "log_sum_exp(${1:x})$0" "log_sum_exp" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_sum_exp(x).yasnippet" nil nil)
		       ("log_softmax" "log_softmax(${1:x})$0" "log_softmax" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_softmax(x).yasnippet" nil nil)
		       ("log_rising_factorial" "log_rising_factorial(${1:x}, ${2:n})$0" "log_rising_factorial" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_rising_factorial(x,n).yasnippet" nil nil)
		       ("log_mix" "log_mix(${1:theta}, ${2:lp1}, ${3:lp2})$0" "log_mix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_mix(theta,lp1,lp2).yasnippet" nil nil)
		       ("log_inv_logit" "log_inv_logit(${1:x})$0" "log_inv_logit" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_inv_logit(x).yasnippet" nil nil)
		       ("log_falling_factorial" "log_falling_factorial(${1:x}, ${2:n})$0" "log_falling_factorial" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_falling_factorial(x,n).yasnippet" nil nil)
		       ("log_diff_exp" "log_diff_exp(${1:x}, ${2:y})$0" "log_diff_exp" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_diff_exp(x,y).yasnippet" nil nil)
		       ("log_determinant" "log_determinant(${1:A})$0" "log_determinant" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log_determinant(A).yasnippet" nil nil)
		       ("log2" "log2(${1:x})$0" "log2" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log2(x).yasnippet" nil nil)
		       ("log2" "log2()$0" "log2" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log2().yasnippet" nil nil)
		       ("log1p_exp" "log1p_exp(${1:x})$0" "log1p_exp" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log1p_exp(x).yasnippet" nil nil)
		       ("log1p" "log1p(${1:x})$0" "log1p" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log1p(x).yasnippet" nil nil)
		       ("log1m_inv_logit" "log1m_inv_logit(${1:x})$0" "log1m_inv_logit" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log1m_inv_logit(x).yasnippet" nil nil)
		       ("log1m_exp" "log1m_exp(${1:x})$0" "log1m_exp" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log1m_exp(x).yasnippet" nil nil)
		       ("log1m" "log1m(${1:x})$0" "log1m" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log1m(x).yasnippet" nil nil)
		       ("log10" "log10(${1:x})$0" "log10" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log10(x).yasnippet" nil nil)
		       ("log10" "log10()$0" "log10" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log10().yasnippet" nil nil)
		       ("log" "log(${1:x})$0" "log" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/log(x).yasnippet" nil nil)
		       ("lmultiply" "lmultiply(${1:x}, ${2:y})$0" "lmultiply" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lmultiply(x,y).yasnippet" nil nil)
		       ("lmgamma" "lmgamma(${1:n}, ${2:x})$0" "lmgamma" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lmgamma(n,x).yasnippet" nil nil)
		       ("lkj_corr_rng" "lkj_corr_rng(${1:K}, ${2:eta})$0" "lkj_corr_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lkj_corr_rng(K,eta).yasnippet" nil nil)
		       ("lkj_corr_lpdf" "lkj_corr_lpdf(${1:y}, ${2:eta})$0" "lkj_corr_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lkj_corr_lpdf(y,eta).yasnippet" nil nil)
		       ("lkj_corr_cholesky_rng" "lkj_corr_cholesky_rng(${1:K}, ${2:eta})$0" "lkj_corr_cholesky_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lkj_corr_cholesky_rng(K,eta).yasnippet" nil nil)
		       ("lkj_corr_cholesky_lpdf" "lkj_corr_cholesky_lpdf(${1:L}, ${2:eta})$0" "lkj_corr_cholesky_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lkj_corr_cholesky_lpdf(L,eta).yasnippet" nil nil)
		       ("lgamma" "lgamma(${1:x})$0" "lgamma" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lgamma(x).yasnippet" nil nil)
		       ("lchoose" "lchoose(${1:x}, ${2:y})$0" "lchoose" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lchoose(x,y).yasnippet" nil nil)
		       ("lbeta" "lbeta(${1:alpha}, ${2:beta})$0" "lbeta" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/lbeta(alpha,beta).yasnippet" nil nil)
		       ("is_nan" "is_nan(${1:x})$0" "is_nan" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/is_nan(x).yasnippet" nil nil)
		       ("is_inf" "is_inf(${1:x})$0" "is_inf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/is_inf(x).yasnippet" nil nil)
		       ("inverse_spd" "inverse_spd(${1:A})$0" "inverse_spd" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inverse_spd(A).yasnippet" nil nil)
		       ("inverse" "inverse(${1:A})$0" "inverse" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inverse(A).yasnippet" nil nil)
		       ("inv_wishart_rng" "inv_wishart_rng(${1:nu}, ${2:Sigma})$0" "inv_wishart_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_wishart_rng(nu,Sigma).yasnippet" nil nil)
		       ("inv_wishart_lpdf" "inv_wishart_lpdf(${1:W}, ${2:nu}, ${3:Sigma})$0" "inv_wishart_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_wishart_lpdf(W,nu,Sigma).yasnippet" nil nil)
		       ("inv_square" "inv_square(${1:x})$0" "inv_square" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_square(x).yasnippet" nil nil)
		       ("inv_sqrt" "inv_sqrt(${1:x})$0" "inv_sqrt" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_sqrt(x).yasnippet" nil nil)
		       ("inv_phi" "inv_phi(${1:p})$0" "inv_phi" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_phi(p).yasnippet" nil nil)
		       ("inv_logit" "inv_logit(${1:y})$0" "inv_logit" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_logit(y).yasnippet" nil nil)
		       ("inv_gamma_rng" "inv_gamma_rng(${1:alpha}, ${2:beta})$0" "inv_gamma_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_gamma_rng(alpha,beta).yasnippet" nil nil)
		       ("inv_gamma_lpdf" "inv_gamma_lpdf(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_gamma_lpdf(y,alpha,beta).yasnippet" nil nil)
		       ("inv_gamma_lcdf" "inv_gamma_lcdf(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_gamma_lcdf(y,alpha,beta).yasnippet" nil nil)
		       ("inv_gamma_lccdf" "inv_gamma_lccdf(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_gamma_lccdf(y,alpha,beta).yasnippet" nil nil)
		       ("inv_gamma_cdf" "inv_gamma_cdf(${1:y}, ${2:alpha}, ${3:beta})$0" "inv_gamma_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_gamma_cdf(y,alpha,beta).yasnippet" nil nil)
		       ("inv_cloglog" "inv_cloglog(${1:y})$0" "inv_cloglog" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_cloglog(y).yasnippet" nil nil)
		       ("inv_chi_square_rng" "inv_chi_square_rng(${1:nu})$0" "inv_chi_square_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_chi_square_rng(nu).yasnippet" nil nil)
		       ("inv_chi_square_lpdf" "inv_chi_square_lpdf(${1:y}, ${2:nu})$0" "inv_chi_square_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_chi_square_lpdf(y,nu).yasnippet" nil nil)
		       ("inv_chi_square_lcdf" "inv_chi_square_lcdf(${1:y}, ${2:nu})$0" "inv_chi_square_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_chi_square_lcdf(y,nu).yasnippet" nil nil)
		       ("inv_chi_square_lccdf" "inv_chi_square_lccdf(${1:y}, ${2:nu})$0" "inv_chi_square_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_chi_square_lccdf(y,nu).yasnippet" nil nil)
		       ("inv_chi_square_cdf" "inv_chi_square_cdf(${1:y}, ${2:nu})$0" "inv_chi_square_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv_chi_square_cdf(y,nu).yasnippet" nil nil)
		       ("inv" "inv(${1:x})$0" "inv" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inv(x).yasnippet" nil nil)
		       ("integrate_ode_rk45" "integrate_ode_rk45(${1:ode}, ${2:initial_state}, ${3:initial_time}, ${4:times}, ${5:theta}, ${6:x_r}, ${7:x_i}, ${8:rel_tol}, ${9:abs_tol}, ${10:max_num_steps})$0" "integrate_ode_rk45" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/integrate_ode_rk45(ode,initial_state,initial_time,times,theta,x_r,x_i,rel_tol,abs_tol,max_num_steps).yasnippet" nil nil)
		       ("integrate_ode_rk45" "integrate_ode_rk45(${1:ode}, ${2:initial_state}, ${3:initial_time}, ${4:times}, ${5:theta}, ${6:x_r}, ${7:x_i})$0" "integrate_ode_rk45" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/integrate_ode_rk45(ode,initial_state,initial_time,times,theta,x_r,x_i).yasnippet" nil nil)
		       ("integrate_ode_bdf" "integrate_ode_bdf(${1:ode}, ${2:initial_state}, ${3:initial_time}, ${4:times}, ${5:theta}, ${6:x_r}, ${7:x_i}, ${8:rel_tol}, ${9:abs_tol}, ${10:max_num_steps})$0" "integrate_ode_bdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/integrate_ode_bdf(ode,initial_state,initial_time,times,theta,x_r,x_i,rel_tol,abs_tol,max_num_steps).yasnippet" nil nil)
		       ("integrate_ode_bdf" "integrate_ode_bdf(${1:ode}, ${2:initial_state}, ${3:initial_time}, ${4:times}, ${5:theta}, ${6:x_r}, ${7:x_i})$0" "integrate_ode_bdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/integrate_ode_bdf(ode,initial_state,initial_time,times,theta,x_r,x_i).yasnippet" nil nil)
		       ("int_step" "int_step(${1:x})$0" "int_step" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/int_step(x).yasnippet" nil nil)
		       ("inc_beta" "inc_beta(${1:alpha}, ${2:beta}, ${3:x})$0" "inc_beta" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/inc_beta(alpha,beta,x).yasnippet" nil nil)
		       ("hypot" "hypot(${1:x}, ${2:y})$0" "hypot" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/hypot(x,y).yasnippet" nil nil)
		       ("hypergeometric_rng" "hypergeometric_rng(${1:N}, ${2:a}, ${3:b})$0" "hypergeometric_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/hypergeometric_rng(N,a,b).yasnippet" nil nil)
		       ("hypergeometric_lpmf" "hypergeometric_lpmf(${1:n}, ${2:N}, ${3:a}, ${4:b})$0" "hypergeometric_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/hypergeometric_lpmf(n,N,a,b).yasnippet" nil nil)
		       ("head" "head(${1:v}, ${2:n})$0" "head" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/head(v,n).yasnippet" nil nil)
		       ("head" "head(${1:sv}, ${2:n})$0" "head" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/head(sv,n).yasnippet" nil nil)
		       ("head" "head(${1:rv}, ${2:n})$0" "head" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/head(rv,n).yasnippet" nil nil)
		       ("gumbel_rng" "gumbel_rng(${1:mu}, ${2:beta})$0" "gumbel_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gumbel_rng(mu,beta).yasnippet" nil nil)
		       ("gumbel_lpdf" "gumbel_lpdf(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gumbel_lpdf(y,mu,beta).yasnippet" nil nil)
		       ("gumbel_lcdf" "gumbel_lcdf(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gumbel_lcdf(y,mu,beta).yasnippet" nil nil)
		       ("gumbel_lccdf" "gumbel_lccdf(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gumbel_lccdf(y,mu,beta).yasnippet" nil nil)
		       ("gumbel_cdf" "gumbel_cdf(${1:y}, ${2:mu}, ${3:beta})$0" "gumbel_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gumbel_cdf(y,mu,beta).yasnippet" nil nil)
		       ("gaussian_dlm_obs_lpdf" "gaussian_dlm_obs_lpdf(${1:y}, ${2:F}, ${3:G}, ${4:V}, ${5:W}, ${6:m0}, ${7:C0})$0" "gaussian_dlm_obs_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gaussian_dlm_obs_lpdf(y,F,G,V,W,m0,C0).yasnippet" nil nil)
		       ("gamma_rng" "gamma_rng(${1:alpha}, ${2:beta})$0" "gamma_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gamma_rng(alpha,beta).yasnippet" nil nil)
		       ("gamma_q" "gamma_q(${1:a}, ${2:z})$0" "gamma_q" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gamma_q(a,z).yasnippet" nil nil)
		       ("gamma_p" "gamma_p(${1:a}, ${2:z})$0" "gamma_p" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gamma_p(a,z).yasnippet" nil nil)
		       ("gamma_lpdf" "gamma_lpdf(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gamma_lpdf(y,alpha,beta).yasnippet" nil nil)
		       ("gamma_lcdf" "gamma_lcdf(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gamma_lcdf(y,alpha,beta).yasnippet" nil nil)
		       ("gamma_lccdf" "gamma_lccdf(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gamma_lccdf(y,alpha,beta).yasnippet" nil nil)
		       ("gamma_cdf" "gamma_cdf(${1:y}, ${2:alpha}, ${3:beta})$0" "gamma_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/gamma_cdf(y,alpha,beta).yasnippet" nil nil)
		       ("frechet_rng" "frechet_rng(${1:alpha}, ${2:sigma})$0" "frechet_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/frechet_rng(alpha,sigma).yasnippet" nil nil)
		       ("frechet_lpdf" "frechet_lpdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/frechet_lpdf(y,alpha,sigma).yasnippet" nil nil)
		       ("frechet_lcdf" "frechet_lcdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/frechet_lcdf(y,alpha,sigma).yasnippet" nil nil)
		       ("frechet_lccdf" "frechet_lccdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/frechet_lccdf(y,alpha,sigma).yasnippet" nil nil)
		       ("frechet_cdf" "frechet_cdf(${1:y}, ${2:alpha}, ${3:sigma})$0" "frechet_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/frechet_cdf(y,alpha,sigma).yasnippet" nil nil)
		       ("fmod" "fmod(${1:x}, ${2:y})$0" "fmod" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/fmod(x,y).yasnippet" nil nil)
		       ("fmin" "fmin(${1:x}, ${2:y})$0" "fmin" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/fmin(x,y).yasnippet" nil nil)
		       ("fmax" "fmax(${1:x}, ${2:y})$0" "fmax" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/fmax(x,y).yasnippet" nil nil)
		       ("fma" "fma(${1:x}, ${2:y}, ${3:z})$0" "fma" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/fma(x,y,z).yasnippet" nil nil)
		       ("floor" "floor(${1:x})$0" "floor" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/floor(x).yasnippet" nil nil)
		       ("fdim" "fdim(${1:x}, ${2:y})$0" "fdim" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/fdim(x,y).yasnippet" nil nil)
		       ("falling_factorial" "falling_factorial(${1:x}, ${2:n})$0" "falling_factorial" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/falling_factorial(x,n).yasnippet" nil nil)
		       ("fabs" "fabs(${1:x})$0" "fabs" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/fabs(x).yasnippet" nil nil)
		       ("exponential_rng" "exponential_rng(${1:beta})$0" "exponential_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exponential_rng(beta).yasnippet" nil nil)
		       ("exponential_lpdf" "exponential_lpdf(${1:y}, ${2:beta})$0" "exponential_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exponential_lpdf(y,beta).yasnippet" nil nil)
		       ("exponential_lcdf" "exponential_lcdf(${1:y}, ${2:beta})$0" "exponential_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exponential_lcdf(y,beta).yasnippet" nil nil)
		       ("exponential_lccdf" "exponential_lccdf(${1:y}, ${2:beta})$0" "exponential_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exponential_lccdf(y,beta).yasnippet" nil nil)
		       ("exponential_cdf" "exponential_cdf(${1:y}, ${2:beta})$0" "exponential_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exponential_cdf(y,beta).yasnippet" nil nil)
		       ("expm1" "expm1(${1:x})$0" "expm1" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/expm1(x).yasnippet" nil nil)
		       ("exp_mod_normal_rng" "exp_mod_normal_rng(${1:mu}, ${2:sigma}, ${3:lambda})$0" "exp_mod_normal_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exp_mod_normal_rng(mu,sigma,lambda).yasnippet" nil nil)
		       ("exp_mod_normal_lpdf" "exp_mod_normal_lpdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exp_mod_normal_lpdf(y,mu,sigma,lambda).yasnippet" nil nil)
		       ("exp_mod_normal_lcdf" "exp_mod_normal_lcdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exp_mod_normal_lcdf(y,mu,sigma,lambda).yasnippet" nil nil)
		       ("exp_mod_normal_lccdf" "exp_mod_normal_lccdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exp_mod_normal_lccdf(y,mu,sigma,lambda).yasnippet" nil nil)
		       ("exp_mod_normal_cdf" "exp_mod_normal_cdf(${1:y}, ${2:mu}, ${3:sigma}, ${4:lambda})$0" "exp_mod_normal_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exp_mod_normal_cdf(y,mu,sigma,lambda).yasnippet" nil nil)
		       ("exp2" "exp2(${1:x})$0" "exp2" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exp2(x).yasnippet" nil nil)
		       ("exp" "exp(${1:x})$0" "exp" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/exp(x).yasnippet" nil nil)
		       ("erfc" "erfc(${1:x})$0" "erfc" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/erfc(x).yasnippet" nil nil)
		       ("erf" "erf(${1:x})$0" "erf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/erf(x).yasnippet" nil nil)
		       ("eigenvectors_sym" "eigenvectors_sym(${1:A})$0" "eigenvectors_sym" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/eigenvectors_sym(A).yasnippet" nil nil)
		       ("eigenvalues_sym" "eigenvalues_sym(${1:A})$0" "eigenvalues_sym" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/eigenvalues_sym(A).yasnippet" nil nil)
		       ("e" "e()$0" "e" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/e().yasnippet" nil nil)
		       ("double_exponential_rng" "double_exponential_rng(${1:mu}, ${2:sigma})$0" "double_exponential_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/double_exponential_rng(mu,sigma).yasnippet" nil nil)
		       ("double_exponential_lpdf" "double_exponential_lpdf(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/double_exponential_lpdf(y,mu,sigma).yasnippet" nil nil)
		       ("double_exponential_lcdf" "double_exponential_lcdf(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/double_exponential_lcdf(y,mu,sigma).yasnippet" nil nil)
		       ("double_exponential_lccdf" "double_exponential_lccdf(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/double_exponential_lccdf(y,mu,sigma).yasnippet" nil nil)
		       ("double_exponential_cdf" "double_exponential_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "double_exponential_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/double_exponential_cdf(y,mu,sigma).yasnippet" nil nil)
		       ("dot_self" "dot_self(${1:x})$0" "dot_self" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/dot_self(x).yasnippet" nil nil)
		       ("dot_product" "dot_product(${1:x}, ${2:y})$0" "dot_product" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/dot_product(x,y).yasnippet" nil nil)
		       ("distance" "distance(${1:x}, ${2:y})$0" "distance" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/distance(x,y).yasnippet" nil nil)
		       ("dirichlet_rng" "dirichlet_rng(${1:alpha})$0" "dirichlet_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/dirichlet_rng(alpha).yasnippet" nil nil)
		       ("dirichlet_lpdf" "dirichlet_lpdf(${1:theta}, ${2:alpha})$0" "dirichlet_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/dirichlet_lpdf(theta,alpha).yasnippet" nil nil)
		       ("dims" "dims(${1:x})$0" "dims" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/dims(x).yasnippet" nil nil)
		       ("digamma" "digamma(${1:x})$0" "digamma" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/digamma(x).yasnippet" nil nil)
		       ("diagonal" "diagonal(${1:x})$0" "diagonal" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/diagonal(x).yasnippet" nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:v}, ${2:m})$0" "diag_pre_multiply" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/diag_pre_multiply(v,m).yasnippet" nil nil)
		       ("diag_pre_multiply" "diag_pre_multiply(${1:rv}, ${2:m})$0" "diag_pre_multiply" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/diag_pre_multiply(rv,m).yasnippet" nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:m}, ${2:v})$0" "diag_post_multiply" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/diag_post_multiply(m,v).yasnippet" nil nil)
		       ("diag_post_multiply" "diag_post_multiply(${1:m}, ${2:rv})$0" "diag_post_multiply" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/diag_post_multiply(m,rv).yasnippet" nil nil)
		       ("diag_matrix" "diag_matrix(${1:x})$0" "diag_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/diag_matrix(x).yasnippet" nil nil)
		       ("determinant" "determinant(${1:A})$0" "determinant" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/determinant(A).yasnippet" nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:x})$0" "cumulative_sum" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cumulative_sum(x).yasnippet" nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:v})$0" "cumulative_sum" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cumulative_sum(v).yasnippet" nil nil)
		       ("cumulative_sum" "cumulative_sum(${1:rv})$0" "cumulative_sum" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cumulative_sum(rv).yasnippet" nil nil)
		       ("csr_to_dense_matrix" "csr_to_dense_matrix(${1:m}, ${2:n}, ${3:w}, ${4:v}, ${5:u})$0" "csr_to_dense_matrix" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/csr_to_dense_matrix(m,n,w,v,u).yasnippet" nil nil)
		       ("csr_matrix_times_vector" "csr_matrix_times_vector(${1:m}, ${2:n}, ${3:w}, ${4:v}, ${5:u}, ${6:b})$0" "csr_matrix_times_vector" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/csr_matrix_times_vector(m,n,w,v,u,b).yasnippet" nil nil)
		       ("csr_extract_w" "csr_extract_w(${1:a})$0" "csr_extract_w" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/csr_extract_w(a).yasnippet" nil nil)
		       ("csr_extract_v" "csr_extract_v(${1:a})$0" "csr_extract_v" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/csr_extract_v(a).yasnippet" nil nil)
		       ("csr_extract_u" "csr_extract_u(${1:a})$0" "csr_extract_u" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/csr_extract_u(a).yasnippet" nil nil)
		       ("crossprod" "crossprod(${1:x})$0" "crossprod" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/crossprod(x).yasnippet" nil nil)
		       ("cov_exp_quad" "cov_exp_quad(${1:x1}, ${2:x2}, ${3:sigma}, ${4:l})$0" "cov_exp_quad" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cov_exp_quad(x1,x2,sigma,l).yasnippet" nil nil)
		       ("cov_exp_quad" "cov_exp_quad(${1:x}, ${2:sigma}, ${3:l})$0" "cov_exp_quad" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cov_exp_quad(x,sigma,l).yasnippet" nil nil)
		       ("cosh" "cosh(${1:x})$0" "cosh" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cosh(x).yasnippet" nil nil)
		       ("cos" "cos(${1:x})$0" "cos" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cos(x).yasnippet" nil nil)
		       ("columns_dot_self" "columns_dot_self(${1:x})$0" "columns_dot_self" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/columns_dot_self(x).yasnippet" nil nil)
		       ("columns_dot_product" "columns_dot_product(${1:x}, ${2:y})$0" "columns_dot_product" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/columns_dot_product(x,y).yasnippet" nil nil)
		       ("cols" "cols(${1:x})$0" "cols" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cols(x).yasnippet" nil nil)
		       ("col" "col(${1:x}, ${2:n})$0" "col" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/col(x,n).yasnippet" nil nil)
		       ("cholesky_decompose" "cholesky_decompose(${1:A})$0" "cholesky_decompose" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cholesky_decompose(A).yasnippet" nil nil)
		       ("chi_square_rng" "chi_square_rng(${1:nu})$0" "chi_square_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/chi_square_rng(nu).yasnippet" nil nil)
		       ("chi_square_lpdf" "chi_square_lpdf(${1:y}, ${2:nu})$0" "chi_square_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/chi_square_lpdf(y,nu).yasnippet" nil nil)
		       ("chi_square_lcdf" "chi_square_lcdf(${1:y}, ${2:nu})$0" "chi_square_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/chi_square_lcdf(y,nu).yasnippet" nil nil)
		       ("chi_square_lccdf" "chi_square_lccdf(${1:y}, ${2:nu})$0" "chi_square_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/chi_square_lccdf(y,nu).yasnippet" nil nil)
		       ("chi_square_cdf" "chi_square_cdf(${1:y}, ${2:nu})$0" "chi_square_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/chi_square_cdf(y,nu).yasnippet" nil nil)
		       ("ceil" "ceil(${1:x})$0" "ceil" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/ceil(x).yasnippet" nil nil)
		       ("cbrt" "cbrt(${1:x})$0" "cbrt" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cbrt(x).yasnippet" nil nil)
		       ("cauchy_rng" "cauchy_rng(${1:mu}, ${2:sigma})$0" "cauchy_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cauchy_rng(mu,sigma).yasnippet" nil nil)
		       ("cauchy_lpdf" "cauchy_lpdf(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cauchy_lpdf(y,mu,sigma).yasnippet" nil nil)
		       ("cauchy_lcdf" "cauchy_lcdf(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cauchy_lcdf(y,mu,sigma).yasnippet" nil nil)
		       ("cauchy_lccdf" "cauchy_lccdf(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cauchy_lccdf(y,mu,sigma).yasnippet" nil nil)
		       ("cauchy_cdf" "cauchy_cdf(${1:y}, ${2:mu}, ${3:sigma})$0" "cauchy_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/cauchy_cdf(y,mu,sigma).yasnippet" nil nil)
		       ("categorical_rng" "categorical_rng(${1:theta})$0" "categorical_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/categorical_rng(theta).yasnippet" nil nil)
		       ("categorical_lpmf" "categorical_lpmf(${1:y}, ${2:theta})$0" "categorical_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/categorical_lpmf(y,theta).yasnippet" nil nil)
		       ("categorical_logit_lpmf" "categorical_logit_lpmf(${1:y}, ${2:beta})$0" "categorical_logit_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/categorical_logit_lpmf(y,beta).yasnippet" nil nil)
		       ("block" "block(${1:x}, ${2:i}, ${3:j}, ${4:n_rows}, ${5:n_cols})$0" "block" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/block(x,i,j,n_rows,n_cols).yasnippet" nil nil)
		       ("binomial_rng" "binomial_rng(${1:N}, ${2:theta})$0" "binomial_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/binomial_rng(N,theta).yasnippet" nil nil)
		       ("binomial_lpmf" "binomial_lpmf(${1:n}, ${2:N}, ${3:theta})$0" "binomial_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/binomial_lpmf(n,N,theta).yasnippet" nil nil)
		       ("binomial_logit_lpmf" "binomial_logit_lpmf(${1:n}, ${2:N}, ${3:alpha})$0" "binomial_logit_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/binomial_logit_lpmf(n,N,alpha).yasnippet" nil nil)
		       ("binomial_lcdf" "binomial_lcdf(${1:n}, ${2:N}, ${3:theta})$0" "binomial_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/binomial_lcdf(n,N,theta).yasnippet" nil nil)
		       ("binomial_lccdf" "binomial_lccdf(${1:n}, ${2:N}, ${3:theta})$0" "binomial_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/binomial_lccdf(n,N,theta).yasnippet" nil nil)
		       ("binomial_cdf" "binomial_cdf(${1:n}, ${2:N}, ${3:theta})$0" "binomial_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/binomial_cdf(n,N,theta).yasnippet" nil nil)
		       ("binary_log_loss" "binary_log_loss(${1:y}, ${2:y_hat})$0" "binary_log_loss" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/binary_log_loss(y,y_hat).yasnippet" nil nil)
		       ("beta_rng" "beta_rng(${1:alpha}, ${2:beta})$0" "beta_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_rng(alpha,beta).yasnippet" nil nil)
		       ("beta_lpdf" "beta_lpdf(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_lpdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_lpdf(theta,alpha,beta).yasnippet" nil nil)
		       ("beta_lcdf" "beta_lcdf(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_lcdf(theta,alpha,beta).yasnippet" nil nil)
		       ("beta_lccdf" "beta_lccdf(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_lccdf(theta,alpha,beta).yasnippet" nil nil)
		       ("beta_cdf" "beta_cdf(${1:theta}, ${2:alpha}, ${3:beta})$0" "beta_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_cdf(theta,alpha,beta).yasnippet" nil nil)
		       ("beta_binomial_rng" "beta_binomial_rng(${1:N}, ${2:alpha}, ${3:beta})$0" "beta_binomial_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_binomial_rng(N,alpha,beta).yasnippet" nil nil)
		       ("beta_binomial_lpmf" "beta_binomial_lpmf(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_binomial_lpmf(n,N,alpha,beta).yasnippet" nil nil)
		       ("beta_binomial_lcdf" "beta_binomial_lcdf(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_binomial_lcdf(n,N,alpha,beta).yasnippet" nil nil)
		       ("beta_binomial_lccdf" "beta_binomial_lccdf(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_binomial_lccdf(n,N,alpha,beta).yasnippet" nil nil)
		       ("beta_binomial_cdf" "beta_binomial_cdf(${1:n}, ${2:N}, ${3:alpha}, ${4:beta})$0" "beta_binomial_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/beta_binomial_cdf(n,N,alpha,beta).yasnippet" nil nil)
		       ("bessel_second_kind" "bessel_second_kind(${1:v}, ${2:x})$0" "bessel_second_kind" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bessel_second_kind(v,x).yasnippet" nil nil)
		       ("bessel_first_kind" "bessel_first_kind(${1:v}, ${2:x})$0" "bessel_first_kind" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bessel_first_kind(v,x).yasnippet" nil nil)
		       ("bernoulli_rng" "bernoulli_rng(${1:theta})$0" "bernoulli_rng" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bernoulli_rng(theta).yasnippet" nil nil)
		       ("bernoulli_lpmf" "bernoulli_lpmf(${1:y}, ${2:theta})$0" "bernoulli_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bernoulli_lpmf(y,theta).yasnippet" nil nil)
		       ("bernoulli_logit_lpmf" "bernoulli_logit_lpmf(${1:y}, ${2:alpha})$0" "bernoulli_logit_lpmf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bernoulli_logit_lpmf(y,alpha).yasnippet" nil nil)
		       ("bernoulli_lcdf" "bernoulli_lcdf(${1:y}, ${2:theta})$0" "bernoulli_lcdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bernoulli_lcdf(y,theta).yasnippet" nil nil)
		       ("bernoulli_lccdf" "bernoulli_lccdf(${1:y}, ${2:theta})$0" "bernoulli_lccdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bernoulli_lccdf(y,theta).yasnippet" nil nil)
		       ("bernoulli_cdf" "bernoulli_cdf(${1:y}, ${2:theta})$0" "bernoulli_cdf" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/bernoulli_cdf(y,theta).yasnippet" nil nil)
		       ("atanh" "atanh(${1:x})$0" "atanh" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/atanh(x).yasnippet" nil nil)
		       ("atan2" "atan2(${1:x}, ${2:y})$0" "atan2" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/atan2(x,y).yasnippet" nil nil)
		       ("atan" "atan(${1:x})$0" "atan" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/atan(x).yasnippet" nil nil)
		       ("asinh" "asinh(${1:x})$0" "asinh" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/asinh(x).yasnippet" nil nil)
		       ("asin" "asin(${1:x})$0" "asin" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/asin(x).yasnippet" nil nil)
		       ("append_row" "append_row(${1:x}, ${2:y})$0" "append_row" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/append_row(x,y).yasnippet" nil nil)
		       ("append_col" "append_col(${1:x}, ${2:y})$0" "append_col" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/append_col(x,y).yasnippet" nil nil)
		       ("acosh" "acosh(${1:x})$0" "acosh" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/acosh(x).yasnippet" nil nil)
		       ("acos" "acos(${1:x})$0" "acos" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/acos(x).yasnippet" nil nil)
		       ("abs" "abs(${1:x})$0" "abs" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/abs(x).yasnippet" nil nil)
		       ("Phi_approx" "Phi_approx(${1:x})$0" "Phi_approx" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/Phi_approx(x).yasnippet" nil nil)
		       ("Phi" "Phi(${1:x})$0" "Phi" nil
			("Functions")
			nil "/Users/jrnold/Documents/projects/stan-mode/stan-snippets/snippets/stan-mode/functions/Phi(x).yasnippet" nil nil)))


