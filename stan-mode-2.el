(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))


;; This mode does not inherit properties from other modes. So, we do not use 
;; the usual `c-add-language' function.
(put 'protobuf-mode 'c-mode-prefix "stan2-")

;; (eval-and-compile
;;   ;; Make our mode known to the language constant system.  Use Java
;;   ;; mode as the fallback for the constants we don't change here.
;;   ;; This needs to be done also at compile time since the language
;;   ;; constants are evaluated then.
;;   (c-add-language 'stan2-mode 'c++-mode))

;; Lexer level syntax
(c-lang-defconst c-symbol-start
  stan2 (concat "[" c-alpha "]"))

(c-lang-defconst c-symbol-chars
  stan2 (concat  c-alnum "_"))

;; Since I cannot set two comments, still treat # as cpp
(c-lang-defconst c-opt-cpp-prefix
  stan2 "#")
;; (c-lang-defconst c-cpp-matchers
;;   stan2 nil)

(c-lang-defconst c-assignment-operators
  stan2 '("<-" "~" "="))

(c-lang-defconst c-operators
  stan2 '((postfix "[" "]" "(" ")")
	  (postfix-if-paren "<" ">")
	  (postfix "'")
	  (prefix "!" "-" "+")
	  (left-assoc "./" ".*")
	  (left-assoc "\\")
	  (left-assoc "/" "*")
	  (left-assoc "+" "-")
	  (left-assoc "<" "<=" ">" ">=")
	  (left-assoc "!=" "==")
	  (left-assoc "&&")
	  (left-assoc "||")
	  ))

(c-lang-defconst c-other-op-syntax-tokens
  stan2 
  '("{" "}" "(" ")" "[" "]" ";" ":" "," "=" "/*" "*/" "//" "#"))

(c-lang-defconst c-stmt-delim-chars
  stan2 "^;{}")

(c-lang-defconst c-stmt-delim-chars-with-comma
  stan2 "^;{},")

;;; Syntatic whitespace

(c-lang-defconst c-line-comment-starter
  stan2 "//")
;; (c-lang-defconst c-line-comment-starter
;;   stan2 "#")

(c-lang-defconst c-block-comment-starter
  stan2 "/*")

(c-lang-defconst c-block-comment-ender
  stan2 "*/")

;; cannot get both # and // as new lines
;; ugly workaround for the 2 types of line comments Stan supports
;; (c-lang-defconst c-comment-start-regexp
;;   ;; Regexp to match the start of any type of comment.
;;   stan2 (let ((re (c-make-keywords-re nil
;; 		   (list (c-lang-const c-line-comment-starter)
;; 		      (c-lang-const c-block-comment-starter)
;; 		      "#"))))
;; 	  (if (memq 'gen-comment-delim c-emacs-features)
;; 	      (concat re "\\|\\s!")
;; 	    re)))

;; (c-lang-defconst c-line-comment-start-regexp
;;   ;; Regexp which matches the start of a line comment (if such exists in the
;;   ;; language; it does in all 7 CC Mode languages).
;;   stan2 (c-make-keywords-re nil
;; 	  (list (c-lang-const c-line-comment-starter)
;; 		"#")))

;;; Keyword lists

(c-lang-defconst c-primitive-type-kwds
  stan2
  '("cholesky_factor_cov" "corr_matrix" "cov_matrix" "int" "matrix" 
    "ordered" "positive_ordered" "real" "row_vector" "simplex" 
    "unit_vector" "vector"))
  
;; no prefixes for primitivesx
(c-lang-defconst c-primitive-type-prefix-kwds
  stan2 nil)

;; no type definitions
(c-lang-defconst c-tyepdef-kwds
  stan2 nil)

;; no type modifiers
(c-lang-defconst c-type-modifier-kwds
  stan2 nil)

(c-lang-defconst c-modifier-kwds
  stan2 nil)

(c-lang-defconst c-protection-kwds
  stan2 nil)

;; e.g. class, struct, unions, 
(c-lang-defconst c-class-decl-kwds
  stan2 nil)
  ;; stan2   '("data" "generated quantities" "model" "parameters" 
  ;; 	    "transformed data" "transformed parameters"))


;; TODO: ?? 
;; Keywords where the following block (if any) contains another
;; declaration level that should not be considered a class
(c-lang-defconst c-other-block-decl-kwds
  stan2   '("data" "generated quantities" "model" "parameters" 
	    "transformed data" "transformed parameters"))

;; Keywords that may be followed by a parenthesis expression that doesn't contain type identifiers
(c-lang-defconst c-paren-non-type-kwds
  stan2 nil)
  ;; stan2 '("print" "increment_log_prob" "T"))

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  stan2 '("else"))

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  stan2 '("for" "if" "while"))

;; ignore break, continue, goto, return
(c-lang-defconst c-simple-stmt-kwds
  stan2 nil)

;; for in Stan does not have ; separated expressions
(c-lang-defconst c-paren-stmt-kwds
  stan2 nil)

;; No case construct
(c-lang-defconst c-case-kwds
  stan2 nil)

;; No colon terminated label statements
(c-lang-defconst c-label-kwds
  stan2 nil)

;; No keywords followed by label id, e.g. goto
(c-lang-defconst c-before-label-kwds
  stan2 nil)

(c-lang-defconst c-constant-kwds
  stan2 '("lp__")) ;; lp__ is very not-constant but is a non-used defined variable that is exposed.

(defconst stan2-font-lock-keywords-1 (c-lang-const c-matchers-1 stan2)
  "Minimal highlighting for stan2 mode.")

(defconst stan2-font-lock-keywords-2 (c-lang-const c-matchers-2 stan2)
  "Fast normal highlighting for stan2 mode.")

(defconst stan2-font-lock-keywords-3 (c-lang-const c-matchers-3 stan2)
  "Accurate normal highlighting for stan2 mode.")

(defvar stan2-font-lock-keywords stan2-font-lock-keywords-3
  "Default expressions to highlight in stan2 mode.")

(defvar stan2-mode-syntax-table nil
  "Syntax table used in stan2-mode buffers.")
(or stan2-mode-syntax-table
    (setq stan2-mode-syntax-table
	  `(lambda () 
	     (let ((table (funcall (c-lang-const c-make-mode-syntax-table stan2))))
	       (modify-syntax-entry ?#  "< b"  table)
	       (modify-syntax-entry ?\n "> b"  table)
	       (modify-syntax-entry ?'  "." table)
	       table))))

;; (modify-syntax-entry ?#  "< b"  stan2-mode-syntax-table)
;; (modify-syntax-entry ?\n "> b"  stan2-mode-syntax-table)
;; (modify-syntax-entry ?'  "." stan2-mode-syntax-table)

(defvar stan2-mode-abbrev-table nil
  "Abbreviation table used in stan2-mode buffers.")

(c-define-abbrev-table 'stan2-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  (list))

(defvar stan2-mode-map (let ((map (c-make-inherited-keymap)))
              ;; Add bindings which are only useful for stan2
              map)
  "Keymap used in stan2-mode buffers.")

;;;###autoload
(defun stan2-mode ()
  "
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `stan2-mode-hook'.

Key bindings:
\\{stan2-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table stan2-mode-syntax-table)
  (setq major-mode 'stan2-mode
	mode-name "Stan"
	local-abbrev-table stan2-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars stan2-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'stan2-mode)
  ;; (easy-menu-add stan2-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'stan2-mode-hook)
  (c-update-modeline))


(provide 'stan2-mode)
