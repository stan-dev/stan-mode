;;; stan-mode.el --- Major mode for editing STAN files

;; Copyright (C) 2012, 2013  Jeffrey Arnold, Daniel Lee

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;;   Daniel Lee <bearlee@alum.mit.edu>
;; Maintainer: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;;   Daniel Lee <bearlee@alum.mit.edu>
;; URL: http://github.com/stan-dev/stan-mode
;; Keywords: languanges
;; Version: 1.2.0
;; Created: 2012-08-18

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; This is a major mode for the Stan modeling language for Bayesian
;; statistics. See http://mc-stan.org/.
;;
;; To load this library:
;;
;;   (require 'stan-mode)
;;
;; This mode currently supports syntax-highlighting, indentation (via
;; the cc-mode indentation engine), imenu, and compiler-mode regular
;; expressions.
;;
;; Yasnippet and flymake support for stan are provided in separate
;; libraries included with stan-mode.
;;
;; Yasnippet support is provided in stan-snippets.
;;
;;   (require 'stan-snippets)
;;
;; Flymake support is provided in flymake-stan.
;;
;;   (require 'flymake-stan)

;;; Code:
(require 'font-lock)
(require 'cc-mode)
(require 'compile)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

;; Contains keywords 
(require 'stan-keywords-lists)

;;
;; Customizable Variables
;;
(defgroup stan-mode nil
  "A mode for Stan"
  :prefix "stan-"
  :group 'languages)

(defconst stan-mode-version "1.2.0"
  "stan-mode version number")

(defconst stan-language-version "2.0.1"
  "Stan language version supported")

(defcustom stan-mode-hook nil
  "Hook run when entering stan-mode"
  :type 'hook
  :group 'stan-mode)

(defcustom stan-comment-start "//"
  "Stan comment style to use"
  :type 'string
  :group 'stan-mode)

(defcustom stan-comment-end ""
  "Stan comment style to use"
  :type 'string
  :group 'stan-mode)

(defcustom stan-stanc-path
  "stanc"
  ;;(if (member system-type '(windows-nt)) "stanc.exe" "stanc")
  "Path to stanc executable"
  :type 'string
  :group 'stan-mode)

(defvar stan-mode-abbrev-table nil
  "Abbrev table used in stan-mode buffers.")


;;; Define language for cc-mode

;; (eval-and-compile
;;   (c-add-language 'stan-mode 'c++-mode))

;; This mode does not inherit properties from other modes. So, we do not use 
;; the usual `c-add-language' function.
(put 'stan-mode 'c-mode-prefix "stan-")

;; (eval-and-compile
;;   ;; Make our mode known to the language constant system.  Use Java
;;   ;; mode as the fallback for the constants we don't change here.
;;   ;; This needs to be done also at compile time since the language
;;   ;; constants are evaluated then.
;;   (c-add-language 'stan-mode 'c++-mode))

;; Lexer level syntax
(c-lang-defconst c-symbol-start
  stan (concat "[" c-alpha "]"))

(c-lang-defconst c-symbol-chars
  stan (concat  c-alnum "_"))

;; Since I cannot set two comments, still treat # as cpp
(c-lang-defconst c-opt-cpp-prefix
  stan "#")
(c-lang-defconst c-opt-cpp-prefix
  stan "\\s *#\\s *")
(c-lang-defconst c-anchored-cpp-prefix
  stan "^\\s *\\(#\\)\\s *")
(c-lang-defconst c-cpp-message-directives
  stan nil)
(c-lang-defconst c-cpp-include-directives
  stan nil)
(c-lang-defconst c-cpp-macro-define
  stan nil)
(c-lang-defconst c-cpp-expr-directives
  stan nil)
(c-lang-defconst c-cpp-expr-functions
  stan nil)
;; (c-lang-defconst c-opt-cpp-start
;;   stan "ab\bc")
;; (c-lang-defconst c-cpp-matchers
;;   stan nil)

(c-lang-defconst c-assignment-operators
  stan '("<-" "~"))

(c-lang-defconst c-operators
  stan '((postfix "[" "]" "(" ")")
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
  stan 
  '("{" "}" "(" ")" "[" "]" ";" ":" "," "=" "/*" "*/" "//" "#"))

(c-lang-defconst c-stmt-delim-chars
  stan "^;{}")

(c-lang-defconst c-stmt-delim-chars-with-comma
  stan "^;{},")

;;; Syntatic whitespace

;; cannot get cc-mode to recognize both // and # as comments
;; setting the regex constants directly does not work either
;; thus # is set as a cpp-macro and the c-offset-alist style 
;; altered
(c-lang-defconst c-line-comment-starter
  stan "//")

(c-lang-defconst c-block-comment-starter
  stan "/*")

(c-lang-defconst c-block-comment-ender
  stan "*/")

;;; Keyword lists

(c-lang-defconst c-primitive-type-kwds
  stan stan-types-list)
  
;; no prefixes for primitivesx
(c-lang-defconst c-primitive-type-prefix-kwds
  stan nil)

;; no type definitions
(c-lang-defconst c-tyepdef-kwds
  stan nil)

;; no type modifiers
(c-lang-defconst c-type-modifier-kwds
  stan nil)

(c-lang-defconst c-modifier-kwds
  stan nil)

(c-lang-defconst c-protection-kwds
  stan nil)

;; e.g. class, struct, unions, 
(c-lang-defconst c-class-decl-kwds
  stan stan-blocks-list)

;; TODO: ?? 
;; Keywords where the following block (if any) contains another
;; declaration level that should not be considered a class
;; (c-lang-defconst c-other-block-decl-kwds
;;   stan   '("data" "generated quantities" "model" "parameters" 
;; 	    "transformed data" "transformed parameters"))
(c-lang-defconst c-block-decls-with-vars
  stan nil)

;; Keywords that may be followed by a parenthesis expression that doesn't contain type identifiers
(c-lang-defconst c-paren-non-type-kwds
  stan nil)
  ;; stan '("print" "increment_log_prob" "T"))

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  stan '("else"))

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  stan '("for" "if" "while"))

;; ignore break, continue, goto, return
(c-lang-defconst c-simple-stmt-kwds
  stan nil)

;; for in Stan does not have ; separated expressions
(c-lang-defconst c-paren-stmt-kwds
  stan nil)

;; No case construct
(c-lang-defconst c-case-kwds
  stan nil)

;; No colon terminated label statements
(c-lang-defconst c-label-kwds
  stan nil)

;; No keywords followed by label id, e.g. goto
(c-lang-defconst c-before-label-kwds
  stan nil)

(c-lang-defconst c-constant-kwds
  stan '("lp__")) ;; lp__ is very not-constant but is a non-used defined variable that is exposed.

(defvar stan-mode-syntax-table nil
  "Syntax table used in stan-mode buffers.")
(or stan-mode-syntax-table
    (setq stan-mode-syntax-table
	  `(lambda () 
	     (let ((table (funcall (c-lang-const c-make-mode-syntax-table stan))))
	       (modify-syntax-entry ?#  "< b"  table)
	       (modify-syntax-entry ?\n "> b"  table)
	       (modify-syntax-entry ?'  "." table)
	       table))))

(defvar stan-mode-abbrev-table nil
  "Abbreviation table used in stan-mode buffers.")

(c-define-abbrev-table 'stan-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  (list))

(defvar stan-mode-map (let ((map (c-make-inherited-keymap)))
              ;; Add bindings which are only useful for stan
              map)
  "Keymap used in stan-mode buffers.")

;; Font-Locks

;; <- and ~
(defvar stan-assign-regexp
  "\\(<-\\|~\\)"
  "Assigment operators")

(defvar stan-blocks-regexp
  (concat "^[[:space:]]*\\(model\\|data\\|transformed[ \t]+data\\|parameters"
          "\\|transformed[ \t]+parameters\\|generated[ \t]+quantities\\)[[:space:]]*{")
  "Stan blocks declaration regexp")

(defun stan-regexp-opt (string)
  (concat "\\_<\\(" (regexp-opt string) "\\)\\_>"))

(defvar stan-var-decl-regexp
  (concat (stan-regexp-opt stan-types-list)
          "\\(?:<.*?>\\)?\\(?:\\[.*?\\]\\)?[[:space:]]+\\([A-Za-z0-9_]+\\)")
    "Stan variable declaration regex")

(defvar stan-font-lock-keywords
  `((,stan-blocks-regexp 1 font-lock-keyword-face)
    (,stan-assign-regexp . font-lock-reference-face)
    ;; Stan types. Look for it to come after the start of a line or semicolon.
    ( ,(concat "\\(^\\|;\\)\\s-*" (regexp-opt stan-types-list 'words)) 2 font-lock-type-face)
    ;; Variable declaration
    (,stan-var-decl-regexp 2 font-lock-variable-name-face)
    ;; keywords
    (,(stan-regexp-opt stan-keywords-list) . font-lock-keyword-face)
    ;; T
    ("\\(T\\)\\[.*?\\]" 1 font-lock-keyword-face)
    ;; check that lower and upper appear after a < or ,
    (,(concat "\\(?:<\\|,\\)\\s-*" (stan-regexp-opt stan-bounds-list))
     1 font-lock-keyword-face)
    (,(stan-regexp-opt stan-functions-list) . font-lock-function-name-face)
    ;; distribution names can only appear after a ~
    (,(concat "~\\s-*\\(" (regexp-opt stan-distribution-list) "\\)\\_>")
     1 font-lock-function-name-face)
    ;; (,(concat "~\\s-*" (stan-regexp stan-distribution-list))
    ;;  . font-lock-function-name-face)
    (,(stan-regexp-opt stan-reserved-list) . font-lock-warning-face)
    ))

;; Compilation Regexp
(defvar stan-compilation-regexp
  '("LOCATION: file=\\([^;]+\\); line=\\([0-9]+\\), column=\\([0-9]+\\)" 1 2 3 nil)
  "Specifications for matching parse errors in Stan.
See `compilation-error-regexp-alist' for help on their format.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'stan stan-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'stan)


;; Misc

(defun stan-version ()
  "Message the current stan-mode version"
  (interactive)
  (message "stan-mode version %s" stan-mode-version))

;;; Imenu tags
(defvar stan-imenu-generic-expression
  `(("Variable" ,stan-var-decl-regexp 2)
    ("Block" ,stan-blocks-regexp 1))
  "Stan mode imenu expression")

;;;###autoload
(defun stan-mode ()
  "The hook `c-mode-common-hook' is run with no args at mode
initialization, then `stan-mode-hook'.

Key bindings:
\\{stan-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table stan-mode-syntax-table)
  (setq major-mode 'stan-mode
	mode-name "Stan"
	local-abbrev-table stan-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars stan-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  ;; this will use manual highlighting
  (c-basic-common-init 'stan-mode c-default-style)

  ;; syntax highlighting
  (setq font-lock-defaults '((stan-font-lock-keywords)))

  ;; compilation error mode
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist '(stan))

  ;; imenu
  (setq imenu-generic-expression stan-imenu-generic-expression)

  ;; conclusion
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'stan-mode-hook)
  (c-update-modeline)
  )

(provide 'stan-mode)

;;; On Load
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stan\\'" . stan-mode))

;;; stan-mode.el ends here
