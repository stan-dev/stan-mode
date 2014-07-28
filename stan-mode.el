;;; stan-mode.el --- Major mode for editing Stan files

;; Copyright (C) 2012, 2013, 2014  Jeffrey Arnold, Daniel Lee

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;;   Daniel Lee <bearlee@alum.mit.edu>
;; Maintainer: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;;   Daniel Lee <bearlee@alum.mit.edu>
;; URL: http://github.com/stan-dev/stan-mode
;; Keywords: languanges
;; Version: 2.4.1
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
;; statistics. (See URL `http://mc-stan.org/`).
;;
;; Usage:
;;   (require 'stan-mode)
;;
;; This mode currently has support for syntax-highlighting, indentation,
;; imenu, flymake, and compilation error checking.
;;
;; See `stan-snippets` for yasnippet support.

;;; Code:
(require 'cc-mode)
(require 'cc-langs)

(require 'font-lock)
(require 'compile)
(require 'flymake)

;; Contains keywords and functions
(require 'stan-keywords-lists)

;;
;; Customizable Variables
;;
(defgroup stan-mode nil
  "A mode for Stan"
  :tag "Stan"
  :prefix "stan-"
  :group 'languages)

(defconst stan-mode-version "2.4.1"
  "stan-mode version number")

(defun stan-version ()
  "Message the current stan-mode version"
  (interactive)
  (message "stan-mode version %s; supports Stan v. %s)"
	   stan-mode-version stan-language-version))

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
  (if (member system-type '(windows-nt cygwin ms-dos))
      "stanc.exe"
    "stanc")
  "Path to stanc executable

This can also be just the name of the stanc executable if it is on the PATH.
"
  :type 'string
  :group 'stan-mode)



;;; cc-mode Language support

;; This mode does not inherit properties from other modes. So, we do not use
;; the usual `c-add-language' function.
(eval-and-compile
  (put 'stan-mode 'c-mode-prefix "stan-"))

;; Lexer level syntax
(c-lang-defconst c-symbol-start
  stan (concat "[" c-alpha "]"))

(c-lang-defconst c-symbol-chars
  stan (concat  c-alnum "_"))

;; Since I cannot set two types of line comments in a language,
;; treat # as a cpp-macro, but kill as much of the functionality as possible
;; Set # to a comment in the syntax table.
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

(c-lang-defconst c-assignment-operators
  stan '("<-" "~"))

(c-lang-defconst c-operators
  stan '((postfix "[" "]" "(" ")")
	  (postfix-if-paren "<" ">")
	  (postfix "'")
	  (left-assoc "^")
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

;; tokens in syntax or parenthesis syntax classes that have uses
;; other than as expression operators
;; As with most of cc-mode, I don't fully understand this
;; c++ doesn't include <> so I won't
(c-lang-defconst c-other-op-syntax-tokens
  stan (append '("#") (c-lang-const c-other-op-syntax-tokens)))

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

;; Treat blocks as classes
;; I tried setting them to c-block-decls-with-vars but then the
;; syntatic symbols for the context were things like indata, inparameters, ...
;; which was more of a pain to deal with.
(c-lang-defconst c-class-decl-kwds
  stan stan-blocks-list)

(c-lang-defconst c-block-decls-with-vars
  stan nil)

(c-lang-defconst c-paren-non-type-kwds
  stan nil)

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

;;; cc-mode indentation

(defvar stan-style
  '("gnu"
    ;; # comments have syntatic class cpp-macro
    (c-offsets-alist . ((cpp-macro . 0)))))

(c-add-style "stan" stan-style)

(setq c-default-style
      (append c-default-style '((stan-mode . "stan"))))

;;; Syntax table
(defconst stan-mode-syntax-table-default
  (let ((table (funcall (c-lang-const c-make-mode-syntax-table stan))))
    ;; treat <> as operators only
    ;; TODO: use syntax-propertize-function to determine context of <>
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> ".")
    ;; Mark single
    (modify-syntax-entry ?#  "< b"  table)
    (modify-syntax-entry ?\n "> b"  table)
    (modify-syntax-entry ?'  "." table)
    table)
  "Default Syntax table for stan-mode buffers.")

(defvar stan-mode-syntax-table nil
  "Syntax table used in stan-mode buffers.")

;; use user-specified syntax table else default
(or stan-mode-syntax-table
    (setq stan-mode-syntax-table stan-mode-syntax-table-default))

;;; Movement

;; map functions to c movement functions
;; these are saved as separate variables incase the code changes in the future
(defvar stan-beginning-of-statement 'c-beginning-of-statement)
(defvar stan-end-of-statement 'c-end-of-statement)
(defvar stan-beginning-of-block 'c-beginning-of-defun)
(defvar stan-beginning-of-block 'c-end-of-defun)
(defvar stan-mark-block 'mark-defun)

;;; Abbrev table

(defvar stan-mode-abbrev-table nil
  "Abbreviation table used in stan-mode buffers.")

(c-define-abbrev-table 'stan-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

;;; Keymap

(defvar stan-mode-map
  (let ((map (c-make-inherited-keymap)))
    ;; Add bindings which are only useful for stan
    (define-key map (kbd "M-a") 'stan-beginning-of-statement)
    (define-key map (kbd "M-e") 'stan-end-of-statement)
    (define-key map (kbd "C-M-a") 'stan-beginning-of-block)
    (define-key map (kbd "C-M-e") 'stan-end-of-block)
    (define-key map (kbd "C-M-h") 'stan-mark-block)
    map)
  "Keymap used in stan-mode buffers.")

;;; Menu

;; this copied and pasted from cc-lang
;; needed to delete parts related to cpp-macros.
;; I should just edit the menu with lisp instead of this.
(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu.	The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  stan `(["Comment Out Region"	comment-region
       (c-fn-region-is-active-p)]
      ["Uncomment Region"	(comment-region (region-beginning)
						(region-end) '(4))
       (c-fn-region-is-active-p)]
      ["Indent Expression"	c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region"	c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement"	c-beginning-of-statement t]
      ["Forward Statement"	c-end-of-statement t]
      ["Backward Block"	        stan-beginning-of-block t]
      ["Forward Block"	        stan-end-of-block t]
      ["Mark Block"	        stan-mark-block t]
      "----"
      ("Style..."
       ["Set Style..."                   c-set-style t]
       ["Show Current Style Name"        (message
					  "Style Name: %s"
					  c-indentation-style) t]
       ["Guess Style from this Buffer"   c-guess-buffer-no-install t]
       ["Install the Last Guessed Style" c-guess-install
	(and c-guess-guessed-offsets-alist
	     c-guess-guessed-basic-offset) ]
       ["View the Last Guessed Style"    c-guess-view
	(and c-guess-guessed-offsets-alist
	     c-guess-guessed-basic-offset) ])
      "----"
      ("Toggle..."
       ["Syntactic indentation" c-toggle-syntactic-indentation t]
       ["Electric mode"		c-toggle-electric-state t]
       ["Auto newline"		c-toggle-auto-newline t]
       ["Hungry delete"		c-toggle-hungry-state t]
       ["Subword mode"		c-subword-mode t])))

(easy-menu-define stan-menu stan-mode-map "Stan Mode Commands"
  (cons "Stan" (c-lang-const c-mode-menu stan)))

;;; Font-locking

;; <- and~ s
(defvar stan-assign-regexp
  "\\(<-\\|~\\)"
  "Assigment operators")

;; Stan parser will accept
;; "transformedparameters", "transformed parameters", "transformed     parameters",
;; and "transformed\tparameters"
(defvar stan-blocks-regexp
  (concat "^[[:space:]]*\\("
	  (mapconcat
	   (lambda (x) (replace-regexp-in-string " " "[[:space:]]*" x))
	   stan-blocks-list "\\|")
	  "\\)[[:space:]]*{")
  "Stan blocks declaration regexp")

(defun stan-regexp-opt (string)
  (concat "\\_<\\(" (regexp-opt string) "\\)\\_>"))

(defvar stan-var-decl-regexp
  (concat (stan-regexp-opt stan-types-list)
          "\\(?:<.*?>\\)?\\(?:\\[.*?\\]\\)?[[:space:]]+\\([A-Za-z][A-Za-z0-9_]*\\)[[:space:]]*[[;]")
    "Stan variable declaration regex")

(defvar stan-func-decl-regexp
  (concat (stan-regexp-opt (append stan-function-return-types-list '("void")))
          "\\(?:<.*?>\\)?\\(?:\\[.*?\\]\\)?[[:space:]]+\\([A-Za-z][A-Za-z0-9_]*\\)[[:space:]]*(")
    "Stan function declaration regex")

(defvar stan-font-lock-keywords
  `((,stan-blocks-regexp 1 font-lock-keyword-face)
    (,stan-assign-regexp . font-lock-reference-face)
    ;; Stan types. Look for it to come after the start of a line or semicolon.
    ( ,(stan-regexp-opt (append stan-types-list stan-function-return-types-list))
      . font-lock-type-face)
    ;; keywords
    (,(stan-regexp-opt stan-keywords-list) . font-lock-keyword-face)
    ;; T
    ("\\_<\\(T\\)\\[.*?\\]" 1 font-lock-keyword-face)
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
    ;; Variable declaration
    (,stan-var-decl-regexp 2 font-lock-variable-name-face)
    (,stan-func-decl-regexp 2 font-lock-variable-name-face)
    ))

;;; Compilation mode

(defvar stan-compilation-regexp
  '("LOCATION: file=\\([^;]+\\); line=\\([0-9]+\\), column=\\([0-9]+\\)" 1 2 3 nil)
  "Specifications for matching parse errors in Stan.
See `compilation-error-regexp-alist' for help on their format.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'stan stan-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'stan)

;;; Imenu mode

(defvar stan-imenu-generic-expression
  `(("Variable" ,stan-var-decl-regexp 2)
    ("Function" ,stan-func-decl-regexp 2)
    ("Block" ,stan-blocks-regexp 1))
  "Stan mode imenu expression")

(defcustom stan-use-imenu t
  "Turn on imenu-mode for Stan files"
  :type 'boolean
  :group 'stan-mode)

;;; flymake-mode

(defvar flymake-stan-stanc-path
  stan-stanc-path
  "Stanc executable to use when running flymake")

(defvar flymake-stan-temp-output-file-name nil
  "Name of the temporary output file produced by stanc when running flymake")

(defun flymake-stan-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (setq flymake-stan-temp-output-file-name
          (make-temp-file "flymake-stan-" nil ".cpp"))
    (list flymake-stan-stanc-path
          (list (concat "--o=" flymake-stan-temp-output-file-name) local-file))))

(defun flymake-stan-cleanup ()
  (flymake-safe-delete-file flymake-stan-temp-output-file-name)
  (flymake-simple-cleanup))

(push '(".+\\.stan$"
	flymake-stan-init
	flymake-stan-cleanup
	flymake-get-real-file-name)
      flymake-allowed-file-name-masks)

;; This is needed. Otherwise the non-zero return code by
;; stanc causes an error.
;; Solution from http://pastebin.com/2Pp4bj9p
;; TODO: make it local to this mode
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;;; auto-complete mode

(defcustom stan-use-auto-complete t
  "Activate auto-complete mode with Stan files

This only has an effect if auto-complete is installed.
"
  :type 'boolean
  :group 'stan-mode)

;; defined to avoid compile warnings
(defvar ac-modes)
(defvar ac-dictionary-directories)
(defvar ac-sources)

(defvar stan--load-auto-complete
      (and (require 'auto-complete nil 'noerror)
	   (require 'auto-complete-config nil 'noerror)
	   stan-use-auto-complete))

(when stan--load-auto-complete
  (setq ac-modes (append ac-modes '(stan-mode)))
  (add-to-list 'ac-dictionary-directories
	       (expand-file-name "ac-dict"
				 (file-name-directory
				  (or load-file-name (buffer-file-name))))))

(defun stan-ac-mode-setup ()
  (when stan--load-auto-complete
    (setq ac-sources '(ac-source-yasnippet
		       ac-source-imenu
		       ac-source-dictionary
		       ac-source-words-in-buffer))))


;;; Mode initialization

;;;###autoload
(defun stan-mode ()
  "A major mode for editing Stan files.

The hook `c-mode-common-hook' is run with no args at mode
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
  (use-local-map stan-mode-map)
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
  (easy-menu-add stan-menu)

  ;; syntax highlighting
  (setq font-lock-defaults '((stan-font-lock-keywords)))

  ;; compilation error mode
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist '(stan))

  ;; imenu
  (when stan-use-imenu
      (progn
	(setq imenu-generic-expression stan-imenu-generic-expression)
	(imenu-add-menubar-index)))

  ;; auto-complete
  (stan-ac-mode-setup)

  ;; conclusion
  (run-hooks 'c-mode-common-hook 'stan-mode-hook)
  (c-update-modeline)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stan\\'" . stan-mode))

(provide 'stan-mode)

;;; stan-mode.el ends here
