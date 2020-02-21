;;; stan-mode.el --- Major mode for editing Stan files -*- lexical-binding: t; -*-

;; Copyright (C) 2012, 2013, 2014, 2015, 2016  Jeffrey Arnold, Daniel Lee
;;               2019 Kazuki Yoshida

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;;         Daniel Lee <bearlee@alum.mit.edu>,
;;         Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>
;; URL: https://github.com/stan-dev/stan-mode/tree/master/stan-mode
;; Keywords: languages,c
;; Version: 10.1.0
;; Created: 2012-08-18
;; Package-Requires: ((emacs "24.4"))

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
;; <https://www.gnu.org/licenses/>

;;; Commentary:

;; This is a major mode for the Stan modeling language for Bayesian
;; statistics.  (See URL `https://mc-stan.org/').

;; This major mode supports syntax-highlighting, indentation,
;; `imenu-mode', and `compilation-mode'.

;; Usage:

;;   (require 'stan-mode)
;;

;;; Code:

;; See `cc-langs.el' for how to develop a derived mode.
;; A small example of a derived mode is available at
;; <http://cc-mode.sourceforge.net/derived-mode-ex.el>.
;; It also contains some useful hints for derived mode developers.
;;
;; Real examples
;; Emacs mode for YANG (RFC 7950)
;;  https://github.com/mbj4668/yang-mode/blob/master/yang-mode.el
;; NVIDIA CUDA Major Mode
;;  https://github.com/chachi/cuda-mode/blob/master/cuda-mode.el

;; This `stan-mode.el' consists of elements extracted from `cc-langs.el'.

;; `cc-mode.el' is only needed for definition of `c-populate-syntax-table'.
;; Otherwise, we get the warning:
;; Warning: the function `c-populate-syntax-table' might not
;; be defined at runtime.
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/index.html#Top
(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an (
;; eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

;; run compiler as inferior of Emacs, parse error messages
(require 'compile)

;; sexp notation for regular expressions
;; https://www.emacswiki.org/emacs/rx
;; https://francismurillo.github.io/2017-03-30-Exploring-Emacs-rx-Macro/
;;
;; For the regexp-to-sexp transformation, use `xr.el'.
;; https://github.com/mattiase/xr
(require 'rx)

;; Contains keywords and functions
(require 'stan-keywords)

;;
;; Customizable Variables
;;
(defgroup stan nil
  "A major mode for Stan."
  :tag "Stan"
  :prefix "stan-"
  :group 'languages)

(defcustom stan-mode-hook nil
  "Hook run when entering `stan-mode'."
  :type 'hook
  :group 'stan)

(defconst stan-comment-start "//"
  "Comment start style to use in `stan-mode'.
Choice has been removed.  This should not be modified.")

(defconst stan-comment-end ""
  "Comment end style to use in `stan-mode'.
Choice has been removed.  This should not be modified.")

(defcustom stan-indentation-offset 2
  "Spaces used for indentation in `stan-mode'.

It is officially recommended to use two spaces.
https://mc-stan.org/docs/2_20/stan-users-guide/white-space.html"
  :type 'integer
  :group 'stan)


(eval-and-compile
  ;; `c-add-language'
  ;; This is intended to be used by modes that inherit CC Mode to add new
  ;; languages.  It should be used at the top level before any calls to
  ;; c-lang-defconst.  MODE is the mode name symbol for the new language,
  ;; and BASE-MODE is the mode name symbol for the language in CC Mode that
  ;; is to be the template for the new mode.
  ;;
  ;; The exact effect of BASE-MODE is to make all language constants that
  ;; haven't got a setting in the new language fall back to their values in
  ;; BASE-MODE.  It does not have any effect outside the language constant
  ;; system.
  ;;
  ;; Make our mode known to the language constant system.  Use c-mode
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'stan-mode 'c-mode)
  (put 'stan-mode 'c-mode-prefix "stan-"))

;; `c-lang-defconst' is a macro defined in `cc-defs.el'.
;; Set the language specific values of the language constant NAME.
;; The second argument can optionally be a docstring.  The rest of the
;; arguments are one or more repetitions of LANG VAL where LANG specifies
;; the language(s) that VAL applies to.  LANG is the name of the
;; language, i.e. the mode name without the "-mode" suffix, or a list
;; of such language names, or t for all languages.  VAL is a form to
;; evaluate to get the value.
;;
;; Neither NAME, LANG nor VAL are evaluated directly - they should not be
;; quoted.  c-lang-defconst-eval-immediately can however be used inside
;; VAL to evaluate parts of it directly.

;; The explanatory strings were taken from `cc-langs.el'.

;; Lexer level syntax
(c-lang-defconst c-symbol-start
  "Regexp that matches the start of a symbol, i.e. any identifier or
keyword.  It's unspecified how far it matches.  Does not contain a \\|
operator at the top level."
  stan (concat "[" c-alpha "]"))

(c-lang-defconst c-symbol-chars
  "Set of characters that can be part of a symbol.
This is of the form that fits inside [ ] in a regexp."
  stan (concat c-alnum "_"))

;; Since we cannot set two types of line comments in a language in `cc-mode',
;; treat # as a cpp-macro, but kill as much of the functionality as possible
;; Set # to a comment in the syntax table via `modify-syntax-entry'.
;;
;; When setting # as both cpp-macro prefix and line comment starter,
;; `cc-mode' syntactic analysis seems to fail upon encountering # as the
;; very first letter in a stan file as in some rstanarm stan files as below.
;;
;; Debugger entered--Lisp error: (args-out-of-range #<buffer bernoulli.stan> 0 1)
;;   parse-partial-sexp(1 0)
;;   c-syntactic-end-of-macro()
;;   c-parse-state()
;;   c-guess-basic-syntax()
;;   c-indent-line()
;;   #f(compiled-function () (interactive nil) #<bytecode 0x400bc57d>)()
;;   c-indent-command(nil)
;;   c-indent-line-or-region(nil nil)
;;   funcall-interactively(c-indent-line-or-region nil nil)
;;   call-interactively(c-indent-line-or-region nil nil)
;;   command-execute(c-indent-line-or-region)
;;
;; Specifically, when trying to indent, `c-guess-basic-syntax' is called.
;; `c-syntactic-end-of-macro' does not correctly detect the end of # cpp-macros.
;; When invoked on the very first line in the buffer, which happens to have
;; #include, it fails to move the point to the end of line.  Instead, it calls
;; `parse-partial-sexp' with arguments FROM 1 and TO 0, resulting in the error.
;;
;; If we force the buffer into `c++-mode', `c-syntactic-end-of-macro' brings
;; the point to the end of #include and returns its numeric point. In `c++-mode'
;; # is a punctuation. This seems to be the intended behavior.
;;
;; Makign # a punctuation in the `stan-mode' syntax table fixes this error,
;; but it will break indentation of legacy files using # comments after
;; statements as # comments will stop behaving as inert comments.
;;
;; Reference
;; `cc-mode' syntactic analysis
;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Analysis.html
;;
(c-lang-defconst c-opt-cpp-prefix
  "Regexp matching the prefix of a cpp directive in the languages that
normally use that macro preprocessor.  Tested at BOL or at BOI.
Assumed to not contain any submatches or \\| operators.

Defined in `c-point' macro.
 BOL: beginning of line.
 BOI: back to indentation

Using the `rx' macro for understandability.
Evaluate the `rx' expression in the `ielm' to see what it becomes."
  stan (rx (seq
            (zero-or-more
             (syntax whitespace))
            (group "#")
            (zero-or-more
             (syntax whitespace)))))

(c-lang-defconst c-anchored-cpp-prefix
  "Regexp matching the prefix of a cpp directive anchored to BOL,
in the languages that have a macro preprocessor.

BOL: beginning of line.

Using the `rx' macro for understandability.
Evaluate the `rx' expression in the `ielm' to see what it becomes."
  stan (rx (seq bol
                (zero-or-more
                 (syntax whitespace))
                (group "#")
                (zero-or-more
                 (syntax whitespace)))))

(c-lang-defconst c-cpp-message-directives
  "List of cpp directives (without the prefix) that are followed by a
string message."
  stan nil)
(c-lang-defconst c-cpp-include-directives
  "List of cpp directives (without the prefix) that are followed by a
file name in angle brackets or quotes."
  stan '("include"))

(c-lang-defconst c-cpp-macro-define
  "Cpp directive (without the prefix) that is followed by a macro
definition, or nil if the language doesn't have any."
  stan nil)
(c-lang-defconst c-cpp-expr-directives
  "List of cpp directives (without the prefix) that are followed by an
expression."
  stan nil)
(c-lang-defconst c-cpp-expr-functions
  "List of functions in cpp expressions."
  stan nil)

(c-lang-defconst c-assignment-operators
  "List of all assignment operators."
  stan '("="))

(c-lang-defconst c-operators
  stan '((postfix "[" "]" "(" ")")
         (postfix-if-paren "<" ">")
         (postfix "'")
         (left-assoc "^")
         (prefix "!" "-" "+")
         (left-assoc "./" ".*")
         (left-assoc "\\")
         (left-assoc "/" "*" "%")
         (left-assoc "+" "-")
         (left-assoc "<" "<=" ">" ">=")
         (left-assoc "!=" "==")
         (left-assoc "&&")
         (left-assoc "||")
         (right-assoc-sequence "?" ":")))

;; tokens in syntax or parenthesis syntax classes that have uses
;; other than as expression operators
;; As with most of cc-mode, I don't fully understand this
;; c++ doesn't include <> so I won't
(c-lang-defconst c-other-op-syntax-tokens
  "List of the tokens made up of characters in the punctuation or
parenthesis syntax classes that have uses other than as expression
operators."
  stan (append '("#")
               (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-stmt-delim-chars
  stan "^;{}")

(c-lang-defconst c-stmt-delim-chars-with-comma
  stan "^;{},")

;;; Syntactic whitespace

;; cannot get cc-mode to recognize both // and # as comments
;; setting the regex constants directly does not work either
;; thus # is set as a cpp-macro and the c-offset-alist style
;; altered
(c-lang-defconst c-line-comment-starter
  "String that starts line comments, or nil if such don't exist.
Line comments are always terminated by newlines.  At least one of
`c-block-comment-starter' and this one is assumed to be set.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  stan "//")

(c-lang-defconst c-block-comment-starter
  "String that starts block comments, or nil if such don't exist.
Block comments are ended by `c-block-comment-ender', which is assumed
to be set if this is.  At least one of `c-line-comment-starter' and
this one is assumed to be set.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  stan "/*")

(c-lang-defconst c-block-comment-ender
  "String that ends block comments, or nil if such don't exist.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  stan "*/")


;;; Keyword lists

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

The list is defined in `stan-keywords.el'"
  stan stan-keywords--types-list)

;; no prefixes for primitivesx
(c-lang-defconst c-primitive-type-prefix-kwds
  stan nil)

;; no type definitions
(c-lang-defconst c-typedef-kwds
  stan nil)

;; no type modifiers
(c-lang-defconst c-type-modifier-kwds
  stan nil)

(c-lang-defconst c-modifier-kwds
  stan nil)

(c-lang-defconst c-protection-kwds
  stan nil)

;; Treat blocks as classes
;; I tried setting them to `c-block-decls-with-vars' but then the
;; syntatic symbols for the context were things like indata, inparameters, ...
;; which was more of a pain to deal with.
(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class.

Note that presence on this list does not automatically treat the
following identifier as a type; the keyword must also be present on
`c-type-prefix-kwds' or `c-type-list-kwds' to accomplish that."
  stan stan-keywords--blocks-list)

(c-lang-defconst c-block-decls-with-vars
  "Keywords introducing declarations that can contain a block which
might be followed by variable declarations, e.g. like \"foo\" in
\"class Foo { ... } foo;\".  So if there is a block in a declaration
like that, it ends with the following `;' and not right away.

The keywords on list are assumed to also be present on one of the
`*-decl-kwds' lists."
  stan nil)

(c-lang-defconst c-paren-nontype-kwds
  stan nil)

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  stan '("else"))

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  stan '("for" "if" "while"))

;; ignore break, continue, goto, return
(c-lang-defconst c-simple-stmt-kwds
  "Statement keywords followed by an expression or nothing."
  stan nil)

;; for in Stan does not have ; separated expressions
(c-lang-defconst c-paren-stmt-kwds
  "Statement keywords followed by a parenthesis expression that
nevertheless contains a list separated with `;' and not `,'."
  stan nil)

;; No case construct
(c-lang-defconst c-case-kwds
  "The keyword(s) which introduce a \"case\" like construct.
This construct is \"<keyword> <expression> :\"."
  stan nil)

;; No colon terminated label statements
(c-lang-defconst c-label-kwds
  "Keywords introducing colon terminated labels in blocks."
  stan nil)

;; No keywords followed by label id, e.g. goto
(c-lang-defconst c-before-label-kwds
  "Keywords that might be followed by a label identifier."
  stan nil)


;;; cc-mode indentation
;; 10 Indentation Engine Basics
;;  https://www.gnu.org/software/emacs/manual/html_node/ccmode/Indentation-Engine-Basics.html
;; 11 Customizing Indentation
;;  https://www.gnu.org/software/emacs/manual/html_node/ccmode/Customizing-Indentation.html


;;; Use this as a mode specific hook to `c-syntactic-end-of-macro'.
(defun stan-syntactic-end-of-macro ()
  "Find the syntactic end of #include or #comment.

In `stan-mode', the syntactic end can only be the line end
of the corresponding line unlike other c++ where a macro
can span multiple lines.

This is intended to be used as a replacement for
`c-syntactic-end-of-macro' while in `stan-mode' through
advising `c-syntactic-end-of-macro' via :override method."
  ;; Original comments regarding c langauges in general:
  ;; Go to the end of a CPP directive, or a "safe" pos just before.
  ;; This is normally the end of the next non-escaped line.  A "safe"
  ;; position is one not within a string or comment.  (The EOL on a line
  ;; comment is NOT "safe").
  ;; This function must only be called from the beginning of a CPP construct.
  ;; Note that this function might do hidden buffer changes.  See the comment
  ;; at the start of cc-engine.el for more info.
  (if (eq major-mode 'stan-mode)
      ;; Only in `stan-mode'.
      (progn
        (if c-macro-cache-syntactic
            ;; If it is set, just go there.
	    (goto-char c-macro-cache-syntactic)
          ;; Otherwise set it.
          (setq c-macro-cache-syntactic (point)))
        (point))
    ;; Otherwise just call the original
    (c-syntactic-end-of-macro)))

(defun stan-advice-add-c-syntactic-end-of-macro ()
  "Add an :override advice to `c-syntactic-end-of-macro'."
  (advice-add 'c-syntactic-end-of-macro
              :override
              #'stan-syntactic-end-of-macro))

(defun stan-advice-remove-c-syntactic-end-of-macro ()
  "Remove an :override advice to `c-syntactic-end-of-macro'."
  (advice-remove 'c-syntactic-end-of-macro
                 #'stan-syntactic-end-of-macro))

(defun stan-indent-line ()
  "Call `c-indent-line' with `c-syntactic-endof-macro' modified.

Use `cl-letf' to dynamically override `c-syntactic-endof-macro',
which is used in the invisible body of `c-indent-line' with
`stan-syntactic-end-of-macro'.  The latter does not suffer from
#include being both comment and preprocessor.
This function is not currently used.  This approach requires
overriding all callers, whereas the advice approach above
needs to override only one callee and is easier."
  (cl-letf (((symbol-function 'c-syntactic-end-of-macro)
             #'stan-syntactic-end-of-macro)))
  (c-indent-line))

(defun stan-style-offset-cpp-macro (_langelem)
  "Function to determine c-offset for #.

_LANGELEM is the syntactic element.  See the help for
`c-syntactic-element' in `cc-vars.el' for the structure.
However, for this function, the argument will just receive
a list (cpp-macro), which is not useful and ignored.

#include should receive offset [0] (absolute zero).
# others should receive offset 0 (relative zero).

The `cc-align.el' file defines various line-up functions for
similar purposes and is useful as a reference."
  ;; References:
  ;; Structure of `c-offsets-alist':
  ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/c_002doffsets_002dalist.html
  ;; Available functions for `c-offsets-alist':
  ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Line_002dUp-Functions.html
  ;; Designing functions for `c-offsets-alist':
  ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Custom-Line_002dUp.html
  (save-excursion
    ;; The point should be in the line to be indented.
    ;; It has to be at BOL for the regexp to function.
    (beginning-of-line)
    (if (looking-at (rx (seq bol
                             (zero-or-more (syntax whitespace))
                             (group "#"
                                    (zero-or-more
                                     (syntax whitespace))
                                    "include")
                             (one-or-more (syntax whitespace)))))
        ;; Absolute zero
        [0]
      ;; Relative zero
      0)))

(defconst stan-style
  '("gnu"
    ;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
    ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Symbols.html
    (c-offsets-alist . ((cpp-macro . stan-style-offset-cpp-macro))))
  "The default Stan indentation style.

The base style is `gnu'.

# has a syntatic class cpp-macro.  It can be a valid #include or
a deprecated line comment # comment.

`stan-style-offset-cpp-macro' separate these.

#include: [0] gives absolute zero (flush to the left).
#comment:  0  gives relative zero depending on the context.")

;; Adds a style to c-style-alist, or updates an existing one.
(c-add-style "stan" stan-style)

(setq c-default-style
      (append c-default-style '((stan-mode . "stan"))))


;;; Syntax table
;; Emacs Syntax Tables
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html
;; Stan Language Syntax
;;  https://mc-stan.org/docs/2_20/reference-manual/language-syntax.html
(defvar stan-mode-syntax-table
  (let ((table (funcall (c-lang-const c-make-mode-syntax-table stan))))
    ;; treat <> as operators only
    ;; TODO: use syntax-propertize-function to determine context of <>
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    ;;
    ;; Necessary to handle # as a comment.
    (modify-syntax-entry ?# "< b"  table)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
    ;; ‘b’ means that c as a comment delimiter belongs to the alternative “b”
    ;; comment style. For a two-character comment starter, this flag is only
    ;; significant on the second char, and for a 2-character comment ender
    ;; it is only significant on the first char.
    (modify-syntax-entry ?\n "> b"  table)
    ;;
    ;; Transpose as in X' is a deliminator "."
    (modify-syntax-entry ?'  "." table)
    table)
  "Default Syntax table for `stan-mode' buffers.")


;;; Movement

;; map functions to c movement functions
;; these are saved as separate variables in case the code changes in the future
(defvar stan-beginning-of-statement 'c-beginning-of-statement)
(defvar stan-end-of-statement 'c-end-of-statement)
(defvar stan-beginning-of-block 'c-beginning-of-defun)
(defvar stan-beginning-of-block 'c-end-of-defun)
(defvar stan-mark-block 'mark-defun)


;;; Abbrev table

(defvar stan-mode-abbrev-table nil
  "Abbreviation table used in `stan-mode' buffers.")

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
  "Keymap used in `stan-mode' buffers.")


;;; Menu

;; this copied and pasted from cc-lang
;; needed to delete parts related to cpp-macros.
;; I should just edit the menu with lisp instead of this.
(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu.  The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  stan `(["Comment Out Region"  comment-region
          (c-fn-region-is-active-p)]
         ["Uncomment Region"    (comment-region (region-beginning)
                                                (region-end) '(4))
          (c-fn-region-is-active-p)]
         ["Indent Expression"   c-indent-exp
          (memq (char-after) '(?\( ?\[ ?\{))]
         ["Indent Line or Region"  c-indent-line-or-region t]
         ["Fill Comment Paragraph" c-fill-paragraph t]
         "----"
         ["Backward Statement"  stan-beginning-of-statement t]
         ["Forward Statement"   stan-end-of-statement t]
         ["Backward Block"      stan-beginning-of-block t]
         ["Forward Block"       stan-end-of-block t]
         ["Mark Block"          stan-mark-block t]
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
          ["Electric mode"         c-toggle-electric-state t]
          ["Auto newline"          c-toggle-auto-newline t]
          ["Hungry delete"         c-toggle-hungry-state t]
          ["Subword mode"          c-subword-mode t])))

(easy-menu-define stan-menu stan-mode-map "Stan Mode Commands"
  (cons "Stan" (c-lang-const c-mode-menu stan)))


;;; Font-locking
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexps.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Regular-Expressions.html
;; https://www.emacswiki.org/emacs/RegularExpression
;;
;; Used `xr' to convert the original regexps to the corresponding `rx' forms.
;;  https://github.com/mattiase/xr
;;
;; Note M-x re-builder supports `rx'.  Use M-x reb-change-syntax.
;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder

(defvar stan-regexp-assign
  (rx (group
       (or "~" "+=" "=")))
  "Regular expression for assigment and sampling operators in Stan.")

;; Stan parser will accept
;; "transformedparameters", "transformed parameters", "transformed     parameters",
;; and "transformed\tparameters"
(defvar stan-regexp-blocks
  (concat "^[[:space:]]*\\("
          (mapconcat
           (lambda (x) (replace-regexp-in-string " "
                                                 "[[:space:]]*"
                                                 x))
           stan-keywords--blocks-list "\\|")
          "\\)[[:space:]]*{")
  "Regular expression for the start of blocks in Stan.")

(defun stan-regexp-opt (strings)
  "Return a regexp to match a string in the list STRINGS.

This is a simple wrapper for ` was needed since `regexp-opt' string
in Aquamacs does not accept the `word' option.

`regexp-opt' is a function to return an efficient regexp to match
a string in the STRINGS list.  The regexp is designed to be more
efficient than just a listing of these strings."
  (concat "\\_<\\(" (regexp-opt strings) "\\)\\_>"))

(defvar stan-regexp-var-decl
  (concat (stan-regexp-opt stan-keywords--types-list)
          (rx (seq
               (optional "<"
                         (*\? not-newline)
                         ">")
               (optional "["
                         (*\? not-newline)
                         "]")
               (one-or-more space)))
          (rx (seq
               (group
                (any "A-Za-z")
                (zero-or-more
                 (any "0-9A-Za-z" "_")))
               (zero-or-more space)
               (any ";["))))
  "Regular expression for variable declarations in Stan.")

(defvar stan-regexp-func-decl
  (concat (stan-regexp-opt (append stan-keywords--function-return-types-list '("void")))
          (rx (seq
               (optional "<"
                         (*\? not-newline)
                         ">")
               (optional "["
                         (*\? not-newline)
                         "]")
               (one-or-more space)
               (group
                (any "A-Za-z")
                (zero-or-more
                 (any "0-9A-Za-z" "_")))
               (zero-or-more space)
               "(")))
  "Regular expression for user-defined functions in Stan.")

;; https://mc-stan.org/docs/2_20/reference-manual/includes-section.html
(defvar stan-regexp-include-directive
  (rx (seq bol
           (zero-or-more (syntax whitespace))
           (group "#"
                  ;; No space is allowed here.
                  "include")
           ;; At least one whitespace is required here.
           (syntax whitespace)
           (group (zero-or-more not-newline))))
  "Regular expression to capture #include directives.
This only matches the first #include preceded by zero
or more whitespaces.
Group 1 is #include.
Group 2 is the file to be included.")

(defvar stan-regexp-string-after-include-directive
  (rx (seq bol
           (zero-or-more (syntax whitespace))
           "#"
           ;; No space is allowed here.
           "include"
           ;; At least one whitespace is required here.
           (one-or-more (syntax whitespace))
           (optional "\""
                     (group (one-or-more not-newline))
                     "\"")))
  "Regular expression to capture #include \"string\".
Group 1 is the string to be highlighted as such.")

(defvar stan-regexp-at-param
  (rx (seq bol
           (zero-or-more (syntax whitespace))
           "*" (syntax whitespace)
           (group "@param")
           (one-or-more (syntax whitespace))
           (group
            (any "A-Za-z")
            (zero-or-more
             (any "0-9A-Za-z" "_")))))
  "Regular expression for @param in the function document string.
Group 1: @param.
Group 2: Argument name.")

(defvar stan-regexp-return
  (rx (seq bol
           (zero-or-more (syntax whitespace))
           "*" (syntax whitespace)
           (group "@return")))
  "Regular expression for @return in the function document string.
Group 1: @return.")

;;;  1. Syntactic font-locking
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntactic-Font-Lock.html#Syntactic-Font-Lock

(defun stan-font-lock-syntactic-face-function (state)
  "Function to determine faces for comments in `stan-mode'.

The argument STATE is the return value of `parse-partial-sexp'
at the beginning of the region to highlight  The function will
return a context-appropriate face or delegate work to the default
function.

This special handling is necessary in the `stan-mode' because of the
rather complicated comment syntaxes.

// is the currently recommended line comment starter.

# is the deprecated line comment starter.  This should receive the
`font-lock-warning-face'.

However, #include is a valid directive to include an external file.
This should receive the `font-lock-preprocessor-face' instead.

To do this, the 8-th position of the STATE is checked.  This is the
character address of the start of the comment or string.  It is nil if
not in one of these.  Then a further pattern matching with performed
with `looking-at' to check the actual pattern.  If it starts with
#include, then it is given nil to be processed further in the search-
based font-locking."
  ;; References:
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntactic-Font-Lock.html
  ;; https://stackoverflow.com/questions/35974198/different-font-lock-scheme-for-special-comments-in-emacs-derived-mode
  ;;
  ;; 8. character address of start of comment or string; nil if not in one.
  (cond
   ;; Comments that start with #include are given nil.
   ((and (nth 8 state)
         (save-excursion
           (goto-char (nth 8 state))
           ;; Need to check at the beginning of line
           ;; to avoid #include after code.
           (progn
             ;; Use progn as `beginning-of-line' return nil.
             (beginning-of-line)
             (looking-at stan-regexp-include-directive))))
    nil)
   ;; All other # comments should receive warning face for deprecation.
   ((and (nth 8 state)
         (save-excursion
           (goto-char (nth 8 state))
           (looking-at (rx (seq
                            (group "#"
                                   (zero-or-more not-newline))
                            eol)))))
    'font-lock-warning-face)
   ;; All other things are delegated to the original function.
   (t (funcall
       (default-value 'font-lock-syntactic-face-function)
       state))))


;;;  2. Search-based font-locking
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
(defvar stan-font-lock-keywords
  `(;; @param PARAM in docstring within /** */
    (,stan-regexp-at-param
     ;; OVERRIDE t to override syntactic /** */ comment font-lock.
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t))
    ;; @return in docstring within /** */
    (,stan-regexp-return
     ;; OVERRIDE t to override syntactic /** */ comment font-lock.
     (1 font-lock-keyword-face t))
    ;; #include is spared from syntactic highlighting.
    ;; Thus, there is no need for overriding.
    (,stan-regexp-include-directive
     (1 font-lock-preprocessor-face nil))
    ;; Recover "string" after #include.
    (,stan-regexp-string-after-include-directive
     ;; Second t is for LAXMATCH, do not fail upon not finding a match.
     (1 font-lock-string-face nil t))
    ;; The above two for # need to come first to avoid substrings matched
    ;; by other rules from messing up the font lock.
    ;;
    (,stan-regexp-blocks
     (1 font-lock-keyword-face))
    (,stan-regexp-assign
     (1 font-lock-constant-face))
    ;; Stan types. Look for it to come after the start of a line or semicolon.
    (,(stan-regexp-opt (append stan-keywords--types-list
                               stan-keywords--function-return-types-list))
     (1 font-lock-type-face))
    ;; keywords
    (,(stan-regexp-opt stan-keywords--keywords-list)
     (1 font-lock-keyword-face))
    ;; T for truncation.
    (,(rx (seq symbol-start
               (group "T")
               "["
               (*\? not-newline)
               "]"))
     (1 font-lock-keyword-face))
    ;; target +=. Different faces for groups 1 and 2.
    (,(rx (seq
           (group "target")
           (zero-or-more
            (syntax whitespace))
           (group "+=")))
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face))
    ;; check that lower and upper appear after a < or ,
    (,(concat (rx (seq
                   (or "<" ",")
                   (zero-or-more
                    (syntax whitespace))))
              (stan-regexp-opt stan-keywords--range-constraints-list))
     (1 font-lock-keyword-face))
    ;; function list
    (,(stan-regexp-opt stan-keywords--functions-list)
     (1 font-lock-function-name-face))
    ;; distribution names can only appear after a ~
    (,(concat "~\\s-*\\("
              (regexp-opt stan-keywords--distribution-list)
              "\\)\\_>")
     (1 font-lock-function-name-face))
    ;; Variable declaration. 2 means second group in the regexp.
    (,stan-regexp-var-decl
     (2 font-lock-variable-name-face))
    ;; User-defined function declaration. 2 means second group in the regexp.
    (,stan-regexp-func-decl
     (2 font-lock-variable-name-face))
    ;; User-defined function string. 2 means second group in the regexp.
    (,stan-regexp-at-param
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ;; Reserved
    (,(stan-regexp-opt stan-keywords--reserved-list)
     (1 font-lock-warning-face))
    ;; Deprecated
    ;; https://mc-stan.org/docs/2_20/reference-manual/deprecated-features-appendix.html
    (,(stan-regexp-opt stan-keywords--deprecated-function-list)
     (1 font-lock-warning-face))
    (,(rx (group
           (zero-or-more wordchar)
           "__"))
     (1 font-lock-warning-face))
    (,(rx (group "<-"))
     (1 font-lock-warning-face)))
  ;;
  ;; References:
  ;; http://ergoemacs.org/emacs/elisp_font_lock_mode.html
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntactic-Font-Lock.html
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
  ;; https://www.emacswiki.org/emacs/FontLockKeywords
  ;;
  "Define faces used in font locking in `stan-mode'.

This is used to set `font-lock-defaults' for `stan-mode'.

Note that the ones above have precedence over ones below.
The following faces are explicitly assigned here:
 - `font-lock-keyword-face'
 - `font-lock-constant-face'
 - `font-lock-function-name-face'
 - `font-lock-variable-name-face'
 - `font-lock-preprocessor-face'
 - `font-lock-type-face'
 - `font-lock-warning-face'

The `xr' function was used to generate these `rx' forms.
Evaluate the `rx' macro for to see the resulting regexp.

The data structure here is explained in the help for
`font-lock-keywords' defined in `font-lock.el'.
For each regular expression (MATCHER), the structure is
as follows:

  (MATCHER
   (SUBEXP1 FACENAME1 [OVERRIDE [LAXMATCH]])
   (SUBEXP2 FACENAME2 [OVERRIDE [LAXMATCH]])
   ...)

SUBEXP (1,2,etc) specifies the subexpression of the MATCHER
subject to the rule.  In the `rx' form, this corresponds
to the `group'.  See the `rx' expression for
`stan-regexp-at-param' as an example.

The font-lock system by default does not overwrite already
font-locked text.  When the optional OVERRIDE is t, it gets
overwritten.  Other options are keep, prepend, and append
as explained in the help for `font-lock-keywords'.

Note that this part represents the search-based font-lock,
which follows the higher priority syntactic font-lock.
The second deals with strings and comments.  Thus, overriding
these must occur explicitly.")

;;; Compilation mode

(defvar stan-regexp-compilation
  '((stan-input-file . ("Input file=\\(.*\\)$" 1))
    (stan-error . ("ERROR at line \\([0-9]+\\)" nil 1)))
  "Specifications for matching parse errors in Stan.

See `compilation-error-regexp-alist' for a description of the format.")

(setq compilation-error-regexp-alist-alist
      (append stan-regexp-compilation compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (append (mapcar #'car stan-regexp-compilation)
              compilation-error-regexp-alist))

;;; Imenu mode

(defvar stan-imenu-generic-expression
  `(("Variable" ,stan-regexp-var-decl 2)
    ("Function" ,stan-regexp-func-decl 2)
    ("Block" ,stan-regexp-blocks 1))
  "List of definition matchers for creating an Imenu index in `stan-mode'.

See `imenu-generic-expression' for a description of the format.")

;;; Mode initialization

;;;###autoload
(define-derived-mode stan-mode prog-mode "Stan"
  "A major mode for editing Stan files.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `stan-mode-hook'.

Key bindings:
\\{stan-mode-map}"
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Derived-Modes.html
  :group 'stan
  :syntax-table stan-mode-syntax-table
  :abbrev-table stan-mode-abbrev-table
  (c-initialize-cc-mode t)
  (use-local-map stan-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  ;; TODO: This gives the error message:
  ;;   Warning: (lambda nil ...) quoted with ' rather than with #'
  ;; I think it is in cc-mode.
  (c-init-language-vars stan-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  ;; this will use manual highlighting
  (c-basic-common-init 'stan-mode c-default-style)
  ;; (c-set-style  "stan")
  (easy-menu-add stan-menu)
  (c-update-modeline)

  ;; Font-locking
  ;; 1. Customized syntactic font-locking
  (setq-local font-lock-syntactic-face-function
              #'stan-font-lock-syntactic-face-function)
  ;; 2. Customized search-based font-locking
  (setq font-lock-defaults '((stan-font-lock-keywords)))
  (when (boundp 'font-lock-extend-after-change-region-function)
    (set (make-local-variable 'font-lock-extend-after-change-region-function)
         nil))

  ;; Indentation
  ;; Advice the callee `c-syntactic-end-of-macro' rather than
  ;; overriding all callers via `cl-letf'.  The former is easier.
  (stan-advice-add-c-syntactic-end-of-macro)

  ;; imenu
  (setq imenu-generic-expression stan-imenu-generic-expression)
  (imenu-add-menubar-index)

  ;; conclusion
  (run-hooks 'c-mode-common-hook))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx (seq ".stan" eos)) . stan-mode))

;;;###autoload
(defun stan-mode-setup ()
  "Set up comment and indent style for `stan-mode'."
  ;; Need to set these buffer locally to values unique to `stan-mode'.
  ;; Otherwise, system-wide values seem to be used.
  ;;
  ;; comment-start is a buffer-local variable defined in newcomment.el.gz.
  (setq-local comment-start stan-comment-start)
  ;; comment-end is a variable defined in newcomment.el.gz.
  ;; Should be an empty string if comments are terminated by end-of-line.
  (setq-local comment-end stan-comment-end)
  ;; https://mc-stan.org/docs/2_20/stan-users-guide/white-space.html
  (setq-local c-basic-offset stan-indentation-offset))

(provide 'stan-mode)

;;; stan-mode.el ends here
