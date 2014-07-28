;;; stan-snippets.el --- Yasnippets for Stan

;; Copyright (C) 2012, 2013, 2014  Jeffrey Arnold

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>
;; URL: http://github.com/stan-dev/stan-mode
;; Keywords: languages
;; Version: 2.4.1
;; Created: 2012-08-18
;; Package-Requires: ((yasnippet "0.8.0") (stan-mode "2.4.1"))

;; This file is not part of GNU Emacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>

;;; Commentary:

;; Adds Yasnippet support for stan.
;;
;; Usage:
;; 
;;   (require 'stan-snippets)

;;; Code:
(require 'yasnippet)
(require 'stan-mode)

(defvar stan-snippets-dir
  (expand-file-name "snippets"
                    (file-name-directory
                     (or load-file-name (buffer-file-name))))
  "Directory containing stan-mode snippets.")

(add-hook
 'stan-mode-hook
 (lambda () 
   ;; this is needed to expand functions with _ in them.
   (setq-local yas-key-syntaxes (list "w_" "w_." "w_.()" "^ "))
   ))

;;;###autoload
(defun stan-snippets-initialize ()
  (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs stan-snippets-dir t))
  (yas-load-directory stan-snippets-dir))

;;;###autoload
(eval-after-load "yasnippet"
  '(stan-snippets-initialize))

(provide 'stan-snippets)

;;; stan-snippets.el ends here
