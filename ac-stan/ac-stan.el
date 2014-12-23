;;; ac-stan.el --- Major mode for editing Stan files

;; Copyright (C) 2014  Jeffrey Arnold

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;; Maintainer: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;; URL: http://github.com/stan-dev/stan-mode
;; Keywords: languages,completion
;; Version: 1.0.0
;; Created: 2014-12-18
;; Package-Requires: ((auto-complete "1.4.0") (stan-mode "3.0.0") (stan-snippets "3.0.0"))

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

;; Auto-complete mode support for `stan-mode'.  This mode uses `imenu-mode',
;; snippets from `stan-yasnippet', and a dictionary to generate completion
;; candidates.
;; 

;;; Code:
(require 'auto-complete)
(require 'stan-mode)
(require 'stan-snippets)

(add-to-list 'ac-dictionary-directories
	     (expand-file-name "ac-dict"
			       (file-name-directory
				(or load-file-name (buffer-file-name)))))

(defun stan-ac-mode-setup ()
  "Setup `auto-complete' mode for `stan-mode'."
  (auto-complete-mode t)
  (yas-minor-mode-on)
  (setq ac-sources '(ac-source-imenu
		     ac-source-yasnippet
		     ac-source-dictionary)))

(add-to-list 'ac-modes 'stan-mode)

(provide 'ac-stan)

;;; ac-stan.el ends here
