;;; ac-stan.el --- Major mode for editing Stan files

;; Copyright (C) 2014  Jeffrey Arnold

;; Author: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;; Maintainer: Jeffrey Arnold <jeffrey.arnold@gmail.com>,
;; URL: http://github.com/stan-dev/stan-mode
;; Keywords: languanges
;; Version: 0.0.1
;; Created: 2014-12-18
;; Package-Requires: ((stan-snippets "3.0.0"))

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

;;; Code:
(require 'auto-complete)
(require 'stan-snippets)

(add-to-list 'ac-dictionary-directories
	     (expand-file-name "ac-dict"
			       (file-name-directory
				(or load-file-name (buffer-file-name)))))

(defun stan-ac-mode-setup ()
  (setq ac-sources '(ac-source-imenu
		     ac-source-yasnippet
		     ac-source-dictionary
		     ac-source-words-in-buffer)))

(provide 'ac-stan)

;;; ac-stan.el ends here
