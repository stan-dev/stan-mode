(require 'flymake)
(require 'stan-mode)

(setq stan-stanc-path "/home/jeff/remotes/stan/stan-src-1.0.3/bin/stanc")

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
    (list stan-stanc-path
          (list (concat "--o=" flymake-stan-temp-output-file-name) local-file))))

(defun flymake-stan-cleanup ()
  (flymake-safe-delete-file flymake-stan-temp-output-file-name)
  (flymake-simple-cleanup))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.stan$"
              flymake-stan-init
              flymake-stan-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (append stan-compilation-error-regexp-alist
              flymake-err-line-patterns))

;; This is needed. Otherwise the non-zero return code by
;; stanc causes an error.
;; Solution from http://pastebin.com/2Pp4bj9p
;; TODO: make it local to this mode
(defadvice flymake-post-syntax-check
  (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

(add-hook 'stan-mode-hook
	  (lambda () (flymake-mode 1)))

(provide 'flymake-stan)
