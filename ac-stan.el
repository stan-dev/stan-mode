(require 'auto-complete)

(add-to-list 'ac-dictionary-directories   
	     (expand-file-name "ac"
			       (file-name-directory
				(or load-file-name (buffer-file-name)))))

(add-hook 'stan-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-dictionary 
			       ac-source-imenu 
			       ac-source-yasnippet
			       ac-source-words-in-buffer
			       ))))

(setq ac-modes (append ac-modes '(stan-mode)))

