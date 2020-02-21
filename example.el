;; Use the QuickTime Player.app on macOS to record a screencast.
;; Use ffmpeg to convert to a gif.
;; https://gist.github.com/dergachev/4627207
;; ffmpeg -i ./example.mov -r 4 -y ./example_new.gif

;; Execute this file as follows when it is open.
;; (load (buffer-file-name))
;; M-x eval-buffer

(require 'demo-it)

(setq demo-it--insert-text-speed :medium)
(setq demo-it--presentation-hide-mode-line nil)

(demo-it-create
 (progn
   (demo-it-presentation "./example_schools_mod_ncp.stan")
   ;; Show the beginning-of-buffer
   (beginning-of-buffer)
   (recenter))
 (progn
   ;; Move to the line to be edited and delete.
   (search-forward "mu ~")
   (beginning-of-line)
   (let ((kill-whole-line nil))
     (kill-line))
   (sit-for 0.5)
   ;; Same as "TAB" in `stan-mode'
   (c-indent-line-or-region)
   (sit-for 0.2)
   ;; https://github.com/howardabrams/demo-it/blob/master/demo-it.org#inserting-text
   (demo-it-insert "muu ~ nor"))
 ;; Found among functions that use `company-idle-delay'.
 ;; Need to stop here to retain the candidates.
 (company-auto-begin)
 ;; Select the first candidate.
 "RET"
 ;; Expand the arguments with stan-snippets
 "TAB"
 ;;
 ;; Work manually here
 ;; (demo-it-insert "0")
 ;; "TAB"
 ;; (demo-it-insert "10")
 ;; "TAB"
 ;; (demo-it-insert ";")
 ;;
 ;; Fix typo
 (search-backward "uu")
 (delete-char 1)
 (progn
   ;; Show the beginning-of-buffer
   (beginning-of-buffer)
   (recenter))
 ;; Recovery
 ;; (progn
 ;;   (goto-line 18)
 ;;   (beginning-of-line)
 ;;   (let ((kill-whole-line nil))
 ;;     (kill-line))
 ;;   (c-indent-line-or-region)
 ;;   (insert "mu ~ normal(0, 10);"))
 )

(demo-it-start)
