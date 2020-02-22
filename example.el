;; Use the QuickTime Player.app on macOS to record a screencast.
;; Use ffmpeg to convert to a gif.
;; https://gist.github.com/dergachev/4627207
;; ffmpeg -i ./example.mov -r 4 -y ./example_new.gif


;; Execute this file as follows when it is open.
;; (load (buffer-file-name))
;; M-x eval-buffer


;; Load the required packages.
(require 'demo-it)


;; Configure demo-it.
(setq demo-it--insert-text-speed :medium)
(setq demo-it--presentation-hide-mode-line nil)


;; Advice for `eldoc-display-message-p'.
(defun eldoc-display-message-p-always-t ()
  "Always return t."
  t)


;; Create and store an ordered list of steps and configuration
(demo-it-create
 ;; Use a single expression to fully automate.
 ;; These are not necessarily exactly same as the
 ;; functions called during an interactive session.
 (progn
   ;; Set up the screen.
   (find-file "./example_schools_mod_ncp.stan")
   ;; Show the beginning-of-buffer
   (beginning-of-buffer)
   (recenter)
   ;;
   ;; Move to the line of interest.
   (search-forward "mu ~")
   (beginning-of-line)
   (let ((kill-whole-line nil))
     (kill-line))
   (sit-for 2)
   ;;
   ;; Indent using `stan-mode'. Same as "TAB" in `stan-mode'
   (c-indent-line-or-region)
   ;; https://github.com/howardabrams/demo-it/blob/master/demo-it.org#inserting-text
   (demo-it-insert "muu ~ nor")
   ;;
   ;; Demonstrate `company-stan'.
   ;; Use the following to check variable assignment.
   ;; (message (flycheck-sexp-to-string this-command))
   ;; The expression is based on @dgutov.
   ;; https://github.com/company-mode/company-mode/issues/956
   (progn
     (company-manual-begin)
     (let ((this-command 'self-insert-command))
       (company-post-command))
     (sit-for 2)
     ;; Insert the selected candidate.
     (company-complete-selection))
   ;;
   ;; Demonstrate `stan-snippets' and `eldoc-stan'.
   (let ((eldoc-idle-delay 0)
         ;; `eldoc-display-message-p' checks these variables.
         (this-command nil)
         (last-command 'self-insert-command))
     ;; `eldoc-message-commands' includes key yasnippet commands.
     ;; https://github.com/joaotavora/yasnippet/issues/796
     ;;
     ;; Make `eldoc-display-message-p' always return t.
     (advice-add 'eldoc-display-message-p
                 :override 'eldoc-display-message-p-always-t)
     ;;
     ;; Insert text while showing `eldoc-stan'.
     ;; `eldoc-schedule-timer' is run in the `post-command-hook'
     ;; to ensure `eldoc-timer' is running. This timer runs
     ;; `eldoc-print-current-symbol-info' when idle for
     ;; `eldoc-idle-delay'. This is not reliable when run
     ;; non-interactively because the idle time is not reset.
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html
     (yas-expand)
     (eldoc-print-current-symbol-info)
     (sit-for 1)
     (demo-it-insert "0")
     (eldoc-print-current-symbol-info)
     (sit-for 1)
     (yas-next-field-or-maybe-expand)
     (eldoc-print-current-symbol-info)
     (sit-for 1)
     (demo-it-insert "10")
     (eldoc-print-current-symbol-info)
     (sit-for 1)
     (yas-next-field-or-maybe-expand)
     (eldoc-print-current-symbol-info)
     (sit-for 1)
     (demo-it-insert ";")
     (eldoc-print-current-symbol-info)
     (sit-for 1)
     ;; Remove advice
     (advice-remove 'eldoc-display-message-p
                    'eldoc-display-message-p-always-t))
   ;;
   ;; Demonstrate `flycheck-stan'.
   (let ((flycheck-display-errors-delay 0.1))
     (search-backward "uu")
     (flycheck-display-error-at-point)
     (sit-for 3)
     (delete-char 1))
   ;;
   ;; Show the beginning-of-buffer
   (beginning-of-buffer)
   (recenter)))

(demo-it-start)
