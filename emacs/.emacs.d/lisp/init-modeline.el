;;; init-modeline.el --- Load Emacs's configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs's modeline.
;;
;;; Code:

(use-package doom-modeline
  :straight
  (:host github :repo "seagle0128/doom-modeline")
  :init
  (require 'all-the-icons)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-all
	doom-modeline-icon t
	doom-modeline-height 45
	all-the-icons-scale-factor 1
	doom-modeline-buffer-encoding nil)
  (doom-modeline-def-segment org-pomodoro
    "Displays `org-pomodoro` timer."
    (if (doom-modeline--active)
	(let ((task (if (and (boundp 'org-clock-current-task)
			     org-clock-current-task)
			(truncate-string-to-width org-clock-current-task
						  30)))
	      (face (if (boundp 'org-pomodoro-state)
			(cl-case org-pomodoro-state
			  (:pomodoro 'org-pomodoro-mode-line)
			  (:overtime 'org-pomodoro-mode-line-overtime)
			  (:short-break 'org-pomodoro-mode-line-break)
			  (:long-break 'org-pomodoro-mode-line-break))))
	      (s (if (boundp 'org-pomodoro-state)
		     (cl-case org-pomodoro-state
		       (:pomodoro
			(propertize org-pomodoro-format 'face 'org-pomodoro-mode-line))
		       (:overtime
			(propertize org-pomodoro-overtime-format
				    'face 'org-pomodoro-mode-line-overtime))
		       (:short-break
			(propertize org-pomodoro-short-break-format
				    'face 'org-pomodoro-mode-line-break))
		       (:long-break
			(propertize org-pomodoro-long-break-format
				    'face 'org-pomodoro-mode-line-break))))))
	  (if (and (fboundp 'org-pomodoro-active-p)
		   (org-pomodoro-active-p)
		   (> (length s) 0))
	      (format s (string-join `(,(propertize
					 (org-pomodoro-format-seconds)
					 'face face)
				       ,task) " "))
	    (format "")))))
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host
	  parrot selection-info)
    '(objed-state misc-info org-pomodoro persp-name battery grip irc mu4e gnus
		  github debug lsp minor-modes input-method indent-info
		  buffer-encoding major-mode process vcs checker))
  (doom-modeline-def-modeline 'tm/doom-modeline-exwm
    '(bar modals misc-info) '(org-pomodoro battery major-mode process))
  (defun tm/set-exwm-modeline ()
    "Set a minimal modeline if exwm-mode is enabled."
    (if (eq major-mode 'exwm-mode)
	(doom-modeline-set-modeline 'tm/doom-modeline-exwm)
      (doom-modeline-set-modeline 'main)))
  :hook
  (after-init . doom-modeline-init)
  (buffer-list-update . tm/set-exwm-modeline))

(provide 'init-modeline)
;;; init-modeline.el ends here
