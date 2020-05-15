;;; counsel-spotify.el --- Control Spotify search and select music with Ivy.
;;
;;; Commentary:
;;
;; This file configures and installs desktop packages.
;;
;;; Code:

(require 'f)
(require 'cl-lib)

(defmacro tm/setq (var-name value)
  "Assign VALUE to VAR-NAME via `setq'."
  (let ((symbol (intern var-name)))
    `(setq ,var-name ,value)))

(use-package counsel-spotify
  :init
  (let ((file (no-littering-expand-etc-file-name "spotify/api-config"))
	(delim "="))
    (if (f-file-p file)
	;; The following let creates a list where each element is a
	;; single line from api-config.
	(let ((config (split-string (with-temp-buffer
			              (insert-file-contents file)
			              (buffer-substring-no-properties (point-min)
								      (point-max)))
				    "\n"
				    t)))
	  ;; It then iterates over each line, splits each line on "=",
	  ;; and yields a list of the form ("key" "value").
	  (dolist (line config)
	    (let* ((k-v (split-string line delim))
		   (counsel-spotify-variable
		    (concat "counsel-spotify-"
			    (car k-v)))
		   (value (car (cdr k-v))))
	      (set (intern counsel-spotify-variable) value)))))))

(use-package desktop-environment
  :config
  (setq desktop-environment-screenlock-command "xlock"))

(use-package windower)

(defvar tm/displays-alist
  '(("eDP-1"
     (("--primary" . t)
      ("--mode" . "1920x1080")
      ("--pos" . "5120x840")
      ("--rotate" . "normal")))
    ("DP-1-2-8"
     (("--primary" . nil)
      ("--mode" . "2560x1440")
      ("--pos" . "2560x0")
      ("--rotate" . "normal")))
    ("DP-1-2-1-8"
     (("--primary" . nil)
      ("--mode" . "2560x1440")
      ("--pos" . "0x0")
      ("--rotate" . "normal")))
    ("DP-1-3"
     (("--primary" . nil)
      ("--mode" . "1920x1080")
      ("--pos" . "0x0")
      ("--rotate" . "normal"))))
  "A list representing display configuration as predetermined by ARandR.
This list is a list of cons cells, the car of which represents the name
of a display and the cdr of which is an alist representing xrandr command
line flags and their corresponding values.")

(defun tm/make-xrandr-command (displays-alist)
  "Create xrandr command args string from DISPLAYS-ALIST."
  (let ((xrandr-cmd-args ""))
    (dolist (display displays-alist)
      (let ((name (car display))
	    (properties (car (cdr display)))
	    assigned-output-name-p
	    return-val)
	(cl-loop for (key . value) in properties
		 collect
		 (catch 'next
		   (progn
		     (unless assigned-output-name-p
		       (progn
			 (setq xrandr-cmd-args
			       (string-join `(,xrandr-cmd-args
					      "--output" ,name)
					    " "))
			 (setq assigned-output-name-p t)
			 (throw 'next nil)))
		     (if (and (symbolp value)
			      (string= (symbol-name value) "t"))
			 (setq xrandr-cmd-args (string-join
						`(,xrandr-cmd-args ,key)
						" "))
		       (setq xrandr-cmd-args
			     (string-join `(,xrandr-cmd-args ,key ,value)
					  " ")))))))
      (setq return-val xrandr-cmd-args))
    return-val))

(defun tm/find-connected-displays ()
  "Return a list of currently connected displays."
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
	(connected-displays '()))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (while (not (eobp))
	(re-search-forward xrandr-output-regexp nil 'noerror)
	(unless (member (match-string 1) connected-displays)
	  (setq connected-displays (cons (match-string 1) connected-displays)))
	(forward-line)))
    connected-displays))

(defun tm/expected-displays-connected-p ()
  "Check the output from xrandr for expected displays in CONNECTED-DISPLAYS."
  (let ((displays-regex
	 (rx (eval (cons 'or (cl-loop for (name props) in tm/displays-alist
				      collect name)))))
	(match-results '()))
    (dolist (display tm/displays-alist)
      (let ((name (car display)))
	(setq match-results
	      (cons (string-match-p displays-regex name)
		    match-results))))
    (eval (cons 'and match-results))))

(defun tm/exwm-change-screen-hook ()
  "Find connected displays and match them against any preconfigured displays."
  (let* ((connected-displays (tm/find-connected-displays))
	 (matched-displays (tm/expected-displays-connected-p))
	 xrandr-args)
    (if matched-displays
	(progn
	  (setq xrandr-args (tm/make-xrandr-command tm/displays-alist))
	  (call-process "/usr/bin/xrandr" nil nil nil xrandr-args)
	  (setq exwm-randr-workspace-monitor-plist (list 3 "eDP-1"
							 2 "DP-1-2-8"
							 1 "DP-1-2-1-8"))))))

(defun tm/volume-amixer-change (amount)
  (with-temp-buffer
    (when (zerop
	   (call-process "amixer" nil (current-buffer) nil
			 "-q"
			 "sset" emms-volume-amixer-control
			 (format "%d%%%s" (abs amount)
				 (if (< amount 0) "-" "+"))))
      (if (re-search-backward "\\[\\([0-9]+%\\)\\]" nil t)
	  (match-string 1)))
    (setq tm/current-volume-state (tm/volume-make-modeline-string))
    (or (memq 'tm/current-volume-state global-mode-string)
	(setq global-mode-string
	      (append global-mode-string '(tm/current-volume-state))))))

(defun tm/toggle-mute ()
  "Toggle mute amixer master volume."
  (interactive)
  (with-temp-buffer
    (when (zerop
	   (call-process "amixer" nil (current-buffer) nil
			 "-q"
			 "sset" (concat emms-volume-amixer-control ","
					(format "%d" emms-volume-amixer-card))
			 "toggle")))
    (setq tm/current-volume-state (tm/volume-make-modeline-string))
    (or (memq 'tm/current-volume-state global-mode-string)
	(setq global-mode-string
	      (append global-mode-string '(tm/current-volume-state))))))

(use-package emms
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-volume-change-function 'tm/volume-amixer-change))

(defun tm/get-volume-status ()
  "Use amixer to fetch system volume status.

Return an alist containing mute status and volume level."
  (with-temp-buffer
    (call-process "amixer" nil (current-buffer) nil
                  "get" (concat emms-volume-amixer-control ","
				(format "%s" emms-volume-amixer-card)))
    (goto-char (point-min))
    (let ((volume-percent
	   (progn
	     (re-search-forward (rx "[" (group digit digit) "%" "]"))
	     (match-string 1)))
	  (volume-status
	   (progn
	     (re-search-forward (rx "[" (group (one-or-more alpha)) "]"))
	     (match-string 1)))
	  volume-state-alist)
      (if (string= volume-status "on")
	  `((muted-p nil)
	    (level ,volume-percent))
	`((muted-p t)
	  (level ,volume-percent))))))

(defun tm/volume-make-modeline-string ()
  "Call `tm/get-volume-status' to derive `global-mode-string'."
  (let ((level (cadr (assoc 'level (tm/get-volume-status))))
	(muted-p (cadr (assoc 'muted-p (tm/get-volume-status))))
	(make-string (lambda (direction value)
		       (propertize
			(format "%s %s%s "
				(cond ((string= direction "up")
				       "🔊")
				      ((string= direction "down")
				       "🔉")
				      ((string= direction "off")
				       "🔇"))
				value "%")
			'font-lock-face font-lock-builtin-face))))
    (if muted-p
	(funcall make-string "off" level)
      (if (< (string-to-number level) 50)
	  (funcall make-string "down" level)
	(funcall make-string "up" level)))))

(use-package exwm
  :preface
  (defun tm/lock-screen ()
    "Lock desktop."
    (interactive)
    (start-process-shell-command "xlock"
				 nil
				 desktop-environment-screenlock-command))
  (defun tm/exwm-rename-buffer-to-title ()
    "Names EXWM buffers after the application running in them."
    (exwm-workspace-rename-buffer exwm-title))
  :hook
  (exwm-floating-setup . exwm-layout-hide-mode-line)
  (exwm-floating-exit . exwm-layout-show-mode-line)
  (exwm-update-title . tm/exwm-rename-buffer-to-title)
  ((exwm-init exwm-randr-screen-change) . tm/exwm-change-screen-hook)
  (exwm-workspace-switch . exwm-input-release-keyboard)
  :init
  (setq exwm-input-global-keys
	`((,(kbd "s-R") . exwm-reset)
	  (,(kbd "C-s-R") . exwm-restart)
	  (,(kbd "s-x") . exwm-input-toggle-keyboard)
	  (,(kbd "s-h") . windmove-left)
	  (,(kbd "s-j") . windmove-down)
	  (,(kbd "s-k") . windmove-up)
	  (,(kbd "s-l") . windmove-right)
	  (,(kbd "s-Q") . kill-this-buffer)
	  (,(kbd "s-b") . ivy-switch-buffer)
	  (,(kbd "s-f") . find-file)
	  (,(kbd "s-d") . counsel-linux-app)
	  (,(kbd "s-SPC") . tm/prefix-command)
	  (,(kbd "C-s-L") . tm/lock-screen)
	  (,(kbd "s-a") . tm/org-agenda-both-today)
	  (,(kbd "s-z") . zoom-mode)
	  (,(kbd "s-r") . helm-org-rifle)
	  (,(kbd "s-c") . org-capture)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "M-s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-move-window ,i))))
		    (number-sequence 0 9))
	  (,(kbd "s-<tab>") . windower-switch-to-last-buffer)
	  (,(kbd "s-o") . windower-toggle-single)
	  (,(kbd "s-\\") . windower-toggle-split)
	  (,(kbd "s-H") . windower-swap-left)
	  (,(kbd "s-J") . windower-swap-below)
	  (,(kbd "s-K") . windower-swap-above)
	  (,(kbd "s-L") . windower-swap-right)
	  (,(kbd "s-F") . exwm-layout-toggle-fullscreen)
	  (,(kbd "s-t") . window-toggle-side-windows)
	  ([XF86AudioRaiseVolume] . emms-volume-raise)
	  ([XF86AudioLowerVolume] . emms-volume-lower)
	  ([XF86AudioMute] . tm/toggle-mute)
	  ([XF86MonBrightnessUp] . (lambda ()
				     (interactive)
				     (shell-command "light -A 5; light")))
	  ([XF86MonBrightnessDown] . (lambda ()
				       (interactive)
				       (shell-command "light -U 5; light")))))

  (display-battery-mode 1)
  (display-time-mode 1)
  (setq display-time-format " 📆 %b %e 🕐 %H:%M 🖳")
  :config
  (setq exwm-workspace-show-all-buffers t
	exwm-input-line-mode-passthrough t
	exwm-manage-configurations '((t char-mode t)))
  (add-to-list 'exwm-input-prefix-keys ?\ )
  (defun tm/ivy--switch-buffer-action (buffer)
    "Switch to BUFFER.

BUFFER may be a string or nil. Conditionally calls
`exwm-workspace-switch-to-buffer' if BUFFER is an EXWM buffer."
    (let ((buffer-mode (save-excursion
			 (with-current-buffer (get-buffer buffer)
			   major-mode))))
      (if (eq buffer-mode 'exwm-mode)
	  (exwm-workspace-switch-to-buffer buffer)
	(if (zerop (length buffer))
	    (switch-to-buffer
	     ivy-text nil 'force-same-window)
	  (let ((virtual (assoc buffer ivy--virtual-buffers))
		(view (assoc buffer ivy-views)))
	    (cond ((and virtual
			(not (get-buffer buffer)))
		   (find-file (cdr virtual)))
		  (view
		   (delete-other-windows)
		   (let (
			 ;; silence "Directory has changed on disk"
			 (inhibit-message t))
		     (ivy-set-view-recur (cadr view))))
		  (t
		   (switch-to-buffer
		    buffer nil 'force-same-window))))))))
  (advice-add 'ivy--switch-buffer-action
	      :override #'tm/ivy--switch-buffer-action))

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 18)

(require 'exwm-randr)
(exwm-randr-enable)

(provide 'init-desktop)
;;; init-desktop.el ends here
