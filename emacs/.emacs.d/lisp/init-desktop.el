;;; counsel-spotify.el --- Control Spotify search and select music with Ivy.
;;
;;; Commentary:
;;
;; This file configures and installs desktop packages.
;;
;;; Code:

(use-package f)

(require 'f)
(require 'cl-lib)

;; TODO: Fix dbus error when playing a spotify track.
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

(use-package windower)

(use-package framemove)

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
	     (re-search-forward (rx "[" (group (one-or-more digit)) "%" "]"))
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
			(format " %s %s%s "
				(cond ((string= direction "up")
				       (all-the-icons-material "volume_up"))
				      ((string= direction "down")
				       (all-the-icons-material "volume_down"))
				      ((string= direction "off")
				       (all-the-icons-material "volume_off")))
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
  (defun tm/get-xrandr-displays ()
    "Return a list of display names from `xrandr`."
    (let ((displays-command (string-join '("echo -n"
                                           "$(xrandr |"
                                           "egrep -o '^[^\s]+\sconnected' |"
                                           "awk '{print $1}')")
                                         " ")))
      (split-string (shell-command-to-string displays-command) " ")))
  (defun tm/exwm-display-setup ()
    (let ((displays (tm/get-xrandr-displays))
          (external-monitor-xrandr-cmd (string-join '("xrandr"
                                                      "--output eDP-1"
                                                      "--mode 1920x1200"
                                                      "--pos %s"
                                                      "--rotate normal"
                                                      "--output %s"
                                                      "--primary"
                                                      "--mode %s"
                                                      "--pos 0x0"
                                                      "--rotate normal")
                                                    " "))
          (no-external-monitor-xrandr-cmd (string-join '("xrandr"
                                                         "--auto"))))
      (cond
       ((member "HDMI-1" (tm/get-xrandr-displays))
        (progn
          (shell-command (format external-monitor-xrandr-cmd "2560x1027" "HDMI-1" "2560x1440"))
          (setq exwm-randr-workspace-monitor-plist (list 0 "eDP-1"
                                                         1 "HDMI-1"
                                                         2 "HDMI-1"))
          (exwm-workspace-switch-create 1)))
       ((member "DP-3" (tm/get-xrandr-displays))
        (progn
          (shell-command (format external-monitor-xrandr-cmd "2560x1027" "DP-3" "2560x1440"))
          (setq exwm-randr-workspace-monitor-plist (list 0 "eDP-1"
                                                         1 "DP-3"
                                                         2 "DP-3"))
          (exwm-workspace-switch-create 1)))
       ((member "DP-2" (tm/get-xrandr-displays))
        (progn
          (shell-command (format external-monitor-xrandr-cmd "2560x1027" "DP-2" "2560x1440"))
          (setq exwm-randr-workspace-monitor-plist (list 0 "eDP-1"
                                                         1 "DP-2"
                                                         2 "DP-2"))
          (exwm-workspace-switch-create 1)))
       (t (shell-command no-external-monitor-xrandr-cmd)))
      (display-battery-mode 1)))
  (defmacro tm/exwm-move (direction)
    (let ((windmove-fn (intern (concat "windmove-" direction)))
          (framemove-fn (intern (concat "fm-" direction "-frame"))))
      `(lambda ()
         (interactive)
         (or (ignore-errors (,windmove-fn)) (,framemove-fn)))))
  :hook
  (exwm-floating-setup . exwm-layout-hide-mode-line)
  (exwm-floating-exit . exwm-layout-show-mode-line)
  (exwm-update-title . tm/exwm-rename-buffer-to-title)
  (exwm-workspace-switch . exwm-input-release-keyboard)
  (exwm-init . (lambda ()
                 (when (executable-find "dunst")
                   (start-process-shell-command "dunst" nil "dunst"))
                 (when (executable-find "nm-applet")
                   (start-process-shell-command "nm-applet" nil "nm-applet"))

                 (when (shell-command "flatpak list --app | grep -q DavMail")
                   (start-process-shell-command "davmail"
                                                nil
                                                "XDG_CURRENT_DESKTOP=GNOME flatpak run org.davmail.DavMail"))
                 (when (executable-find "goimapnotify")
                   (start-process-shell-command "goimapnotify" nil "goimapnotify -conf ~/.config/goimapnotify.json"))
                 (when (executable-find "shutter")
                   (start-process-shell-command "shutter" "shutter" "shutter --min_at_startup"))))
  (after-init . tm/exwm-display-setup)
  :init
  (setq exwm-input-global-keys
	`((,(kbd "C-s-R") . exwm-restart)
	  (,(kbd "s-R") . exwm-reset)
	  (,(kbd "s-i") . exwm-input-toggle-keyboard)
	  (,(kbd "s-h") . ,(tm/exwm-move "left"))
	  (,(kbd "s-j") . ,(tm/exwm-move "down"))
	  (,(kbd "s-k") . ,(tm/exwm-move "up"))
	  (,(kbd "s-l") . ,(tm/exwm-move "right"))
	  (,(kbd "s-Q") . kill-this-buffer)
	  (,(kbd "s-b") . ivy-switch-buffer)
	  (,(kbd "s-f") . find-file)
	  (,(kbd "s-d") . counsel-linux-app)
	  (,(kbd "s-SPC") . tm/prefix-command)
	  (,(kbd "C-s-L") . tm/lock-screen)
	  (,(kbd "s-a") . tm/toggle-agenda)
	  (,(kbd "s-z") . zoom-mode)
	  (,(kbd "s-r") . helm-org-rifle)
	  (,(kbd "s-c") . org-capture)
	  (,(kbd "s-D") . treemacs)
	  (,(kbd "s-x") . counsel-M-x)
	  (,(kbd "s-B") . ibuffer-sidebar-toggle-sidebar)
	  (,(kbd "s-n") . tm/toggle-org-roam-today)
	  (,(kbd "s-<return>") . multi-vterm)
          (,(kbd "s-<escape>") . vterm-send-escape)
	  (,(kbd "s-!") . delete-other-windows)
	  (,(kbd "s-@") . split-window-below)
	  (,(kbd "s-#") . split-window-right)
	  (,(kbd "s-)") . delete-window)
	  (,(kbd "s-M-h") . windower-move-border-left)
	  (,(kbd "s-M-l") . windower-move-border-right)
	  (,(kbd "s-M-j") . windower-move-border-below)
	  (,(kbd "s-M-k") . windower-move-border-above)
	  (,(kbd "s-/") . notmuch)
	  (,(kbd "s-?") . notmuch-search)
	  (,(kbd "s-m") . notmuch-jump-search)
          (,(kbd "s-w") . ace-window)
          (,(kbd "s-W") . ace-delete-window)
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

  (display-time-mode 1)
  :config
  (setq exwm-workspace-show-all-buffers t
	exwm-input-line-mode-passthrough t
	exwm-manage-configurations '((t char-mode t))
        exwm-systemtray-height 20)
  (add-to-list 'exwm-input-prefix-keys ?\ )
  (defun tm/ivy--switch-buffer-action (buffer)
    "Switch to BUFFER.

BUFFER may be a string or nil. Conditionally calls
`exwm-workspace-switch-to-buffer' if BUFFER is an EXWM buffer."
    (let ((buffer-mode (unless (assoc buffer ivy--virtual-buffers)
			 (save-excursion
			   (with-current-buffer (get-buffer buffer)
			     major-mode)))))
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

(use-package exwm-mff
  :straight
  (:host github :repo "ieure/exwm-mff")
  :hook
  (after-init . exwm-mff-mode))

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 18)

(require 'exwm-randr)
(exwm-randr-enable)

(exwm-init)

(provide 'init-desktop)
;;; init-desktop.el ends here
