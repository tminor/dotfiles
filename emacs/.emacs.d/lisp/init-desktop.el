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
(provide 'init-desktop)
;;; init-desktop.el ends here
