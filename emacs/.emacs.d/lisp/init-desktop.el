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

(provide 'init-desktop)
;;; init-desktop.el ends here
