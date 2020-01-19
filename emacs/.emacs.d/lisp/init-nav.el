;;; init-nav.el --- Configure Emacs navigation -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures navigation for Emacs buffers, windwos, etc.
;;
;;; Code:

(tm/leader-def
  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-right
  "0" 'delete-window)

(use-package ibuffer
  :demand t
  :general
  (tm/leader-def
    "B" 'ibuffer))

(use-package avy
  :general
  (:states 'motion
   :keymaps 'override
   "zg" 'avy-goto-char
   "z2" 'avy-goto-char-2
   "z1" 'avy-goto-char-timer))

(use-package winner
  :general
  (tm/leader-def
    :infix "w"
    :prefix-command 'tm/window-prefix-command
    :prefix-map 'tm/window-prefix-map
    "" '(:which-key "window prefix" :ignore t)
    "u" 'winner-undo
    "C-r" 'winner-redo)
  :config
  (winner-mode 1))

(use-package ace-window
  :init
  (custom-set-faces '(aw-leading-char-face
                      ((t (:foreground "red" :height 3.0)))))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  (tm/leader-def
    :infix "w"
    :prefix-command 'tm/window-prefix-command
    "" '(:which-key "window prefix" :ignore t )
    "m" 'ace-window
    "d" 'ace-delete-window
    "s" 'ace-swap-window))

(use-package zoom
  :straight
  (:host github :repo "cyrus-and/zoom")
  :general
  (tm/leader-def
    :infix "t"
    :prefix-command 'tm/toggle-prefix-command
    :prefix-map 'tm/toggle-prefix-map
    "" '(:which-key "toggle prefix" :ignore t)
    "z" 'zoom-mode)
  :config
  (setq zoom-size '(0.618 . 0.618)))

(defun tm/new-empty-text-buffer ()
  "Create a new empty text buffer.

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'"
  (interactive)
  (let ((buf (generate-new-buffer "*scratch/text*")))
    (switch-to-buffer buf)
    (setq initial-major-mode 'text-mode)
    buf))

(defun tm/new-empty-lisp-buffer ()
  "Create a new empty lisp buffer.

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'"
  (interactive)
  (let ((buf (generate-new-buffer "*scratch/lisp*")))
    (switch-to-buffer buf)
    (setq initial-major-mode 'lisp-mode)
    buf))

(tm/leader-def
  :infix "S"
  :prefix-command 'tm/scratch-prefix-command
  :prefix-map 'tm/scratch-prefix-map
  "" '(:which-key "scratch prefix" :ignore t)
  "t" 'tm/new-empty-text-buffer
  "l" 'tm/new-empty-lisp-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `display-buffer' configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  ;; https://github.com/ndwarshuis/.emacs.d
  (defmacro tm/with-advice (adlist &rest body)
    "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
    (declare (debug ((&rest (&rest form)) body))
	     (indent 1))
    `(progn
       ,@(mapcar (lambda (adform)
		   (cons 'advice-add adform))
		 adlist)
       (unwind-protect (progn ,@body)
	 ,@(mapcar (lambda (adform)
		     `(advice-remove ,(car adform) ,(nth 2 adform)))
		   adlist))))

  (with-eval-after-load 'org
    (defun tm/org-todo-position (buffer alist)
      (let ((win (car (cl-delete-if-not
		       (lambda (window)
			 (with-current-buffer (window-buffer window)
			   (memq major-mode
				 '(org-mode org-agenda-mode))))
		       (window-list)))))
	(when win
	  (let ((buffer (window-buffer win)))
	    (display-buffer-in-side-window ((side . right)
					    (slot . 1)))))))

    (defun tm/org-todo-window-advice (orig-fn &optional current-state)
      "Advice to fix window placement in `org-fast-todo-selection'."
      (let ((override '("\\*Org todo\\*" tm/org-todo-position)))
	(add-to-list 'display-buffer-alist override)
	(tm/with-advice
	    ((#'org-switch-to-buffer-other-window :override #'pop-to-buffer))
	  (unwind-protect (funcall orig-fn)
	    (setq display-buffer-alist
		  (delete override display-buffer-alist))))))

    (advice-add #'org-fast-todo-selection :around #'tm/org-todo-window-advice))

  ;; Some modes seem to ignore `display-buffer-alist'; this stack
  ;; exchange answer provides a method for working around this problem:
  ;; https://stackoverflow.com/a/21764397
  (defun tm/mark-this-window-as-satellite ()
    "Mark the current window as the satellite window."
    (interactive)
    (mapc (lambda (win) (set-window-parameter win 'satellite nil))
          (window-list))
    (set-window-parameter nil 'satellite t)
    (message "Window: %s is now the satellite window."
             (selected-window)))

  (defun tm/get-satellite-window ()
    "Find and return the satellite window or nil if non exists."
    (find-if (lambda (win)
               (window-parameter win 'satellite))
             (window-list)))

  (defun tm/display-buffer-in-satellite (buffer ignore)
    "Display the buffer in the satellite window, or the first window \
    it finds if there is no satellite."
    (let ((satellite-window (or (tm/get-satellite-window)
                                (first (window-list)))))
      (select-window satellite-window)
      (display-buffer-same-window buffer nil)
      (display-buffer-record-window 'reuse satellite-window buffer)
      satellite-window))

  (setq display-buffer-alist
        ;; Help and stuff at the right
        `((,(rx string-start (or "*Apropos"
                                 "*Backtrace"
                                 "*Compile-Log*"
                                 "*Man"
                                 "*Process List*"
                                 "*Python"
                                 "*Warnings*"
                                 "*WoMan"
                                 "*compilation"
                                 "*helpful"
				 "magit: "
				 "*Org Select*"
                                 (and (0+ anything) ".pdf")
                                 (and (1+ not-newline) " output*"))) ; AUCTeX
           (display-buffer-reuse-window display-buffer-in-side-window)
           (direction . rightmost)
           (side . right)
           (window-width . 80)
           (window-height . 0.45))
          ;; Side window on bottom:
          (,(rx string-start (or "*Calendar"
                                 "*Reconcile"))
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (direction . bottom))
          ;; Right side, below the inferior buffer
          ;; (,(rx string-start (or "*help"
          ;;                        "R_x11"))
          ;;  (display-buffer-reuse-window my/display-window-at-right)
          ;;  (window-width . 80))
          ;; Full frame
          ;; (,(rx string-start "magit: ")
          ;;  (display-buffer-reuse-window my/display-buffer-fullframe))
          ;; Right side, above the inferior buffer
          ;; (,(rx string-start "*R dired")
          ;;  (display-buffer-reuse-window display-buffer-in-direction)
          ;;  (direction . rightmost)
          ;;  (side . right)
          ;;  (slot . -1)
          ;;  (window-height . 10))
          ;; Use same window
          (,(rx string-start (or "*Annotate "
                                 "*edit-indirect"
                                 "magit-log: "
                                 "magit-refs: "
                                 "*Org Src"))
           (display-buffer-reuse-window display-buffer-same-window))
	  (,(rx string-start (or "*Org Agenda*"))
	   (display-buffer-reuse-window display-buffer-in-side-window)
	   (direction . rightmost)
	   (side . right)))))

(provide 'init-nav)
;;; init-nav.el ends here
