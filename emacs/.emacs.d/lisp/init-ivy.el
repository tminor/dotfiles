;;; init-ivy.el --- Configure Ivy for completing read -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Ivy as a default for `completing-read' and
;; friends.
;;
;;; Code:

(use-package ivy-rich
  :straight
  (:host github :repo "Yevgnen/ivy-rich")
  :init
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon :width 2)
            (ivy-rich-switch-buffer-path
             (:width 20 :face font-lock-keyword-face))
            (ivy-rich-candidate
             (:width 30))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face font-lock-function-name-face :align right))
            (ivy-rich-switch-buffer-major-mode
             (:width 12 :face font-lock-string-face))
            (ivy-rich-switch-buffer-project
             (:width 15 :face font-lock-variable-name-face)))
           :predicate
           (lambda (cand) (get-buffer cand)))
	  counsel-find-file
	  (:columns
	   ((ivy-read-file-transformer)
	    (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
	  counsel-M-x
	  (:columns
	   ((counsel-M-x-transformer (:width 40))
	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
	  counsel-describe-function
	  (:columns
	   ((counsel-describe-function-transformer (:width 40))
	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
	  counsel-describe-variable
	  (:columns
	   ((counsel-describe-variable-transformer (:width 40))
	    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
	  counsel-recentf
	  (:columns
	   ((ivy-rich-candidate (:width 0.8))
	    (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
	  package-install
	  (:columns
	   ((ivy-rich-candidate (:width 30))
	    (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
	    (ivy-rich-package-archive-summary (:width 7
					       :face font-lock-builtin-face))
	    (ivy-rich-package-install-summary (:face font-lock-doc-face)))))
	ivy-rich-parse-remote-buffer nil)
  :hook
  (after-init . ivy-rich-mode))

(use-package ivy
  :general
  (ivy-minibuffer-map
   "M-j" 'ivy-next-line
   "M-k" 'ivy-previous-line)
  (tm/leader-def
    "b" 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

(use-package counsel
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file)
  (imap minibuffer-local-command
    "C-r" 'counsel-minibuffer-history)
  (tm/leader-def
    "x" 'counsel-M-x
    "y" 'counsel-yank-pop
    "F" 'counsel-org-goto-all)
  (tm/leader-def
    :infix "f"
    :prefix-command 'tm/find-prefix-command
    "" '(:which-key "find prefix" :ignore t)
    "f" 'counsel-find-file
    "F" 'find-file-other-window
    "j" 'counsel-file-jump
    "l" 'counsel-locate)
  (tm/leader-def
    :infix "h"
    :prefix-command 'tm/help-prefix-command
    "" '(:which-key "help prefix" :ignore t)
    "F" 'counsel-describe-face
    "b" 'counsel-descbinds
    "f" 'counsel-describe-function
    "v" 'counsel-describe-variable)
  :custom
  (counsel-find-file-ignore-regexp "\\`\\.")
  (counsel-yank-pop-preselect-last t)
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :config
  (when (eq system-type 'darwin)
    (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))

  (setq conusel-org-goto-display-style 'path
        counsel-org-headline-path-separator ": "
        counsel-org-goto-face-style 'org
        counsel-org-headline-display-todo t
        counsel-grep-base-command "rg -Sz -M 120 --no-heading --line-number --color never %s %s"
        counsel-rg-base-command "rg -Sz -M 120 --no-heading --line-number --color never %s ."
        counsel-yank-pop-separator "\n─────────────────────────\n"
        counsel-find-file-ignore-regexp (rx (or (group string-start (char ".#"))
                                                (group (char "~#") string-end)
                                                (group ".elc" string-end)
                                                (group ".pyc" string-end)
                                                (group ".import.scm" string-end)
                                                (group ".so" string-end))))
  (counsel-mode 1)
  (defalias 'locate #'counsel-locate))

(use-package swiper)

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package prescient)

(use-package ivy-prescient
  :hook
  (ivy-mode . ivy-prescient-mode))

(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode))

(use-package ivy-posframe
  :config
  (defun +ivy-posframe-display-exwm (str)
    (ivy-posframe--display
     str
     (lambda (info)
       (let* ((workarea (elt exwm-workspace--workareas exwm-workspace-current-index))
	      (x (aref workarea 0))
	      (y (aref workarea 1))

	      (fw (aref workarea 2))
	      (fh (aref workarea 3))

	      (pw (plist-get info :posframe-width))
	      (ph (plist-get info :posframe-height)))

	 (cons (+ x (/ (- fw pw) 2)) (+ y (/ (- fh ph) 2)))))))

  (setq ivy-posframe-display-functions-alist
	'((t . +ivy-posframe-display-exwm))

	ivy-posframe-parameters '((parent-frame nil)
				  (z-group . above)))

  ;; force set frame-position on every posframe display
  (advice-add 'posframe--set-frame-position :before
	      (lambda (&rest args)
		(setq-local posframe--last-posframe-pixel-position nil))))

(provide 'init-ivy)
;;; init-ivy.el ends here
