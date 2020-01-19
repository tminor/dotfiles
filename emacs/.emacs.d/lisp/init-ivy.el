;;; init-ivy.el --- Configure Ivy for completing read -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Ivy as a default for `completing-read' and
;; friends.
;;
;;; Code:

(use-package ivy-rich
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
           (lambda (cand) (get-buffer cand)))))
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

(provide 'init-ivy)
;;; init-ivy.el ends here
