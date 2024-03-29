;;; init-settings.el --- Configure default Emacs behavior -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Disables features such as the scroll bar, menu, etc.
;;
;;; Code:

(require 'recentf)
(require 'savehist)

(defun tm/recentf-keep-predicate (file)
  "Return non-nil if FILE should be kept in the recent list.

It handles the case of remote files as well."
  (cond
   ((file-remote-p file))
   ((file-readable-p file))))

(setq recentf-max-saved-items 200)

;; Disable GUI features.
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Configure cleanup at midnight.
(use-package midnight
  :demand t
  :config
  (setq clean-buffer-list-delay-general 2
        clean-buffer-list-kill-regexps '("\\`\\*Man "
                                         "\\`\\*helpful "
                                         "\\`\\*notmuch-"
                                         "\\magit"
                                         "\\`\\*CPU-Profiler-"
                                         "\\`\\*deadgrep"
                                         "\\`\\*Calendar"
                                         "\\`\\*Proced"
                                         "\\`\\*WoMan"
                                         "\\`\\*forge"
                                         "\\`\\*Free keys")))

(midnight-mode)

(savehist-mode 1)

(use-package no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; One of the most common types of files that Emacs creates
  ;; automatically is auto-save files. The following variable stores
  ;; these files elsewhere.
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Tell `custom' to store its state under ~/.emacs.d/etc/
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  ;; Store history file in ~/.emacs.d/var/
  (setq savehist-file (no-littering-expand-var-file-name "history")))

(use-package clipmon
  :defer 5
  :hook
  (after-init . (lambda () (clipmon-mode-start)))
  (after-init . (lambda () (clipmon-persist)))
  :config
  (setq kill-ring-max 10000)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (setq savehist-autosave-interval (* 5 60)))

(use-package rainbow-mode
  :hook
  (after-init . rainbow-mode))

(use-package expand-region
  :general
  (tm/leader-def
    "=" 'er/expand-region)
  (:keymaps '(local override)
   :states '(normal motion)
   "g=" 'er/expand-region)
  :straight
  (:host github :repo "magnars/expand-region.el"))

(defun tm/disable-scroll-bars (frame)
  "Toggle scrollbar in FRAME."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(add-hook 'after-make-frame-functions 'tm/disable-scroll-bars)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package taihaku-theme
  :straight
  (:host github :repo "tminor/taihaku-theme"))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; https://github.com/bbatsov/emacs.d/blob/965d39c245bdbe79e88dd228756a9cf621670ac0/init.el#L99-L104
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(use-package persistent-scratch
  :straight
  (:host github :repo "Fanael/persistent-scratch")
  :hook
  (after-init . persistent-scratch-setup-default))

(provide 'init-settings)
;;; init-settings.el ends here
