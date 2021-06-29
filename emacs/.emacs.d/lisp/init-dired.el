;;; init-dired.el --- Install and configure dired and related packages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures dired and related packages.
;;
;;; Code:

;; (use-package dired-plus
;;   :after 'dired)

(use-package all-the-icons-dired
  :straight
  (:host github :repo "jtbm37/all-the-icons-dired")
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package treemacs)

(use-package treemacs-evil)

(use-package treemacs-projectile)

(use-package dired-subtree)

(provide 'init-dired)
;;; init-dired.el ends here
