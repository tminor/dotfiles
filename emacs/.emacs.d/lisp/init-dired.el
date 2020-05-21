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

(use-package dired-sidebar
  :straight
  (:host github :repo "jojojames/dired-sidebar")
  :commands (dired-sidebar-toggle-sidebar)
  :hook
  (dired-sidebar-mode-hook . (lambda ()
			       (unless (file-remote-p default-directory)
				 (auto-revert-mode))))
  :config
  (setq dired-sidebar-theme 'icons
	dired-sidebar-use-term-integration t))

(provide 'init-dired)
;;; init-dired.el ends here
