;;; init-projectile.el --- Install and configure Projectile -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file installs and configures Projectile.
;;
;;; Code:

(use-package projectile-ripgrep)

(use-package projectile
  :init
  (require 'projectile-ripgrep)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  ;; https://github.com/bbatsov/projectile/issues/1323
  (setq projectile-git-submodule-command nil)
  :general
  (tm/leader-def
    "p" '(:keymap projectile-command-map
          :package projectile
          :which-key "projectile prefix")))

(provide 'init-projectile)
;;; init-projectile.el ends here
