;;; init-shell.el --- Configuration for shell scripts -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for shell scripts.
;;
;;; Code:

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package fish-mode)

(use-package powershell)

(provide 'init-shell)
;;; init-shell.el ends here
