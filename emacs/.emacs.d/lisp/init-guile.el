;;; init-guile.el --- Install Guile packages.
;;
;;; Commentary:
;;
;; This file configures and installs Guile packages.
;;
;;; Code:

(use-package geiser
  :config
  (setq geiser-guile-binary "/usr/bin/guile2.2"))

(provide 'init-guile)
;;; init-guile.el ands here
