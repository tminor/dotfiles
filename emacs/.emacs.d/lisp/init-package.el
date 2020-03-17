;;; init-package.el --- Configure package management -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures `straight.el' as Emacs's package manager.  It
;; also installs `use-package' for config management.
;;
;;; Code:

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Bootstrap `use-package' for configuration management
(straight-use-package 'use-package)
;; Implies (use-package :straight t) by default.
(setq straight-use-package-by-default t)

(setq straight-vc-git-default-protocol 'https)

(provide 'init-package)
;;; init-package.el ends here
