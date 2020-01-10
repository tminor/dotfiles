;;; init-dired.el --- Install and configure dired and related packages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures dired and related packages.
;;
;;; Code:

(use-package dired+
  :init
  (with-eval-after-load "dired" '(require 'dired+)))

(provide 'init-dired)
;;; init-dired.el ends here
