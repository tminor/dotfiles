;;; init-regex.el --- Regular expression configurations -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs and regular expressions.
;;
;;; Code:

(use-package pcre2el)

(use-package visual-regexp-steroids
  :straight
  (:host github :repo "benma/visual-regexp-steroids.el")
  :init
  (setq vr/engine 'pcre2el))

(use-package visual-regexp
  :general
  (tm/leader-def
    "r" 'vr/isearch-forward
    "R" 'vr/isearch-backward)
  :init
  (require 'visual-regexp-steroids)
  (require 'pcre2el))

(provide 'init-regex)
;;; init-regex.el ends here
