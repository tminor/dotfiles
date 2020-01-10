;;; init-lisp.el --- Configuration for LISP modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures packages for LISP editing.
;;
;;; Code:

(use-package lispy
  :hook
  (lisp-mode . lispy-mode))

(use-package lispyville
  :init
  (with-eval-after-load 'lispyville
    (lispyville-set-key-theme
     '(slurp/barf-cp
       mark-toggle)))
  :hook
  (emacs-lisp-mode . lispyville-mode)
  (emacs-lisp-mode . (lambda ()
		       (setq-local lisp-indent-function
                                   #'tm/lisp-indent-function))))

(provide 'init-lisp)
;;; init-lisp.el ends here
