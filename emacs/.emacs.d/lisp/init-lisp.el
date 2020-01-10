;;; init-lisp.el --- Configuration for LISP modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures packages for LISP editing.
;;
;;; Code:

(use-package lispy
  :hook
  ((lisp-mode emacs-lisp-mode) . lispy-mode)
  :config
  (lispy-set-key-theme '(lispy c-digits)))

(use-package lispyville
  :init
  (with-eval-after-load 'lispyville
    (lispyville-set-key-theme
     '(slurp/barf-cp
       mark-toggle)))
  :hook
  ((emacs-lisp-mode lisp-mode) . lispyville-mode)
  ((emacs-lisp-mode lisp-mode) . (lambda ()
				   (setq-local lisp-indent-function
					       #'tm/lisp-indent-function))))

(provide 'init-lisp)
;;; init-lisp.el ends here
