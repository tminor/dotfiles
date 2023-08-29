;;; init-lisp.el --- Configuration for LISP modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures packages for LISP editing.
;;
;;; Code:

(use-package lispy
  :general
  (:keymaps 'lispy-mode-map
   :states '(normal motion)
   "Q" 'lispy-ace-paren)
  :hook
  ((lisp-mode emacs-lisp-mode scheme-mode) . lispy-mode)
  :config
  (lispy-set-key-theme '(lispy c-digits)))

(use-package lispyville
  :general
  (:keymaps '(lisp-mode-map local)
   :states '(normal motion)
   "H" 'lispyville-move-up
   "L" 'lispyville-move-down)
  :init
  (with-eval-after-load 'lispyville
    (lispyville-set-key-theme
     '(slurp/barf-cp
       mark-toggle)))
  :hook
  ((emacs-lisp-mode lisp-mode scheme-mode) . lispyville-mode)
  ((emacs-lisp-mode lisp-mode scheme mode) .
   (lambda ()
     (setq-local lisp-indent-function
		 #'tm/lisp-indent-function))))

(use-package slime)
(provide 'init-lisp)
;;; init-lisp.el ends here
