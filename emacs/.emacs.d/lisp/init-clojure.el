;;; init-clojure.el --- Configuration for Clojure -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for Clojure.
;;
;;; Code:

(use-package cider
  :defer t
  :config
  (setq cider-repl-history-file (no-littering-expand-var-file-name ".cider-repl-history")
        nrepl-log-messages t))

(use-package flycheck-clj-kondo)
(use-package flycheck-joker)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo)
  (require 'flycheck-joker)
  (dolist (checker '(clj-kondo-clj
                     clj-kondo-cljs
                     clj-kondo-cljc
                     clj-kondo-edn))
    (setq flycheck-checkers
          (cons checker
                (delq checker flycheck-checkers))))
  (dolist (checkers '((clj-kondo-clj . clojure-joker)
                      (clj-kondo-cljs . clojurescript-joker)
                      (clj-kondo-cljc . clojure-joker)
                      (clj-kondo-edn . edn-joker)))
    (flycheck-add-next-checker (car checkers)
                               (cons 'error (cdr checkers))))
  :hook
  (clojure-mode . lispy-mode)
  (clojure-mode . lispyville-mode))

(provide 'init-clojure)
;;; init-clojure.el ends here
