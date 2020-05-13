;;; init-clojure.el --- Configuration for Clojure -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for Clojure.
;;
;;; Code:

(use-package cider
  :defer t
  :general
  (:keymaps 'cider-mode-map
   :states '(normal motion)
   "gd" 'cider-doc)
  :config
  (setq cider-repl-history-file (no-littering-expand-var-file-name
				 ".cider-repl-history")
        nrepl-log-messages t)
  (flycheck-clojure-setup)
  :hook
  (clojure-interaction-mode . cider-turn-on-eldoc-mode))

(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function
	   #'flycheck-pos-tip-error-messages)))

(use-package flycheck-clj-kondo
  :config
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers)))))

(use-package flycheck-joker)

(use-package clojure-mode
  :hook
  (clojure-mode . lispyville-mode)
  (clojure-mode . cider-mode)
  :config
  (require 'flycheck-clj-kondo)
  (require 'flycheck-joker)
  (dolist (checkers '((clj-kondo-clj . clojure-joker)
		      (clj-kondo-cljs . clojurescript-joker)
		      (clj-kondo-cljc . clojure-joker)
		      (clj-kondo-edn . edn-joker)
		      (clojure-joker . clojure-cider-eastwood)))
    (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))

(provide 'init-clojure)
;;; init-clojure.el ends here
