;;; init-prog-mode.el --- Configure general prog-mode behavior -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures general behavior expected for any `prog-mode'
;; buffers.
;;
;;; Code:

(require 'whitespace)
(require 'hideshow)
(require 'prog-mode)

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(use-package prog-mode
  :straight nil
  :config
  (setq prettify-symbols-unprettify-at-point t)
  (dolist (symbol '((">="  . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥))
                    ("<="  . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤))
                    ("lambda" . ?λ)))
    (cl-pushnew symbol prettify-symbols-alist :test #'equal))
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . whitespace-mode)
  (prog-mode . hs-minor-mode))

(global-prettify-symbols-mode 1)

(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'column
        highlight-indent-guides-responsive 'stack))

(use-package highlight-parentheses
  :init
  (highlight-parentheses-mode 1)
  :hook
  (prog-mode . highlight-parentheses-mode))

(use-package eldoc
  ;; https://github.com/jwiegley/dot-emacs/blob/4e87553c2f2d21e30be885bdfba83b40c4bf0bed/init.el
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package hideshow
  :straight nil
  :general
  (tm/leader-def
    :infix "TAB"
    :prefix-command 'tm/hideshow-prefix-command
    :prefix-map 'tm/hideshow-prefix-map
    "" '(:which-key "hideshow prefix" :ignore t)
    "s" 'hs-show-all
    "h" 'hs-hide-all
    "B" 'hs-show-block
    "b" 'hs-hide-block
    "l" 'hs-hide-level)
  (:keymaps 'prog-mode-map
   :states 'normal
   "TAB" 'hs-toggle-hiding))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook
  (eval-expression-minibuffer-setup . smartparens-mode)
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :config
  (require 'smartparens-config)
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*"
                   :actions '(insert wrap)
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p)
                   :wrap "C-_")
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))))

(provide 'init-prog-mode)
;;; init-prog-mode.el ends here
