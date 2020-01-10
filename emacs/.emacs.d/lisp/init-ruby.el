;;; init-ruby.el --- Ruby configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for Ruby.
;;
;;; Code:

(use-package eruby-mode
  :straight
  (:host github :repo "petere/emacs-eruby-mode"))

(use-package enh-ruby-mode
  :requires
  (exec-path-from-shell)
  :config
  (eval-after-load "hideshow"
    '(add-to-list
      'hs-special-modes-alist
      `(enh-ruby-mode
        ,(rx (or "def" "class" "module" "do" "{" "[" "(")) ; Block start
        ,(rx (or "}" "]" ")" "end"))		           ; Block end
        ,(rx (or "#" "=begin"))		; Comment start
        ruby-forward-sexp nil)))
  ;; https://dev.to/thiagoa/ruby-and-emacs-tip-advanced-pry-integration-33bk
  (setenv "VISUAL" "emacsclient")
  (setenv "EDITOR" (getenv "VISUAL"))
  :init
  (add-to-list 'auto-mode-alist
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
                 . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  (exec-path-from-shell-initialize)
  :hook
  (enh-ruby-mode . eldoc-mode)
  (enh-ruby-mode . yard-mode)
  (enh-ruby-mode . robe-mode))

(use-package robe
  :hook
  (ruby-mode . robe-mode)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package yard-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
