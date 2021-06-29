;;; init-ruby.el --- Ruby configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for Ruby.
;;
;;; Code:

(use-package crystal-mode)

(use-package ruby-mode
  :straight nil
  :requires
  (exec-path-from-shell)
  :config
  (eval-after-load "hideshow"
    '(add-to-list
      'hs-special-modes-alist
      `(ruby-mode
        ,(rx (or "def" "class" "module" "do" "{" "[" "(" "if" "unless" "case")) ; Block start
        ,(rx (or "}" "]" ")" "end"))    ; Block end
        ,(rx (or "#" "=begin"))		; Comment start
        ruby-forward-sexp nil)))
  ;; https://dev.to/thiagoa/ruby-and-emacs-tip-advanced-pry-integration-33bk
  (setenv "VISUAL" "emacsclient")
  (setenv "EDITOR" (getenv "VISUAL"))
  (setq ruby-align-chained-calls t)
  :init
  (add-to-list 'auto-mode-alist
               ;; TODO: Refactor this with `rx'.
               '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
                 . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  (exec-path-from-shell-initialize)
  :hook
  (ruby-mode . eldoc-mode)
  (ruby-mode . yard-mode))

(use-package rvm
  :hook
  (ruby-mode . rvm-use-default))

(use-package yard-mode)

(use-package ruby-refactor
  :general
  (:keymaps 'ruby-mode-map
   :states '(normal motion)
   "gr" 'emr-show-refactor-menu)
  :hook
  (ruby-mode . (lambda () (ruby-refactor-mode-launch))))

(use-package inf-ruby
  :hook
  (inf-ruby-mode . company-mode))

(use-package evil-ruby-text-objects
  :straight
  (:host github :repo "porras/evil-ruby-text-objects")
  :hook
  (ruby-mode . evil-ruby-text-objects-mode))

(use-package poly-erb
  :init
  (define-polymode poly-systemd+erb-mode
    :innermodes '(poly-erb-innermode))
  (add-to-list 'auto-mode-alist '("\\.service.erb\\'" . poly-systemd+erb-mode)))

(use-package rspec-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
