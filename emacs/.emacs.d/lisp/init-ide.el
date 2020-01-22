;;; init-ide.el --- Configure IDE features -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures features such as syntax checking, linting, and
;; completion.
;;
;;; Code:

(use-package pos-tip)

(require 'pos-tip)

(use-package flycheck-pos-tip)

(use-package flycheck
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  :config
  ;; Config for RuboCop
  (setq flycheck-rubocoprc ".rubocop.yml")
  ;; Config for Puppet lint
  (setq flycheck-puppet-lint-rc "/home/tminor/.puppet-lint.rc")
  ;; Sets path to most recent puppet executable.
  (setq flycheck-puppet-parser-executable "/home/tminor/.gem/ruby/gems/puppet-6.5.0/bin/puppet")
  :hook
  (after-init . global-flycheck-mode))

(use-package company
  :init
  (setq company-idle-delay 0.02)
  :hook
  (prog-mode . company-mode))

(use-package company-quickhelp
  :hook
  (company-mode . company-quickhelp-mode))

(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode))

(use-package lsp-mode
  :hook
  (ruby-mode . lsp))
(use-package lsp-ui
  :hook
  (ruby-mode . lsp-ui-mode))
(use-package company-lsp)
(use-package lsp-treemacs)

(provide 'init-ide)
;;; init-ide.el ends here
