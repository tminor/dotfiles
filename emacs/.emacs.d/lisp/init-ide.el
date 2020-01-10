;;; init-ide.el --- Configure IDE features -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures features such as syntax checking, linting, and
;; completion.
;;
;;; Code:

(require 'pos-tip)

(use-package flycheck
  :config
  ;; Config for RuboCop
  (setq flycheck-rubocoprc ".rubocop.yml")
  ;; Config for Puppet lint
  (setq flycheck-puppet-lint-rc "/home/tminor/.puppet-lint.rc")
  ;; Sets path to most recent puppet executable.
  (setq flycheck-puppet-parser-executable "/home/tminor/.gem/ruby/gems/puppet-6.5.0/bin/puppet")
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-pos-tip
  :after '(flycheck pos-tip)
  :hook
  (prog-mode . (lambda () (flycheck-pos-tip-mode 1))))

(use-package company
  :init
  (setq company-idle-delay 0.02)
  :hook
  (prog-mode . company-mode))

(use-package company-quickhelp
  :after '(pos-tip company)
  :hook
  (prog-mode . company-quickhelp-mode))

(provide 'init-ide)
;;; init-ide.el ends here
