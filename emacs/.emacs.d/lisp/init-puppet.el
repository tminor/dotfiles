;;; init-puppet.el --- Puppet configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for Puppet.
;;
;;; Code:

(use-package puppet-mode
  :config
  (setq flycheck-puppet-parser-executable (executable-find "puppet"))
  (setq flycheck-puppet-lint-executable (executable-find "puppet-lint")))

(use-package lsp-puppet
  :straight
  (:host github :repo "tminor/lsp-puppet")
  :init
  (setq lsp-puppet-server-install-dir "/home/tminor/.emacs.d/.cache/lsp/puppet/puppet-editor-services/"
        lsp-puppet-modulepath '("/home/tminor/src/puppet/modules" "~/src/puppet/sssmodules")))

(provide 'init-puppet)
;;; init-puppet.el ends here
