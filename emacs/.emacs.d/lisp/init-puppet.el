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

(provide 'init-puppet)
;;; init-puppet.el ends here
