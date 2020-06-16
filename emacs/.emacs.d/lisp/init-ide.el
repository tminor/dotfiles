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
  (ruby-mode . lsp)
  (sh-mode . lsp)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :general
  (tm/leader-def
    :infix "t"
    :prefix-command 'tm/toggle-prefix-command
    :prefix-map 'tm/toggle-prefix-map
    :major-modes 'prog-mode
    "" '(:which-key "toggle prefix" :ignore t)
    "d" 'lsp-ui-doc-mode)
  :hook
  (ruby-mode . lsp-ui-mode))

(use-package company-lsp)
(use-package lsp-treemacs)

(use-package highlight-thing
  :hook
  (prog-mode . highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0.1)
  (setq highlight-thing-prefer-active-region t))

(general-define-key
 :keymaps 'prog-mode-map
 :states '(normal motion)
 "g." 'xref-find-definitions)

(use-package drag-stuff
  :hook
  (prog-mode . (lambda () (drag-stuff-mode t)))
  :general
  (:keymaps '(prog-mode-map)
   :states '(normal motion)
   "J" 'drag-stuff-down
   "K" 'drag-stuff-up
   "H" 'drag-stuff-left
   "L" 'drag-stuff-right)
  (:keymaps '(text-mode-map)
   :states '(motion normal visual)
   "gDh" 'drag-stuff-left
   "gDl" 'drag-stuff-right
   "gDj" 'drag-stuff-down
   "gDk" 'drag-stuff-up))

(use-package emr)

(provide 'init-ide)
;;; init-ide.el ends here
