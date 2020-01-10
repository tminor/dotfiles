;;; init-help.el --- Install and configure help packages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file installs and configures packages that assist help
;; functionality.
;;
;;; Code:

(tm/leader-def
  :infix "h"
  :prefix-command 'tm/help-prefix-command
  :prefix-map 'tm/help-prefix-map
  "l" 'view-lossage
  "g" 'general-describe-keybindings)

(use-package helpful
  :general
  (tm/leader-def
    :infix "h"
    :prefix-command 'tm/help-prefix-command
    :prefix-map 'tm/help-prefix-map
    "" '(:which-key "help prefix" :ignore t)
    "s" 'helpful-symbol
    "k" 'helpful-key
    "m" 'man))

(use-package info+
  :straight
  (:host github :repo "emacsmirror/info-plus")
  :general
  (tm/leader-def
    :infix "h"
    :prefix-command 'tm/help-prefix-command
    :prefix-map 'tm/help-prefix-map
    "" '(:which-key "help prefix" :ignore t)
    "i" 'info)
  :init
  (with-eval-after-load "info" '(require 'info+)))

(provide 'init-help)
;;; init-help.el ends here
