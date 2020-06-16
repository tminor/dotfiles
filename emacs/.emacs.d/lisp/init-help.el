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
    "m" 'man
    "Lf" 'list-faces-display)
  :config
  (setq find-function-C-source-directory "~/src/emacs-26.3"))

(use-package info-plus
  :general
  (tm/leader-def
    :infix "h"
    :prefix-command 'tm/help-prefix-command
    :prefix-map 'tm/help-prefix-map
    "" '(:which-key "help prefix" :ignore t)
    "i" 'info)
  :hook
  (Info-selection . (lambda () (unless (eq buffer-face-mode 'variable-pitch)
			    (variable-pitch-mode))))
  (Info-mode . (lambda () (unless (eq buffer-face-mode 'variable-pitch)
		       (variable-pitch-mode)))))

(use-package info-lookup
  :straight nil
  :general
  (tm/leader-def
    :infix "h"
    :prefix-command 'tm/help-prefix-command
    :prefix-map 'tm/help-prefix-map
    "S" 'info-lookup-symbol))

(use-package info-colors
  :straight
  (:host github :repo "ubolonton/info-colors")
  :hook
  (Info-mode . rainbow-delimiters-mode)
  (Info-selection . info-colors-fontify-node))

(provide 'init-help)
;;; init-help.el ends here
