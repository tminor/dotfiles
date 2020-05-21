;;; init-term.el --- Configure an Emacs terminal emulator -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This space reserved for commentary.
;;
;;
;;; Code:

(use-package vterm
  :config
  (setq vterm-shell "fish"
	tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
	tramp-default-method "ssh")
  :hook
  (evil-insert-state-entry
   . (lambda ()
       (when (eq major-mode 'vterm-mode)
	 (setq-local cursor-type 'hbar)
	 (setq-local evil-move-cursor-back nil)))))

(use-package multi-vterm
  :straight
  (:host github :repo "suonlight/multi-vterm")
  :general
  (:keymaps 'vterm-mode-map
   :states '(normal motion)
   "gn" 'multi-vterm-next
   "gp" 'multi-vterm-prev))

(provide 'init-term)
;;; init-ivy.el ends here
