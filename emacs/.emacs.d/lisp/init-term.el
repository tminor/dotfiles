;;; init-term.el --- Configure an Emacs terminal emulator -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This space reserved for commentary.
;;
;;
;;; Code:

(use-package vterm
  :general
  (:keymaps 'vterm-mode-map
   :states '(insert)
   "<C-backspace>" (lambda ()
		     (interactive)
		     (vterm-send-key (kbd "C-w")))
   "<C-left>" 'vterm-send-M-b
   "<C-right>" 'vterm-send-M-f)
  :config
  (setq vterm-shell "fish"
	tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
	tramp-default-method "ssh"
        vterm-buffer-name-string "vterm %s")
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

(use-package systemd
  :straight
  (:host github :repo "holomorph/systemd-mode"
   :files (:defaults "*" "*/*"))
  :init
  (add-to-list 'auto-mode-alist '("\\.service.erb\\'" . systemd-mode))
  :hook
  (systemd-mode . poly-systemd+erb-mode))

(provide 'init-term)
;;; init-ivy.el ends here
