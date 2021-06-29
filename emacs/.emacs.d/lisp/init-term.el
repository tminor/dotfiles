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

(use-package fish-colors
  :straight
  (:host github :repo "snyball/emacs-fish-colors")
  :config
  (setq fish-color-to-emacs-face '((fish_color_autosuggestion . font-lock-comment-face)
                                   (fish_color_cancel . error)
                                   (fish_color_command . font-lock-keyword-face)
                                   (fish_color_comment . font-lock-comment-face)
                                   (fish_color_cwd . default)
                                   (fish_color_cwd_root . default)
                                   (fish_color_end . default)
                                   (fish_color_error . error)
                                   (fish_color_escape . default)
                                   (fish_color_history_current . default)
                                   (fish_color_host . default)
                                   (fish_color_host_remote . default)
                                   (fish_color_match . default)
                                   (fish_color_normal . default)
                                   (fish_color_operator . font-lock-function-name-face)
                                   (fish_color_param . font-lock-constant-face)
                                   (fish_color_quote . font-lock-string-face)
                                   (fish_color_redirection . font-lock-function-name-face)
                                   (fish_color_search_match . default)
                                   (fish_color_selection . default)
                                   (fish_color_status . default)
                                   (fish_color_user . default))))

(provide 'init-term)
;;; init-ivy.el ends here
