;;; init-eshell.el --- Configure Eshell -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Eshell and associated packages.
;;
;;; Code:

(require 'eshell)

(defun tm/setup-eshell-ivy-completion ()
  "Init Ivy completion for Eshell."
  (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
  ;; only if you want to use the minibuffer for completions instead of
  ;; the in-buffer interface
  (setq-local ivy-display-functions-alist
              (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                    ivy-display-functions-alist)))

;; Source: https://emacs.stackexchange.com/a/9521
;;
;; Example usage:
;;   ls "prefix-{A,B,C}.suffix"(|eshell-brace-expansion)
(defun eshell-brace-expansion (str)
  "Treat STR as an expandable brace expression, à la Bash."
  (let* ((parts (split-string str "[{}]"))
         (prefix (car parts))
         (body   (nth 1 parts))
         (suffix (nth 2 parts)))
    (mapcar (lambda (x) (concat prefix x suffix))
            (split-string body ","))))

(use-package egp
  :straight
  (dotfiles :host github :repo "dieggsy/dotfiles"
       :files (("emacs/.emacs.d/lisp/egp.el" . "egp.el")))
  :commands egp-theme)

(use-package eshell
  :init
  (require 'egp)
  :custom
  (eshell-prompt-function #'egp-theme)
  :config
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode))
  (setup-esh-help-eldoc))

(setq tramp-default-proxies-alist
      `((,(rx ".bor.usg.edu") nil "/ssh:tminor@solaire.sss.usg.edu:")))

(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode)
  (eshell-mode . tm/setup-eshell-ivy-completion))

(use-package fish-completion
  :straight (:host gitlab :repo "ambrevar/emacs-fish-completion"))

(use-package eshell-up
  :straight (:type git :host github :repo "peterwvj/eshell-up"))

(use-package eshell-prompt-extras)
(use-package eshell-did-you-mean)

(use-package aweshell
  :after (eshell-up)
  :general
  (tm/leader-def
    :infix "E"
    :prefix-command 'tm/eshell-prefix-command
    :prefix-map 'tm/eshell-prefix-map
    "" '(:which-key "eshell prefix" :ignore t)
    "n" 'aweshell-new
    "l" 'aweshell-next
    "h" 'aweshell-prev
    "b" 'aweshell-switch-buffer
    "u" 'eshell-up)
  :straight (:type git :host github :repo "manateelazycat/aweshell"
             :files ("*aweshell.el"))
  :init
  (setq aweshell-valid-command-color "#3EDAD4")
  (exec-path-from-shell-initialize))

(use-package eshell-fringe-status
  :hook
  (eshell-mode . eshell-fringe-status-mode))

;; Eshell aliases
(setq eshell-aliases-file (no-littering-expand-etc-file-name "eshell/aliases")
      eshell-prompt-function #'tm/eshell-prompt)

(defun tm/buffer-to-string (buf-name)
  "Call `buffer-string' against BUF-NAME."
  (interactive)
  (with-current-buffer buf-name
    (buffer-string)))

(use-package esh-help
  :straight
  (:host github :repo "tom-tan/esh-help"))

(provide 'init-eshell)
;;; init-eshell.el ends here
