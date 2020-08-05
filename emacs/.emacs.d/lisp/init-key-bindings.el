;;; init-key-bindings.el --- Configure Evil/General -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures `evil-mode' for Emacs.  `general' is used for
;; custom key bindings.
;;
;;; Code:

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-override-mode)
  (general-auto-unbind-keys)
  ;; Define a macro that binds commands to a "leader" key (SPC, in
  ;; this case).
  (general-create-definer tm/leader-def
    :states '(normal motion visual emacs insert)
    :keymaps '(local override)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :prefix-command 'tm/prefix-command
    :prefix-map 'tm/prefix-map))

(use-package evil
  :init
  (setq evil-want-keybinding nil
	evil-want-integration t
	evil-want-fine-undo t)
  :general
  ;; Override `evil-search-forward' binding in favor of `swiper'.
  (:states '(normal motion)
   :keymaps '(local override)
   "/" 'swiper)
  :config
  (evil-mode 1))

(use-package which-key
  :defer 1
  :general
  (tm/leader-def
    :infix "h"
    :prefix-command 'tm/help-prefix-command
    :prefix-map 'tm/help-prefix-map
    "t" 'which-key-show-top-level
    "M" 'which-key-show-major-mode)
  :config
  (which-key-mode 1))

(use-package evil-collection
  :custom
  (evil-collection-company-use-tng nil)
  (evil-collection-setup-minibuffer t)
  (evil-collection-term-sync-state-and-mode-p t)
  :init
  (evil-collection-init))

(use-package evil-magit
  :after magit
  :init
  (setq evil-magit-use-y-for-yank nil))

(use-package evil-org
  :after org
  :straight
  (:type git :host github :repo "Somelauw/evil-org-mode")
  :commands 'evil-org-agenda-set-keys
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . (lambda ()
                     (evil-org-set-key-theme '(textobjects
                                               insert
                                               navigation
                                               additional
                                               shift
                                               todo
                                               heading)))))

(use-package evil-surround
  :straight
  (:host github :repo "emacs-evil/evil-surround")
  :hook
  (after-init . global-evil-surround-mode))

(use-package free-keys
  :general
  (tm/leader-def
    :infix "h"
    :prefix-command 'tm/help-prefix-command
    :prefix-map 'tm/help-prefix-map
    "K" 'free-keys)
  :config
  (setq free-keys-modifiers '("" "C" "M" "C-M" "s")))

(use-package evil-multiedit
  :general
  (:keymaps '(override local)
   :states '(visual)
   "R" 'evil-multiedit-match-all))

(use-package evil-mc
  :hook
  (after-init . global-evil-mc-mode)
  :config
  (setq evil-mc-enable-bar-cursor t))

(provide 'init-key-bindings)
;;; init-key-bindings.el ends here
