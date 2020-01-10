;;; init-vc.el --- Configuration for version control -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for version control.
;;
;;; Code:

(use-package magit
  :general
  (tm/leader-def
    :infix "m"
    :prefix-command 'tm/magit-prefix-command
    :prefix-map 'tm/magit-prefix-map
    "" '(:which-key "magit prefix" :ignore t)
    "B" 'tm/magit-blame-toggle
    "C" 'magit-clone
    "L" 'magit-log-buffer-file
    "a" 'magit-submodule-add
    "b" 'magit-branch
    "c" 'magit-checkout
    "f" 'magit-find-file
    "l" 'magit-log-all
    "s" 'magit-status
    "p" 'magit-file-popup
    "A" 'vc-annotate)
  :config
  (setq magit-diff-refine-hunk t
        auto-revert-check-vc-info t
        git-commit-summary-max-length 50
        git-commit-major-mode 'org-mode))

(use-package magit-todos)

(use-package forge
  :straight
  (:host github :repo "magit/forge")
  :after magit)

(use-package vc-hooks
  :straight nil
  :init
  (setq vc-follow-symlinks t))

(use-package smerge-mode
  :straight nil
  :general
  (:keymaps 'smerge-mode-map
   :states '(normal)
   "g <up>" 'smerge-keep-upper
   "g <down>" 'smerge-keep-lower
   "]n" 'smerge-next
   "[p" 'smerge-prev
   "gc" 'smerge-keep-current
   "gr" 'smerge-resolve))

(provide 'init-vc)
;;; init-vc.el ends here
