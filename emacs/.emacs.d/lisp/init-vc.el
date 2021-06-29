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
  :after magit
  :config
  (add-to-list 'forge-alist '("gitlab.usg.edu"
			      "gitlab.usg.edu/api/v4"
			      "gitlab.usg.edu"
			      forge-gitlab-repository))
  (setq forge-topic-list-columns
	'(("#" 4
	   (lambda (a b)
	     (> (car a) (car b)))
	   (:right-align t) number nil)
	  ("Title" 35 t nil title  nil)
	  ("State" 6 t nil state nil)
	  ("Updated" 10 t nill updated nil))))

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

(use-package ghub
  :config
  (setq ghub-use-workaround-for-emacs-bug 'force))

(use-package git-link
  :config
  (setq git-link-open-in-browser t))

(use-package diff-hl
  :hook
  (prog-mode . diff-hl-margin-mode)
  (prog-mode . diff-hl-mode))

(provide 'init-vc)
;;; init-vc.el ends here
