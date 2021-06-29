;;; init-markup.el --- Configuration for mark up languages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures mark up languages (and other things like
;; YAML).
;;
;;; Code:

(use-package markdown-mode
  :general
  (:keymaps 'markdown-mode-map
   :states '(normal motion)
   "TAB" 'markdown-cycle))

(use-package pandoc-mode
  :general
  (tm/leader-def
    "P" 'pandoc-main-hydra/body))

(use-package yaml-mode
  :straight
  (:host github :repo "yoshiki/yaml-mode")
  :general
  (:keymaps 'yaml-mode-map
   :states '(normal motion)
   "TAB" 'outline-toggle-children)
  :hook
  (yaml-mode . tm/yaml-mode-outline-hook)
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (defun tm/yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))
  (defun tm/yaml-mode-outline-hook ()
    (outline-minor-mode)
    (setq outline-regexp "^\\([ ]\\{2\\}\\)*\\([-] \\)?\\([\"][^\"]*[\"]\\|[a-zA-Z0-9_-]*\\): *\\([>|]\\|&[a-zA-Z0-9_-]*\\)?$")
    (setq outline-level 'tm/yaml-outline-level)))

(use-package csv-mode)

(use-package grip-mode
  :general
  (:keymaps '(markdown-mode-map)
   :states '(motion normal)
   "gp" 'grip-start-preview)
  :config
  (setq grip-update-after-change nil)
  (setq grip-preview-use-webkit nil))

(use-package flycheck-mmark
  :hook
  (flycheck-mode . (lambda () (flycheck-mmark-setup))))

(use-package mermaid-mode
  :config
  (setq mermaid-mmdc-location "/home/tminor/node_modules/.bin/mmdc"))

(provide 'init-markup)
;;; init-markup.el ends here
