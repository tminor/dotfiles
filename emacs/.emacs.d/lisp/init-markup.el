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
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

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

(provide 'init-markup)
;;; init-markup.el ends here
