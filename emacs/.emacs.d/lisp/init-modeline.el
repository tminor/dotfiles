;;; init-modeline.el --- Load Emacs's configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs's modeline.
;;
;;; Code:

(use-package doom-modeline
  :init
  (require 'all-the-icons)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-all)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 45)
  :hook (after-init . doom-modeline-init))

(provide 'init-modeline)
;;; init-modeline.el ends here
