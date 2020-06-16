;;; init-web.el --- Configuration for web template editing -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for HTML/JSP/etc.
;;
;;; Code:

(use-package web-mode
  :general
  (:keymaps 'web-mode-map
   :states '(normal motion)
   "TAB" 'web-mode-fold-or-unfold)
  :config
  (setq web-mode-engines-alist `(("jsp" . ,(rx ".jsp")))))

(provide 'init-web)
;;; init-web.el ends here
