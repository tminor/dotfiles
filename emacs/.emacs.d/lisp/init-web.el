;;; init-web.el --- Configuration for web template editing -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for HTML/JSP/etc.
;;
;;; Code:

(use-package web-mode
  :config
  (setq web-mode-engines-alist '(("jsp" . "\\.jsp"))))

(provide 'init-web)
;;; init-web.el ends here
