;;; init-golang.el --- Go configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for Go.
;;
;;; Code:

(use-package go-mode
  :straight
  (:host github :repo "dominikh/go-mode.el"))

(use-package go-koans
  :straight
  (:host github :repo "exu/go-koans.el"))

(use-package flycheck-golangci-lint
  :hook
  (go-mode . flycheck-golangci-lint-setup))

(use-package go-dlv)

(provide 'init-golang)
;;; init-golang.el ends here
