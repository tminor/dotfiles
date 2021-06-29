;;; init-jsonnet.el --- Configuration for Jsonnet -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for Jsonnet.
;;
;;; Code:

(use-package jsonnet-mode
  :straight
  (:host github :repo "tminor/jsonnet-mode" :branch "main")
  :config
  (eval-after-load "hideshow"
    '(add-to-list
      'hs-special-modes-alist
      `(jsonnet-mode
        ,(rx (or "{" "[" "("))          ; Block start
        ,(rx (or "}" "]" ")"))          ; Block end
        ,(rx (or "#" "//"))		; Comment start
        nil nil)))
  :hook
  (jsonnet-mode . (lambda ()
                    (setq-local comment-start "//")
                    (setq-local comment-end ""))))

(provide 'init-jsonnet)
;;; init-jsonnet.el ends here
