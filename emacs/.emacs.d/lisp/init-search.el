;;; init-search.el --- Configure grep/rg/ag packages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures packages for search utilities such as grep,
;; rg, etc.
;;
;;; Code:

(use-package ripgrep)
(use-package deadgrep
  :general
  (tm/leader-def
    :infix "s"
    :prefix-command 'tm/search-prefix-command
    :prefix-map 'tm/search-prefix-map
    "" '(:which-key "search prefix" :ignore t)
    "d" 'deadgrep))

(provide 'init-search)
;;; init-search.el ends here
