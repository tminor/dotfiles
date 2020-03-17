;;; init-prose.el --- Configure packages for writing prose -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Installs and configures spell checking, grammar checking, etc.
;;
;;; Code:

(use-package flyspell
  :hook
  (text-mode . flyspell-mode))

(use-package powerthesaurus
  :general
  (tm/leader-def
    :infix "s"
    :prefix-command 'tm/search-prefix-command
    :prefix-map 'tm/search-prefix-map
    "" '(:which-key "search prefix" :ignore t)
    "t" 'powerthesaurus-lookup-word-dwim))

(use-package define-word
  :general
  (tm/leader-def
    :infix "s"
    :prefix-command 'tm/search-prefix-command
    :prefix-map 'tm/search-prefix-map
    "" '(:which-key "search prefix" :ignore t)
    "D" 'define-word-at-point))

(provide 'init-prose)
;;; init-prose.el ends here
