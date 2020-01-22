;;; init-xml.el --- Configure XML-related modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures XML modes.
;;
;;; Code:

(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode "<!--\\|<[^/>]*[^/]>"
			 "-->\\|</[^/>]*[^/]>"

			 "<!--"
			 sgml-skip-tag-forward
			 nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)
;;; init-xml.el ends here
