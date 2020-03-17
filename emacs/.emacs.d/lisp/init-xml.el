;;; init-xml.el --- Configure XML-related modes -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures XML modes.
;;
;;; Code:

(require 'sgml-mode)
(require 'nxml-mode)

(use-package tidy)

(defun tm/tidy-buffer-xml (beg end)
  "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg
			   end
			   "tidy -xml -q -i"
			   (current-buffer)
			   t
			   "*tidy-errors*" t))

(add-to-list 'hs-special-modes-alist
             '(nxml-mode "<!--\\|<[^/>]*[^/]>"
			 "-->\\|</[^/>]*[^/]>"

			 "<!--"
			 sgml-skip-tag-forward
			 nil))

(general-define-key :keymaps 'nxml-mode-map
		    :states '(normal)
		    "TAB" 'hs-toggle-hiding)

(add-hook 'nxml-mode-hook 'hs-minor-mode)

(provide 'init-xml)
;;; init-xml.el ends here
