;;; init.el --- Load Emacs's configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs.
;;
;;; Code:

(use-package anaconda-mode
  :straight
  (:host github :repo "pythonic-emacs/anaconda-mode")
  :hook
  (python-mode . '(anaconda-mode anaconda-eldoc-mode)))

(provide 'init-python)
;;; init-python.el ends here
