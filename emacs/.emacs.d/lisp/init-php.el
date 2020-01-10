;;; init-php.el --- Configuration for PHP -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures Emacs for PHP.
;;
;;; Code:

(use-package php-mode
  :straight
  (:host github :repo "emacs-php/php-mode")
  :init
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:php\\|phtml\\)\\'" . php-mode)))

(provide 'init-php)
;;; init-php.el ends here
