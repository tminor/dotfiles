;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; for native-comp branch
(when (fboundp 'native-compile-async)
  (if (yes-or-no-p "async compile?")
      (setq comp-async-jobs-number 4 ;; not using all cores
            comp-deferred-compilation t
            comp-deferred-compilation-black-list '())
    (setq comp-deferred-compilation nil)))

;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
(setq debug-on-error nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun tm/minibuffer-increase-gc-cons-threshold ()
  "Disable GC before entering the minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))
(defun tm/miniguffer-decrease-gc-cons-threshold ()
  "Enable GC when leaving the minibuffer."
  (setq gc-cons-threshold (* 20 1024 1024)))

(add-hook 'minibuffer-setup-hook #'tm/minibuffer-increase-gc-cons-threshold)
(add-hook 'minibuffer-exit-hook #'tm/miniguffer-decrease-gc-cons-threshold)

(setq default-directory "/home/tminor")

(require 'init-package)
(require 'init-key-bindings)
(require 'init-settings)
(require 'init-fonts)
(require 'init-exec-path)
(require 'init-desktop)
(require 'init-ivy)
(require 'init-nav)
(require 'init-modeline)
(require 'init-org)
(require 'init-vc)
(require 'init-dired)
(require 'init-help)
(require 'init-search)
(require 'init-ide)
(require 'init-dired)
(require 'init-hydra)
(require 'init-eshell)
(require 'init-mail)
(require 'init-projectile)
(require 'init-log)
(require 'init-prose)
(require 'init-term)

(require 'init-prog-mode)
(require 'init-clojure)
(require 'init-coffee)
(require 'init-dired)
(require 'init-elisp)
(require 'init-golang)
(require 'init-jsonnet)
(require 'init-lisp)
(require 'init-markup)
(require 'init-php)
(require 'init-puppet)
(require 'init-python)
(require 'init-regex)
(require 'init-ruby)
(require 'init-xml)
(require 'init-shell)
(require 'init-yasnippet)
(require 'init-web)
(require 'init-guile)

(enable-theme 'taihaku)
(enable-theme 'local)

(put 'narrow-to-region 'disabled nil)
