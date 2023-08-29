;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(setq comp-deferred-compilation nil)
(server-start)

(define-obsolete-variable-alias
  'native-comp-deferred-compilation-deny-list
  'native-comp-jit-compilation-deny-list
  "Renamed")

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

(setq default-directory "~/")

(defun tm/load (lib)
  (message (format "loading %s" lib))
  (require lib))

(tm/load 'init-package)
(tm/load 'init-key-bindings)
(tm/load 'init-settings)
(tm/load 'init-fonts)
(tm/load 'init-exec-path)
(tm/load 'init-desktop)
(tm/load 'init-ivy)
(tm/load 'init-nav)
(tm/load 'init-modeline)
(tm/load 'init-org)
(tm/load 'init-vc)
(tm/load 'init-dired)
(tm/load 'init-help)
(tm/load 'init-search)
(tm/load 'init-ide)
(tm/load 'init-dired)
(tm/load 'init-projectile)
(tm/load 'init-prose)
(tm/load 'init-term)

(tm/load 'init-prog-mode)
(tm/load 'init-elisp)
(tm/load 'init-golang)
(tm/load 'init-lisp)
(tm/load 'init-markup)
(tm/load 'init-php)
(tm/load 'init-regex)
(tm/load 'init-ruby)
(tm/load 'init-xml)
(tm/load 'init-shell)
(tm/load 'init-web)

(enable-theme 'taihaku)
(enable-theme 'local)

(put 'narrow-to-region 'disabled nil)
