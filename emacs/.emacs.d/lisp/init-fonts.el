;;; init-fonts.el --- Install and configure fonts -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file enables usage of fonts like font-awesome/all-the-icons.
;;
;;; Code:

(defmacro eval-after-make-graphic-frame (hook-id &rest forms)
  "Run HOOK-ID hook with FORMS after making a frame on a graphic display.
Add a hook to `after-make-frame-functions' with form wrapped in a
`display-graphic-p' condition."
  (let ((hook-name (intern
                    (concat
                     "+eval-after-make-graphic-frame--"
                     hook-id
                     "-hook"))))
    `(progn
       (defun ,hook-name (frame)
         (with-selected-frame frame
           (when (display-graphic-p frame)
             ,@forms)))
       (add-hook 'after-make-frame-functions #',hook-name))))

(use-package all-the-icons)

(eval-and-compile
  (defvar tm/line-spacing nil
    "Spacing between lines.")
  (defvar tm/default-font-height 120))

(eval-and-compile
  (defvar tm/fixed-pitch-font "Fira Emacs"
"Font used for fixed-pitch faces.")

  (defvar tm/variable-pitch-font "Fira Sans"
"Font used for variable-pitch faces.")

  (defvar tm/serif-font "Noto Serif"
"Font used for serif faces.")

  (defvar tm/unicode-font "Dejavu Sans Mono"
"Fallback font used for unicode glyphs.")

  (defvar tm/emoji-font "Noto Emoji"
"Font used for symbol/emoji faces."))

(eval-after-make-graphic-frame
 "setup-emoji-font"
 (set-fontset-font "fontset-default" 'symbol
		   (font-spec :family tm/emoji-font) nil 'prepend))

(setq local-theme-default-font-height tm/default-font-height
      local-theme-line-spacing tm/line-spacing
      local-theme-fixed-pitch-font tm/fixed-pitch-font
      local-theme-variable-pitch-font tm/variable-pitch-font
      local-theme-serif-font tm/serif-font)

(load-file (concat user-emacs-directory "lisp/init-local-theme.el"))

(if (and (file-exists-p (concat user-emacs-directory "lisp/fira-code.el"))
	 (file-exists-p (concat user-emacs-directory "lisp/fira-code-data.el")))
    (progn
      (require 'fira-code)
      (add-hook 'prog-mode-hook #'fira-code-mode)))

(provide 'init-fonts)
;;; init-fonts.el ends here
