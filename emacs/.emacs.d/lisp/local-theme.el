;;; init-local-theme.el --- Add local theme modifications. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file adds local theme changes that depend on particular fonts.
;;
;;; Code:

(deftheme local "Local theme overrides")

(defgroup local-theme nil
  "Local theme customization options."
  :group 'local)

;;;
;; Typography

(defcustom local-theme-default-font-height nil
  "Default font height."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-line-spacing 0
  "Spacing between lines."
  :type 'number
  :group 'local-theme)

(defcustom local-theme-fixed-pitch-font "Monospace"
  "Font used for fixed-pitch."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-variable-pitch-font "sans-serif"
  "Font used for variable-pitch."
  :type 'string
  :group 'local-theme)

(defcustom local-theme-serif-font "serif"
  "Font used for serif."
  :type 'string
  :group 'local-theme)

;;;
;; Theme Faces

(custom-theme-set-faces
 'local
 `(default        ((t (:height ,local-theme-default-font-height :family ,local-theme-fixed-pitch-font :weight light))))
 `(fixed-pitch    ((t (:height 1.0 :family ,local-theme-fixed-pitch-font :weight light))))
 `(variable-pitch ((t (:height 1.0 :family ,local-theme-variable-pitch-font :weight normal))))
 `(line-number    ((t (:inherit fixed-pitch)))))

;;;
;; Theme Variables
(custom-theme-set-variables
 'local
 `(line-spacing ,local-theme-line-spacing))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'local)
(provide 'local-theme)
;;; init-local-theme.el ends here
