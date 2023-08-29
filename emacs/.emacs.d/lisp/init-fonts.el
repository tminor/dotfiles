;;; init-fonts.el --- Install and configure fonts -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file enables usage of fonts like font-awesome/all-the-icons.
;;
;;; Code:

(setq use-default-font-for-symbols t)

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

(use-package all-the-icons
  :config
  (let* ((font-files '("all-the-icons.ttf"
                       "file-icons.ttf"
                       "fontawesome.ttf"
                       "material-design-icons.ttf"
                       "octicons.ttf"
                       "weathericons.ttf"))
         (font-dest (cond
                     ;; Default Linux install directories
                     ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                      (concat (or (getenv "XDG_DATA_HOME")
                                  (concat (getenv "HOME") "/.local/share"))
                              "/fonts/"
                              all-the-icons-fonts-subdirectory))
                     ;; Default MacOS install directory
                     ((eq system-type 'darwin)
                      (concat (getenv "HOME")
                              "/Library/Fonts/"
                              all-the-icons-fonts-subdirectory))))
         (font-file-paths (mapcar (lambda (f) (concat font-dest f)) font-files)))
    (when (require 'dash nil t)
      (unless (-all? #'file-exists-p font-file-paths)
        (all-the-icons-install-fonts)))))

(eval-and-compile
  (defvar tm/line-spacing nil
    "Spacing between lines.")
  (defvar tm/default-font-height 120))

(eval-and-compile
  (defvar tm/fixed-pitch-font "Fira Mono"
    "Font used for fixed-pitch faces.")

  (defvar tm/variable-pitch-font "Fira Sans"
    "Font used for variable-pitch faces.")

  (defvar tm/serif-font "Noto Serif"
    "Font used for serif faces.")

  (defvar tm/unicode-font "Dejavu Sans Mono"
    "Fallback font used for unicode glyphs.")

  (defvar tm/emoji-font "Material Icons"
    "Font used for symbol/emoji faces."))

(eval-after-make-graphic-frame
 "setup-emoji-font"
 (set-fontset-font "fontset-default" 'symbol
                   (font-spec :name "FontAwesome")))

(setq local-theme-default-font-height tm/default-font-height
      local-theme-line-spacing tm/line-spacing
      local-theme-fixed-pitch-font tm/fixed-pitch-font
      local-theme-variable-pitch-font tm/variable-pitch-font
      local-theme-serif-font tm/serif-font)

(load-file (concat user-emacs-directory "lisp/init-local-theme.el"))

(use-package ligature
  :straight
  (:host github :repo "mickeynp/ligature.el")
  :hook
  (after-init
   . (lambda ()
       (eval-after-make-graphic-frame
        "configure-ligature-settings"
        ;; Enable the www ligature in every possible major mode
        (ligature-set-ligatures 't '("www"))
        ;; Enable ligatures in programming modes
        (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                             ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                             "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                             "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                             "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                             "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                             "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                             "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                             "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                             "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
        (global-ligature-mode 't)))))

(provide 'init-fonts)
;;; init-fonts.el ends here
