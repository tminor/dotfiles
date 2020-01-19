;;; init-dired.el --- Install and configure dired and related packages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures dired and related packages.
;;
;;; Code:

;; (use-package dired+
;;   :init
;;   (with-eval-after-load "dired" '(require 'dired+)))

(use-package dired-collapse)

(use-package dired-subtree)

(use-package diredfl)

(use-package dired-rainbow
  :after dired
  :demand t
  :config
  (dired-rainbow-define-chmod executable-unix "#0F0" "-[rw-]+x.*")
  (dired-rainbow-define
   immediate
   "#FEDA98"
   "\\(?:\\(?:R\\(?:EADME\\|eadme\\)\\|readme\\)\\)\\(?:[^ ]*?\\)?\\|Makefile\\|Cargo\\.toml\\|SConstruct\\|CMakeLists\\.txt\\|build\\.gradle\\|Rakefile\\|Gruntfile\\.js\\|Gruntfile\\.coffee")
  (dired-rainbow-define
   image
   "#3EDAD4"
   ("png" "jpeg" "jpg" "gif" "bmp" "tiff" "tif"
    "ppm" "pgm" "pbm" "pnm" "webp" "raw" "arw"
    "svg" "stl" "eps" "dvi" "ps" "cbr"
    "cbz" "xpm" "ico" "cr2" "orf" "nef"))
  (dired-rainbow-define
   video
   "#742FD1"
   ("avi" "flv" "m2v" "mkv" "mov" "mp4" "mpeg"
    "mpg" "ogm" "ogv" "vob" "wmv" "webm" "m2ts"
    "ts"))
  (dired-rainbow-define
   music
   "#742FD1"
   ("aac" "m4a" "mp3" "ogg" "wma" "mka" "opus"))
  (dired-rainbow-define
   lossless-music
   "#A044A0"
   ("alac" "ape" "flac" "wav"))
  (dired-rainbow-define
   crypto
   "#DD0B53"
   ("asc" "enc" "gpg" "pgp" "sig" "signature" "pfx" "p12"))
  (dired-rainbow-define
   document
   "#9A99E7"
   ("djvu" "doc" "docx" "dvi" "eml" "eps" "fotd"
    "odp" "odt" "pdf" "ppt" "pptx" "rtf"
    "xls" "xlsx"))
  (dired-rainbow-define
   compiled
   "#FFB5D8"
   ("class" "elc" "hi" "o" "pyc")))

(use-package dired-du
  :straight (:host github :repo "emacsmirror/dired-du")
  :config
  (setq dired-du-size-format t))

(provide 'init-dired)
;;; init-dired.el ends here
