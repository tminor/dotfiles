#! /usr/bin/env -S emacs -Q --script

(require 'org)
(require 'ob)
(require 'ob-scheme)
(require 'cl-lib)

(cl-letf ((geiser-scheme-implementation "guile")
          (org-src-preserve-indentation t)
          ((symbol-function 'message) #'ignore))
  (mapc (lambda (f)
          (if (file-name-absolute-p f)
              (org-babel-tangle-file f)
            (org-babel-tangle-file (concat default-directory f))))
        command-line-args-left))
