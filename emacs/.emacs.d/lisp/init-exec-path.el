;;; init-exec-path.el --- Initialize $PATH -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; Ensure $PATH is properly initialized when Emacs starts.
;;
;;; Code:

(defvar tm/environment-variables nil
  "A list of strings representing environment variables.")

(use-package exec-path-from-shell
  :demand t
  :config
  (setq tm/environment-variables '("REPODIR"
                                   "RUBYLIB"
                                   "RUBYBINDIR"
                                   "PUPPETDIR"
                                   "PATH"))
  (dolist (env-var tm/environment-variables)
    (add-to-list 'exec-path-from-shell-variables env-var))
  ;; https://github.com/purcell/exec-path-from-shell/issues/87
  (setq exec-path-from-shell-arguments '("-l"))
  ;; https://github.com/syl20bnr/spacemacs/issues/4755
  (setq explicit-shell-file-name "/bin/bash")
  (setq shell-file-name "bash"))

(use-package keychain-environment
  :init
  (keychain-refresh-environment))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
