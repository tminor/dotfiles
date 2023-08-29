;;; init-ide.el --- Configure IDE features -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures features such as syntax checking, linting, and
;; completion.
;;
;;; Code:

(use-package pos-tip)

(require 'pos-tip)

(use-package flycheck-pos-tip)

(use-package flycheck
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  :config
  ;; Config for RuboCop
  (setq flycheck-rubocoprc ".rubocop.yml")
  ;; Config for Puppet lint
  (setq flycheck-puppet-lint-rc "/home/tminor/.puppet-lint.rc")
  ;; Sets path to most recent puppet executable.
  (setq flycheck-puppet-parser-executable "/home/tminor/.gem/ruby/gems/puppet-6.5.0/bin/puppet")
  :hook
  (after-init . global-flycheck-mode))

(use-package company
  :init
  (setq company-idle-delay 0.02)
  :hook
  (prog-mode . company-mode)
  :custom-face
  (company-tooltip ((t (:family "Fira Mono"))))
  :config
  (setq company-tooltip-align-annotations t))

(use-package aggressive-indent
  :init
  (setq global-aggressive-indent-mode t))

(use-package lsp-mode
  :general
  (tm/leader-def
    :major-modes 'prog-mode
    "l" 'lsp-command-map)
  :hook
  (ruby-mode . lsp)
  (sh-mode . lsp)
  (go-mode . lsp)
  (java-mode . lsp)
  (crystal-mode . lsp)
  (python-mode . lsp)
  (puppet-mode . lsp)
  (js-mode . lsp)
  (css-mode . lsp)
  (rust-mode . lsp)
  :init
  (setq lsp-keymap-prefix nil)
  :config
  (setq lsp-prefer-flymake nil)
  (with-eval-after-load 'lsp-go
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection '("gopls"
                                                              "-remote=auto"
                                                              "-"))
                      :major-modes '(go-mode go-dot-mod-mode)
                      :language-id "go"
                      :priority 0
                      :remote? t
                      :server-id 'gopls
                      :completion-in-comments? t
                      :library-folders-fn #'lsp-go--library-default-directories
                      :after-open-fn (lambda ()
                                       ;; https://github.com/golang/tools/commit/b2d8b0336
                                       (setq-local lsp-completion-filter-on-incomplete nil)))))
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (defun tm/lsp-cancel-download ()
    (interactive)
    (let* ((clients (lsp--filter-clients #'lsp--client-download-in-progress?))
           (client (lsp--completing-read
                    "Which servers download would you like to cancel?"
                    clients
                    (-compose #'symbol-name #'lsp--client-server-id)
                    nil
                    t)))
      (setf (lsp--client-download-in-progress? client) nil))))

(use-package lsp-ui
  :general
  (tm/leader-def
    :infix "t"
    :prefix-command 'tm/toggle-prefix-command
    :prefix-map 'tm/toggle-prefix-map
    :major-modes 'prog-mode
    "" '(:which-key "toggle prefix" :ignore t)
    "d" 'lsp-ui-doc-mode)
  :hook
  (ruby-mode . lsp-ui-mode))

(use-package lsp-java)

(use-package company-lsp)

(use-package company-box
  :hook
  (company-mode . company-box-mode)
  ;; (prog-mode . company-box--set-mode)
  :config
  ;; (setq company-tooltip-minimum-width 60)
  )

(use-package lsp-treemacs)

(use-package highlight-thing
  :hook
  (prog-mode . highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0.1)
  (setq highlight-thing-prefer-active-region t))

(general-define-key
 :keymaps 'prog-mode-map
 :states '(normal motion)
 "g." 'xref-find-definitions)

(use-package drag-stuff
  :hook
  (prog-mode . (lambda () (drag-stuff-mode t)))
  :general
  (:keymaps '(prog-mode-map)
   :states '(normal motion)
   "J" 'drag-stuff-down
   "K" 'drag-stuff-up
   "H" 'drag-stuff-left
   "L" 'drag-stuff-right)
  (:keymaps '(text-mode-map)
   :states '(motion normal visual)
   "gDh" 'drag-stuff-left
   "gDl" 'drag-stuff-right
   "gDj" 'drag-stuff-down
   "gDk" 'drag-stuff-up))

(use-package emr)

(use-package string-inflection
  :preface
  (defun tm/string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ;; for emacs-lisp-mode
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ;; for python
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ;; for java
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     (t
      ;; default
      (string-inflection-all-cycle))))
  :general
  (:keymaps 'prog-mode-map
   :states '(motion  normal iedit)
   "U" 'tm/string-inflection-cycle-auto))

;; TODO: The recipe for this package needs to be tweaked; it fails to
;; load files in its "languages" directory.
;; (use-package lsp-sonarlint
;;   :straight
;;   (:host github :repo "emacs-lsp/lsp-sonarlint"
;;    :files (:defaults "languages/*/*" "server/*"))
;;   :init
;;   (require 'lsp-sonarlint-ruby)
;;   :config
;;   (setq lsp-sonarlint-ruby-enabled t
;;         lsp-sonarlint-server-path "/home/tminor/.emacs.d/straight/build/lsp-sonarlint/sonarlint-language-server.jar"))

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms))))

(use-package yasnippet
  :general
  (:keymaps '(yas-keymap yas/keymap)
   :states 'insert
   "C-<tab>" 'yas-next-field
   "C-<iso-lefttab>" 'yas-prev-field)
  :init
  (setq tm/enable-company-yas t)
  (defun tm/backend-with-yas (backend)
    (if (or (not tm/enable-company-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :hook
  (prog-mode . (lambda ()
                 (setq company-backends
                       (mapcar #'tm/backend-with-yas company-backends))))
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package polymode)
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(provide 'init-ide)
;;; init-ide.el ends here
