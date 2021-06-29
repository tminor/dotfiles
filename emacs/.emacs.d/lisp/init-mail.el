;;; init-mail.el --- Install and configure mail packages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file installs and configures packages related to mail.
;;
;;; Code:

(require 'org-mime)
(require 'notifications)
(require 'smtpmail)
(require 'smtpmail-async)

(defun tm/notmuch-refresh-feed-buffer ()
  "Invoke `notmuch-refresh-this-buffer' specified buffer.

The buffer is silently refreshed, i.e. they are not forced to
be displayed."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (with-current-buffer buffer
        (when (and (memq buffer-mode '(notmuch-show-mode
                                       notmuch-tree-mode
                                       notmuch-search-mode
                                       notmuch-hello-mode))
                   (string= (buffer-name) "*notmuch-saved-search-feed*"))
          (notmuch-refresh-this-buffer))))))

(defun tm/set-feed-faces ()
  "Set `notmuch-search-line-faces' in feed buffer."
  (if (string= (buffer-name) "*notmuch-saved-search-feed*")
      (progn
        (setq notmuch-search-line-faces '(("git-commits" . '(:foreground "green"))
                                          ("nagios" . '(:foreground "red"))
                                          ("servicenow" . '(:foreground "yellow"))
                                          ("tenshi-uga" . '(:foreground "DodgerBlue1"))
                                          ("tenshi-db300" . '(:foreground "DodgerBlue1"))
                                          ("ssladmin" . '(:foreground "pink"))
                                          ("unread" . '(:background "gray15"))))
        (make-local-variable 'notmuch-search-line-faces))
    (setq notmuch-search-line-faces
          '(("unread" . notmuch-search-unread-face)
            ("flagged" . notmuch-search-flagged-face)))))

(defun tm/notmuch-notify (time-range)
  "Generate desktop notifcations for new mail received in TIME-RANGE.

This function utilizes `notmuch-call-notmuch-sexp' to fetch the
latest messages tagged inbox and send a notification to the
desktop.  TIME-RANGE should be the beginning of an Xapian date
range.  For example, an input of \"20mins\" translates to
\"date:20mins..\"."
  (let* ((latest-messages
          (apply #'notmuch-call-notmuch-sexp `("search"
					       "--format=sexp"
					       "--format-version=4"
					       "--sort=newest-first"
					       "tag:inbox"
					       ,(format "date:%s.." time-range))))
         (who)
         (when)
         (what)
         (mail-message)
         (body))
    (mapcar (lambda (mail-message)
              (setq when (plist-get mail-message :date_relative))
              (setq who (if (string-match-p "|"
                                            (plist-get mail-message :authors))
                            (progn (string-match "[[:space:],]\\{0,2\\}\\([a-zA-z[:space:]]+\\)|"
                                                 (plist-get mail-message
                                                            :authors))
                                   (match-string 1 (plist-get mail-message
                                                              :authors)))
                          (plist-get mail-message :authors)))
              (setq what (plist-get mail-message :subject))
              (setq body (format "<b>%s</b>\n<b>%s</b>\n\n%s" when who what))
              (async-start
               `(lambda ()
                  (require 'notifications)
                  (notifications-notify :title "New message(s)!\n"
                                        :body ,body
                                        :app-name "notmuchmail"))
               'ignore))
            latest-messages)))

(defun tm/notmuch-make-unread-string ()
  "Calculate number of unread inbox messages."
  (let* ((unread-mail (apply
                       #'notmuch-call-notmuch-sexp
                       '("search"
                         "--format=sexp"
                         "--format-version=4"
                         "--sort=newest-first"
                         "--output=messages"
                         "tag:inbox"
                         "and tag:unread"))))
    (propertize
     (format "%s %s"
	     " ✉"
	     (length unread-mail))
     'font-lock-face '(:foreground "#61dafb"))))

(defun tm/notmuch-unread ()
  "Show unread message count in mode-line."
  (setq tm/notmuch-inbox-state (tm/notmuch-make-unread-string))
  (or (memq 'tm/notmuch-inbox-state global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(tm/notmuch-inbox-state)))))

;; TODO: Add defgroup for notmuchfeed faces
(defface notmuch-feed-gluu
  '((t :foreground "purple2"
       :background "#6e6ed3d31110"
       :weight bold))
  "Highlight gluu messages.")
(defface notmuch-feed-nagios-problem
  '((t :foreground "#ffff00008080"
       :weight bold))
  "Highlight gluu messages.")
(defface notmuch-feed-nagios-recovery
  '((t :foreground "SpringGreen"
       :weight bold))
  "Highlight gluu messages.")
(defface notmuch-feed-inbox
  '((t :foreground "cyan"
       :weight bold))
  "Highlight gluu messages.")

(defun tm/highlight-notmuch-feed ()
  "Highlight interesting words in saved search."
  (let ((phrase-alist '(("idp-demo-prod" . notmuch-feed-gluu)
                        ("PROBLEM" . notmuch-feed-nagios-problem)
                        ("RECOVERY" . notmuch-feed-nagios-recovery)
                        ("inbox" . notmuch-feed-inbox))))
    (cl-loop for (key . value) in phrase-alist do
             (highlight-phrase key value))))

(use-package htmlize)
(use-package org-mime)
(use-package w3m
  :if (executable-find "w3m"))

(use-package notmuch
  :if (executable-find "notmuch")
  :init
  (require 'ol-notmuch)
  :general
  (tm/leader-def
    "/" 'notmuch)
  (:keymaps 'notmuch-show-mode-map
   :state '(normal motion)
   "gV" 'notmuch-show-view-raw-message)
  :config
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  ;; Tell Emacs how to send mail
  (setq sendmail-program "/usr/bin/msmtp")
  ;; Configure Fcc
  (setq notmuch-fcc-dirs "work/Sent +sent -new"
        notmuch-maildir-use-notmuch-insert t)
  ;; Other variables
  (setq notmuch-search-oldest-first nil
        message-kill-buffer-on-exit t
        notmuch-show-indent-messages-width 4
        notmuch-multipart/alternative-discouraged '("text/html" "text/plain")
        notmuch-archive-tags '("-inbox" "+archive"))
  ;; Refresh feed buffer at specified intervals if it's open
  ;; (run-with-timer 0 30 'tm/notmuch-refresh-feed-buffer)
  ;; Render HTML with w3m
  (setq mm-text-html-renderer 'w3m)
  ;; Shows saved searches in `notmuch-hello' even if they're empty.
  (setq notmuch-show-empty-saved-searches t)
  ;; Saved searches for notmuch-hello
  (setq notmuch-show-insert-text/plain-hook '(notmuch-wash-convert-inline-patch-to-part
                                              notmuch-wash-wrap-long-lines
                                              notmuch-wash-tidy-citations
                                              notmuch-wash-elide-blank-lines))
  (setq notmuch-saved-searches
        '((:name "inbox"
           :key "i"
           :query "tag:inbox"
           :count-query "tag:inbox and tag:unread")
          (:name "git issues"
	   :key "g"
           :query "tag:lists/854 and not tag:archive and date:1week..now"
           :count-query "tag:lists/854 and not tag:archive and date:1week..now and tag:unread")
          (:name "feed"
	   :key "f"
           :query "date:\"1hours..now\"")
          (:name "nagios-gluu"
           :query "tag:idp-demo-prod and tag:nagios and date:14days..today"
           :count-query "tag:idp-demo-prod and tag:nagios and date:14days..today and tag:unread")
          (:name "nagios"
	   :key "n"
           :query "tag:nagios not 'subject:\"/Project: gluu/\"' and date:3days..today"
           :count-query "tag:nagios not 'subject:\"/Project: gluu/\"' and date:3days..today and tag:unread")
          (:name "flappy vs capy"
           :query "subject:\"/gluu/\" and tag:nagios and date:3days..now")
          (:name "git messages"
           :query "tag:git and date:4days..today"
           :count-query "tag:git and date:4days..today and tag:unread")
          (:name "service now"
           :query "tag:servicenow and date:3days..today and not subject:\"/(Resolved|Closed)/\""
           :count-query "tag:servicenow and date:3days..today and not subject:\"/(Resolved|Closed)/\" and tag:unread")
          (:name "ssl"
	   :key "s"
           :query "tag:ssladmin and date:6days..today"
           :count-query "tag:ssladmin and date:6days..today and tag:unread")
          (:name "cron daemon"
	   :key "c"
           :query "date:4days..today and from:\"(Cron Daemon)\""
           :count-query "date:4days..today and from:\"(Cron Daemon)\" and tag:unread")
          (:name "tenshi"
	   :key "t"
           :query "date:2days..today and tag:tenshi"
           :count-query "date:2days..today and tag:tenshi and tag:unread")
          (:name "sent"
           :query "tag:sent"
           :key "s"))))

;;; SMTP settings are necessary for sending mail from EXWM via
;;; `async-smtpmail-send-it'.
(setq smtpmail-smtp-server "solaire.sss.usg.edu"
      smtpmail-smtp-service 1025
      user-mail-address "thomas.minor@usg.edu"
      message-send-mail-function 'async-smtpmail-send-it
      send-mail-function 'async-smtpmail-send-it
      smtpmail-smtp-user "thomas.minor@usg.edu"
      smtpmail-queue-dir "~/mail/work/work/queued-mail/"
      smtpmail-queue-mail nil)

(provide 'init-mail)
;;; init-mail.el ends here
