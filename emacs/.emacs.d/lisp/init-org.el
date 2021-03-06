;;; init-org.el --- Install and configure org and related packages -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file configures org and installs related packages.
;;
;;; Code:

(require 'epa-file)
(require 'dash)
(require 'autorevert)
(require 'cl-extra)
(require 'cl)

(use-package org)
(use-package org-contrib
  :straight (:host github :repo "emacsmirror/org-contrib"))

(use-package pdf-tools
  :general
  (:keymaps '(pdf-view-mode-map)
   :states '(normal motion)
   "gp" 'pdf-view-goto-page
   "go" 'pdf-occur)
  :hook
  (pdf-view-mode . (lambda () (pdf-tools-enable-minor-modes))))

;; (pdf-tools-install)

(use-package org-mime)

(use-package org-pdftools
  :straight
  (:host github :repo "fuxialexander/org-pdftools")
  :hook
  (org-load . org-pdftools-setup-link))

(use-package org-noter
  :straight
  (:host github :repo "weirdNox/org-noter")
  :config
  (setq org-noter-always-create-frame nil
        org-noter-doc-property-in-notes t
        org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil))

(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode))

(use-package origami
  :hook
  (org-agenda-mode . origami-mode))

(use-package org-super-agenda
  :after 'org-agenda
  :config
  (evil-set-initial-state 'org-super-agenda-mode 'motion))

(with-eval-after-load 'org-super-agenda
 (setq org-super-agenda-header-map
        (make-sparse-keymap)) )

(use-package org-web-tools)

(use-package org-pomodoro
  :general
  (:keymaps 'org-agenda-mode-map
   :states '(normal motion)
   "gp" 'org-pomodoro)
  (tm/leader-def
    :infix "t"
    :prefix-command 'tm/toggle-prefix-command
    :prefix-map 'tm/toggle-prefix-map
    "p" (lambda () (if org-pomodoro-play-sounds
		  (setq org-pomodoro-play-sounds nil)
		(setq org-pomodoro-play-sounds t))))
  :config
  ;; (advice-add 'org-pomodoro-notify
  ;;             :override (lambda ()
  ;;       		  (async-start
  ;;       		   (lambda (title message)
  ;;       		     "Send a desktop notification with TITLE and MESSAGE.
  ;; Use `notifications-notify' instead of `alert'."
  ;;       		     (notifications-notify :title title
  ;;       					   :body message))
  ;;       		   'ignore)))
  (setq org-pomodoro-play-sounds t
	org-pomodoro-format "🍅 %s"
	org-pomodoro-overtime-format "+%s"
	org-pomodoro-short-break-format "🍅 %s"
	org-pomodoro-long-break-format "🎉 %s"
        org-pomodoro-play-sounds nil)
  (defun org-pomodoro-start (&optional state)
    "Start the `org-pomodoro` timer.
The argument STATE is optional.  The default state is `:pomodoro`."
    (when org-pomodoro-timer (cancel-timer org-pomodoro-timer))
    (org-pomodoro-set (or state :pomodoro))
    (when (eq org-pomodoro-state :pomodoro)
      (org-pomodoro-maybe-play-sound :start)
      (run-hooks 'org-pomodoro-started-hook))
    (org-agenda-maybe-redo))
  (setq org-clock-clocked-in-display nil))

(use-package org-variable-pitch
  :diminish org-variable-pitch-minor-mode
  :hook
  (org-mode . org-variable-pitch-minor-mode)
  :custom
  (org-variable-pitch-fixed-font "FiraCode")
  :init
  (require 'org-indent))

(use-package org-journal
  :general
  (tm/leader-def
    :infix "j"
    :prefix-command 'tm/org-journal-prefix-command
    :prefix-map 'tm/org-journal-prefix-map
    "" '(:which-key "org-journal prefix" :ignore t)
    "c" 'org-journal-new-entry
    "l" 'org-journal-next-entry
    "h" 'org-journal-previous-entry)
  :custom
  (org-journal-dir "~/org/work/journal")
  (org-journal-file-type 'weekly)
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-enable-encryption t)
  (org-journal-encrypt-journal t))

(use-package org-sync
  :straight
  (:host github :repo "arbox/org-sync"))

(mapc 'load
      '("org-sync"
        "org-sync-bb"
        "org-sync-github"
        "org-sync-gitlab"))

;;; Function defs:
(defun tm/org-remove-inherited-local-tags ()
  "Remove local tags that can be inherited instead."
  (let* ((target-tags-local (org-get-tags-at nil 'local))
         ;; We have to remove the local tags otherwise they would not show up
         ;; as being inherited if they are present on parents---the local tag
         ;; would "override" the parent
         (target-tags-inherited
          (unwind-protect
              (progn
                (org-set-tags-to nil)
                (org-get-tags-at))
            (org-set-tags-to target-tags-local))))
    (-each target-tags-local
      (lambda (tag)
        (when (member tag target-tags-inherited)
          (org-toggle-tag tag 'off))))))

(defmacro tm/org-make-level-faces (level)
  "Generate commands and set vars for creating new `org-level-face's up to LEVEL."
  (let ((num 0))
    (while (< num (+ (string-to-number level) 1))
      (progn
        `(defface ,(intern (concat "org-level-"
                                   level))
           (org-compatible-face nil
             '((((class color) (min-colors 16) (background light))
                (:foreground "RosyBrown"))
               (((class color) (min-colors 16) (background dark))
                (:foreground "LightSalmon"))
               (((class color) (min-colors 8)) (:foreground "green"))))
           ,(format "Face used for level %s headlines." level)
           :group 'org-faces)
        (let ((face (intern (concat "org-level-" level))))
          (unless (member face
                          org-level-faces)
            (setq org-level-faces (append org-level-faces
                                          (list face))))))
      (setq num (+ num 1))))
  (setq org-n-level-faces
        (string-to-number level)))

; Defs for hashing and updating a heading's modification time.

(defun tm/getentryhash ()
  "Get the hash sum of the text in current entry, except :HASH:
d :MODIFIED: property texts."
  (save-excursion
    (let* ((full-str
            (buffer-substring-no-properties (point-min)
                                            (point-max)))
           (str-nohash
            (if (string-match "^ *:HASH:.+\n" full-str)
                (replace-match "" nil nil full-str)
              full-str))
           (str-nohash-nomod
            (if (string-match "^ *:MODIFIED:.+\n" str-nohash)
                (replace-match "" nil nil str-nohash)
              str-nohash))
           (str-nohash-nomod-nopropbeg
            (if (string-match "^ *:PROPERTIES:\n" str-nohash-nomod)
                (replace-match "" nil nil str-nohash-nomod)
              str-nohash-nomod))
           (str-nohash-nomod-nopropbeg-end
            (if (string-match "^ *:END:\n" str-nohash-nomod-nopropbeg)
                (replace-match "" nil nil str-nohash-nomod-nopropbeg)
              str-nohash-nomod-nopropbeg)))
      (secure-hash 'md5 str-nohash-nomod-nopropbeg-end))))

(defun tm/update-modification-time ()
  "Set the :MODIFIED: property of the current entry to NOW and date :HASH: property."
  (save-excursion
    (save-restriction
      (let* ((beg
              (progn
                (org-back-to-heading)
                (point)))
             (end
              (progn
                (outline-next-heading)
                (- (point) 1))))
        (narrow-to-region beg end)
        (org-set-property "HASH"
                          (format "%s" (tm/getentryhash)))
        (org-set-property "MODIFIED"
                          (format-time-string "[%Y-%m-%d %a %H:%M]"))))))

(defun tm/skip-nonmodified ()
  "Skip headings whose :MODIFIED: properties are unchanged."
  (let ((next-headline
         (save-excursion
           (or (outline-next-heading)
               (point-max)))))
    (save-restriction
      (let* ((beg
              (progn
                (org-back-to-heading)
                (point)))
             (end
              (progn
                (outline-next-heading)
                (- (point) 1))))
        (narrow-to-region beg end)
        (if (string= (org-entry-get (point) "HASH" nil)
                     (format "%s" (tm/getentryhash)))
            next-headline
          nil)))))

;; Functions for agenda navigation.
;; TODO: Debug functions.
;;
;; Source: https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html

(defun tm/org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (tm/org-agenda-goto-header))

(defun tm/org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (tm/org-agenda-goto-header t))

(defun tm/org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

;; Defs for reverting buffers automatically if they change on disk.
;;
;; Source: https://stackoverflow.com/a/13946304

(defvar tm/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions.

e `auto-mode-alist' All elements of this alist are
ecked, meaning you can enable multiple minor modes for the same
gexp.")

(defun tm/enable-minor-mode-based-on-extension ()
  "Check file name against `tm/auto-minor-mode-alist' to enable minor modes.
e checking happens for all pairs in tm/auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name (file-name-sans-versions buffer-file-name))
          (remote-id (file-remote-p buffer-file-name))
          (case-fold-search auto-mode-case-fold)
          (alist tm/auto-minor-mode-alist))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(setq auto-revert-verbose nil)

;; Source: https://emacs.stackexchange.com/a/26369
(defun tm/org-cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by PROP.

 a is before b, return -1. If a is after b, return 1. If they
e equal return t."
  (lexical-let ((prop prop))
    #'(lambda (a b)
        (let* ((a-pos (get-text-property 0 'org-marker a))
               (b-pos (get-text-property 0 'org-marker b))
               (a-date (or (org-entry-get a-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (b-date (or (org-entry-get b-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (cmp (compare-strings a-date nil nil b-date nil nil)))
          (if (eq cmp t) nil (signum cmp))))))

;; Defs for `org-capture'

(defun transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c)
               (if (equal c ?[) ?\( (if (equal c ?]) ?\) c)))
           string-to-transform)))

(setq org-base-directory "~/org/")

;; Adds functions, advice, etc. for killing a new frame if one has
;; been created by org-capture browser extension.
;;
;; https://github.com/sprig/org-capture-extension#example-closins-the-frame-after-a-capture
(defvar tm/delete-frame-after-capture 0
  "Whether to delete the last frame after the current capture.")

;; TODO: Is this needed?
(defun tm/delete-frame-if-neccessary (&rest r)
  (cond
   ((= tm/delete-frame-after-capture 0) nil)
   ((> tm/delete-frame-after-capture 1)
    (setq tm/delete-frame-after-capture (- tm/delete-frame-after-capture 1)))
   (t
    (setq tm/delete-frame-after-capture 0)
    (delete-frame))))
(advice-add 'org-capture-finalize
            :after 'tm/delete-frame-if-neccessary)
(advice-add 'org-capture-kill
            :after 'tm/delete-frame-if-neccessary)
(advice-add 'org-capture-refile
            :after 'tm/delete-frame-if-neccessary)

(require 'org-id)

(defun tm/org-add-ids-to-all ()
  "Add IDs to all headings in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (org-id-get-create))))

(defun tm/org-super-agenda-origami-fold-default ()
  "Fold certain groups by default in `org-super-agenda' buffer."
  (forward-line 2)
  (unless (eobp)
    (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
             while (origami-forward-fold-same-level (current-buffer) (point)))
    (--each tm/org-super-agenda-auto-show-groups
      (goto-char (point-min))
      (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
        (origami-show-node (current-buffer) (point))))))

(setq tm/org-super-agenda-auto-show-groups '("Other items"
                                             "Sprint"))

;; Found here: https://emacs.stackexchange.com/a/30449
(defun tm/last-weekday-of-month-p (date)
  "Verify whether DATE is the last weekday in the current month."
  (let* ((day-of-week (calendar-day-of-week date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-month-day (calendar-last-day-of-month month year))
         (month-day (cadr date)))
    (or
     ;; it's the last day of the month & it is a weekday
     (and (eq month-day last-month-day)
          (memq day-of-week '(1 2 3 4 5)))
     ;; it's a friday, and it's the last-but-one or last-but-two day of
     ;; the month
     (and (eq day-of-week 5)
          (or (eq month-day (1- last-month-day))
              (eq month-day (1- (1- last-month-day))))))))

(defun tm/org-agenda-work-week (&optional arg)
  "Opens agenda for this week."
  (interactive)
  (org-agenda arg "ww"))
(defun tm/org-agenda-work-today (&optional arg)
  "Opens agenda for today."
  (interactive)
  (org-agenda arg "wt"))
(defun tm/org-agenda-both-today (&optional arg)
  "Opens today's agenda for both home and work."
  (interactive)
  (org-agenda arg "b"))

(defmacro tm/org-get-headings-command (fn-suffix target)
  "Generate a command for capturing to TARGET."
  `(defun ,(intern (concat "tm/org-get-headings-"
                           (symbol-name fn-suffix))) ()
     ,(format "Return `point' for heading in %S" target)
     (interactive)
     (let* ((file (concat (if (string= ,target
                                       "main.org")
                              user-emacs-directory
                            org-base-directory)
                          ,target))
            (buf (find-buffer-visiting file)))
       (unless buf
         (find-file file))
       (with-current-buffer
           buf
         ;; Gets headings from TARGET and fontifies them before collecting
         ;; them in `heading-point-alist', each cons cell of which reprents a
         ;; heading (with text properties) pointing at the value for that
         ;; heading's point.  `heading-point-alist' is passed to
         ;; `completing-read' read, ultimately calling `goto-char' against the
         ;; point from the chosen cons cell.
         (let* ((heading-point-alist '())
                (headings
                 (org-map-entries
                  (lambda ()
                    (cl-pushnew `(,(save-excursion
                                     (org-format-outline-path
                                      (org-get-outline-path t)))
                                  . ,(goto-char (point)))
                                heading-point-alist
                                :test #'equal)))))
           (goto-char (cdr (assoc
                            (completing-read "File under: "
                                             heading-point-alist)
                            heading-point-alist))))))))

(defun tm/toggle-agenda ()
  "Toggle `org-agenda' in side window."
  (interactive)
  (let ((buffer (get-buffer "*Org Agenda*")))
    (if buffer
	(if (not (get-buffer-window-list buffer))
	    (display-buffer-in-side-window buffer
					   '((display-buffer-reuse-window display-buffer-in-side-window)
					     (direction . rightmost)
					     (side . right)
					     (window-width . 100)))
	  (window-toggle-side-windows))
      (tm/org-agenda-both-today))))

(org-link-set-parameters "helpful"
                         :follow #'helpful-symbol
                         :store #'tm/org-helpful-store-link)

(defun tm/org-helpful-store-link ()
  "Store a link to a helpful buffer."
  (when (memq major-mode '(helpful-mode))
    (let* (desc link)
      (org-store-link-props :type "helpful")
      (setq desc (buffer-name))
      (setq link (concat "helpful:" (symbol-name helpful--sym)))
      (org-add-link-props :link link :description desc)
      link)))

(defun tm/yank-org-link (text)
  "Yank a link from TEXT.

TEXT represents a formatted Org link."
  (if (derived-mode-p 'org-mode)
      (insert text)
    (string-match org-bracket-link-regexp text)
    (insert (substring text (match-beginning 1) (match-end 1)))))

(defun tm/archive-in-subtree (orig-fun &rest args)
  "Called by `advice-add' with ORIG-FUN/ARGS to archive in subheadings of archive.org."
  (interactive)
  (letf* ((heading (save-excursion
                     (widen)
                     (while (not (= 1 (org-up-heading-safe))))
                     (org-heading-components)))
          (title (nth 4 heading))
          (heading-stars (let ((i (nth 0 heading))
                               (num 0)
                               (stars ()))
                           (while (< num (nth 0 heading))
                             (setq stars (cons "*" stars))
                             (setq num (1+ num)))
                           (apply #'concat stars)))
          (org-archive-location
           (format "%s/archive.org::%s %s" org-directory heading-stars title))
          (org-archive--compute-location
           (lambda (location) (unless (string-match "::" location)
                           (error "Invalid archive location: %S" location))
             (let ((current-file (buffer-file-name (buffer-base-buffer)))
	           (file-fmt (substring location 0 (match-beginning 0)))
	           (heading-fmt (substring location (match-end 0))))
               (cons
                ;; File part.
                (if (org-string-nw-p file-fmt)
	            (expand-file-name
	             (format file-fmt (file-name-nondirectory current-file)))
                  current-file)
                ;; Heading part.
                (format heading-fmt (file-name-nondirectory current-file)))))))
    (funcall-interactively orig-fun)))

(advice-add 'org-archive-subtree :around #'tm/archive-in-subtree)

(defun tm/todo-to-int (todo)
  "Convert TODO to an integer representation."
  (first (-non-nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (-map (lambda (x) (first (split-string  x "(")))
                                 (rest keywords))))
                      (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                  org-todo-keywords))))

(defun tm/org-sort-key ()
  "Produce a string representing priotity."
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (tm/todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)))

(defun tm/org-sort-entries ()
  "Sort org entries by TODO keyword and priority."
  (interactive)
  (org-sort-entries nil ?f #'tm/org-sort-key))

(defun tm/org-retrieve-url-from-point ()
  "Retrieve a URL from an Org link's text properties."
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (tm/yank-org-link)) text)
      (kill-new text))))

;; TODO: The following three functions are (supposedly) meant to find
;; and copy a URL from a formatted Org link. In practice, this doesn't
;; seem to work and could use some debugging. Preferably, a DWIM
;; version would work without an active region.
(defun tm/smarter-kill-ring-save ()
  "Kill text in region.

If an Org link is detected in the region's text properties, this
command will kill the referenced URL.

Source:
https://emacs.stackexchange.com/a/3990"
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'tm/org-retrieve-url-from-point))))

(defun tm/org-add-log-note (&optional _purpose)
  "Pop up a window for taking a note, and add this note later.
The default definition fails in a side window due to a call to
`delete-other-windows'."
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-setup nil)
  (setq org-log-note-window-configuration (current-window-configuration))
  (move-marker org-log-note-return-to (point))
  (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
  (goto-char org-log-note-marker)
  (let* ((buf (get-buffer-create "*Org Note*")))
    (with-current-buffer buf
      (erase-buffer))
    (switch-to-buffer-other-window buf))
  (if (memq org-log-note-how '(time state))
      (org-store-log-note)
    (let ((org-inhibit-startup t)) (org-mode))
    (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
		    (cl-case org-log-note-purpose
                      (clock-out "stopped clock")
                      (done  "closed todo item")
                      (reschedule "rescheduling")
                      (delschedule "no longer scheduled")
                      (redeadline "changing deadline")
                      (deldeadline "removing deadline")
                      (refile "refiling")
                      (note "this entry")
                      (state
                       (format "state change from \"%s\" to \"%s\""
                               (or org-log-note-previous-state "")
                               (or org-log-note-state "")))
                      (t (error "This should not happen")))))
    (when org-log-note-extra (insert org-log-note-extra))
    (setq-local org-finish-function 'org-store-log-note)
    (run-hooks 'org-log-buffer-setup-hook)))

(advice-add 'org-add-log-note
            :override #'tm/org-add-log-note)

;; Configure keybindings:
(general-define-key :keymaps 'org-mode-map
                    :states '(normal)
                    "<S-iso-lefttab>" 'org-global-cycle
                    "+" 'org-add-note
                    "U" 'org-toggle-narrow-to-subtree
                    "gj" 'outline-next-heading
                    "gk" 'outline-previous-heading
                    "gK" 'outline-up-heading
                    "gJ" 'outline-forward-same-level
                    "gI" 'org-clock-in
                    "gO" 'org-clock-out
                    "gp" 'org-pomodoro
                    "J" 'org-clock-timestamps-down
                    "K" 'org-clock-timestamps-up)
(general-define-key :keymaps '(org-agenda-mode-map)
                    :states '(normal motion)
                    "gJ" 'tm/org-agenda-next-header
                    "gK" 'tm/org-agenda-previous-header
                    "gH" 'org-habit-toggle-display-in-agenda
                    "gT" 'org-agenda-entry-text-mode
                    "ct" 'counsel-org-tag-agenda
                    "st" 'org-agenda-filter-by-tag
                    "<backtab>" 'origami-toggle-node)
(tm/leader-def
  :infix "o"
  :prefix-command 'tm/org-prefix-command
  :prefix-map 'tm/org-prefix-map
  "" '(:which-key "org prefix" :ignore t)
  "a" 'org-agenda
  "c" 'org-capture
  "b" 'org-switchb
  "h" 'org-recent-headings-ivy
  "j" 'tm/org-journal-prefix-command
  "l" 'org-store-link
  "L" 'org-insert-link
  "q" 'org-set-tags-command
  "1" 'tm/org-agenda-both-today
  "2" 'tm/org-agenda-work-today
  "3" 'tm/org-agenda-work-week)

;; Setup EPA/GPG encryption.
;; (setq epa-pinentry-mode 'loopback)
;; (epa-file-enable)
;; (setq org-crypt-key nil)

;; `org-mode' hooks:
(add-hook 'evil-insert-state-exit-hook
          #'(lambda ()
              (if (and (string= major-mode
                                "org-mode")
                       (not (string= (buffer-name) "*Org Note*")))
                  (save-buffer))))
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook
          #'(lambda () (dolist (symbol '(("#+TITLE:" . ?\u22ee)
                                    ("#+begin_src" . ?\u03bb)
                                    ("#+BEGIN_SRC" . ?\u03bb)
                                    ("#+end_src" . ?\u224b)
                                    ("#+END_SRC" . ?\u224b)
                                    ("#+begin_quote" . ?\u201c)
                                    ("#+BEGIN_QUOTE" . ?\u201c)
                                    ("#+end_quote" . ?\u201d)
                                    ("#+END_QUOTE" . ?\u201d)))
                    (cl-pushnew symbol prettify-symbols-alist
                                :test #'equal))))
(add-hook 'before-save-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-map-entries #'tm/update-modification-time
                               nil
                               'file
                               #'tm/skip-nonmodified))))
(add-hook 'org-mode-hook #'prettify-symbols-mode)


;; General Org settings:
(add-to-list 'org-file-apps
             '("\\.pdf\\'" . (lambda (file link)
                               (org-pdftools-open link))))
;; (tm/org-make-level-faces "16")
;; Makes more outline path faces available.
(setq org-n-level-faces 15)
(setq org-startup-indented t
      org-src-fontify-natively t
      org-todo-keywords '((sequence "TODO(t!)"
                                    "WAIT(w!)"
                                    "PROJ(p!)"
                                    "|"
                                    "DONE(d!)"
                                    "CANC(c!)"
                                    "NOTE(n!)"))
      org-log-into-drawer t
      org-use-fast-todo-selection 'expert
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-modules '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info
                             org-irc org-mhe org-rmail org-w3m org-habit-plus
                             org-id org-drill org-protocol org-man org-tempo)
      org-todo-keyword-faces '(("TODO" :background "#DD0B53" :foreground "#EAEAEA"
                                :weight bold :box (:line-width -1
                                                   :style released-button))
                               ("DONE" :background "#0F0" :foreground "#000"
                                :weight bold :box (:line-width -1
                                                   :style released-button))
                               ("WAIT" :background "#FEDA98" :foreground "#000"
                                :weight bold :box (:line-width -1
                                                   :style released-button))
                               ("PROJ" :background "#742FD1" :foreground "#EAEAEA"
                                :weight bold :box (:line-width -1
                                                   :style released-button))
                               ("CANC" :background "#F94FA0"
                                :foreground "#EAEAEA" :weight bold
                                :box (:line-width -1
                                      :style released-button))
                               ("MEETING" :background "#3EDAD4" :foreground "#000"
                                :weight bold :box (:line-width -1
                                                   :style released-button)))
      org-priority-faces '((?A . (:foreground "#DD0B53" :weight bold))
                           (?B . (:foreground "#FEDA98"))
                           (?C . (:foreground "#0F0")))
      org-log-done 'time
      org-stuck-projects '("-FUTURE/PROJ"
			   ("TODO")
			   nil))

;; `org-capture' settings:
(setq org-refile-targets
      `((org-agenda-files . (:maxlevel . 10))
        (,(concat user-emacs-directory "main.org") . (:maxlevel . 16)))
      ;; Disabling this setting enables ivy read completion.
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path 'file
      org-hide-emphasis-markers t)

;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 ()
                                (compose-region (match-beginning 1)
                                                (match-end 1) "\u2022"))))))

;; `org-agenda' settings:
(setq org-work-directory (concat org-base-directory "work/"))
(setq org-home-directory (concat org-base-directory "home/"))

(add-hook 'find-file-hook #'tm/enable-minor-mode-based-on-extension)
(add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)
(add-hook 'org-agenda-finalize-hook #'tm/org-super-agenda-origami-fold-default)

(with-eval-after-load 'org
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-clock-idle-time 5
      ;; calendar.org has gotten very large and I received an error
      ;; indicating this variable should be increased
      undo-outer-limit 25170000
      ;; Files that `org-agenda' uses to populate its commands/views
      org-agenda-files `(,(concat org-base-directory "capture.org")
                         ,(concat org-base-directory "todo.org")
                         ,(concat org-base-directory "calendar/"))
      ;; Don't show scheduled TODOs in `org-agenda'
      org-agenda-todo-ignore-scheduled t
      ;; Tell `org-agenda' to use `current-buffer' to avoid destroying
      ;; precious window layouts
      org-agenda-window-setup 'current-window
      ;; The following variables make agenda separators look nicer
      ;; ﹌
      ;; Setting to " " because it breaks origami
      org-agenda-block-separator nil
      ;; ✔
      org-habit-completed-glyph 10004
      ;; × 𝚇
      org-habit-today-glyph 120455
      ;; TODO: Define a function that changes the clock symbol based
      ;; on the current time.
      org-agenda-current-time-string (format "⁕⁕⁕⁕  %s  ⁕⁕⁕⁕"
                                             (propertize "🕖"
                                                         :foreground "#F94FA0"
                                                         :weight 'bold))
      org-habit-graph-column 45
      org-habit-show-habits-only-for-today nil
      org-agenda-skip-unavailable-files t
      org-id-link-to-org-use-id t)

(setq org-agenda-custom-commands
      `(("b" "Daily agenda for both work and home"
         ((agenda
           ""
           ((org-agenda-use-tag-inheritance nil)
            (org-agenda-todo-ignore-scheduled 'past)
            (org-agenda-files '("~/org/todo.org" "~/org/calendar"))
            (org-agenda-time-grid (quote
                                   ((daily today remove-match)
                                    (300 600 900 1200 1500 1800 2100)
                                    "......" "----------------")))
            (org-agenda-span 'day)
            (org-agenda-overriding-header "")
            (org-super-agenda-groups
             '((:name "Trash"
                :discard (:tag "HABIT"))))
            (org-agenda-hide-tags-regexp
             (rx (or (and (not (in "H"))
                          (not (in "O"))
                          (not (in "M"))
                          (not (in "E")))
                     (and (not (in "W"))
                          (not (in "O"))
                          (not (in "R"))
                          (not (in "K"))))))))
	  (stuck
	   ""
	   ((org-agenda-use-tag-inheritance nil)
            (org-agenda-overriding-header "")
	    (org-super-agenda-groups
	     '((:name "Stuck projects"
		:todo "PROJ")))))
          (tags
           "+REFILE"
           ((org-agenda-use-tag-inheritance nil)
            (org-agenda-files '("~/org/capture.org"))
            (org-agenda-prefix-format
             ,(concat "    %5(org-entry-get nil \"MODIFIED\") "))
            (org-agenda-sorting-strategy '(effort-down))
            (org-agenda-cmp-user-defined (tm/org-cmp-date-property
                                          "MODIFIED"))
            (org-agenda-sorting-strategy '(user-defined-down))
            (org-agenda-overriding-header "")
            (org-agenda-hide-tags-regexp
             (rx (zero-or-more anything)))
            (org-overriding-columns-format
             (concat "%40ITEM(Task) "
                     "%TODO "
                     "%3PRIORITY "
                     "%17Effort(Estimated Effort){:} "
                     "%CLOCKSUM"))
            (org-super-agenda-groups
             '((:name "Needs refiling"
                :tag "REFILE"
                :order 0)))))
          (tags
           "/DONE|TODO|WAIT|CANC|PROJ"
           ((org-agenda-files '("~/org/todo.org"))
            (org-agenda-prefix-format
             ,(concat "    %5(org-entry-get nil \"MODIFIED\") %e "))
            (org-agenda-todo-ignore-scheduled t)
            (org-agenda-sorting-strategy '(effort-down))
            (org-agenda-cmp-user-defined (tm/org-cmp-date-property
                                          "MODIFIED"))
            (org-agenda-sorting-strategy '(user-defined-down))
            (org-agenda-overriding-header "")
            (org-overriding-columns-format
             (concat "%40ITEM(Task) "
                     "%TODO "
                     "%3PRIORITY "
                     "%17Effort(Estimated Effort){:} "
                     "%CLOCKSUM"))
            (org-super-agenda-groups
             '((:order-multi (0 (:name "Sprint (DOING)"
                                 :and (:property ("SPRINT" "t") :not (:scheduled t)
                                       :todo "DOIN"))
                                (:name "Sprint (TODO)"
                                 :and (:property ("SPRINT" "t") :not (:scheduled t)
                                       :todo "TODO"))))
               (:name "High priority"
                :order 1
                :and (:todo "TODO" :priority "A" :tag "TASKS"
                      :not (:scheduled t)))
               (:name "Medium priority"
                :order 2
                :and (:todo "TODO" :priority "B" :tag "TASKS"
                      :not (:scheduled t)))
               (:name "Low priority"
                :order 3
                :and (:todo "TODO" :priority "C" :tag "TASKS"
                      :not (:scheduled t)))
               (:name "Archive DONE tasks"
                :order 4
                :and (:todo ("DONE" "CANC") :not (:tag "PROJECTS"
                                                  :tag "FUTURE")))
               (:name "Projects"
                :order 5
                :and (:todo "PROJ" :not (:tag "FUTURE")))
               (:name "Attic: tasks for the Future™"
                :order 6
                :tag "FUTURE")))))))))

(with-eval-after-load 'f
  (dolist (file org-agenda-files nil)
    (if (f-file-p file)
        (add-to-list 'tm/auto-minor-mode-alist
                     `(,file . auto-revert-mode))
      (if (f-dir-p file)
          (let ((files (f-files file
                                (lambda (path) (f-ext? path "org")))))
            (dolist (file files nil)
              (add-to-list 'tm/auto-minor-mode-alist
                           `(,file . auto-revert-mode))))))))

;; `org-capture' settings
(with-eval-after-load 'org
  (tm/org-get-headings-command todo "todo.org"))

(setq org-default-notes-file "/home/tminor/org/capture.org")

(with-eval-after-load 'org
  (setq
   org-capture-templates
   `(
     ("j" "Journal entry" entry
      ;; Target
      (function (lambda ()
                  (org-journal-new-entry t)
                  (goto-char (point-min))))
      ,(concat "* %(format-time-string org-journal-time-format)"
               "%^{Title}\n"
               ":LOGBOOK:\n"
               "- State \"TODO\"    from \"\"        %U\n"
               ":END:\n\n"
               "%i%?"))

     ("c" "Emacs configuration change" entry
      ;; Target
      (file+function "~/.emacs.d/main.org" tm/org-get-headings-config)
      ;; Template
      ,(concat "* TODO %?\n"
               ":LOGBOOK:\n"
               "- State \"TODO\"    from \"\"        %U\n"
               ":END:\n")
      :empty-lines 1)

     ("d" "Doing now (switch clocked task)" entry
      ;; Target
      (file+function "~/org/todo.org" tm/org-get-headings-todo)
      ;; Template
      ,(concat "* TODO %?\n"
               ":LOGBOOK:\n"
               "- State \"TODO\"    from \"\"        %U\n"
               ":END:\n")
      :empty-lines 1
      :clock-in t
      :clock-keep t)

     ("t" "Task" entry
      ;; Target
      (file+function "~/org/todo.org" tm/org-get-headings-todo)
      ;; Template
      ,(concat "* TODO %?\n"
               ":PROPERTIES:\n"
               ":NOTIFY: todo\n"
               ":END:\n"
               ":LOGBOOK:\n"
               "- State \"TODO\"    from \"\"        %U\n"
               ":END:\n")
      :empty-lines 1))))

(use-package org-ql)

(use-package calfw
  :general
  (:keymaps 'cfw:calendar-mode-map
   :states '(normal motion)
   "SPC" 'tm/prefix-command
   "RET" 'cfw:org-open-agenda-day
   "j" 'cfw:navi-next-week-command
   "k" 'cfw:navi-previous-week-command
   "l" 'cfw:navi-next-day-command
   "h" 'cfw:navi-previous-day-command
   "^" 'cfw:navi-goto-week-begin-command
   "$" 'cfw:navi-goto-week-end-command
   "gg" 'cfw:navi-goto-first-date-command
   "G" 'cfw:navi-goto-last-date-command
   "[" 'cfw:navi-previous-month-command
   "]" 'cfw:navi-next-month-command
   "." 'cfw:navi-goto-today-command
   "gd" 'cfw:navi-goto-date-command
   "J" 'cfw:navi-next-item-command
   "K" 'cfw:navi-prev-item-command
   "zm" 'cfw:change-view-month
   "zw" 'cfw:change-view-week
   "zt" 'cfw:change-view-two-weeks
   "zd" 'cfw:change-view-day
   "gr" 'cfw:refresh-calendar-buffer
   "C-RET" 'cfw:org-onclick)
  (tm/leader-def
    "c" 'cfw:open-org-calendar)
  :config
  (evil-set-initial-state 'cfw:calendar-mode 'motion)
  (setq cfw:org-face-agenda-item-foreground-color "#606A92"))

(use-package calfw-org)
(require 'calfw-org)

(use-package org-download)
(require 'org-download)

(use-package ace-link
  :general
  (:keymaps 'org-mode-map
   :states '(normal motion)
   "RET" 'ace-link-org)
  (:keymaps 'helpful-mode-map
   :states '(normal motion)
   "o" 'ace-link-help)
  (:keymaps 'woman-mode-map
   :states '(normal motion)
   "o" 'ace-link-woman))

(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
                                     '((python     . t)
                                       (ruby       . t)
                                       (shell      . t))))

(use-package org-roam
  :defer t
  :straight
  (:host github :repo "jethrokuan/org-roam"
   :branch "v2")
  :general
  (tm/leader-def
    :infix "or"
    :prefix-command 'tm/org-roam-prefix-command
    :prefix-map 'tm/org-roam-prefix-map
    "" '(:which-key "org-roam prefix" :ignore t)
    "o" 'org-roam
    "f" 'org-roam-find-file
    "g" 'org-roam-show-graph
    "i" 'org-roam-insert
    "d" 'org-roam-dailies-today)
  :config
  (setq org-roam-directory "~/src/blog/org-src/"
        org-roam-dailies-directory (concat org-roam-directory "dailies")
        org-roam-tag-sources '(prop all-directories)
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%(completing-read \"Choose note directory: \" (cons org-roam-directory (-filter #'f-directory-p (directory-files-recursively org-roam-directory (rx (zero-or-more any)) t))))/${slug}"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (org-roam-setup)
  :hook
  (org-mode . company-mode))

;; (use-package org-fc
;;   :straight
;;   (:host github :repo "l3kn/org-fc"))

;; (use-package notdeft
;;   :straight
;;   (:host github :repo "hasu/notdeft" :branch "xapian")
;;   :preface
;;   (defun tm/notdeft-bury-or-clear ()
;;     (interactive)
;;     (if notdeft-filter-string
;;         (notdeft-filter-clear)
;;       (bury-buffer)))
;;   :general
;;   (tm/leader-def
;;     "i" 'notdeft)
;;   (:keymaps 'notdeft-mode-map
;;    :states '(normal motion)
;;    "q" 'tm/notdeft-bury-or-clear
;;    "gr" 'notdeft-refresh
;;    "S" 'notdeft-filter)
;;   :config
;;   (setq notdeft-directory (concat org-directory "/notes")
;; 	notdeft-directories `(,(concat org-directory "/notes/"))
;;         notdeft-xapian-program (executable-find "notdeft-xapian")))

(use-package helm-org-rifle
  :config
  (defun tm/helm-org-rifle ()
    "Search `org-roam' files and `org-agenda' files."
    (interactive)
    (helm-org-rifle-directories `(,org-roam-directory
                                  ,org-roam-dailies-directory))))

(use-package counsel-org-clock
  :general
  (tm/leader-def
    :infix "oC"
    :prefix-command 'tm/counsel-prefix-command
    :prefix-map 'tm/counsel-prefix-map
    "g" 'counsel-org-clock-goto
    "h" 'counsel-org-clock-history))

(use-package org-wild-notifier
  :config
  (setq alert-default-style 'libnotify
        org-wild-notifier-keyword-whitelist '()
        org-wild-notifier-tags-whitelist '()
        org-wild-notifier-alert-time '(60 30 20 15 10 5 3 2 1))
  :hook
  (after-init . org-wild-notifier-mode))

(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///home/tminor/src/reveal.js"))

;; (use-package ox-hugo)

(provide 'init-org)
;;; init-org.el ends here
