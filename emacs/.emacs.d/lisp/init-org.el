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
(require 'org)

;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;
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

;;; Defs for hashing and updating a heading's modification time.

(defun tm/getentryhash ()
  "Get the hash sum of the text in current entry, except :HASH:
and :MODIFIED: property texts."
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
  "Set the :MODIFIED: property of the current entry to NOW and
update :HASH: property."
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

See `auto-mode-alist' All elements of this alist are
checked, meaning you can enable multiple minor modes for the same
regexp.")

(defun tm/enable-minor-mode-based-on-extension ()
  "Check file name against `tm/auto-minor-mode-alist' to enable minor modes.
The checking happens for all pairs in tm/auto-minor-mode-alist"
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

If a is before b, return -1. If a is after b, return 1. If they
are equal return t."
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

;; Autofold agenda sections via `origami-mode'.
(defvar tm/org-super-agenda-auto-show-groups
    '("Unscheduled Tasks" "Archive DONE tasks" "Other items" "Schedule"))

(defun tm/org-super-agenda-origami-fold-default ()
  "Fold certain groups by default in `org-super-agenda' buffer."
  (forward-line 3)
  (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
	   while (origami-forward-fold-same-level (current-buffer) (point)))
  (--each tm/org-super-agenda-auto-show-groups
    (goto-char (point-min))
    (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
      (origami-show-node (current-buffer) (point)))))

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
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions end here
;;;;;;;;;;;;;;;;;;;;;;;;

(setq epa-pinentry-mode 'loopback)
(epa-file-enable)

(use-package org-mime)

(use-package pdf-tools)
(pdf-tools-install)

(use-package org-pdftools
  :straight
  (:host github :repo "fuxialexander/org-pdftools"))

(use-package org-noter
  :straight
  (:host github :repo "fuxialexander/org-noter"))

(use-package org-plus-contrib
  :init
  (setq org-crypt-key nil)
  :hook
  (after-init . (lambda ()
                  (if (try-completion "main.org"
                                      (mapcar #'buffer-name
                                              (buffer-list)))
                      (kill-buffer "main.org"))))
  (evil-insert-state-exit . (lambda ()
                              (if (string= major-mode
                                           "org-mode")
                                  (save-buffer))))
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (dolist (symbol '(("#+TITLE:" . ?\u22ee)
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
  (before-save-hook . (lambda ()
			(when (eq major-mode 'org-mode)
			  (org-map-entries #'tm/update-modification-time
					   nil
                                           'file
                                           #'tm/skip-nonmodified))))
  :general
  (:keymaps 'org-mode-map
   :states '(normal)
   "<S-iso-lefttab>" 'org-global-cycle
   "+" 'org-add-note)
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
  :config
  (require 'org-pdftools)
  (require 'org-noter)
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdftools-open link))))
  (setq org-tags-column -76)
  (tm/org-make-level-faces "16")
  ;; Makes more outline path faces available.
  (setq org-n-level-faces 15)
  ;; Sets org file base directory.
  (setq org-base-directory "~/org/")
  (setq org-work-directory (concat org-base-directory "work/"))
  (setq org-home-directory (concat org-base-directory "home/"))
  ;; Adds indentation based on heading level.
  (setq org-startup-indented t)
  ;; Enables syntax highlighting for code blocks.
  (setq org-src-fontify-natively t)
  ;; Define TODO/DONE-state keywords
  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "WAIT(w@)"
                    "PROJECT(p)"
                    "|"
                    "DONE(d@)"
                    "CANCELLED(c@)"
                    "NOTE(n!)")))
  ;; Enable state change logging in :LOGBOOK: drawer
  (setq org-log-into-drawer t)
  ;; Enable fast todo selection
  (setq org-use-fast-todo-selection t)
  ;; Make headlines look different for TODO/DONE states
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  ;; Org modules notes:
  ;;
  ;; - The values set below were probably copied from elsewhere, so I'm
  ;;   not sure what most of them do.
  ;; - Org Easy templates uses a different system in org 9.2.
  ;;   `org-tempo' provides the old functionality by loading it with
  ;;   `org-modules'.
  ;;
  (setq org-modules
        '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info
                   org-irc org-mhe org-rmail org-w3m org-habit-plus
                   org-id org-drill))
  ;;;;;;;;;;
  ;; Faces
  ;;;;;;;;;;
  ;;
  ;; Task state faces
  (setq org-todo-keyword-faces
        '(("TODO" :background "#DD0B53" :foreground "#EAEAEA"
           :weight bold :box (:line-width -1 :style released-button))
          ("DONE" :background "#0F0" :foreground "#000" :weight bold
           :box (:line-width -1 :style released-button))
          ("WAIT" :background "#FEDA98" :foreground "#000" :weight bold
           :box (:line-width -1 :style released-button))
          ("PROJECT" :background "#742FD1" :foreground "#EAEAEA" :weight bold
           :box (:line-width -1 :style released-button))
          ("CANCELLED" :background "#F94FA0" :foreground "#EAEAEA"
           :weight bold :box (:line-width -1 :style released-button))
          ("MEETING" :background "#3EDAD4" :foreground "#000"
           :weight bold :box (:line-width -1 :style released-button))))
  ;; Priority faces
  (setq org-priority-faces '((?A . (:foreground "#DD0B53" :weight bold))
                             (?B . (:foreground "#FEDA98"))
                             (?C . (:foreground "#0F0"))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; `org-refile' section
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; `org-refile-targets' controls completion candidates for
  ;; `org-refile'.  The following enables refiling to any file in
  ;; `org-agenda-files' and any heading 10 levels or less therein.
  (setq org-refile-targets
        `((org-agenda-files . (:maxlevel . 10))
          (,(concat user-emacs-directory "main.org") . (:maxlevel . 16))))
  ;; Setting the following to nil enables ivy read completions.
  (setq org-outline-path-complete-in-steps nil)
  ;; Includes file names in refile targets.
  (setq org-refile-use-outline-path 'file)
  ;; Beautifying org-mode in Emacs:
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 ()
                                  (compose-region (match-beginning 1)
                                                  (match-end 1) "\u2022")))))))

(use-package org-bullets
  :hook (org-mode . (lambda ()
		      (org-bullets-mode
			   1)))
  :config (setq org-bullets-bullet-list
		'("⑴" "⑵" "⑶" "⑷" "⑸" "⑹" "⑺" "⑻" "⑼" "⑽" "⑾"
		  "⑿" "⒀" "⒁" "⒂" "⒃")))

(use-package org-agenda
  :straight nil
  :general
  (:keymaps '(org-agenda-mode-map)
   :states '(normal motion)
   "J" 'tm/org-agenda-next-header
   "K" 'tm/org-agenda-previous-header
   "gH" 'org-habit-toggle-display-in-agenda
   "<backtab>" 'origami-toggle-node)
  :hook
  (find-file . tm/enable-minor-mode-based-on-extension)
  (org-agenda-finalize . tm/org-super-agenda-origami-fold-default)
  :init
  (require 'evil-org)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :config
  (require 'org-habit)
  (setq org-clock-idle-time 5
        ;; calendar.org has gotten very large and I received an error
        ;; indicating this variable should be increased
        undo-outer-limit 25170000
        ;; Enables journal files in agenda views.
        org-journal-enable-agenda-integration t
        ;; Files that `org-agenda' uses to populate its commands/views
        org-agenda-files `(,(concat org-base-directory "mobile-capture.org")
                           ,(concat org-base-directory "capture.org")
                           ,(concat org-base-directory "notes.org")
                           ,(concat org-base-directory "todo.org"))
        ;; Don't show scheduled TODOs in `org-agenda'
        org-agenda-todo-ignore-scheduled t
        ;; Tell `org-agenda' to use `current-buffer' to avoid destroying
        ;; precious window layouts
        org-agenda-window-setup 'current-window
        ;; The following variables make agenda separators look nicer
        ;; ﹌
        org-agenda-block-separator 65100
        ;; ✔
        org-habit-completed-glyph 10004
        ;; × 𝚇
        org-habit-today-glyph 120455
        org-agenda-current-time-string (format "⁕⁕⁕⁕  %s  ⁕⁕⁕⁕"
                                               (propertize "🕖"
                                                           :foreground "#F94FA0"
                                                           :weight 'bold))
        org-habit-graph-column 45
        org-habit-show-habits-only-for-today nil)
  ;; Activate `auto-revert-mode' for calendar.org
  (add-to-list 'tm/auto-minor-mode-alist
               '("homecal.org" . auto-revert-mode))
  (add-to-list 'tm/auto-minor-mode-alist
               '("workcal.org" . auto-revert-mode))
  (add-to-list 'tm/auto-minor-mode-alist
               '("oncallcal.org" . auto-revert-mode))
  (dolist (file org-agenda-files nil)
    (add-to-list 'tm/auto-minor-mode-alist
                 `(,file . auto-revert-mode)))
  ;; Create `org-agenda' custom commands
  (setq org-agenda-custom-commands
        `(("b" "Daily agenda for both work and home"
           ((agenda
             ""
             ((org-agenda-todo-ignore-scheduled 'past)
              (org-agenda-time-grid (quote
                                     ((daily today remove-match)
                                      (300 600 900 1200 1500 1800 2100)
                                      "......" "----------------")))
              (org-agenda-span 'day)
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
            (tags
             "+REFILE"
             ((org-agenda-files '("~/org/mobile-capture.org"))
              (org-agenda-prefix-format
               ,(concat "    %5(org-entry-get nil \"MODIFIED\") "))
              (org-agenda-sorting-strategy '(effort-down))
              (org-agenda-cmp-user-defined (tm/org-cmp-date-property
                                            "MODIFIED"))
              (org-agenda-sorting-strategy '(user-defined-down))
              (org-agenda-hide-tags-regexp
               (rx (zero-or-more anything)))))
            (tags
             "/DONE|TODO|WAIT|CANCELLED|PROJECT"
             ((org-agenda-files '("~/org/todo.org"))
              (org-agenda-prefix-format
               ,(concat "    %5(org-entry-get nil \"MODIFIED\") "))
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-sorting-strategy '(effort-down))
              (org-agenda-cmp-user-defined (tm/org-cmp-date-property
                                            "MODIFIED"))
              (org-agenda-sorting-strategy '(user-defined-down))
              (org-agenda-hide-tags-regexp
               (rx (zero-or-more anything)))
              (org-super-agenda-groups
               '((:name "Archive DONE tasks"
                  :todo ("DONE" "CANCELLED")
                  :order 5)
                 (:name "WAITing tasks"
                  :todo "WAIT"
                  :discard (:scheduled t)
                  :order 1)
                 (:name "Refileable items"
                  :tag "REFILE"
                  :discard (:scheduled t :tag "HOME")
                  :order 3)
                 (:name "Attic: Tasks for the Future"
                  :tag "FUTURE"
                  :order 4)
                 (:name "Projects"
                  :todo "PROJECT"
                  :order 2)
                 (:name "Unscheduled Tasks"
                  :todo "TODO"
                  :order 0)))))))
          ("w" . "Work agenda views")
          ("wt" "Today's work agenda"
           ((agenda
             ""
             ((org-agenda-todo-ignore-scheduled 'past)
              (org-agenda-time-grid (quote
                                     ((daily today remove-match)
                                      (300 600 900 1200 1500 1800 2100)
                                      "......" "----------------")))
              (org-agenda-span 'day)
              (org-super-agenda-groups
               '((:name "Trash"
                  :discard (:tag "HOME"))
                 (:name "Schedule"
                  :anything t)))
              (org-agenda-hide-tags-regexp
               (rx (zero-or-more anything)))))
            (tags
             "+REFILE"
             ((org-agenda-prefix-format
               ,(concat "    %5(org-entry-get nil \"MODIFIED\") "))
              (org-agenda-sorting-strategy '(effort-down))
              (org-agenda-cmp-user-defined (tm/org-cmp-date-property
                                            "MODIFIED"))
              (org-agenda-sorting-strategy '(user-defined-down))
              (org-agenda-hide-tags-regexp
               (rx (zero-or-more anything)))))
            (tags
             "/DONE|TODO|WAIT|CANCELLED|PROJECT"
             ((org-agenda-prefix-format
               ,(concat "    %5(org-entry-get nil \"MODIFIED\") "))
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-sorting-strategy '(effort-down))
              (org-agenda-cmp-user-defined (tm/org-cmp-date-property
                                            "MODIFIED"))
              (org-agenda-sorting-strategy '(user-defined-down))
              (org-agenda-hide-tags-regexp
               (rx (zero-or-more anything)))
              (org-super-agenda-groups
               '((:name "Trash"
                  :discard (:tag "HOME"))
                 (:name "Archive DONE tasks"
                  :todo ("DONE" "CANCELLED")
                  :order 5)
                 (:name "WAITing tasks"
                  :todo "WAIT"
                  :discard (:scheduled t)
                  :order 1)
                 (:name "Refileable items"
                  :tag "REFILE"
                  :discard (:scheduled t :tag "HOME")
                  :order 3)
                 (:name "Attic: Tasks for the Future"
                  :tag "FUTURE"
                  :order 4)
                 (:name "Projects"
                  :todo "PROJECT"
                  :order 2)
                 (:name "Unscheduled Tasks"
                  :todo "TODO"
                  :order 0)))))))
          ("h" . "Home agenda views")
          ("hw" "Weekly home agenda"
           ((agenda
             ""
             ((org-agenda-todo-ignore-scheduled 'past)
              (org-agenda-time-grid (quote
                                     ((daily today remove-match)
                                      (300 600 900 1200 1500 1800 2100)
                                      "......" "----------------")))
              (org-agenda-span 'week)))
            (todo
             "TODO"
             ((org-agenda-prefix-format
               ,(concat "    %5(org-entry-get nil \"MODIFIED\") "))
              (org-agenda-todo-ignore-scheduled t)
              (org-agenda-sorting-strategy '(effort-down))
              (org-agenda-hide-tags-regexp
               (rx (zero-or-more anything)))
              (org-agenda-tag-filter-preset '("-FUTURE"))))))
          ("ht" "Today's home agenda"
           ((agenda
             ""
             ((org-agenda-todo-ignore-scheduled 'past)
              (org-agenda-time-grid (quote
                                     ((daily today remove-match)
                                      (300 600 900 1200 1500 1800 2100)
                                      "......" "----------------")))
              (org-agenda-span 'day)
              (org-agenda-hide-tags-regexp
               (rx (zero-or-more anything)))))
            (todo
             "TODO"
             ((org-agenda-prefix-format
               ,(concat "    %5(org-entry-get nil \"MODIFIED\") "))
              (org-agenda-sorting-strategy '(effort-down))
              (org-agenda-cmp-user-defined (tm/org-cmp-date-property
                                            "MODIFIED"))
              (org-agenda-sorting-strategy '(user-defined-down))
              (org-agenda-hide-tags-regexp (rx
                                            (zero-or-more anything)))
              (org-super-agenda-groups
               '((:name "Refileable items."
                  :tag "REFILE"
                  :discard (:scheduled t :tag "WORK")
                  :order 2)
                 (:name "Attic: Tasks for the Future."
                  :tag "FUTURE"
                  :order 1)
                 (:name "Unscheduled Tasks."
                  :todo "TODO"
                  :order 0))))))))))

(use-package origami
    :hook
    (org-agenda-mode . origami-mode))

(use-package org-super-agenda
  :after 'org-agenda
  :config
  (evil-set-initial-state 'org-super-agenda-mode 'motion)
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map
        (progn (copy-keymap org-agenda-mode-map)
               (evil-org-agenda-set-keys)))
  (require 'origami))

(use-package org-capture
  :straight nil
  :after 'org
  :config
  (tm/org-get-headings-command todo "todo.org")
  (tm/org-get-headings-command notes "notes.org")
  (tm/org-get-headings-command config "main.org")

  ;; Set default capture file
  (setq org-default-notes-file "~/org/capture.org")

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
               ":LOGBOOK:\n"
               "- State \"TODO\"    from \"\"        %U\n"
               ":END:\n")
      :empty-lines 1)

     ("n" "Note" entry
      ;; Target
      (file+function "~/org/notes.org" tm/org-get-headings-notes)
      ;; Template
      ,(concat "* %?\n"
               ":LOGBOOK:\n"
               "- State \"\"        from \"\"        %U\n"
               ":END:\n")
      :empty-lines 1)

     ("l" "Link" entry
      ;; Target
      (file+function "~/org/notes.org" tm/org-get-headings-notes)
      ;; Template
      ,(concat "* %(format-time-string org-journal-time-format) "
               "%(org-web-tools--org-link-for-url) :link:\n\n"
               ":LOGBOOK:\n"
               "- State \"\"        from \"\"        %U\n"
               ":END:\n\n"
               "%?")
      :empty-lines 1)

     ;; `org-capture' browser extension templates
     ("p" "Link with quote for org-protocol" entry
      ;; Target
      (file+function "~/org/notes.org" tm/org-get-headings-notes)
      ;; Template
      ,(concat
        "* %^{Title}\n"
        ":LOGBOOK:\n"
        "- State \"\"        from \"\"        %U\n"
        ":END:\n\n"
        "Source: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
        "%(progn (setq tm/delete-frame-after-capture 2)")
      :empty-lines 1)

     ("L" "Link for org-protocol" entry
      ;; Target
      (file+function "~/org/notes.org" tm/org-get-headings-notes)
      ;; Template
      ,(concat
        "%(org-web-tools--url-as-readable-org \"%:link\")"
        "%?")
      :empty-lines 1))))

(use-package org-web-tools
  :straight t)

(use-package org-pomodoro
  :general
  (:keymaps 'org-agenda-mode-map
   :states '(normal motion)
   "gp" 'org-pomodoro)
  :config
  (advice-add 'org-pomodoro-notify
              :override (lambda (title message)
                          "Send a desktop notification with TITLE and MESSAGE.
Use `notifications-notify' instead of `alert'."
                          (notifications-notify :title title
                                                :body message))))

(use-package org-variable-pitch
  :diminish org-variable-pitch-minor-mode
  :hook
  (org-mode . org-variable-pitch-minor-mode)
  :custom
  (org-variable-pitch-fixed-font "DejaVu Sans mono")
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
  (org-journal-enable-agenda-integration t)
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-enable-encryption t)
  (org-journal-encrypt-journal t))

(use-package org-notify
  :straight nil
  :demand t
  :after 'org
  :config
  (setq user-mail-address "thomas.minor@usg.edu")
  (org-notify-add 'todo
                  '(:time "15m" :period "20s" :duration 10
                    :actions (-notify/window -message))
                  '(:time "5m" :period "20s" :duration 10
                    :actions (-notify/window -message))
                  '(:time "1m" :period "20s" :duration 10
                    :actions (-notify/window -message -ding))))

(provide 'init-org)
;;; init-org.el ends here
