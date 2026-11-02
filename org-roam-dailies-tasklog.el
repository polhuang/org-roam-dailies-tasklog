;;;; org-roam-dailies-tasklog.el --- Automatically log task events to org-roam dailies -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0"))
;; Keywords: org-mode, org-roam, productivity, logging
;; URL: https://github.com/polhuang/org-roam-dailies-tasklog.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-id)
(require 'subr-x)
(require 'cl-lib)
(require 'org-roam)
(require 'org-roam-dailies)

;;; Customization

(defgroup org-roam-dailies-tasklog nil
  "Automatically log task events to org-roam daily notes."
  :group 'org-roam
  :prefix "org-roam-dailies-tasklog-")

(defcustom org-roam-dailies-tasklog-completion-states '("DONE")
  "List of TODO states considered as completion states."
  :type '(repeat string)
  :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-debug nil
  "Enable debug messages."
  :type 'boolean
  :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-time-format "%I:%M %p"
  "Time display format, as accepted by `format-time-string'.
Use %H:%M for 24-hour time.  Refresh displays after changing this option."
  :type 'string :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-compact-duration nil
  "When non-nil, display durations as 1h 30m instead of 1:30."
  :type 'boolean :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-time-column nil
  "Column at which timing starts, or nil for five spaces after the title.
Long titles always get at least five spaces."
  :type '(choice (const nil) integer) :group 'org-roam-dailies-tasklog)

(defvar org-roam-dailies-tasklog--updating nil
  "Non-nil while tasklog is updating a generated entry.")

;;; Internal functions

(defun org-roam-dailies-tasklog--debug (format-string &rest args)
  "Print debug message if debug mode is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when org-roam-dailies-tasklog-debug
    (apply #'message (concat "org-roam-dailies-tasklog: " format-string) args)))

(defun org-roam-dailies-tasklog--get-daily-note-path ()
  "Get the file path for today's org-roam daily note.
Creates the directory and file if they don't exist."
  (let* ((time (current-time))
         (file-name (format-time-string "%Y-%m-%d.org" time))
         (daily-dir (expand-file-name
                     (or org-roam-dailies-directory "daily")
                     org-roam-directory))
         (file-path (expand-file-name file-name daily-dir)))
    ;; Create directory if it doesn't exist
    (unless (file-exists-p daily-dir)
      (make-directory daily-dir t)
      (org-roam-dailies-tasklog--debug "Created dailies directory: %s" daily-dir))
    ;; Create file with basic structure if it doesn't exist
    (unless (file-exists-p file-path)
      (with-temp-file file-path
        (insert (format "#+title: %s\n\n"
			(format-time-string "%Y-%m-%d" time))))
      (org-roam-dailies-tasklog--debug "Created daily note: %s" file-path))
    file-path))

(defun org-roam-dailies-tasklog--get-subtree-content ()
  "Get the current heading and its body, excluding child headings.
Returns heading text, TODO state, and subtree content as an alist."
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading (org-get-heading t t t t))  ; no tags, todo, priority, comment
           (todo-state (org-get-todo-state))
           (content-start (progn (forward-line 1) (point)))
           (content-end (save-excursion
                          (outline-next-heading)
                          (point)))
           (content (string-trim (buffer-substring-no-properties content-start content-end))))
      (list (cons 'heading heading)
            (cons 'todo-state todo-state)
            (cons 'content content)))))

(defun org-roam-dailies-tasklog--get-latest-clock-entry (content)
  "Extract the LATEST (newest) clock entry line from CONTENT's LOGBOOK.
In org-mode, the most recent clock entry is at the TOP of the LOGBOOK.
Returns the clock line string or nil if no clock data found."
  (when (and content (string-match ":LOGBOOK:" content))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (when (re-search-forward ":LOGBOOK:" nil t)
        (let ((logbook-end (when (save-excursion
                                   (re-search-forward ":END:" nil t))
                             (match-beginning 0))))
          (when logbook-end
            ;; Find the FIRST clock entry after :LOGBOOK: (this is the newest)
            (when (re-search-forward "^CLOCK: \\[.*" logbook-end t)
              (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))))))))

(defun org-roam-dailies-tasklog--parse-clock-times (clock-line)
  "Parse CLOCK-LINE and extract start time, end time, and duration.
Returns (START-TS END-TS DURATION) or nil if parsing fails.
START-TS and END-TS are timestamp strings, DURATION is formatted like \"1:30\"."
  (when clock-line
    (cond
     ;; Complete clock entry: CLOCK: [START]--[END] => DURATION
     ((string-match "CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\] =>[ \t]+\\([0-9]+:[0-9]+\\)" clock-line)
      (list (match-string 1 clock-line)
            (match-string 2 clock-line)
            (match-string 3 clock-line)))
     ;; Incomplete clock entry (still running): CLOCK: [START]
     ((string-match "CLOCK: \\[\\([^]]+\\)\\]$" clock-line)
      (list (match-string 1 clock-line)
            (format-time-string "%Y-%m-%d %a %H:%M")
            "0:00"))
     (t nil))))

(defun org-roam-dailies-tasklog--format-time (timestamp)
  "Format Org TIMESTAMP using the configured time format."
  (when timestamp
    (condition-case nil
        (format-time-string org-roam-dailies-tasklog-time-format
                            (org-time-string-to-time timestamp))
      (error nil))))

(defun org-roam-dailies-tasklog--duration-label (duration)
  "Format DURATION according to the display preference."
  (if (and org-roam-dailies-tasklog-compact-duration
           (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" duration))
      (let ((hours (string-to-number (match-string 1 duration)))
            (minutes (string-to-number (match-string 2 duration))))
        (string-join (delq nil (list (when (> hours 0) (format "%dh" hours))
                                     (when (or (> minutes 0) (= hours 0))
                                       (format "%dm" minutes)))) " "))
    duration))

;;; Timing display

(defvar-local org-roam-dailies-tasklog--time-overlays nil
  "Timing overlays owned by tasklog in this buffer.")

(defun org-roam-dailies-tasklog--clear-time-overlays ()
  "Remove this buffer's tasklog timing overlays."
  (mapc #'delete-overlay org-roam-dailies-tasklog--time-overlays)
  (setq org-roam-dailies-tasklog--time-overlays nil))

(defun org-roam-dailies-tasklog--daily-buffer-p ()
  "Return non-nil if this buffer visits a dated org-roam daily note."
  (and (derived-mode-p 'org-mode)
       buffer-file-name
       (boundp 'org-roam-directory)
       (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\'"
                       (file-name-nondirectory buffer-file-name))
       (equal (file-name-as-directory
               (expand-file-name (file-name-directory buffer-file-name)))
              (file-name-as-directory
               (expand-file-name
                (or (and (boundp 'org-roam-dailies-directory)
                         org-roam-dailies-directory)
                    "daily")
                org-roam-directory)))))

(defun org-roam-dailies-tasklog--refresh-time-overlays (&optional beg end _old-length)
  "Refresh timing displays in the changed region BEG to END, or everywhere."
  (unless (and beg (org-roam-dailies-tasklog--daily-buffer-p))
    (org-roam-dailies-tasklog--clear-time-overlays))
  (when (and org-roam-dailies-tasklog-mode
             (org-roam-dailies-tasklog--daily-buffer-p))
    (save-excursion
      (save-restriction
        (widen)
        (save-match-data
          (let* ((from (if beg
                           (progn (goto-char beg)
                                  (if (re-search-backward "^\\* " nil t)
                                      (line-beginning-position) (point-min)))
                         (point-min)))
                 (to (if end
                         (progn (goto-char end) (forward-line 1)
                                (if (re-search-forward "^\\* " nil t)
                                    (line-beginning-position) (point-max)))
                       (point-max))))
            (setq org-roam-dailies-tasklog--time-overlays
                  (cl-delete-if
                   (lambda (overlay)
                     (when (or (not (overlay-buffer overlay))
                               (and (>= (overlay-start overlay) from)
                                    (<= (overlay-start overlay) to)))
                       (delete-overlay overlay) t))
                   org-roam-dailies-tasklog--time-overlays))
            (goto-char from)
            (while (re-search-forward "^\\* " to t)
              (let* ((end (line-end-position))
                     ;; Old notes already have literal timing in their headings.
                     (legacy (string-match-p
                              " → .* \\[[0-9]+:[0-9]+\\]$"
                              (buffer-substring-no-properties (point) end)))
                     (info (org-roam-dailies-tasklog--get-subtree-content))
                     (clock-line (org-roam-dailies-tasklog--get-latest-clock-entry
                                  (alist-get 'content info)))
                     (times (org-roam-dailies-tasklog--parse-clock-times clock-line))
                     (start (org-roam-dailies-tasklog--format-time (nth 0 times)))
                     (finish (org-roam-dailies-tasklog--format-time (nth 1 times))))
		(when (and (not legacy) start finish)
                  (let* ((running (not (string-match-p "--" clock-line)))
			 (padding (make-string
                                   (if org-roam-dailies-tasklog-time-column
                                       (max 5 (- org-roam-dailies-tasklog-time-column
						 (save-excursion (goto-char end)
								 (current-column))))
                                     5) ?\s))
			 (label (concat padding
					(if running
                                            (format "%s → running" start)
                                          (format "%s → %s [%s]" start finish
                                                  (org-roam-dailies-tasklog--duration-label
                                                   (nth 2 times))))))
			 (overlay (make-overlay end end)))
                    (overlay-put overlay 'after-string
				 (propertize label 'face 'shadow))
                    (push overlay org-roam-dailies-tasklog--time-overlays)))))))))))

(defun org-roam-dailies-tasklog--setup-time-overlays ()
  "Enable timing display in a daily-note buffer when tasklog is active."
  (when (and org-roam-dailies-tasklog-mode
             (org-roam-dailies-tasklog--daily-buffer-p))
    (add-hook 'after-change-functions
              #'org-roam-dailies-tasklog--refresh-time-overlays nil t)
    (add-hook 'change-major-mode-hook
              #'org-roam-dailies-tasklog--clear-time-overlays nil t)
    (org-roam-dailies-tasklog--refresh-time-overlays)))

;;; Session storage

(defun org-roam-dailies-tasklog--find-session (source-id session-id)
  "Find SOURCE-ID and SESSION-ID in the current daily buffer."
  (goto-char (point-min))
  (catch 'found
    (while (re-search-forward "^\\* " nil t)
      (when (and (equal source-id (org-entry-get nil "TASKLOG_SOURCE_ID"))
                 (equal session-id (org-entry-get nil "TASKLOG_SESSION")))
        (beginning-of-line)
        (throw 'found (point))))))

(defun org-roam-dailies-tasklog--update-clock (clock-line)
  "Replace only the managed clock line with CLOCK-LINE at this heading.
Leave notes, properties, other clock lines, and child headings untouched."
  (when clock-line
    (save-excursion
      (org-back-to-heading t)
      (let ((limit (save-excursion (outline-next-heading) (point)))
            (old (org-entry-get nil "TASKLOG_CLOCK")))
        (if (and old (re-search-forward (concat "^" (regexp-quote old) "$") limit t))
            (replace-match clock-line t t)
          (org-back-to-heading t)
          (if (re-search-forward "^:LOGBOOK:[ \t]*$" limit t)
              (progn (forward-line 1) (insert clock-line "\n"))
            (org-back-to-heading t)
            (org-end-of-meta-data t)
            (unless (bolp) (insert "\n"))
            (insert ":LOGBOOK:\n" clock-line "\n:END:\n")))
        (org-back-to-heading t)
        (org-entry-put nil "TASKLOG_CLOCK" clock-line)))))

(defun org-roam-dailies-tasklog--write-session (file source-id session-id heading state clock-line)
  "Update a session in FILE using SOURCE-ID and SESSION-ID.
HEADING and STATE come from the source.  CLOCK-LINE is nil for status updates."
  (let* ((existing (find-buffer-visiting file))
         (buffer (find-file-noselect file))
         (org-roam-dailies-tasklog--updating t))
    (unwind-protect
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (widen)
              (atomic-change-group
                (unless (org-roam-dailies-tasklog--find-session source-id session-id)
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert "\n* " heading "\n")
                  (forward-line -1)
                  (org-entry-put nil "TASKLOG_SOURCE_ID" source-id)
                  (org-entry-put nil "TASKLOG_SESSION" session-id)
                  (org-entry-put nil "TASKLOG_STATE" "")
                  (org-end-of-meta-data t)
                  (insert "Source: [[id:" source-id "][Original task]]\n")
                  (org-back-to-heading t))
                ;; Match the previously stored state even if this daily file has
                ;; different TODO keywords.  Never replace the entry's body.
                (let ((old (or (org-get-todo-state)
                               (org-entry-get nil "TASKLOG_STATE"))))
                  (beginning-of-line)
                  (when (looking-at "\\*+ ") (goto-char (match-end 0)))
                  (when (and old (not (string-empty-p old))
                             (looking-at (concat (regexp-quote old) " ")))
                    (delete-region (point) (match-end 0)))
                  (when (and state (not (string-empty-p state)))
                    (insert state " "))
                  (org-entry-put nil "TASKLOG_STATE" (or state "")))
                (org-roam-dailies-tasklog--update-clock clock-line)))
            (save-buffer)))
      (when (and (not existing) (buffer-live-p buffer)
                 (not (buffer-modified-p buffer)))
        (kill-buffer buffer)))))

(defun org-roam-dailies-tasklog--log-event (event &optional new-session)
  "Log EVENT at the current source heading, starting NEW-SESSION if requested."
  (unless org-roam-dailies-tasklog--updating
    (condition-case err
        (save-excursion
          (org-back-to-heading t)
          ;; Generated entries are snapshots, never sources for further logging.
          (unless (org-entry-get nil "TASKLOG_SOURCE_ID")
            (let* ((info (org-roam-dailies-tasklog--get-subtree-content))
                   (source-id (org-id-get-create))
                   (session (unless new-session (org-entry-get nil "TASKLOG_SESSION")))
                   (file (unless new-session (org-entry-get nil "TASKLOG_FILE")))
                   (clock (when (memq event '(clock-in clock-out))
                            (org-roam-dailies-tasklog--get-latest-clock-entry
                             (alist-get 'content info)))))
              (unless (and session file)
                (setq session (org-id-new)
                      file (org-roam-dailies-tasklog--get-daily-note-path)))
              (org-roam-dailies-tasklog--write-session
               file source-id session (alist-get 'heading info)
               (alist-get 'todo-state info) clock)
              ;; Persist routing on the source, so midnight and Emacs restarts
              ;; do not change which daily entry receives subsequent updates.
              (org-entry-put nil "TASKLOG_SESSION" session)
              (org-entry-put nil "TASKLOG_FILE" file)
              t)))
      (error (message "org-roam-dailies-tasklog: %s failed: %s"
                      event (error-message-string err))
             nil))))

;;; Hook handlers and commands

(defun org-roam-dailies-tasklog--handle-clock-in ()
  "Create a new daily session for a source clock-in."
  (org-roam-dailies-tasklog--log-event 'clock-in t))

(defun org-roam-dailies-tasklog--handle-clock-out ()
  "Finish the recorded session, including sessions started before midnight."
  (org-roam-dailies-tasklog--log-event 'clock-out))

(defun org-roam-dailies-tasklog--handle-todo-state-change ()
  "Sync state changes to the latest session, including reopened tasks.
Untracked tasks create an entry only for configured completion states.
Earlier sessions remain historical snapshots; daily edits do not sync back."
  (when (and (not org-roam-dailies-tasklog--updating)
             (not (org-entry-get nil "TASKLOG_SOURCE_ID"))
             (or (org-entry-get nil "TASKLOG_SESSION")
                 (member (org-get-todo-state)
                         org-roam-dailies-tasklog-completion-states)))
    (org-roam-dailies-tasklog--log-event 'state)))

;;;###autoload
(defun org-roam-dailies-tasklog-visit-source ()
  "Visit the original task associated with the daily entry at point."
  (interactive)
  (let ((id (org-entry-get nil "TASKLOG_SOURCE_ID")))
    (unless id (user-error "This is not a tasklog session entry"))
    (org-id-goto id)))

;;;###autoload
(defun org-roam-dailies-tasklog-refresh ()
  "Refresh timing overlays in all open daily notes after changing options."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (org-roam-dailies-tasklog--refresh-time-overlays))))

;;; Minor mode

;;;###autoload
(define-minor-mode org-roam-dailies-tasklog-mode
  "Automatically log task events to org-roam daily notes.

When enabled, this mode logs the following events to your org-roam daily note:
  - Clock-in events (when you start working on a task)
  - Clock-out events (when you stop working on a task, with duration)
  - State changes for tracked tasks, including reopening completed tasks
  - Completion of untracked tasks in configured completion states

Each clock-in creates a new entry with only that session's data.
Times appear beside headings as overlays, derived from the LOGBOOK;
they are not stored in the heading text."
  :global t
  :group 'org-roam-dailies-tasklog
  :lighter " TaskLog"
  (if org-roam-dailies-tasklog-mode
      (progn
        (add-hook 'org-mode-hook #'org-roam-dailies-tasklog--setup-time-overlays)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (org-roam-dailies-tasklog--setup-time-overlays)))
        (add-hook 'org-clock-in-hook #'org-roam-dailies-tasklog--handle-clock-in)
        (add-hook 'org-clock-out-hook #'org-roam-dailies-tasklog--handle-clock-out)
        (add-hook 'org-after-todo-state-change-hook #'org-roam-dailies-tasklog--handle-todo-state-change)
        (message "org-roam-dailies-tasklog-mode enabled"))
    (progn
      (remove-hook 'org-mode-hook #'org-roam-dailies-tasklog--setup-time-overlays)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (remove-hook 'after-change-functions
                       #'org-roam-dailies-tasklog--refresh-time-overlays t)
          (remove-hook 'change-major-mode-hook
                       #'org-roam-dailies-tasklog--clear-time-overlays t)
          (org-roam-dailies-tasklog--clear-time-overlays)))
      (remove-hook 'org-clock-in-hook #'org-roam-dailies-tasklog--handle-clock-in)
      (remove-hook 'org-clock-out-hook #'org-roam-dailies-tasklog--handle-clock-out)
      (remove-hook 'org-after-todo-state-change-hook #'org-roam-dailies-tasklog--handle-todo-state-change)
      (message "org-roam-dailies-tasklog-mode disabled"))))

(provide 'org-roam-dailies-tasklog)
;;; org-roam-dailies-tasklog.el ends here
