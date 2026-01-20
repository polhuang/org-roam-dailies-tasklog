;;;; org-roam-dailies-tasklog.el --- Automatically log task events to org-roam dailies -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
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
(require 'org-roam)

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
  "Get the entire content of current subtree (everything after heading).
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

(defun org-roam-dailies-tasklog--format-time (timestamp-str)
  "Extract time and format as HH:MM AM/PM from org timestamp string.
Supports org-mode format (YYYY-MM-DD Day HH:MM).
Returns nil for invalid timestamps."
  (when (and timestamp-str (stringp timestamp-str))
    (let ((hour nil)
          (minute nil))
      ;; Try org-mode format: YYYY-MM-DD Day HH:MM
      (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) [A-Za-z]+ \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" timestamp-str)
        (setq hour (string-to-number (match-string 4 timestamp-str)))
        (setq minute (match-string 5 timestamp-str)))
      ;; Validate and format
      (when (and hour minute (>= hour 0) (<= hour 23))
        (let* ((am-pm (if (< hour 12) "AM" "PM"))
               (display-hour (cond
                              ((= hour 0) 12)
                              ((> hour 12) (- hour 12))
                              (t hour))))
          (format "%02d:%s %s" display-hour minute am-pm))))))

(defun org-roam-dailies-tasklog--find-last-entry-by-heading (heading daily-file)
  "Find the LAST (most recent) entry with HEADING in DAILY-FILE.
Returns (START . END) positions if found, nil otherwise."
  (when (file-exists-p daily-file)
    (with-current-buffer (find-file-noselect daily-file)
      (save-excursion
        (goto-char (point-min))
        (let ((search-heading (regexp-quote heading))
              last-match-start last-match-end)
          ;; Find all matches and keep the last one
          (while (re-search-forward
                  (concat "^\\* .+? " search-heading "\\b")
                  nil t)
            (setq last-match-start (line-beginning-position))
            (setq last-match-end (save-excursion
                                   (if (outline-next-heading)
                                       (point)
                                     (point-max)))))
          ;; Return the last match found
          (when last-match-start
            (cons last-match-start last-match-end)))))))

(defun org-roam-dailies-tasklog--get-existing-content (heading daily-file)
  "Get the content for the LAST entry with HEADING in DAILY-FILE.
Returns the content string if entry found, nil otherwise."
  (when (file-exists-p daily-file)
    (with-current-buffer (find-file-noselect daily-file)
      (save-excursion
        (goto-char (point-min))
        (let ((search-heading (regexp-quote heading))
              last-content)
          ;; Find all matches and keep the last one
          (while (re-search-forward
                  (concat "^\\* .+? " search-heading "\\b")
                  nil t)
            (let ((content-start (progn (forward-line 1) (point)))
                  (content-end (save-excursion
                                 (if (outline-next-heading)
                                     (point)
                                   (point-max)))))
              (setq last-content (string-trim (buffer-substring-no-properties content-start content-end)))))
          last-content)))))

(defun org-roam-dailies-tasklog--extract-text-content (content)
  "Extract text content from CONTENT, excluding PROPERTIES and LOGBOOK sections.
Returns the text that appears after these sections."
  (when content
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Skip past PROPERTIES section if present
      (when (re-search-forward "^:PROPERTIES:" nil t)
        (when (re-search-forward "^:END:" nil t)
          (forward-line 1)))
      ;; Skip past LOGBOOK section if present
      (when (re-search-forward "^:LOGBOOK:" nil t)
        (when (re-search-forward "^:END:" nil t)
          (forward-line 1)))
      ;; Get remaining text
      (let ((text (string-trim (buffer-substring-no-properties (point) (point-max)))))
        (if (string-empty-p text) nil text)))))

(defun org-roam-dailies-tasklog--format-log-entry (task-info event-type duration &optional preserve-content)
  "Format TASK-INFO as a log entry for EVENT-TYPE with optional DURATION.
TASK-INFO is an alist containing task information.
EVENT-TYPE is a string describing the event (e.g., \"CLOCKED IN\", \"DONE\").
DURATION is an optional string describing time spent (e.g., \"0:30\").
PRESERVE-CONTENT is optional content from existing daily entry to preserve."
  (let* ((heading (alist-get 'heading task-info))
         (state (or (alist-get 'todo-state task-info) "IN PROGRESS"))
         (content (alist-get 'content task-info))
         ;; Extract only the latest clock entry
         (latest-clock-line (org-roam-dailies-tasklog--get-latest-clock-entry content))
         ;; Parse the clock times
         (clock-data (org-roam-dailies-tasklog--parse-clock-times latest-clock-line))
         (start-ts (nth 0 clock-data))
         (end-ts (nth 1 clock-data))
         (duration-str (nth 2 clock-data))
         ;; Format times for display
         (start-time (org-roam-dailies-tasklog--format-time start-ts))
         (end-time (org-roam-dailies-tasklog--format-time end-ts))
         ;; Extract text content from existing daily note entry (we want to preserve it)
         (preserved-text (when preserve-content
                          (org-roam-dailies-tasklog--extract-text-content preserve-content)))
         ;; Build the LOGBOOK section with only the latest entry
         (logbook-section (if latest-clock-line
                              (concat ":LOGBOOK:\n" latest-clock-line "\n:END:")
                            "")))
    (let* ((time-str (format "%s → %s [%s]"
                             (or start-time "??:??")
                             (or end-time "??:??")
                             (or duration-str "0:00")))
           ;; Calculate padding for right alignment at column 88, minimum 5 spaces
           (prefix-len (+ 2 (length state) 1 (length heading)))  ; "* STATE HEADING"
           (target-col 88)
           (padding (max 5 (- target-col prefix-len (length time-str)))))
      (let ((entry (concat
                     ;; Heading format: * STATE HEADING         START → END [DURATION]
                     (format "* %s %s%s%s\n"
                             state
                             heading
                             (make-string padding ?\s)
                             time-str)
                     ;; Add LOGBOOK section
                     (when logbook-section
                       (concat logbook-section "\n"))
                     ;; Add preserved text from daily note
                     (when preserved-text
                       (concat preserved-text "\n")))))
        ;; Ensure entry ends with a newline
        (if (string-suffix-p "\n" entry)
            entry
          (concat entry "\n"))))))

(defun org-roam-dailies-tasklog--upsert-to-daily (formatted-entry heading &optional append-only)
  "Update existing entry for HEADING or append FORMATTED-ENTRY if not found.
Opens the daily note file, searches for existing entry with HEADING,
and either replaces it or appends a new entry.
If APPEND-ONLY is non-nil, always append a new entry without searching."
  (let* ((daily-file (org-roam-dailies-tasklog--get-daily-note-path))
         (buffer-was-open (find-buffer-visiting daily-file))
         (entry-pos (unless append-only
                      (org-roam-dailies-tasklog--find-last-entry-by-heading heading daily-file))))
    (with-current-buffer (find-file-noselect daily-file)
      (save-excursion
        (if (and entry-pos (not append-only))
            ;; Replace existing entry
            (progn
              (org-roam-dailies-tasklog--debug "Updating existing entry for: %s" heading)
              (goto-char (car entry-pos))
              (delete-region (car entry-pos) (cdr entry-pos))
              (insert formatted-entry)
              ;; Ensure proper spacing after the entry
              (unless (or (eobp) (looking-at "^\\s-*$"))
                (insert "\n")))
          ;; Append new entry
          (org-roam-dailies-tasklog--debug "Creating new entry for: %s" heading)
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (when (> (point-max) 1)
            (insert "\n"))
          (insert formatted-entry)))
      (save-buffer)
      ;; Close buffer if it wasn't already open
      (unless buffer-was-open
        (kill-buffer)))))

(defun org-roam-dailies-tasklog--log-event (event-type duration &optional append-only)
  "Log an event of EVENT-TYPE with optional DURATION to the daily note.
EVENT-TYPE is a string like \"CLOCKED IN\", \"CLOCKED OUT\", or \"DONE\".
DURATION is an optional string describing time spent (e.g., \"0:30\").
If APPEND-ONLY is non-nil, always create a new entry instead of updating existing.
Returns t on success, nil on failure."
  (condition-case err
      (progn
        ;; Validation
        (unless (featurep 'org-roam)
          (error "org-roam is not available"))
        (unless (derived-mode-p 'org-mode)
          (error "Not in an org-mode buffer"))
        (unless (org-at-heading-p)
          (org-back-to-heading t))

        ;; Main logic
        (org-roam-dailies-tasklog--debug "Logging event: %s (append-only: %s)" event-type append-only)
        (let* ((task-info (org-roam-dailies-tasklog--get-subtree-content))
               (heading (alist-get 'heading task-info))
               (daily-file (org-roam-dailies-tasklog--get-daily-note-path))
               ;; Get existing content from daily note if entry already exists (only if not append-only)
               (existing-content (unless append-only
                                  (org-roam-dailies-tasklog--get-existing-content heading daily-file))))
          (unless task-info
            (error "Failed to extract task information from current heading"))
          (unless heading
            (error "Failed to extract heading text"))

          (org-roam-dailies-tasklog--debug "Task heading: %s" heading)
          (org-roam-dailies-tasklog--debug "Task state: %s" (alist-get 'todo-state task-info))
          (when existing-content
            (org-roam-dailies-tasklog--debug "Preserving existing content from daily note"))

          (let ((formatted-entry
                 (org-roam-dailies-tasklog--format-log-entry
                  task-info event-type duration existing-content)))
            (unless formatted-entry
              (error "Failed to format log entry"))

            (org-roam-dailies-tasklog--debug "Formatted entry length: %d chars"
                                             (length formatted-entry))
            (org-roam-dailies-tasklog--upsert-to-daily formatted-entry heading append-only)
            (org-roam-dailies-tasklog--debug "Successfully logged event")
            t)))
    (error
     (message "org-roam-dailies-tasklog: Failed to log event '%s': %s"
              event-type
              (error-message-string err))
     nil)))

;;; Hook handlers

(defun org-roam-dailies-tasklog--handle-clock-in ()
  "Handle clock-in events by logging to daily note.
Always creates a new entry for each clock-in (new work session)."
  (org-roam-dailies-tasklog--log-event "CLOCKED IN" nil t))

(defun org-roam-dailies-tasklog--handle-clock-out ()
  "Handle clock-out events by logging to daily note with duration.
Updates the most recent entry for this task."
  (let ((duration (when org-clock-out-time
                    ;; Calculate duration from the clock line
                    (save-excursion
                      (when (org-at-clock-log-p)
                        (let ((start (point)))
                          (when (looking-at org-clock-line-re)
                            ;; Extract the duration from the clock line
                            (let ((effort-string (match-string 1)))
                              effort-string))))))))
    (org-roam-dailies-tasklog--log-event "CLOCKED OUT" duration nil)))

(defun org-roam-dailies-tasklog--handle-todo-state-change ()
  "Handle TODO state changes by logging completion states to daily note.
Updates the most recent entry for this task."
  (when (member org-state org-roam-dailies-tasklog-completion-states)
    (org-roam-dailies-tasklog--log-event org-state nil nil)))

;;; Minor mode

;;;###autoload
(define-minor-mode org-roam-dailies-tasklog-mode
  "Automatically log task events to org-roam daily notes.

When enabled, this mode logs the following events to your org-roam daily note:
  - Clock-in events (when you start working on a task)
  - Clock-out events (when you stop working on a task, with duration)
  - Task completion events (when you mark a task as DONE)

Each clock-in creates a new entry in the daily note with only that session's data."
  :global t
  :group 'org-roam-dailies-tasklog
  :lighter " TaskLog"
  (if org-roam-dailies-tasklog-mode
      (progn
        (add-hook 'org-clock-in-hook #'org-roam-dailies-tasklog--handle-clock-in)
        (add-hook 'org-clock-out-hook #'org-roam-dailies-tasklog--handle-clock-out)
        (add-hook 'org-after-todo-state-change-hook #'org-roam-dailies-tasklog--handle-todo-state-change)
        (message "org-roam-dailies-tasklog-mode enabled"))
    (progn
      (remove-hook 'org-clock-in-hook #'org-roam-dailies-tasklog--handle-clock-in)
      (remove-hook 'org-clock-out-hook #'org-roam-dailies-tasklog--handle-clock-out)
      (remove-hook 'org-after-todo-state-change-hook #'org-roam-dailies-tasklog--handle-todo-state-change)
      (message "org-roam-dailies-tasklog-mode disabled"))))

(provide 'org-roam-dailies-tasklog)
;;; org-roam-dailies-tasklog.el ends here
