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

(defcustom org-roam-dailies-tasklog-time-format "%I:%M %p"
  "Format string for timestamps in log entries.
Uses `format-time-string' syntax."
  :type 'string
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

(defun org-roam-dailies-tasklog--is-timestamp-today-p (timestamp-str)
  "Check if TIMESTAMP-STR is from today.
TIMESTAMP-STR should be in org-mode format (YYYY-MM-DD Day HH:MM)."
  (when (and timestamp-str (stringp timestamp-str))
    (let ((today (format-time-string "%Y-%m-%d")))
      (string-prefix-p today timestamp-str))))

(defun org-roam-dailies-tasklog--calculate-clock-sum (content)
  "Calculate total clocked time from CONTENT's LOGBOOK for today only.
Returns formatted time string like \"0:35\" or nil if no clock data."
  (when (string-match ":LOGBOOK:" content)
    (let ((total-minutes 0)
          (today (format-time-string "%Y-%m-%d")))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        ;; Match complete clock entries with timestamps
        (while (re-search-forward "CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\] =>[ \t]+\\([0-9]+\\):\\([0-9]+\\)" nil t)
          (let ((start-timestamp (match-string 1))
                (hours (string-to-number (match-string 3)))
                (minutes (string-to-number (match-string 4))))
            ;; Only include if start timestamp is from today
            (when (org-roam-dailies-tasklog--is-timestamp-today-p start-timestamp)
              (setq total-minutes (+ total-minutes (* hours 60) minutes))))))
      (when (> total-minutes 0)
        (format "%d:%02d" (/ total-minutes 60) (% total-minutes 60))))))

(defun org-roam-dailies-tasklog--get-clock-range (content)
  "Extract earliest start time and latest end time from CONTENT's LOGBOOK for today only.
Returns (START-TIME . END-TIME) or nil if no clock data.
Handles incomplete clock entries (clock-in without clock-out)."
  (when (string-match ":LOGBOOK:" content)
    (let (start-times end-times incomplete-start)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        ;; Match complete clock entries (with end time)
        (while (re-search-forward "CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\]" nil t)
          (let ((start-ts (match-string 1))
                (end-ts (match-string 2)))
            ;; Only include if start timestamp is from today
            (when (org-roam-dailies-tasklog--is-timestamp-today-p start-ts)
              (push start-ts start-times)
              (push end-ts end-times))))
        ;; Match incomplete clock entries (clock-in without clock-out)
        (goto-char (point-min))
        (when (re-search-forward "CLOCK: \\[\\([^]]+\\)\\]$" nil t)
          (let ((start-ts (match-string 1)))
            ;; Only include if it's from today
            (when (org-roam-dailies-tasklog--is-timestamp-today-p start-ts)
              (setq incomplete-start start-ts)))))
      (when (or start-times incomplete-start)
        ;; If there's an incomplete clock, use current time as end
        (let ((earliest-start (if incomplete-start
                                  (if start-times
                                      (car (sort (cons incomplete-start start-times) #'string<))
                                    incomplete-start)
                                (car (sort start-times #'string<))))
              (latest-end (if incomplete-start
                              (format-time-string "%Y-%m-%d %a %H:%M")
                            (car (sort end-times #'string>)))))
          (cons earliest-start latest-end))))))

(defun org-roam-dailies-tasklog--format-time (timestamp-str)
  "Extract time and format as HH:MM AM/PM from org timestamp string.
Supports both org-mode format (YYYY-MM-DD Day HH:MM) and ISO 8601 (YYYY-MM-DDTHH:MM).
Returns nil for invalid or unparseable timestamps."
  (when (and timestamp-str (stringp timestamp-str))
    (let ((hour nil)
          (minute nil))
      ;; Try org-mode format: YYYY-MM-DD Day HH:MM
      (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) [A-Za-z]+ \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" timestamp-str)
        (setq hour (string-to-number (match-string 4 timestamp-str)))
        (setq minute (match-string 5 timestamp-str)))
      ;; Try ISO 8601 format: YYYY-MM-DDTHH:MM or YYYY-MM-DD HH:MM
      (when (and (not hour)
                 (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[T ]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" timestamp-str))
        (setq hour (string-to-number (match-string 4 timestamp-str)))
        (setq minute (match-string 5 timestamp-str)))
      ;; Validate and format if we successfully extracted time
      (when (and hour minute (>= hour 0) (<= hour 23)
                 (>= (string-to-number minute) 0)
                 (<= (string-to-number minute) 59))
        (let* ((am-pm (if (< hour 12) "AM" "PM"))
               (display-hour (cond
                              ((= hour 0) 12)
                              ((> hour 12) (- hour 12))
                              (t hour))))
          (format "%02d:%s %s" display-hour minute am-pm))))))

(defun org-roam-dailies-tasklog--get-scheduled-range (content)
  "Extract scheduled time range from CONTENT if present.
Returns (START-TIME . END-TIME) or nil if no scheduled time range.
Handles midnight boundary cases where end time is before start time."
  (when (and content (stringp content)
             (string-match "SCHEDULED: <\\([^>]+\\)>" content))
    (let ((timestamp (match-string 1 content)))
      ;; Match time range format: YYYY-MM-DD Day HH:MM-HH:MM
      (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\([A-Za-z]+\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" timestamp)
        (let* ((date-part (match-string 1 timestamp))
               (day-part (match-string 2 timestamp))
               (start-time (match-string 3 timestamp))
               (end-time (match-string 4 timestamp))
               (start-str (concat date-part " " day-part " " start-time))
               (end-str (concat date-part " " day-part " " end-time)))
          ;; Check for midnight boundary case
          (when (org-roam-dailies-tasklog--crosses-midnight-p start-time end-time)
            (org-roam-dailies-tasklog--debug
             "Detected midnight boundary: %s-%s" start-time end-time)
            ;; Add one day to end time
            (let* ((parsed-date (org-parse-time-string start-str))
                   (next-day (time-add (apply #'encode-time parsed-date)
                                       (* 24 60 60))))
              (setq end-str (format-time-string
                             (format "%%Y-%%m-%%d %%a %s" end-time)
                             next-day))))
          (cons start-str end-str))))))

(defun org-roam-dailies-tasklog--crosses-midnight-p (start-time end-time)
  "Check if time range from START-TIME to END-TIME crosses midnight.
START-TIME and END-TIME should be strings in HH:MM format."
  (when (and start-time end-time
             (string-match "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" start-time))
    (let ((start-hour (string-to-number (match-string 1 start-time)))
          (start-min (string-to-number (match-string 2 start-time))))
      (when (string-match "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" end-time)
        (let ((end-hour (string-to-number (match-string 1 end-time)))
              (end-min (string-to-number (match-string 2 end-time))))
          (or (< end-hour start-hour)
              (and (= end-hour start-hour) (< end-min start-min))))))))

(defun org-roam-dailies-tasklog--find-entry-by-heading (heading daily-file)
  "Find existing entry with HEADING in DAILY-FILE.
Returns (START . END) positions if found, nil otherwise."
  (when (file-exists-p daily-file)
    (with-current-buffer (find-file-noselect daily-file)
      (save-excursion
        (goto-char (point-min))
        (let ((search-heading (regexp-quote heading)))
          (when (re-search-forward
                 (concat "^\\* .+? " search-heading "\\b")
                 nil t)
            (let ((start (line-beginning-position))
                  (end (save-excursion
                         (if (outline-next-heading)
                             (point)
                           (point-max)))))
              (cons start end))))))))

(defun org-roam-dailies-tasklog--get-existing-content (heading daily-file)
  "Get the content (everything after heading line) for HEADING in DAILY-FILE.
Returns the content string if entry found, nil otherwise."
  (when (file-exists-p daily-file)
    (with-current-buffer (find-file-noselect daily-file)
      (save-excursion
        (goto-char (point-min))
        (let ((search-heading (regexp-quote heading)))
          (when (re-search-forward
                 (concat "^\\* .+? " search-heading "\\b")
                 nil t)
            (let ((content-start (progn (forward-line 1) (point)))
                  (content-end (save-excursion
                                 (if (outline-next-heading)
                                     (point)
                                   (point-max)))))
              (string-trim (buffer-substring-no-properties content-start content-end)))))))))

(defun org-roam-dailies-tasklog--calculate-scheduled-duration (start-str end-str)
  "Calculate duration between START-STR and END-STR timestamps.
Returns formatted duration string like \"1:30\".
Handles midnight boundary cases where end is on the next day.
Returns nil for invalid inputs or negative durations."
  (when (and start-str end-str
             (stringp start-str) (stringp end-str))
    (condition-case err
        (let* ((start-time (org-parse-time-string start-str))
               (end-time (org-parse-time-string end-str)))
          ;; Validate that we got valid parse results
          (when (and start-time end-time
                     (nth 1 start-time) (nth 2 start-time)  ; minute and hour exist
                     (nth 1 end-time) (nth 2 end-time))
            (let* ((start-seconds (apply #'encode-time start-time))
                   (end-seconds (apply #'encode-time end-time))
                   (duration-seconds (time-subtract end-seconds start-seconds))
                   (duration-minutes (/ (time-to-seconds duration-seconds) 60)))
              (when (> duration-minutes 0)
                (let ((hours (floor (/ duration-minutes 60)))
                      (minutes (floor (mod duration-minutes 60))))
                  (format "%d:%02d" hours minutes))))))
      (error
       (org-roam-dailies-tasklog--debug
        "Failed to calculate duration: %s -> %s (error: %s)"
        start-str end-str (error-message-string err))
       nil))))

(defun org-roam-dailies-tasklog--filter-logbook-today (content)
  "Filter CONTENT to only include LOGBOOK entries from today.
Returns the content with only today's CLOCK entries in the LOGBOOK.
If content has no LOGBOOK, returns content unchanged."
  (if (and content (string-match ":LOGBOOK:" content))
    (let ((today (format-time-string "%Y-%m-%d"))
          (filtered-lines '()))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        ;; Find the LOGBOOK section
        (when (re-search-forward ":LOGBOOK:" nil t)
          (let ((logbook-start (match-beginning 0))
                (logbook-end (when (re-search-forward ":END:" nil t)
                              (match-end 0))))
            (when logbook-end
              ;; Extract lines between LOGBOOK and END
              (goto-char logbook-start)
              (forward-line 1)
              (while (< (point) (- logbook-end 5))  ; -5 to skip :END:
                (let ((line (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
                  ;; Keep line if it's a CLOCK entry from today or not a CLOCK entry
                  (when (or (not (string-prefix-p "CLOCK:" (string-trim line)))
                           (and (string-match "CLOCK: \\[\\([^]]+\\)\\]" line)
                                (org-roam-dailies-tasklog--is-timestamp-today-p
                                 (match-string 1 line))))
                    (push line filtered-lines)))
                (forward-line 1))
              ;; Reconstruct the content with filtered LOGBOOK
              (let ((before-logbook (buffer-substring-no-properties (point-min) logbook-start))
                    (after-logbook (buffer-substring-no-properties logbook-end (point-max))))
                (concat before-logbook
                       ":LOGBOOK:\n"
                       (if filtered-lines
                           (concat (mapconcat 'identity (nreverse filtered-lines) "\n") "\n")
                         "")
                       ":END:"
                       after-logbook)))))))
    ;; If no LOGBOOK found, return content unchanged
    content))

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
         ;; Filter content to only include today's LOGBOOK entries
         (filtered-content (org-roam-dailies-tasklog--filter-logbook-today content))
         (clock-sum (org-roam-dailies-tasklog--calculate-clock-sum content))
         (clock-range (org-roam-dailies-tasklog--get-clock-range content))
         ;; Fall back to scheduled time range if no clock data
         (scheduled-range (unless clock-range
                           (org-roam-dailies-tasklog--get-scheduled-range content)))
         (time-range (or clock-range scheduled-range))
         (start-time (when time-range
                       (org-roam-dailies-tasklog--format-time (car time-range))))
         (end-time (when time-range
                     (org-roam-dailies-tasklog--format-time (cdr time-range))))
         ;; Calculate duration from scheduled time if no clock data
         (calculated-duration (when (and scheduled-range (not clock-sum))
                               (org-roam-dailies-tasklog--calculate-scheduled-duration
                                (car scheduled-range) (cdr scheduled-range))))
         ;; Extract text content from preserved content if it exists
         (preserved-text (when preserve-content
                          (org-roam-dailies-tasklog--extract-text-content preserve-content)))
         ;; Combine filtered content with preserved text
         (final-content (if preserved-text
                           (concat filtered-content "\n" preserved-text)
                         filtered-content)))
    (let* ((time-str (format "%s → %s [%s]"
                             (or start-time "??:??")
                             (or end-time "??:??")
                             (or clock-sum calculated-duration "00:00")))
           ;; Calculate padding for right alignment at column 88, minimum 5 spaces
           (prefix-len (+ 2 (length state) 1 (length heading)))  ; "* STATE HEADING"
           (target-col 88)
           (padding (max 5 (- target-col prefix-len (length time-str)))))
      (concat
       ;; Heading format: * STATE HEADING         START → END [DURATION]
       (format "* %s %s%s%s\n"
               state
               heading
               (make-string padding ?\s)
               time-str)
       ;; Use final content with filtered LOGBOOK and preserved text
       final-content))))

(defun org-roam-dailies-tasklog--upsert-to-daily (formatted-entry heading)
  "Update existing entry for HEADING or append FORMATTED-ENTRY if not found.
Opens the daily note file, searches for existing entry with HEADING,
and either replaces it or appends a new entry. Manages buffer state
to avoid disrupting the user's window layout."
  (let* ((daily-file (org-roam-dailies-tasklog--get-daily-note-path))
         (buffer-was-open (find-buffer-visiting daily-file))
         (entry-pos (org-roam-dailies-tasklog--find-entry-by-heading heading daily-file)))
    (with-current-buffer (find-file-noselect daily-file)
      (save-excursion
        (if entry-pos
            ;; Replace existing entry
            (progn
              (org-roam-dailies-tasklog--debug "Updating existing entry for: %s" heading)
              (goto-char (car entry-pos))
              (delete-region (car entry-pos) (cdr entry-pos))
              (insert formatted-entry))
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

(defun org-roam-dailies-tasklog--log-event (event-type duration)
  "Log an event of EVENT-TYPE with optional DURATION to the daily note.
EVENT-TYPE is a string like \"CLOCKED IN\", \"CLOCKED OUT\", or \"DONE\".
DURATION is an optional string describing time spent (e.g., \"0:30\").
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
        (org-roam-dailies-tasklog--debug "Logging event: %s" event-type)
        (let* ((task-info (org-roam-dailies-tasklog--get-subtree-content))
               (heading (alist-get 'heading task-info))
               (daily-file (org-roam-dailies-tasklog--get-daily-note-path))
               ;; Get existing content from daily note if entry already exists
               (existing-content (org-roam-dailies-tasklog--get-existing-content heading daily-file)))
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
            (org-roam-dailies-tasklog--upsert-to-daily formatted-entry heading)
            (org-roam-dailies-tasklog--debug "Successfully logged event")
            t)))
    (error
     (message "org-roam-dailies-tasklog: Failed to log event '%s': %s"
              event-type
              (error-message-string err))
     nil)))

;;; Hook handlers

(defun org-roam-dailies-tasklog--handle-clock-in ()
  "Handle clock-in events by logging to daily note."
  (org-roam-dailies-tasklog--log-event "CLOCKED IN" nil))

(defun org-roam-dailies-tasklog--handle-clock-out ()
  "Handle clock-out events by logging to daily note with duration."
  (let ((duration (when org-clock-out-time
                    ;; Calculate duration from the clock line
                    (save-excursion
                      (when (org-at-clock-log-p)
                        (let ((start (point)))
                          (when (looking-at org-clock-line-re)
                            ;; Extract the duration from the clock line
                            (let ((effort-string (match-string 1)))
                              effort-string))))))))
    (org-roam-dailies-tasklog--log-event "CLOCKED OUT" duration)))

(defun org-roam-dailies-tasklog--handle-todo-state-change ()
  "Handle TODO state changes by logging completion states to daily note."
  (when (member org-state org-roam-dailies-tasklog-completion-states)
    (org-roam-dailies-tasklog--log-event org-state nil)))

;;; Minor mode

;;;###autoload
(define-minor-mode org-roam-dailies-tasklog-mode
  "Automatically log task events to org-roam daily notes.

When enabled, this mode logs the following events to your org-roam daily note:
  - Clock-in events (when you start working on a task)
  - Clock-out events (when you stop working on a task, with duration)
  - Task completion events (when you mark a task as DONE)

Each event captures the task heading, timestamp, tags, properties, and body
content, and appends it to the end of today's org-roam daily note."
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
