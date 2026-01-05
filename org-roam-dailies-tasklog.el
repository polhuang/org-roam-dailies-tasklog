;;;; org-roam-dailies-tasklog.el --- Automatically log task events to org-roam dailies -*- lexical-binding: t; -*-

;; Author: Paul Huang
;; Version: 0.0.1
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

(defcustom org-roam-dailies-tasklog-log-clock-in t
  "Whether to log clock-in events.
Disabled by default since clock-out shows cumulative time."
  :type 'boolean
  :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-log-clock-out t
  "Whether to log clock-out events."
  :type 'boolean
  :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-log-done t
  "Whether to log task completion events."
  :type 'boolean
  :group 'org-roam-dailies-tasklog)

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

(defun org-roam-dailies-tasklog--calculate-clock-sum (content)
  "Calculate total clocked time from CONTENT's LOGBOOK.
Returns formatted time string like \"0:35\" or nil if no clock data."
  (when (string-match ":LOGBOOK:" content)
    (let ((total-minutes 0))
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (while (re-search-forward "=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)" nil t)
          (let ((hours (string-to-number (match-string 1)))
                (minutes (string-to-number (match-string 2))))
            (setq total-minutes (+ total-minutes (* hours 60) minutes)))))
      (when (> total-minutes 0)
        (format "%d:%02d" (/ total-minutes 60) (% total-minutes 60))))))

(defun org-roam-dailies-tasklog--get-clock-range (content)
  "Extract earliest start time and latest end time from CONTENT's LOGBOOK.
Returns (START-TIME . END-TIME) or nil if no clock data.
Handles incomplete clock entries (clock-in without clock-out)."
  (when (string-match ":LOGBOOK:" content)
    (let (start-times end-times incomplete-start)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        ;; Match complete clock entries (with end time)
        (while (re-search-forward "CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\]" nil t)
          (push (match-string 1) start-times)
          (push (match-string 2) end-times))
        ;; Match incomplete clock entries (clock-in without clock-out)
        (goto-char (point-min))
        (when (re-search-forward "CLOCK: \\[\\([^]]+\\)\\]$" nil t)
          (setq incomplete-start (match-string 1))))
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
  "Extract time and format as H:MM AM/PM from org timestamp string."
  (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) [A-Za-z]+ \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" timestamp-str)
    (let* ((hour (string-to-number (match-string 4 timestamp-str)))
           (minute (match-string 5 timestamp-str))
           (am-pm (if (< hour 12) "AM" "PM"))
           (display-hour (cond
                          ((= hour 0) 12)
                          ((> hour 12) (- hour 12))
                          (t hour))))
      (format "%d:%s %s" display-hour minute am-pm))))

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

(defun org-roam-dailies-tasklog--format-log-entry (task-info event-type duration)
  "Format TASK-INFO as a log entry for EVENT-TYPE with optional DURATION.
TASK-INFO is an alist containing task information.
EVENT-TYPE is a string describing the event (e.g., \"CLOCKED IN\", \"DONE\").
DURATION is an optional string describing time spent (e.g., \"0:30\")."
  (let* ((heading (alist-get 'heading task-info))
         (state (or (alist-get 'todo-state task-info) "IN PROGRESS"))
         (content (alist-get 'content task-info))
         (clock-sum (org-roam-dailies-tasklog--calculate-clock-sum content))
         (clock-range (org-roam-dailies-tasklog--get-clock-range content))
         (start-time (when clock-range
                       (org-roam-dailies-tasklog--format-time (car clock-range))))
         (end-time (when clock-range
                     (org-roam-dailies-tasklog--format-time (cdr clock-range)))))
    (concat
     ;; Heading format: * STATE HEADING         START → END [DURATION]
     (format "* %s %s%s%s → %s [%s]\n"
             state
             heading
             (make-string (max 1 (- 33 (length heading))) ?\s)
             (or start-time "??:??")
             (or end-time "??:??")
             (or clock-sum "0:00"))
     ;; Everything else from the original task unchanged
     content)))

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
               (heading (alist-get 'heading task-info)))
          (when task-info
            (let ((formatted-entry
                   (org-roam-dailies-tasklog--format-log-entry
                    task-info event-type duration)))
              (org-roam-dailies-tasklog--upsert-to-daily formatted-entry heading)
              t)))
        t)
    (error
     (message "org-roam-dailies-tasklog: Failed to log event: %s"
              (error-message-string err))
     nil)))

;;; Hook handlers

(defun org-roam-dailies-tasklog--handle-clock-in ()
  "Handle clock-in events by logging to daily note."
  (when org-roam-dailies-tasklog-log-clock-in
    (org-roam-dailies-tasklog--log-event "CLOCKED IN" nil)))

(defun org-roam-dailies-tasklog--handle-clock-out ()
  "Handle clock-out events by logging to daily note with duration."
  (when org-roam-dailies-tasklog-log-clock-out
    (let ((duration (when org-clock-out-time
                      ;; Calculate duration from the clock line
                      (save-excursion
                        (when (org-at-clock-log-p)
                          (let ((start (point)))
                            (when (looking-at org-clock-line-re)
                              ;; Extract the duration from the clock line
                              (let ((effort-string (match-string 1)))
                                effort-string))))))))
      (org-roam-dailies-tasklog--log-event "CLOCKED OUT" duration))))

(defun org-roam-dailies-tasklog--handle-todo-state-change ()
  "Handle TODO state changes by logging completion states to daily note."
  (when (and org-roam-dailies-tasklog-log-done
             (member org-state org-roam-dailies-tasklog-completion-states))
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
