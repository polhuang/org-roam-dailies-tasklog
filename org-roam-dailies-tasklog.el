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

(defcustom org-roam-dailies-tasklog-include-body t
  "Whether to include task body content in log entries."
  :type 'boolean
  :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-include-logbook t
  "Whether to include LOGBOOK drawer in log entries."
  :type 'boolean
  :group 'org-roam-dailies-tasklog)

(defcustom org-roam-dailies-tasklog-log-clock-in t
  "Whether to log clock-in events."
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
           (content-end (progn (org-end-of-subtree t) (point)))
           (content (buffer-substring-no-properties content-start content-end)))
      (list (cons 'heading heading)
            (cons 'todo-state todo-state)
            (cons 'content content)))))

(defun org-roam-dailies-tasklog--format-log-entry (task-info event-type duration)
  "Format TASK-INFO as a log entry for EVENT-TYPE with optional DURATION.
TASK-INFO is an alist containing task information.
EVENT-TYPE is a string describing the event (e.g., \"CLOCKED IN\", \"DONE\").
DURATION is an optional string describing time spent (e.g., \"0:30\")."
  (let* ((time-str (format-time-string org-roam-dailies-tasklog-time-format))
         (heading (alist-get 'heading task-info))
         (state (or event-type (alist-get 'todo-state task-info)))
         (tags (alist-get 'tags task-info))
         (properties (alist-get 'properties task-info))
         (body (alist-get 'body task-info))
         (logbook (alist-get 'logbook task-info))
         (file-path (alist-get 'file-path task-info)))
    (concat
     ;; Heading line
     (format "* %s: %s %s%s\n"
             time-str
             state
             heading
             (if duration (format " (%s)" duration) ""))
     ;; Properties drawer
     ":PROPERTIES:\n"
     ;; Add original file path
     (when file-path
       (format ":ORIGINAL_FILE: %s\n" file-path))
     ;; Add tags as a property if present
     (when tags
       (format ":TAGS: %s\n" (mapconcat #'identity tags " ")))
     ;; Add other properties, filtering out standard ones that are redundant
     (mapconcat
      (lambda (prop)
        (let ((key (car prop))
              (val (cdr prop)))
          ;; Filter out properties that are redundant or internal
          (unless (member key '("CATEGORY" "BLOCKED" "FILE" "ITEM" "TODO"))
            (format ":%s: %s" key val))))
      properties
      "\n")
     "\n:END:\n"
     ;; Logbook drawer if present
     (when (and logbook (not (string-empty-p logbook)))
       (concat ":LOGBOOK:\n" logbook "\n:END:\n"))
     ;; Body content if present
     (when (and body (not (string-empty-p body)))
       (concat body "\n")))))

(defun org-roam-dailies-tasklog--append-to-daily (formatted-entry)
  "Append FORMATTED-ENTRY to today's org-roam daily note.
Opens the daily note file, appends the entry to the end, and saves.
Manages buffer state to avoid disrupting the user's window layout."
  (let* ((daily-file (org-roam-dailies-tasklog--get-daily-note-path))
         (buffer-was-open (find-buffer-visiting daily-file)))
    (org-roam-dailies-tasklog--debug "Appending entry to: %s" daily-file)
    (with-current-buffer (find-file-noselect daily-file)
      (save-excursion
        (goto-char (point-max))
        ;; Ensure proper spacing
        (unless (bolp)
          (insert "\n"))
        ;; Add a blank line before the entry if the file isn't empty
        (when (> (point-max) 1)
          (insert "\n"))
        (insert formatted-entry))
      (save-buffer)
      (org-roam-dailies-tasklog--debug "Entry appended successfully")
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
        (let ((task-info (org-roam-dailies-tasklog--extract-task-info)))
          (when task-info
            (let ((formatted-entry
                   (org-roam-dailies-tasklog--format-log-entry
                    task-info event-type duration)))
              (org-roam-dailies-tasklog--append-to-daily formatted-entry)
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
