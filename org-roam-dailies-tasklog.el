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

(defun org-roam-dailies-tasklog--get-entry-body ()
  "Get the body text of the current org entry.
Returns the content below the heading, excluding property drawers and logbook."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (progn
                   (forward-line 1)
                   ;; Skip property drawer if present
                   (when (looking-at org-property-drawer-re)
                     (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                     (forward-line 1))
                   ;; Skip logbook drawer if present
                   (when (looking-at "^[ \t]*:LOGBOOK:[ \t]*$")
                     (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                     (forward-line 1))
                   (point)))
          (end (progn
                 (outline-next-heading)
                 (point))))
      (string-trim (buffer-substring-no-properties start end)))))

(defun org-roam-dailies-tasklog--get-logbook-content ()
  "Get the LOGBOOK drawer content for the current entry.
Returns nil if no LOGBOOK drawer exists."
  (save-excursion
    (org-back-to-heading t)
    (let ((limit (save-excursion (outline-next-heading) (point))))
      (when (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" limit t)
        (let ((start (point))
              (end (progn
                     (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)
                     (match-beginning 0))))
          (string-trim (buffer-substring-no-properties start end)))))))

(defun org-roam-dailies-tasklog--extract-task-info ()
  "Extract all relevant information from the current task.
Returns an alist with task information including heading, state, tags,
properties, body content, and logbook."
  (save-excursion
    (org-back-to-heading t)
    (let ((heading (org-get-heading t t t t))
          (todo-state (org-get-todo-state))
          (tags (org-get-tags))
          (properties (org-entry-properties nil 'all))
          (body (when org-roam-dailies-tasklog-include-body
                  (org-roam-dailies-tasklog--get-entry-body)))
          (logbook (when org-roam-dailies-tasklog-include-logbook
                     (org-roam-dailies-tasklog--get-logbook-content)))
          (file-path (buffer-file-name)))
      (list (cons 'heading heading)
            (cons 'todo-state todo-state)
            (cons 'tags tags)
            (cons 'properties properties)
            (cons 'body body)
            (cons 'logbook logbook)
            (cons 'file-path file-path)))))

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

(provide 'org-roam-dailies-tasklog)
;;; org-roam-dailies-tasklog.el ends here
