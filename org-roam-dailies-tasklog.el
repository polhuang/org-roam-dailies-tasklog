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

(provide 'org-roam-dailies-tasklog)
;;; org-roam-dailies-tasklog.el ends here
