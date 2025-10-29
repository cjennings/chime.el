;;; chime-debug.el --- Debug functions for chime.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

;; Author: Craig Jennings <c@cjennings.net>
;; Keywords: notification alert org org-agenda debug
;; URL: https://github.com/cjennings/chime.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains debug functions for troubleshooting chime.el behavior.
;; It is only loaded when `chime-debug' is non-nil.
;;
;; Enable with:
;;   (setq chime-debug t)
;;   (require 'chime)
;;
;; Available debug functions:
;; - `chime--debug-dump-events' - Show all stored upcoming events
;; - `chime--debug-dump-tooltip' - Show tooltip content
;; - `chime--debug-config' - Show complete configuration dump
;;
;; All functions write to *Messages* buffer without cluttering echo area.

;;; Code:

;; chime-debug.el is loaded by chime.el, so chime is already loaded
;; No need for (require 'chime) here

;;;###autoload
(defun chime--debug-dump-events ()
  "Dump all upcoming events to *Messages* buffer for debugging.
Shows events stored in `chime--upcoming-events' with their times and titles."
  (interactive)
  (if (not chime--upcoming-events)
      (message "Chime: No upcoming events stored")
    (chime--log-silently "=== Chime Debug: Upcoming Events (%d total) ==="
                         (length chime--upcoming-events))
    (let ((grouped (chime--group-events-by-day chime--upcoming-events)))
      (dolist (day-group grouped)
        (let ((day-label (car day-group))
              (events (cdr day-group)))
          (chime--log-silently "\n%s:" day-label)
          (dolist (event-item events)
            (let* ((event (car event-item))
                   (time-info (nth 1 event-item))
                   (minutes (nth 2 event-item))
                   (title (cdr (assoc 'title event)))
                   (timestamp-str (car time-info)))
              (chime--log-silently "  [%s] %s (%s)"
                                   timestamp-str
                                   title
                                   (chime--time-left (* minutes 60))))))))
    (chime--log-silently "=== End Chime Debug ===\n")
    (message "Dumped %d events to *Messages* buffer" (length chime--upcoming-events))))

;;;###autoload
(defun chime--debug-dump-tooltip ()
  "Dump current tooltip content to *Messages* buffer for debugging.
Shows the tooltip text that would appear when hovering over the modeline."
  (interactive)
  (if (not chime--upcoming-events)
      (message "Chime: No upcoming events stored")
    (let ((tooltip-text (chime--make-tooltip chime--upcoming-events)))
      (if (not tooltip-text)
          (message "Chime: No tooltip content available")
        (chime--log-silently "=== Chime Debug: Tooltip Content ===")
        (chime--log-silently "%s" tooltip-text)
        (chime--log-silently "=== End Chime Debug ===\n")
        (message "Dumped tooltip content to *Messages* buffer")))))

;;;###autoload
(defun chime--debug-config ()
  "Dump chime configuration and status to *Messages* buffer.
Shows all relevant settings, agenda files, and current state."
  (interactive)
  (chime--log-silently "=== Chime Debug: Configuration ===")
  (chime--log-silently "Mode enabled: %s" chime-mode)
  (chime--log-silently "Process running: %s" (process-live-p chime--process))
  (chime--log-silently "Last check: %s"
                       (if chime--last-check-time
                           (format-time-string "%Y-%m-%d %H:%M:%S" chime--last-check-time)
                         "never"))
  (chime--log-silently "\nModeline settings:")
  (chime--log-silently "  chime-enable-modeline: %s" chime-enable-modeline)
  (chime--log-silently "  chime-modeline-lookahead-minutes: %s" chime-modeline-lookahead-minutes)
  (chime--log-silently "  chime-modeline-string: %s"
                       (if chime-modeline-string
                           (format "\"%s\"" chime-modeline-string)
                         "nil"))
  (chime--log-silently "\nNotification settings:")
  (chime--log-silently "  chime-alert-time: %s" chime-alert-time)
  (chime--log-silently "  chime-notification-title: %s" chime-notification-title)
  (chime--log-silently "  chime-alert-severity: %s" chime-alert-severity)
  (chime--log-silently "\nFilters:")
  (chime--log-silently "  chime-keyword-blacklist: %s" chime-keyword-blacklist)
  (chime--log-silently "  chime-keyword-whitelist: %s" chime-keyword-whitelist)
  (chime--log-silently "  chime-tags-blacklist: %s" chime-tags-blacklist)
  (chime--log-silently "  chime-tags-whitelist: %s" chime-tags-whitelist)
  (chime--log-silently "\nOrg agenda files (%d):" (length org-agenda-files))
  (dolist (file org-agenda-files)
    (chime--log-silently "  - %s %s"
                         file
                         (if (file-exists-p file) "" "[MISSING]")))
  (chime--log-silently "\nStored events: %s"
                       (if chime--upcoming-events
                           (format "%d" (length chime--upcoming-events))
                         "none"))
  (chime--log-silently "=== End Chime Debug ===\n")
  (message "Chime: Configuration dumped to *Messages* buffer"))

(provide 'chime-debug)
;;; chime-debug.el ends here
