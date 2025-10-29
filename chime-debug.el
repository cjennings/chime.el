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
;; - `chime-debug-monitor-event-loading' - Monitor when events are first loaded
;;
;; All functions write to *Messages* buffer without cluttering echo area.
;;
;; The event loading monitor is particularly useful for diagnosing timing
;; issues where the modeline takes a while to populate after Emacs startup.
;; It will send a libnotify notification and log detailed timing information
;; when events are first loaded.

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

;;; Event Loading Monitor

(defvar chime--debug-startup-time nil
  "Time when Emacs finished starting, for measuring event load delay.")

(defvar chime--debug-first-load-notified nil
  "Whether we've already notified about the first event load.")

(defun chime--debug-notify-first-load ()
  "Send notification and log when events are first loaded.
This helps diagnose timing issues with event hydration after Emacs startup."
  (when (and chime--upcoming-events
             (not chime--debug-first-load-notified))
    (setq chime--debug-first-load-notified t)
    (let* ((now (current-time))
           (startup-delay (if chime--debug-startup-time
                              (float-time (time-subtract now chime--debug-startup-time))
                            nil))
           (event-count (length chime--upcoming-events))
           (first-event (car chime--upcoming-events))
           (first-title (when first-event
                          (cdr (assoc 'title (car first-event))))))
      ;; Log to *Messages*
      (chime--log-silently "=== Chime Debug: First Event Load ===")
      (chime--log-silently "Time: %s" (format-time-string "%Y-%m-%d %H:%M:%S" now))
      (when startup-delay
        (chime--log-silently "Delay after Emacs startup: %.2f seconds" startup-delay))
      (chime--log-silently "Events loaded: %d" event-count)
      (chime--log-silently "Modeline string: %s"
                           (if chime-modeline-string
                               (format "\"%s\"" chime-modeline-string)
                             "nil"))
      (when first-title
        (chime--log-silently "First event: %s" first-title))
      (chime--log-silently "=== End Chime Debug ===\n")

      ;; Send libnotify notification
      (let ((summary (format "Chime: Events Loaded (%d)" event-count))
            (body (if startup-delay
                      (format "Loaded %.2fs after startup\nFirst: %s"
                              startup-delay
                              (or first-title "Unknown"))
                    (format "First: %s" (or first-title "Unknown")))))
        (alert body :title summary :severity 'moderate)))))

;;;###autoload
(defun chime-debug-monitor-event-loading ()
  "Enable monitoring of event loading timing.
Logs to *Messages* and sends libnotify notification when events are first
loaded after Emacs startup. Useful for diagnosing hydration delays.

To enable:
  (setq chime-debug t)
  (require 'chime)
  (chime-debug-monitor-event-loading)"
  (interactive)
  ;; Record startup time
  (setq chime--debug-startup-time (current-time))
  (setq chime--debug-first-load-notified nil)

  ;; Add advice to chime--update-modeline to monitor when events are populated
  (advice-add 'chime--update-modeline :after
              (lambda (&rest _)
                (chime--debug-notify-first-load)))

  (chime--log-silently "Chime debug: Event loading monitor enabled")
  (chime--log-silently "  Startup time recorded: %s"
                       (format-time-string "%Y-%m-%d %H:%M:%S" chime--debug-startup-time))
  (message "Chime: Event loading monitor enabled"))

;;;###autoload
(defun chime-debug-stop-monitor-event-loading ()
  "Disable monitoring of event loading timing."
  (interactive)
  (advice-remove 'chime--update-modeline
                 (lambda (&rest _)
                   (chime--debug-notify-first-load)))
  (chime--log-silently "Chime debug: Event loading monitor disabled")
  (message "Chime: Event loading monitor disabled"))

(provide 'chime-debug)
;;; chime-debug.el ends here
