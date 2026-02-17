;;; chime-debug.el --- Debug functions for chime.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Craig Jennings

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
  (chime--log-silently "  chime-alert-intervals: %s" chime-alert-intervals)
  (chime--log-silently "  chime-notification-title: %s" chime-notification-title)
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
  (require \\='chime)
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

;;; Async Process Debugging

(defvar chime--debug-async-start-time nil
  "Time when the last async check started.")

(defvar chime--debug-async-check-count 0
  "Number of async checks performed since Emacs started.")

(defvar chime--debug-async-failures 0
  "Number of async check failures since Emacs started.")

(defvar chime--debug-async-timeout-threshold 30
  "Warn if async process takes longer than this many seconds.")

(defun chime--debug-log-async-start ()
  "Log when an async check starts."
  (setq chime--debug-async-check-count (1+ chime--debug-async-check-count))
  (setq chime--debug-async-start-time (current-time))
  (chime--log-silently "[Chime Async #%d] Starting event check at %s"
                       chime--debug-async-check-count
                       (format-time-string "%H:%M:%S" chime--debug-async-start-time)))

(defun chime--debug-log-async-complete (events)
  "Log when an async check completes successfully.
EVENTS is the list of events returned."
  (when chime--debug-async-start-time
    (let* ((duration (float-time (time-subtract (current-time)
                                                chime--debug-async-start-time)))
           (event-count (length events)))
      (chime--log-silently "[Chime Async #%d] Completed in %.2fs - found %d event%s"
                           chime--debug-async-check-count
                           duration
                           event-count
                           (if (= event-count 1) "" "s"))
      (when (> duration chime--debug-async-timeout-threshold)
        (chime--log-silently "[Chime Async #%d] WARNING: Slow async process (%.2fs)"
                             chime--debug-async-check-count
                             duration)
        (message "Chime: Slow event check took %.2fs" duration))
      (setq chime--debug-async-start-time nil))))

(defun chime--debug-log-async-error (error-data)
  "Log when an async check fails with an error.
ERROR-DATA is the error information from the async process."
  (setq chime--debug-async-failures (1+ chime--debug-async-failures))
  (chime--log-silently "[Chime Async #%d] ERROR: %s"
                       chime--debug-async-check-count
                       (prin1-to-string error-data))
  (message "Chime: Event check failed - see *Messages* for details")
  (setq chime--debug-async-start-time nil))

;;;###autoload
(defun chime--debug-show-async-stats ()
  "Show statistics about async process performance."
  (interactive)
  (chime--log-silently "=== Chime Debug: Async Stats ===")
  (chime--log-silently "Total checks: %d" chime--debug-async-check-count)
  (chime--log-silently "Failures: %d" chime--debug-async-failures)
  (chime--log-silently "Success rate: %.1f%%"
                       (if (> chime--debug-async-check-count 0)
                           (* 100.0 (/ (float (- chime--debug-async-check-count
                                                chime--debug-async-failures))
                                      chime--debug-async-check-count))
                         0.0))
  (chime--log-silently "Currently running: %s"
                       (if (and chime--process (process-live-p chime--process))
                           (format "yes (started %s)"
                                  (if chime--debug-async-start-time
                                      (format "%.1fs ago"
                                             (float-time (time-subtract (current-time)
                                                                       chime--debug-async-start-time)))
                                    "unknown"))
                         "no"))
  (chime--log-silently "=== End Chime Debug ===\n")
  (message "Chime: Async stats dumped to *Messages*"))

;;; Feature/Package Loading Tracker

(defun chime--debug-show-loaded-features ()
  "Show which chime-related features and packages are currently loaded."
  (let ((chime-features '(org org-agenda org-duration org-contacts
                         dash alert async chime chime-debug))
        (loaded '())
        (not-loaded '()))
    (dolist (feature chime-features)
      (if (featurep feature)
          (push feature loaded)
        (push feature not-loaded)))
    (list :loaded (nreverse loaded)
          :not-loaded (nreverse not-loaded))))

;;;###autoload
(defun chime--debug-dump-loaded-features ()
  "Dump which chime-related features are currently loaded.
Useful for diagnosing lazy-loading issues."
  (interactive)
  (let ((result (chime--debug-show-loaded-features)))
    (chime--log-silently "=== Chime Debug: Loaded Features ===")
    (chime--log-silently "Loaded features:")
    (dolist (feature (plist-get result :loaded))
      (chime--log-silently "  ✓ %s" feature))
    (when (plist-get result :not-loaded)
      (chime--log-silently "\nNot loaded:")
      (dolist (feature (plist-get result :not-loaded))
        (chime--log-silently "  ✗ %s" feature)))
    (chime--log-silently "=== End Chime Debug ===\n")
    (message "Chime: Feature list dumped to *Messages*")))

;;;###autoload
(defun chime-debug-enable-async-monitoring ()
  "Enable comprehensive async process monitoring.
Logs every async check start, completion, and failure.
Warns about slow processes and tracks statistics."
  (interactive)
  ;; Add advice to chime--fetch-and-process to track async lifecycle
  (advice-add 'chime--fetch-and-process :before
              (lambda (&rest _)
                (chime--debug-log-async-start)))

  (chime--log-silently "Chime debug: Async process monitoring enabled")
  (message "Chime: Async monitoring enabled - use M-x chime--debug-show-async-stats"))

;;;###autoload
(defun chime-debug-disable-async-monitoring ()
  "Disable async process monitoring."
  (interactive)
  (advice-remove 'chime--fetch-and-process
                 (lambda (&rest _)
                   (chime--debug-log-async-start)))
  (chime--log-silently "Chime debug: Async process monitoring disabled")
  (message "Chime: Async monitoring disabled"))

;;;###autoload
(defun chime-debug-force-check ()
  "Force an immediate chime-check and dump diagnostics.
Shows loaded features before the check and logs async process details."
  (interactive)
  (chime--log-silently "\n=== Chime Debug: Forced Check ===")
  (chime--log-silently "Time: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))

  ;; Show what's loaded
  (let ((result (chime--debug-show-loaded-features)))
    (chime--log-silently "Loaded features: %s"
                         (mapconcat #'symbol-name (plist-get result :loaded) ", "))
    (when (plist-get result :not-loaded)
      (chime--log-silently "Not loaded: %s"
                           (mapconcat #'symbol-name (plist-get result :not-loaded) ", "))))

  ;; Show process state
  (chime--log-silently "Process alive: %s"
                       (if (and chime--process (process-live-p chime--process))
                           "yes (check already running)"
                         "no"))

  ;; Show org-agenda-files
  (chime--log-silently "Org agenda files: %d" (length org-agenda-files))
  (chime--log-silently "=== Starting Check ===\n")

  ;; Run the check
  (chime-check)

  (message "Chime: Forced check initiated - see *Messages* for details"))

;;; Notification Filtering Debugger

;;;###autoload
(defun chime--debug-notification-filtering (event-time interval)
  "Debug why a notification might not be generated for EVENT-TIME and INTERVAL.
EVENT-TIME should be an Emacs time value (from encode-time or current-time).
INTERVAL is the number of minutes before the event to notify.

This function traces through the entire notification filtering pipeline:
1. Timestamp string generation
2. Day-wide event filtering (chime--has-timestamp)
3. Table-flat combination generation
4. Timestamp-within-interval-p matching

Example usage:
  (chime--debug-notification-filtering
    (time-add (current-time) (seconds-to-time (* 4 24 3600)))  ; 4 days from now
    5760)  ; 4 days in minutes

Or for a specific date/time:
  (chime--debug-notification-filtering
    (encode-time 0 0 10 22 11 2025)  ; Nov 22, 2025 at 10:00
    10)  ; 10 minutes before"
  (interactive
   (let* ((days (read-number "Days from now: " 0))
          (hours (read-number "Additional hours: " 0))
          (minutes (read-number "Additional minutes: " 10))
          (total-seconds (+ (* days 86400) (* hours 3600) (* minutes 60)))
          (event-time (time-add (current-time) (seconds-to-time total-seconds)))
          (interval (read-number "Notification interval (minutes): " 10)))
     (list event-time interval)))

  (let* ((timestamp-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-time))
         (now (current-time))
         (event `((times . ((,timestamp-str . ,event-time)))
                  (title . "Debug Test Event")
                  (intervals . ((,interval . medium))))))

    (chime--log-silently "\n=== Chime Debug: Notification Filtering ===")
    (chime--log-silently "Current time: %s"
                         (format-time-string "%Y-%m-%d %H:%M:%S" now))
    (chime--log-silently "Event time:   %s"
                         (format-time-string "%Y-%m-%d %H:%M:%S" event-time))
    (chime--log-silently "Interval:     %d minutes" interval)
    (chime--log-silently "Timestamp:    %s" timestamp-str)

    ;; Calculate time differences
    (let* ((seconds-until (float-time (time-subtract event-time now)))
           (minutes-until (/ seconds-until 60.0))
           (hours-until (/ minutes-until 60.0))
           (days-until (/ hours-until 24.0)))
      (chime--log-silently "\nTime until event:")
      (chime--log-silently "  %.2f seconds (%.2f minutes, %.2f hours, %.2f days)"
                           seconds-until minutes-until hours-until days-until))

    ;; Step 1: Check timestamp recognition
    (chime--log-silently "\n--- Step 1: Timestamp Recognition ---")
    (let ((has-time (chime--has-timestamp timestamp-str)))
      (chime--log-silently "chime--has-timestamp: %s" has-time)
      (when (string-match org-ts-regexp0 timestamp-str)
        (chime--log-silently "org-ts-regexp0 matches: yes")
        (chime--log-silently "match-beginning 7 (time component): %s"
                             (match-beginning 7)))
      (unless has-time
        (chime--log-silently "⚠ WARNING: Timestamp not recognized as having time component!")
        (chime--log-silently "  This event would be filtered out by chime--filter-day-wide-events")))

    ;; Step 2: Check time equality
    (chime--log-silently "\n--- Step 2: Time Matching ---")
    (let* ((check-time (time-add now (seconds-to-time (* 60 interval))))
           (check-time-str (format-time-string "%Y-%m-%d %H:%M:%S" check-time))
           (event-time-str (format-time-string "%Y-%m-%d %H:%M:%S" event-time))
           (matches (chime--time= check-time event-time)))
      (chime--log-silently "Current time + interval: %s" check-time-str)
      (chime--log-silently "Event time:              %s" event-time-str)
      (chime--log-silently "chime--time= result: %s" matches)

      ;; Show the formatted strings that chime--time= compares
      (chime--log-silently "\nFormatted comparison strings:")
      (chime--log-silently "  Current + interval: %s"
                           (format-time-string "%Y-%m-%d %H:%M" check-time))
      (chime--log-silently "  Event time:         %s"
                           (format-time-string "%Y-%m-%d %H:%M" event-time))

      (unless matches
        (chime--log-silently "⚠ Times do not match - notification would NOT be generated")
        (let ((diff-seconds (abs (float-time (time-subtract check-time event-time)))))
          (chime--log-silently "  Time difference: %.0f seconds (%.2f minutes)"
                               diff-seconds (/ diff-seconds 60.0)))))

    ;; Step 3: Test chime--timestamp-within-interval-p
    (chime--log-silently "\n--- Step 3: chime--timestamp-within-interval-p ---")
    (let ((result (chime--timestamp-within-interval-p event-time interval)))
      (chime--log-silently "Result: %s" result)
      (if result
          (chime--log-silently "✓ Event is within notification interval")
        (chime--log-silently "✗ Event is NOT within notification interval")))

    ;; Step 4: Test chime--notifications
    (chime--log-silently "\n--- Step 4: chime--notifications (full pipeline) ---")
    (let* ((times (cdr (assoc 'times event)))
           (intervals (cdr (assoc 'intervals event)))
           (filtered-times (chime--filter-day-wide-events times))
           (result (chime--notifications event)))
      (chime--log-silently "Input times: %d" (length times))
      (chime--log-silently "After day-wide filter: %d" (length filtered-times))
      (chime--log-silently "Final notifications: %d" (length result))

      (if (> (length result) 0)
          (progn
            (chime--log-silently "✓ Notification WOULD be generated")
            (dolist (notif result)
              (chime--log-silently "  Message: %s" (car notif))
              (chime--log-silently "  Severity: %s" (cdr notif))))
        (chime--log-silently "✗ Notification would NOT be generated")
        (chime--log-silently "\nPossible reasons:")
        (when (= (length filtered-times) 0)
          (chime--log-silently "  - Timestamp filtered out as day-wide event"))
        (when (not (chime--timestamp-within-interval-p event-time interval))
          (chime--log-silently "  - Event not within %d-minute interval" interval))))

    (chime--log-silently "=== End Chime Debug ===\n")
    (message "Chime: Notification filtering debug complete - see *Messages*")))

(provide 'chime-debug)
;;; chime-debug.el ends here
