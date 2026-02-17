;;; testutil-time.el --- Time utilities for dynamic test timestamps -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;; Author: Craig Jennings <c@cjennings.net>

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

;; Utilities for generating dynamic timestamps in tests.
;; Tests should use relative time relationships (TODAY, TOMORROW, etc.)
;; instead of hardcoded dates to avoid test expiration.

;;; Code:

(require 'org)

;;; Core Time Generation

(defun test-time-now ()
  "Return a base 'now' time that's always valid.
Uses actual current time + 30 days to ensure tests remain valid.
Always returns 10:00 AM on that day for consistency."
  (let* ((now (current-time))
         (decoded (decode-time now))
         (future-time (time-add now (days-to-time 30))))
    ;; Set to 10:00 AM for consistency
    (encode-time 0 0 10
                 (decoded-time-day (decode-time future-time))
                 (decoded-time-month (decode-time future-time))
                 (decoded-time-year (decode-time future-time)))))

(defun test-time-at (days hours minutes)
  "Return time relative to test-time-now.
DAYS, HOURS, MINUTES can be positive (future) or negative (past).
Examples:
  (test-time-at 0 0 0)    ; NOW
  (test-time-at 0 2 0)    ; 2 hours from now
  (test-time-at -1 0 0)   ; Yesterday at same time
  (test-time-at 1 0 0)    ; Tomorrow at same time"
  (let* ((base (test-time-now))
         (seconds (+ (* days 86400)
                     (* hours 3600)
                     (* minutes 60))))
    (time-add base (seconds-to-time seconds))))

;;; Convenience Functions

(defun test-time-today-at (hour minute)
  "Return time for TODAY at HOUR:MINUTE.
Example: (test-time-today-at 14 30) ; Today at 2:30 PM"
  (let* ((base (test-time-now))
         (decoded (decode-time base)))
    (encode-time 0 minute hour
                 (decoded-time-day decoded)
                 (decoded-time-month decoded)
                 (decoded-time-year decoded))))

(defun test-time-yesterday-at (hour minute)
  "Return time for YESTERDAY at HOUR:MINUTE."
  (test-time-at -1 (- hour 10) minute))

(defun test-time-tomorrow-at (hour minute)
  "Return time for TOMORROW at HOUR:MINUTE."
  (test-time-at 1 (- hour 10) minute))

(defun test-time-days-ago (days &optional hour minute)
  "Return time for DAYS ago, optionally at HOUR:MINUTE.
If HOUR/MINUTE not provided, uses 10:00 AM."
  (let ((h (or hour 10))
        (m (or minute 0)))
    (test-time-at (- days) (- h 10) m)))

(defun test-time-days-from-now (days &optional hour minute)
  "Return time for DAYS from now, optionally at HOUR:MINUTE.
If HOUR/MINUTE not provided, uses 10:00 AM."
  (let ((h (or hour 10))
        (m (or minute 0)))
    (test-time-at days (- h 10) m)))

;;; Timestamp String Generation

(defun test-timestamp-string (time &optional all-day-p)
  "Convert Emacs TIME to org timestamp string.
If ALL-DAY-P is non-nil, omit time component: <2025-10-24 Thu>
Otherwise include time: <2025-10-24 Thu 14:00>

Correctly calculates day-of-week name to match the date."
  (let* ((decoded (decode-time time))
         (year (decoded-time-year decoded))
         (month (decoded-time-month decoded))
         (day (decoded-time-day decoded))
         (hour (decoded-time-hour decoded))
         (minute (decoded-time-minute decoded))
         (dow (decoded-time-weekday decoded))
         (day-names ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])
         (day-name (aref day-names dow)))
    (if all-day-p
        (format "<%04d-%02d-%02d %s>" year month day day-name)
      (format "<%04d-%02d-%02d %s %02d:%02d>" year month day day-name hour minute))))

(defun test-timestamp-range-string (start-time end-time)
  "Create range timestamp from START-TIME to END-TIME.
Example: <2025-10-24 Thu>--<2025-10-27 Sun>"
  (format "%s--%s"
          (test-timestamp-string start-time t)
          (test-timestamp-string end-time t)))

(defun test-timestamp-repeating (time repeater &optional all-day-p)
  "Add REPEATER to timestamp for TIME.
REPEATER should be like '+1w', '.+1d', '++1m'
Example: <2025-10-24 Thu +1w>"
  (let ((base-ts (test-timestamp-string time all-day-p)))
    ;; Remove closing > and add repeater
    (concat (substring base-ts 0 -1) " " repeater ">")))

;;; Mock Helpers

(defmacro with-test-time (base-time &rest body)
  "Execute BODY with mocked current-time returning BASE-TIME.
BASE-TIME can be generated with test-time-* functions.

Example:
  (with-test-time (test-time-now)
    (do-something-that-uses-current-time))"
  `(cl-letf (((symbol-function 'current-time)
              (lambda () ,base-time)))
     ,@body))

(provide 'testutil-time)
;;; testutil-time.el ends here
