;;; test-chime-timestamp-parse.el --- Tests for chime--timestamp-parse -*- lexical-binding: t; -*-

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

;; Unit tests for chime--timestamp-parse function.
;; Tests cover normal cases, boundary cases, and error cases.

;;; Code:

;; Initialize package system for batch mode
(when noninteractive
  (package-initialize))

(require 'ert)

;; Load dependencies required by chime
(require 'dash)
(require 'alert)
(require 'async)
(require 'org-agenda)

;; Load chime from parent directory
(load (expand-file-name "../chime.el") nil t)

;; Load test utilities
(require 'testutil-general (expand-file-name "testutil-general.el"))
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Setup and Teardown

(defun test-chime-timestamp-parse-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-timestamp-parse-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-timestamp-parse-standard-timestamp-returns-time-list ()
  "Test that a standard timestamp with time component returns a time list.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        ;; Should return a time list (list of integers)
        (should (listp result))
        (should (= (length result) 2))
        (should (integerp (car result)))
        (should (integerp (cadr result)))
        ;; Result should not be nil
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-scheduled-timestamp-returns-time-list ()
  "Test that a SCHEDULED timestamp parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 9 0))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-deadline-timestamp-returns-time-list ()
  "Test that a DEADLINE timestamp parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 17 0))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-timestamp-with-weekly-repeater-returns-time-list ()
  "Test that a timestamp with +1w repeater parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (format-time-string "<%Y-%m-%d %a %H:%M +1w>" time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-timestamp-with-completion-repeater-returns-time-list ()
  "Test that a timestamp with .+1d repeater parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 8 0))
             (timestamp (format-time-string "<%Y-%m-%d %a %H:%M .+1d>" time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-timestamp-with-catchup-repeater-returns-time-list ()
  "Test that a timestamp with ++1w repeater parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 10 30))
             (timestamp (format-time-string "<%Y-%m-%d %a %H:%M ++1w>" time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-timestamp-with-time-range-returns-start-time ()
  "Test that a timestamp with time range returns the start time.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (format-time-string "<%Y-%m-%d %a %H:%M-15:30>" time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-timestamp-with-date-range-returns-start-date ()
  "Test that a timestamp with date range returns start date time.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time1 (test-time-tomorrow-at 10 0))
             (time2 (test-time-days-from-now 2))
             (timestamp (concat (test-timestamp-string time1) "--"
                                (format-time-string "<%Y-%m-%d %a %H:%M>" time2)))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-timestamp-parse-midnight-timestamp-returns-time-list ()
  "Test that midnight (00:00) timestamp parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 0 0))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-last-minute-of-day-returns-time-list ()
  "Test that last minute of day (23:59) timestamp parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 23 59))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-year-boundary-new-years-eve-returns-time-list ()
  "Test that New Year's Eve timestamp parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (decoded (decode-time now))
             (year (nth 5 decoded))
             ;; Create Dec 31 at 23:30 for current test year
             (time (encode-time 0 30 23 31 12 year))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-year-boundary-new-years-day-returns-time-list ()
  "Test that New Year's Day timestamp parses correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (decoded (decode-time now))
             (year (1+ (nth 5 decoded)))  ; Next year
             ;; Create Jan 1 at 00:30 for next test year
             (time (encode-time 0 30 0 1 1 year))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-single-digit-time-returns-time-list ()
  "Test that single-digit hours and minutes parse correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 1 5))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-leap-year-feb-29-returns-time-list ()
  "Test that Feb 29 in leap year parses correctly.

REFACTORED: Uses dynamic timestamps (2024 leap year)"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* (;; Use 2024 as a known leap year
             (time (encode-time 0 0 14 29 2 2024))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-month-boundary-end-of-month-returns-time-list ()
  "Test that end of month timestamp parses correctly.

REFACTORED: Uses dynamic timestamps (Oct 31)"
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (decoded (decode-time now))
             (year (nth 5 decoded))
             ;; Create Oct 31 at 14:00 for current test year
             (time (encode-time 0 0 14 31 10 year))
             (timestamp (test-timestamp-string time))
             (result (chime--timestamp-parse timestamp)))
        (should (listp result))
        (should (= (length result) 2))
        (should result))
    (test-chime-timestamp-parse-teardown)))

;;; Bug Reproduction Tests

(ert-deftest test-chime-timestamp-parse-tomorrow-timestamp-returns-correct-date ()
  "Test that a tomorrow timestamp is parsed as tomorrow, not today.
This reproduces the bug where timestamps like <2025-11-03 Mon 10:00-10:30>
on Nov 02 are incorrectly grouped as 'Today' instead of 'Tomorrow'."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (with-test-time (encode-time 0 23 11 2 11 2025) ; Nov 02, 2025 11:23:00 AM
        (let* ((tomorrow-timestamp "<2025-11-03 Mon 10:00-10:30>")
               (parsed (chime--timestamp-parse tomorrow-timestamp))
               (now (current-time)))
          ;; Should parse successfully
          (should parsed)
          ;; Convert parsed time (HIGH LOW) to full time by appending (0 0)
          (let* ((parsed-time (append parsed '(0 0)))
                 (parsed-decoded (decode-time parsed-time))
                 (time-diff-seconds (- (time-to-seconds parsed-time)
                                      (time-to-seconds now))))
            ;; Verify the parsed date is Nov 03, 2025 (not Nov 02!)
            (should (= 3 (decoded-time-day parsed-decoded)))
            (should (= 11 (decoded-time-month parsed-decoded)))
            (should (= 2025 (decoded-time-year parsed-decoded)))
            ;; Verify the parsed time is 10:00
            (should (= 10 (decoded-time-hour parsed-decoded)))
            (should (= 0 (decoded-time-minute parsed-decoded)))
            ;; Time difference should be ~22h 37m (81420 seconds)
            (should (> time-diff-seconds 81360))  ; At least 22h 36m
            (should (< time-diff-seconds 81480)))))  ; At most 22h 38m
    (test-chime-timestamp-parse-teardown)))

;;; Error Cases

(ert-deftest test-chime-timestamp-parse-empty-string-returns-nil ()
  "Test that empty string returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-nil-input-returns-nil ()
  "Test that nil input returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp nil)
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-missing-opening-bracket-returns-nil ()
  "Test that timestamp missing opening bracket returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "2025-10-24 Fri 14:00>")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-missing-closing-bracket-returns-nil ()
  "Test that timestamp missing closing bracket returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "<2025-10-24 Fri 14:00")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-invalid-date-format-returns-nil ()
  "Test that invalid date format returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "<10-24-2025 Fri 14:00>")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-invalid-month-returns-nil ()
  "Test that invalid month value returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "<2025-13-24 Fri 14:00>")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-invalid-day-returns-nil ()
  "Test that invalid day value returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "<2025-10-32 Fri 14:00>")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-invalid-time-hour-returns-nil ()
  "Test that invalid hour value returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "<2025-10-24 Fri 25:00>")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-invalid-time-minute-returns-nil ()
  "Test that invalid minute value returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "<2025-10-24 Fri 14:60>")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(ert-deftest test-chime-timestamp-parse-date-only-no-time-returns-nil ()
  "Test that day-wide timestamp without time returns nil."
  (test-chime-timestamp-parse-setup)
  (unwind-protect
      (let* ((timestamp "<2025-10-24 Fri>")
             (result (chime--timestamp-parse timestamp)))
        (should (null result)))
    (test-chime-timestamp-parse-teardown)))

(provide 'test-chime-timestamp-parse)
;;; test-chime-timestamp-parse.el ends here
