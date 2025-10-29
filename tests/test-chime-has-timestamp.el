;;; test-chime-has-timestamp.el --- Tests for chime--has-timestamp -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Craig Jennings

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

;; Unit tests for chime--has-timestamp function.
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

(defun test-chime-has-timestamp-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-has-timestamp-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-has-timestamp-standard-timestamp-with-time-returns-non-nil ()
  "Test that standard timestamp with time returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (timestamp (test-timestamp-string time))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-timestamp-without-brackets-returns-non-nil ()
  "Test that timestamp without brackets but with time returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (timestamp (format-time-string "%Y-%m-%d %a %H:%M" time))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-timestamp-with-time-range-returns-non-nil ()
  "Test that timestamp with time range returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (format-time-string "<%Y-%m-%d %a %H:%M-15:30>" time))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-scheduled-with-time-returns-non-nil ()
  "Test that SCHEDULED timestamp with time returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 9 0))
             (timestamp (concat "SCHEDULED: " (test-timestamp-string time)))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-deadline-with-time-returns-non-nil ()
  "Test that DEADLINE timestamp with time returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 17 0))
             (timestamp (concat "DEADLINE: " (test-timestamp-string time)))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-repeater-with-time-returns-non-nil ()
  "Test that timestamp with repeater and time returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (format-time-string "<%Y-%m-%d %a %H:%M +1w>" time))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-midnight-timestamp-returns-non-nil ()
  "Test that midnight timestamp (00:00) returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 0 0))
             (timestamp (test-timestamp-string time))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-has-timestamp-day-wide-timestamp-returns-nil ()
  "Test that day-wide timestamp without time returns nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 0 0))
             (timestamp (test-timestamp-string time t))
             (result (chime--has-timestamp timestamp)))
        (should-not result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-date-only-returns-nil ()
  "Test that date-only timestamp without day name returns nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 0 0))
             (timestamp (format-time-string "<%Y-%m-%d>" time))
             (result (chime--has-timestamp timestamp)))
        (should-not result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-single-digit-hour-returns-non-nil ()
  "Test that timestamp with single-digit hour returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 9 0))
             (timestamp (format-time-string "<%Y-%m-%d %a %-H:%M>" time))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-embedded-in-text-returns-non-nil ()
  "Test that timestamp embedded in text returns non-nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (concat "Meeting scheduled for " (test-timestamp-string time) " in conference room"))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-multiple-timestamps-returns-non-nil ()
  "Test that string with multiple timestamps returns non-nil for first match.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time1 (test-time-tomorrow-at 14 0))
             (time2 (test-time-days-from-now 2))
             (timestamp (concat (test-timestamp-string time1) " and "
                                (format-time-string "<%Y-%m-%d %a %H:%M>" time2)))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

;;; Error Cases

(ert-deftest test-chime-has-timestamp-empty-string-returns-nil ()
  "Test that empty string returns nil."
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((timestamp "")
             (result (chime--has-timestamp timestamp)))
        (should-not result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-nil-input-returns-nil ()
  "Test that nil input returns nil."
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((timestamp nil)
             (result (chime--has-timestamp timestamp)))
        (should-not result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-no-timestamp-returns-nil ()
  "Test that string without timestamp returns nil."
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((timestamp "Just a regular string with no timestamp")
             (result (chime--has-timestamp timestamp)))
        (should-not result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-invalid-format-returns-nil ()
  "Test that invalid timestamp format returns nil.

REFACTORED: Uses dynamic timestamps (keeps invalid format for testing)"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             ;; Intentionally wrong format (MM-DD-YYYY instead of YYYY-MM-DD) for testing
             (timestamp (format-time-string "<%m-%d-%Y %a %H:%M>" time))
             (result (chime--has-timestamp timestamp)))
        (should-not result))
    (test-chime-has-timestamp-teardown)))

(ert-deftest test-chime-has-timestamp-partial-timestamp-returns-nil ()
  "Test that partial timestamp returns nil.

REFACTORED: Uses dynamic timestamps (keeps partial format for testing)"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 0 0))
             ;; Intentionally incomplete timestamp for testing
             (timestamp (format-time-string "<%Y-%m-%d" time))
             (result (chime--has-timestamp timestamp)))
        (should-not result))
    (test-chime-has-timestamp-teardown)))

;;; org-gcal Integration Tests

(ert-deftest test-chime-has-timestamp-org-gcal-time-range-returns-non-nil ()
  "Test that org-gcal time range format is detected.
org-gcal uses format like <2025-10-24 Fri 17:30-18:00> which should be detected.

REFACTORED: Uses dynamic timestamps"
  (test-chime-has-timestamp-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 17 30))
             (timestamp (format-time-string "<%Y-%m-%d %a %H:%M-18:00>" time))
             (result (chime--has-timestamp timestamp)))
        (should result))
    (test-chime-has-timestamp-teardown)))

(provide 'test-chime-has-timestamp)
;;; test-chime-has-timestamp.el ends here
