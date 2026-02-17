;;; test-chime-format-event-for-tooltip.el --- Tests for chime--format-event-for-tooltip -*- lexical-binding: t; -*-

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

;; Unit tests for chime--format-event-for-tooltip function.
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

(defun test-chime-format-event-for-tooltip-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-format-event-for-tooltip-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-format-event-for-tooltip-normal-minutes ()
  "Test formatting event with minutes until event.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 10))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      10
                      "Team Meeting")))
        (should (stringp result))
        (should (string-match-p "Team Meeting" result))
        (should (string-match-p "02:10 PM" result))
        (should (string-match-p "10 minutes" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-normal-hours ()
  "Test formatting event with hours until event.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 15 30))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      90
                      "Afternoon Meeting")))
        (should (stringp result))
        (should (string-match-p "Afternoon Meeting" result))
        (should (string-match-p "03:30 PM" result))
        (should (string-match-p "1 hour" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-normal-multiple-hours ()
  "Test formatting event with multiple hours until event.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 17 0))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      300
                      "End of Day Review")))
        (should (stringp result))
        (should (string-match-p "End of Day Review" result))
        (should (string-match-p "05:00 PM" result))
        (should (string-match-p "5 hours" result)))
    (test-chime-format-event-for-tooltip-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-format-event-for-tooltip-boundary-exactly-one-day ()
  "Test formatting event exactly 1 day away (1440 minutes).

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-days-from-now 1 9 0))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      1440
                      "Tomorrow Event")))
        (should (stringp result))
        (should (string-match-p "Tomorrow Event" result))
        (should (string-match-p "09:00 AM" result))
        (should (string-match-p "in 1 day" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-boundary-multiple-days ()
  "Test formatting event multiple days away.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-days-from-now 3 10 0))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      4320  ; 3 days
                      "Future Meeting")))
        (should (stringp result))
        (should (string-match-p "Future Meeting" result))
        (should (string-match-p "10:00 AM" result))
        (should (string-match-p "in 3 days" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-boundary-just-under-one-day ()
  "Test formatting event just under 1 day away (1439 minutes).

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 8 59))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      1439
                      "Almost Tomorrow")))
        (should (stringp result))
        (should (string-match-p "Almost Tomorrow" result))
        (should (string-match-p "08:59 AM" result))
        ;; Should show hours/minutes, not days
        (should (string-match-p "23 hours" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-boundary-zero-minutes ()
  "Test formatting event happening right now (0 minutes).

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-today-at 14 0))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      0
                      "Current Event")))
        (should (stringp result))
        (should (string-match-p "Current Event" result))
        (should (string-match-p "02:00 PM" result))
        (should (string-match-p "right now" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-boundary-one-minute ()
  "Test formatting event 1 minute away.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-today-at 14 1))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      1
                      "Imminent Event")))
        (should (stringp result))
        (should (string-match-p "Imminent Event" result))
        (should (string-match-p "02:01 PM" result))
        (should (string-match-p "1 minute" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-boundary-long-title ()
  "Test formatting event with very long title.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-today-at 14 10))
             (timestamp (test-timestamp-string time))
             (long-title (make-string 200 ?x))
             (result (chime--format-event-for-tooltip
                      timestamp
                      10
                      long-title)))
        (should (stringp result))
        (should (string-match-p long-title result)))
    (test-chime-format-event-for-tooltip-teardown)))

;;; Error Cases

(ert-deftest test-chime-format-event-for-tooltip-error-nil-title ()
  "Test formatting with nil title doesn't crash.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-today-at 14 10))
             (timestamp (test-timestamp-string time)))
        ;; Should not crash with nil title
        (should-not (condition-case nil
                        (progn
                          (chime--format-event-for-tooltip
                           timestamp
                           10
                           nil)
                          nil)
                      (error t))))
    (test-chime-format-event-for-tooltip-teardown)))

(ert-deftest test-chime-format-event-for-tooltip-error-empty-title ()
  "Test formatting with empty title.

REFACTORED: Uses dynamic timestamps"
  (test-chime-format-event-for-tooltip-setup)
  (unwind-protect
      (let* ((time (test-time-today-at 14 10))
             (timestamp (test-timestamp-string time))
             (result (chime--format-event-for-tooltip
                      timestamp
                      10
                      "")))
        (should (stringp result))
        (should (string-match-p "02:10 PM" result)))
    (test-chime-format-event-for-tooltip-teardown)))

(provide 'test-chime-format-event-for-tooltip)
;;; test-chime-format-event-for-tooltip.el ends here
