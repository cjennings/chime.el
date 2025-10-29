;;; test-chime-overdue-todos.el --- Tests for overdue TODO functionality -*- lexical-binding: t; -*-

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

;; Tests for overdue TODO functionality controlled by
;; `chime-show-any-overdue-with-day-wide-alerts'.
;;
;; When enabled (default t): Show overdue TODO items with day-wide alerts
;; When disabled (nil): Only show today's all-day events, not overdue items
;;
;; "Overdue" means events with timestamps in the past (before today).

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

;;; Test Helper Functions

(defun test-overdue--create-event (title timestamp has-time)
  "Create test event with TITLE and TIMESTAMP.
HAS-TIME determines if timestamp has time component."
  (let* ((parsed-time (when has-time
                        (apply 'encode-time (org-parse-time-string timestamp))))
         (times (list (cons timestamp parsed-time))))
    `((title . ,title)
      (times . ,times)
      (intervals . (10)))))

;;; Setup and Teardown

(defun test-chime-overdue-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-overdue-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Tests for chime-event-has-any-passed-time

(ert-deftest test-overdue-has-passed-time-yesterday-all-day ()
  "Test that all-day event from yesterday is recognized as passed."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((event (test-overdue--create-event
                    "Yesterday Event"
                    "<2025-10-27 Mon>"
                    nil)))  ; all-day event
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))  ; Oct 28
          (should (chime-event-has-any-passed-time event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-has-passed-time-today-all-day ()
  "Test that all-day event from today is recognized as passed.

TIME RELATIONSHIPS:
  Current time: TODAY at 10:00 AM
  Event: TODAY (all-day, no specific time)

DAY-OF-WEEK REQUIREMENTS:
  None - any day of week works

SPECIAL PROPERTIES:
  - All-day event: Yes (no time component)
  - Timed event: No
  - Repeating: No
  - Range: No

EXPECTED BEHAVIOR:
  chime-event-has-any-passed-time should return t because the event
  date (today) is not in the future.

CURRENT IMPLEMENTATION (as of 2025-10-28):
  Mock current-time: 2025-10-28 10:00
  Event timestamp: <2025-10-28 Tue>

REFACTORING NOTES:
  Simple case - just needs TODAY timestamp and TODAY current-time.

REFACTORED: Uses dynamic timestamps via testutil-time.el"
  (test-chime-overdue-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (today-timestamp (test-timestamp-string now t))
             (event (test-overdue--create-event
                     "Today Event"
                     today-timestamp
                     nil)))  ; all-day event
        (with-test-time now
          (should (chime-event-has-any-passed-time event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-has-passed-time-tomorrow-all-day ()
  "Test that all-day event from tomorrow is NOT recognized as passed."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((event (test-overdue--create-event
                    "Tomorrow Event"
                    "<2025-10-29 Wed>"
                    nil)))  ; all-day event
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))  ; Oct 28
          (should-not (chime-event-has-any-passed-time event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-has-passed-time-timed-event-past ()
  "Test that timed event in the past is recognized as passed."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((event (test-overdue--create-event
                    "Past Meeting"
                    "<2025-10-28 Tue 09:00>"
                    t)))  ; timed event
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 14 28 10 2025))))  ; Oct 28, 14:00
          (should (chime-event-has-any-passed-time event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-has-passed-time-timed-event-future ()
  "Test that timed event in the future is NOT recognized as passed."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((event (test-overdue--create-event
                    "Future Meeting"
                    "<2025-10-28 Tue 16:00>"
                    t)))  ; timed event
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 14 28 10 2025))))  ; Oct 28, 14:00
          (should-not (chime-event-has-any-passed-time event))))
    (test-chime-overdue-teardown)))

;;; Tests for chime-display-as-day-wide-event with overdue setting

(ert-deftest test-overdue-display-yesterday-all-day-with-overdue-enabled ()
  "Test that yesterday's all-day event is displayed when overdue is enabled."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-show-any-overdue-with-day-wide-alerts t)
            (chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Yesterday Birthday"
                    "<2025-10-27 Mon>"
                    nil)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          (should (chime-display-as-day-wide-event event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-display-yesterday-all-day-with-overdue-disabled ()
  "Test that yesterday's all-day event is NOT displayed when overdue is disabled.
This prevents showing old birthdays/holidays from the past."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-show-any-overdue-with-day-wide-alerts nil)
            (chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Yesterday Birthday"
                    "<2025-10-27 Mon>"
                    nil)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          (should-not (chime-display-as-day-wide-event event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-display-yesterday-timed-with-overdue-enabled ()
  "Test that yesterday's timed event is displayed when overdue is enabled."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-show-any-overdue-with-day-wide-alerts t)
            (chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Yesterday Meeting"
                    "<2025-10-27 Mon 14:00>"
                    t)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          (should (chime-display-as-day-wide-event event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-display-yesterday-timed-with-overdue-disabled ()
  "Test that yesterday's timed event is NOT displayed when overdue is disabled."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-show-any-overdue-with-day-wide-alerts nil)
            (chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Yesterday Meeting"
                    "<2025-10-27 Mon 14:00>"
                    t)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          (should-not (chime-display-as-day-wide-event event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-display-today-all-day-always-shown ()
  "Test that today's all-day event is always displayed regardless of overdue setting."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Today Birthday"
                    "<2025-10-28 Tue>"
                    nil)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          ;; Should show with overdue enabled
          (let ((chime-show-any-overdue-with-day-wide-alerts t))
            (should (chime-display-as-day-wide-event event)))
          ;; Should also show with overdue disabled (it's today, not overdue)
          (let ((chime-show-any-overdue-with-day-wide-alerts nil))
            (should (chime-display-as-day-wide-event event)))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-display-week-old-all-day-with-overdue-enabled ()
  "Test that week-old all-day event is displayed when overdue is enabled."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-show-any-overdue-with-day-wide-alerts t)
            (chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Week Old Event"
                    "<2025-10-21 Tue>"
                    nil)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          (should (chime-display-as-day-wide-event event))))
    (test-chime-overdue-teardown)))

(ert-deftest test-overdue-display-week-old-all-day-with-overdue-disabled ()
  "Test that week-old all-day event is NOT displayed when overdue is disabled.
This prevents showing old birthdays/holidays from past weeks."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-show-any-overdue-with-day-wide-alerts nil)
            (chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Week Old Event"
                    "<2025-10-21 Tue>"
                    nil)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          (should-not (chime-display-as-day-wide-event event))))
    (test-chime-overdue-teardown)))

;;; Tests verifying overdue doesn't affect future events

(ert-deftest test-overdue-future-event-not-affected-by-overdue-setting ()
  "Test that future events are not affected by overdue setting."
  (test-chime-overdue-setup)
  (unwind-protect
      (let ((chime-day-wide-advance-notice nil)
            (event (test-overdue--create-event
                    "Future Event"
                    "<2025-10-30 Thu>"
                    nil)))
        (cl-letf (((symbol-function 'current-time)
                   (lambda () (encode-time 0 0 10 28 10 2025))))
          ;; Should NOT show with overdue enabled (it's future, not today)
          (let ((chime-show-any-overdue-with-day-wide-alerts t))
            (should-not (chime-display-as-day-wide-event event)))
          ;; Should NOT show with overdue disabled (it's future, not today)
          (let ((chime-show-any-overdue-with-day-wide-alerts nil))
            (should-not (chime-display-as-day-wide-event event)))))
    (test-chime-overdue-teardown)))

(provide 'test-chime-overdue-todos)
;;; test-chime-overdue-todos.el ends here
