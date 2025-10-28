;;; test-chime-process-notifications.el --- Tests for chime--process-notifications -*- lexical-binding: t; -*-

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

;; Unit tests for chime--process-notifications function.
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

;;; Setup and Teardown

(defun test-chime-process-notifications-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-process-notifications-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-process-notifications-normal-single-event-calls-notify ()
  "Test that single event with notification calls chime--notify."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-called nil)
            (notify-messages '()))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg)
                      (setq notify-called t)
                      (push msg notify-messages)))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   ;; Event at 14:10 (10 minutes from now)
                   (event-time (encode-time 0 10 14 24 10 2025))
                   (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                            (title . "Team Meeting")
                            (intervals . (10))))
                   (events (list event)))
          (chime--process-notifications events)
          ;; Should call notify
          (should notify-called)
          (should (= 1 (length notify-messages)))
          (should (string-match-p "Team Meeting" (car notify-messages)))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-multiple-events-calls-notify-multiple-times ()
  "Test that multiple events with notifications call chime--notify multiple times."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-count 0))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-count (1+ notify-count))))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   ;; Event 1 at 14:10
                   (event-time-1 (encode-time 0 10 14 24 10 2025))
                   (event1 `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time-1))))
                             (title . "Meeting 1")
                             (intervals . (10))))
                   ;; Event 2 at 14:05
                   (event-time-2 (encode-time 0 5 14 24 10 2025))
                   (event2 `((times . ((("<2025-10-24 Fri 14:05>" . ,event-time-2))))
                             (title . "Meeting 2")
                             (intervals . (5))))
                   (events (list event1 event2)))
          (chime--process-notifications events)
          ;; Should call notify twice (once per event)
          (should (= 2 notify-count))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-deduplication-removes-duplicates ()
  "Test that duplicate notification messages are deduplicated."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-messages '()))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg) (push msg notify-messages)))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   ;; Two events with same title and time - should dedupe
                   (event-time (encode-time 0 10 14 24 10 2025))
                   (event1 `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                             (title . "Team Meeting")
                             (intervals . (10))))
                   (event2 `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                             (title . "Team Meeting")
                             (intervals . (10))))
                   (events (list event1 event2)))
          (chime--process-notifications events)
          ;; Should only call notify once due to deduplication
          (should (= 1 (length notify-messages)))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-day-wide-notifications-called-at-right-time ()
  "Test that day-wide notifications are sent when current time matches."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-count 0))
        (cl-letf* ((mock-time (encode-time 0 0 9 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-count (1+ notify-count))))
                   ;; Mock day-wide time to return true
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () t))
                   ((symbol-function 'chime-day-wide-notifications)
                    (lambda (events) (list "Day-wide alert")))
                   ;; Day-wide event
                   (event-time (encode-time 0 0 0 24 10 2025))
                   (event `((times . ((("<2025-10-24 Fri>" . ,event-time))))
                            (title . "All Day Event")
                            (intervals . ())))
                   (events (list event)))
          (chime--process-notifications events)
          ;; Should call notify at least once for day-wide
          (should (>= notify-count 1))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-no-day-wide-when-wrong-time ()
  "Test that day-wide notifications are not sent when time doesn't match."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((day-wide-called nil))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify) (lambda (msg) nil))
                   ;; Mock day-wide time to return false
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   ((symbol-function 'chime-day-wide-notifications)
                    (lambda (events)
                      (setq day-wide-called t)
                      '()))
                   (event-time (encode-time 0 0 0 24 10 2025))
                   (event `((times . ((("<2025-10-24 Fri>" . ,event-time))))
                            (title . "All Day Event")
                            (intervals . ())))
                   (events (list event)))
          (chime--process-notifications events)
          ;; Day-wide function should not be called
          (should-not day-wide-called)))
    (test-chime-process-notifications-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-process-notifications-boundary-empty-events-no-notifications ()
  "Test that empty events list produces no notifications."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-called nil))
        (cl-letf* (((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-called t)))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   (events '()))
          (chime--process-notifications events)
          ;; Should not call notify
          (should-not notify-called)))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-boundary-events-with-no-matches-no-notifications ()
  "Test that events with no matching notifications don't call notify."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-called nil))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-called t)))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   ;; Event at 15:00 (50 minutes away, doesn't match any interval)
                   (event-time (encode-time 0 0 15 24 10 2025))
                   (event `((times . ((("<2025-10-24 Fri 15:00>" . ,event-time))))
                            (title . "Future Event")
                            (intervals . (10))))
                   (events (list event)))
          (chime--process-notifications events)
          ;; Should not call notify
          (should-not notify-called)))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-boundary-single-event-edge-case ()
  "Test processing single event works correctly."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-count 0))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-count (1+ notify-count))))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   (event-time (encode-time 0 10 14 24 10 2025))
                   (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                            (title . "Single Event")
                            (intervals . (10))))
                   (events (list event)))
          (chime--process-notifications events)
          ;; Should call notify exactly once
          (should (= 1 notify-count))))
    (test-chime-process-notifications-teardown)))

;;; Error Cases

(ert-deftest test-chime-process-notifications-error-nil-events-handles-gracefully ()
  "Test that nil events parameter doesn't crash."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-called nil))
        (cl-letf* (((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-called t)))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil)))
          ;; Should not error with nil events
          (should-not (condition-case nil
                          (progn (chime--process-notifications nil) nil)
                        (error t)))
          ;; Should not call notify
          (should-not notify-called)))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-error-invalid-event-structure-handles-gracefully ()
  "Test that invalid event structure doesn't crash."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-called nil))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-called t)))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   ;; Invalid event: missing required fields
                   (events (list '((invalid . "structure")))))
          ;; Should not crash even with invalid events
          (should-not (condition-case nil
                          (progn (chime--process-notifications events) nil)
                        (error t)))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-error-mixed-valid-invalid-events-processes-valid ()
  "Test that mix of valid and invalid events processes valid ones."
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-count 0))
        (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                   ((symbol-function 'current-time) (lambda () mock-time))
                   ((symbol-function 'chime--notify)
                    (lambda (msg) (setq notify-count (1+ notify-count))))
                   ((symbol-function 'chime-current-time-is-day-wide-time)
                    (lambda () nil))
                   ;; Valid event
                   (event-time (encode-time 0 10 14 24 10 2025))
                   (valid-event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                                  (title . "Valid Event")
                                  (intervals . (10))))
                   ;; Invalid event
                   (invalid-event '((invalid . "data")))
                   (events (list valid-event invalid-event)))
          ;; Should not crash
          (should-not (condition-case nil
                          (progn (chime--process-notifications events) nil)
                        (error t)))
          ;; Should process at least the valid event
          (should (>= notify-count 1))))
    (test-chime-process-notifications-teardown)))

(provide 'test-chime-process-notifications)
;;; test-chime-process-notifications.el ends here
