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
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Setup and Teardown

(defun test-chime-process-notifications-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-process-notifications-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-process-notifications-normal-single-event-calls-notify ()
  "Test that single event with notification calls chime--notify.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 14:10 (10 minutes from now)
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time))
             (notify-called nil)
             (notify-messages '()))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg)
                       (setq notify-called t)
                       (push msg notify-messages)))
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil)))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Team Meeting")
                            (intervals . ((10 . medium)))))
                   (events (list event)))
              (chime--process-notifications events)
              ;; Should call notify
              (should notify-called)
              (should (= 1 (length notify-messages)))
              (should (string-match-p "Team Meeting" (caar notify-messages)))))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-multiple-events-calls-notify-multiple-times ()
  "Test that multiple events with notifications call chime--notify multiple times.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event 1 at 14:10
             (event-time-1 (test-time-today-at 14 10))
             (timestamp-str-1 (test-timestamp-string event-time-1))
             ;; Event 2 at 14:05
             (event-time-2 (test-time-today-at 14 5))
             (timestamp-str-2 (test-timestamp-string event-time-2))
             (notify-count 0))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg) (setq notify-count (1+ notify-count))))
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil)))
            (let* ((event1 `((times . ((,timestamp-str-1 . ,event-time-1)))
                             (title . "Meeting 1")
                             (intervals . ((10 . medium)))))
                   (event2 `((times . ((,timestamp-str-2 . ,event-time-2)))
                             (title . "Meeting 2")
                             (intervals . ((5 . medium)))))
                   (events (list event1 event2)))
              (chime--process-notifications events)
              ;; Should call notify twice (once per event)
              (should (= 2 notify-count))))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-deduplication-removes-duplicates ()
  "Test that duplicate notification messages are deduplicated.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Two events with same title and time - should dedupe
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time))
             (notify-messages '()))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg) (push msg notify-messages)))
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil)))
            (let* ((event1 `((times . ((,timestamp-str . ,event-time)))
                             (title . "Team Meeting")
                             (intervals . ((10 . medium)))))
                   (event2 `((times . ((,timestamp-str . ,event-time)))
                             (title . "Team Meeting")
                             (intervals . ((10 . medium)))))
                   (events (list event1 event2)))
              (chime--process-notifications events)
              ;; Should only call notify once due to deduplication
              (should (= 1 (length notify-messages)))))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-day-wide-notifications-called-at-right-time ()
  "Test that day-wide notifications are sent when current time matches.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 9 0))
             ;; Day-wide event
             (event-time (test-time-today-at 0 0))
             (timestamp-str (test-timestamp-string event-time t))  ; Day-wide
             (notify-count 0))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg) (setq notify-count (1+ notify-count))))
                    ;; Mock day-wide time to return true
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () t))
                    ((symbol-function 'chime-day-wide-notifications)
                     (lambda (events) (list "Day-wide alert"))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "All Day Event")
                            (intervals . ())))
                   (events (list event)))
              (chime--process-notifications events)
              ;; Should call notify at least once for day-wide
              (should (>= notify-count 1))))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-normal-no-day-wide-when-wrong-time ()
  "Test that day-wide notifications are not sent when time doesn't match.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 0 0))
             (timestamp-str (test-timestamp-string event-time t))  ; Day-wide
             (day-wide-called nil))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify) (lambda (msg) nil))
                    ;; Mock day-wide time to return false
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil))
                    ((symbol-function 'chime-day-wide-notifications)
                     (lambda (events)
                       (setq day-wide-called t)
                       '())))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "All Day Event")
                            (intervals . ())))
                   (events (list event)))
              (chime--process-notifications events)
              ;; Day-wide function should not be called
              (should-not day-wide-called)))))
    (test-chime-process-notifications-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-process-notifications-boundary-empty-events-no-notifications ()
  "Test that empty events list produces no notifications.

REFACTORED: No timestamps used"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-called nil))
        (cl-letf (((symbol-function 'chime--notify)
                   (lambda (msg) (setq notify-called t)))
                  ((symbol-function 'chime-current-time-is-day-wide-time)
                   (lambda () nil)))
          (let ((events '()))
            (chime--process-notifications events)
            ;; Should not call notify
            (should-not notify-called))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-boundary-events-with-no-matches-no-notifications ()
  "Test that events with no matching notifications don't call notify.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 15:00 (60 minutes away, doesn't match 10 min interval)
             (event-time (test-time-today-at 15 0))
             (timestamp-str (test-timestamp-string event-time))
             (notify-called nil))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg) (setq notify-called t)))
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil)))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Future Event")
                            (intervals . ((10 . medium)))))
                   (events (list event)))
              (chime--process-notifications events)
              ;; Should not call notify
              (should-not notify-called)))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-boundary-single-event-edge-case ()
  "Test processing single event works correctly.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time))
             (notify-count 0))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg) (setq notify-count (1+ notify-count))))
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil)))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Single Event")
                            (intervals . ((10 . medium)))))
                   (events (list event)))
              (chime--process-notifications events)
              ;; Should call notify exactly once
              (should (= 1 notify-count))))))
    (test-chime-process-notifications-teardown)))

;;; Error Cases

(ert-deftest test-chime-process-notifications-error-nil-events-handles-gracefully ()
  "Test that nil events parameter doesn't crash.

REFACTORED: No timestamps used"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let ((notify-called nil))
        (cl-letf (((symbol-function 'chime--notify)
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
  "Test that invalid event structure doesn't crash.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (notify-called nil))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg) (setq notify-called t)))
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil)))
            (let* (;; Invalid event: missing required fields
                   (events (list '((invalid . "structure")))))
              ;; Should not crash even with invalid events
              (should-not (condition-case nil
                              (progn (chime--process-notifications events) nil)
                            (error t)))))))
    (test-chime-process-notifications-teardown)))

(ert-deftest test-chime-process-notifications-error-mixed-valid-invalid-events-processes-valid ()
  "Test that mix of valid and invalid events processes valid ones.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-process-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Valid event
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time))
             (notify-count 0))
        (with-test-time now
          (cl-letf (((symbol-function 'chime--notify)
                     (lambda (msg) (setq notify-count (1+ notify-count))))
                    ((symbol-function 'chime-current-time-is-day-wide-time)
                     (lambda () nil)))
            (let* ((valid-event `((times . ((,timestamp-str . ,event-time)))
                                  (title . "Valid Event")
                                  (intervals . ((10 . medium)))))
                   ;; Invalid event
                   (invalid-event '((invalid . "data")))
                   (events (list valid-event invalid-event)))
              ;; Should not crash
              (should-not (condition-case nil
                              (progn (chime--process-notifications events) nil)
                            (error t)))
              ;; Should process at least the valid event
              (should (>= notify-count 1))))))
    (test-chime-process-notifications-teardown)))

(provide 'test-chime-process-notifications)
;;; test-chime-process-notifications.el ends here
