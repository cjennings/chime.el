;;; test-chime-check-event.el --- Tests for chime--check-event -*- lexical-binding: t; -*-

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

;; Unit tests for chime--check-event function.
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
(require 'testutil-events (expand-file-name "testutil-events.el"))

;;; Normal Cases

(ert-deftest test-chime-check-event-single-notification-returns-message ()
  "Test that single matching notification returns formatted message.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let* ((now (test-time-today-at 14 0))
           ;; Event at 14:10 (10 minutes from now)
           (event-time (test-time-today-at 14 10))
           (event (test-make-simple-event "Team Meeting" event-time 10 'medium)))
      (with-test-time now
        (let ((result (chime--check-event event)))
          ;; Should return list with one formatted message
          (should (listp result))
          (should (= 1 (length result)))
          (should (consp (car result)))
          (should (stringp (caar result)))  ; Message part
          (should (symbolp (cdar result)))  ; Severity part
          ;; Message should contain title and time information
          (should (string-match-p "Team Meeting" (caar result)))
          (should (string-match-p "02:10 PM" (caar result)))
          (should (string-match-p "in 10 minutes" (caar result))))))))

(ert-deftest test-chime-check-event-multiple-notifications-returns-multiple-messages ()
  "Test that multiple matching notifications return multiple formatted messages.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let* ((now (test-time-today-at 14 0))
           ;; Two events: 14:10 and 14:05
           (event-time-1 (test-time-today-at 14 10))
           (event-time-2 (test-time-today-at 14 5))
           (timestamp-str-1 (test-timestamp-string event-time-1))
           (timestamp-str-2 (test-timestamp-string event-time-2))
           (event (test-make-event-data
                   "Important Call"
                   (list (cons timestamp-str-1 event-time-1)
                         (cons timestamp-str-2 event-time-2))
                   '((10 . medium) (5 . medium)))))  ; Both match
      (with-test-time now
        (let ((result (chime--check-event event)))
          ;; Should return two formatted messages
          (should (listp result))
          (should (= 2 (length result)))
          (should (cl-every #'consp result))  ; All items are cons cells
          ;; Both should mention the title
          (should (string-match-p "Important Call" (caar result)))
          (should (string-match-p "Important Call" (car (cadr result)))))))))

(ert-deftest test-chime-check-event-zero-interval-returns-right-now-message ()
  "Test that zero interval produces 'right now' message.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let* ((now (test-time-today-at 14 0))
           ;; Event at exactly now
           (event-time (test-time-today-at 14 0))
           (event (test-make-simple-event "Daily Standup" event-time 0 'high)))
      (with-test-time now
        (let ((result (chime--check-event event)))
          (should (listp result))
          (should (= 1 (length result)))
          (should (string-match-p "Daily Standup" (caar result)))
          (should (string-match-p "right now" (caar result))))))))

;;; Boundary Cases

(ert-deftest test-chime-check-event-no-matching-notifications-returns-empty-list ()
  "Test that event with no matching times returns empty list.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let* ((now (test-time-today-at 14 0))
           ;; Event at 14:20 (doesn't match 10 minute interval)
           (event-time (test-time-today-at 14 20))
           (event (test-make-simple-event "Future Event" event-time 10 'medium)))
      (with-test-time now
        (let ((result (chime--check-event event)))
          (should (listp result))
          (should (= 0 (length result))))))))

(ert-deftest test-chime-check-event-day-wide-event-returns-empty-list ()
  "Test that day-wide event (no time) returns empty list.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let* ((now (test-time-today-at 14 0))
           (event-time (test-time-today-at 14 0))
           (timestamp-str (test-timestamp-string event-time t))  ; all-day format
           (event (test-make-event-data "All Day Event"
                                        (list (cons timestamp-str event-time))
                                        '((10 . medium)))))
      (with-test-time now
        (let ((result (chime--check-event event)))
          (should (listp result))
          (should (= 0 (length result))))))))

;;; Error Cases

(ert-deftest test-chime-check-event-empty-times-returns-empty-list ()
  "Test that event with no times returns empty list.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let* ((now (test-time-today-at 14 0))
           (event (test-make-event-data "No Times Event" '() '((10 . medium)))))
      (with-test-time now
        (let ((result (chime--check-event event)))
          (should (listp result))
          (should (= 0 (length result))))))))

(ert-deftest test-chime-check-event-empty-intervals-returns-empty-list ()
  "Test that event with no intervals returns empty list.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let* ((now (test-time-today-at 14 0))
           (event-time (test-time-today-at 14 10))
           (event (test-make-simple-event "No Intervals Event" event-time nil nil)))
      (setcdr (assoc 'intervals event) '())  ; Override with empty intervals
      (with-test-time now
        (let ((result (chime--check-event event)))
          (should (listp result))
          (should (= 0 (length result))))))))

(ert-deftest test-chime-check-event-error-nil-event-handles-gracefully ()
  "Test that nil event parameter doesn't crash.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let ((now (test-time-today-at 14 0)))
      (with-test-time now
        ;; Should not error with nil event
        (should-not (condition-case nil
                        (progn (chime--check-event nil) nil)
                      (error t)))))))

(ert-deftest test-chime-check-event-error-invalid-event-structure-handles-gracefully ()
  "Test that invalid event structure doesn't crash.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let ((now (test-time-today-at 14 0))
          ;; Event missing required fields
          (invalid-event '((invalid . "structure"))))
      (with-test-time now
        ;; Should not crash even with invalid event
        (should-not (condition-case nil
                        (progn (chime--check-event invalid-event) nil)
                      (error t)))))))

(ert-deftest test-chime-check-event-error-event-with-nil-times-handles-gracefully ()
  "Test that event with nil times field doesn't crash.

REFACTORED: Uses testutil-events helpers"
  (with-test-setup
    (let ((now (test-time-today-at 14 0))
          (event '((times . nil)
                   (title . "Event with nil times")
                   (intervals . (10)))))
      (with-test-time now
        ;; Should not crash
        (should-not (condition-case nil
                        (progn (chime--check-event event) nil)
                      (error t)))))))

(provide 'test-chime-check-event)
;;; test-chime-check-event.el ends here
