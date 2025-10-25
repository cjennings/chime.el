;;; test-chime-check-event.el --- Tests for chime--check-event -*- lexical-binding: t; -*-

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

;;; Setup and Teardown

(defun test-chime-check-event-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-check-event-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-check-event-single-notification-returns-message ()
  "Test that single matching notification returns formatted message."
  (test-chime-check-event-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Event at 14:10 (10 minutes from now)
                 (event-time (encode-time 0 10 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                          (title . "Team Meeting")
                          (intervals . (10))))
                 (result (chime--check-event event)))
        ;; Should return list with one formatted message
        (should (listp result))
        (should (= 1 (length result)))
        (should (stringp (car result)))
        ;; Message should contain title and time information
        (should (string-match-p "Team Meeting" (car result)))
        (should (string-match-p "02:10 PM" (car result)))
        (should (string-match-p "in 10 minutes" (car result))))
    (test-chime-check-event-teardown)))

(ert-deftest test-chime-check-event-multiple-notifications-returns-multiple-messages ()
  "Test that multiple matching notifications return multiple formatted messages."
  (test-chime-check-event-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Two events: 14:10 and 14:05
                 (event-time-1 (encode-time 0 10 14 24 10 2025))
                 (event-time-2 (encode-time 0 5 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time-1)
                                     ("<2025-10-24 Fri 14:05>" . ,event-time-2))))
                          (title . "Important Call")
                          (intervals . (10 5))))  ; Both match
                 (result (chime--check-event event)))
        ;; Should return two formatted messages
        (should (listp result))
        (should (= 2 (length result)))
        (should (cl-every #'stringp result))
        ;; Both should mention the title
        (should (string-match-p "Important Call" (car result)))
        (should (string-match-p "Important Call" (cadr result))))
    (test-chime-check-event-teardown)))

(ert-deftest test-chime-check-event-zero-interval-returns-right-now-message ()
  "Test that zero interval produces 'right now' message."
  (test-chime-check-event-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Event at exactly now
                 (event-time (encode-time 0 0 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:00>" . ,event-time))))
                          (title . "Daily Standup")
                          (intervals . (0))))
                 (result (chime--check-event event)))
        (should (listp result))
        (should (= 1 (length result)))
        (should (string-match-p "Daily Standup" (car result)))
        (should (string-match-p "right now" (car result))))
    (test-chime-check-event-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-check-event-no-matching-notifications-returns-empty-list ()
  "Test that event with no matching times returns empty list."
  (test-chime-check-event-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Event at 14:20 (doesn't match 10 minute interval)
                 (event-time (encode-time 0 20 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:20>" . ,event-time))))
                          (title . "Future Event")
                          (intervals . (10))))
                 (result (chime--check-event event)))
        (should (listp result))
        (should (= 0 (length result))))
    (test-chime-check-event-teardown)))

(ert-deftest test-chime-check-event-day-wide-event-returns-empty-list ()
  "Test that day-wide event (no time) returns empty list."
  (test-chime-check-event-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 (event-time (encode-time 0 0 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri>" . ,event-time))))
                          (title . "All Day Event")
                          (intervals . (10))))
                 (result (chime--check-event event)))
        (should (listp result))
        (should (= 0 (length result))))
    (test-chime-check-event-teardown)))

;;; Error Cases

(ert-deftest test-chime-check-event-empty-times-returns-empty-list ()
  "Test that event with no times returns empty list."
  (test-chime-check-event-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 (event `((times . (()))
                          (title . "No Times Event")
                          (intervals . (10))))
                 (result (chime--check-event event)))
        (should (listp result))
        (should (= 0 (length result))))
    (test-chime-check-event-teardown)))

(ert-deftest test-chime-check-event-empty-intervals-returns-empty-list ()
  "Test that event with no intervals returns empty list."
  (test-chime-check-event-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 (event-time (encode-time 0 10 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                          (title . "No Intervals Event")
                          (intervals . ())))
                 (result (chime--check-event event)))
        (should (listp result))
        (should (= 0 (length result))))
    (test-chime-check-event-teardown)))

(provide 'test-chime-check-event)
;;; test-chime-check-event.el ends here
