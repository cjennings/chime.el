;;; test-chime-notifications.el --- Tests for chime--notifications -*- lexical-binding: t; -*-

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

;; Unit tests for chime--notifications function.
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

(defun test-chime-notifications-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-notifications-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-notifications-single-time-single-interval-returns-pair ()
  "Test that single time with single interval returns one notification pair.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 14:10 (10 minutes from now)
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            ;; Should return list with one pair
            (should (listp result))
            (should (= 1 (length result))))))
    (test-chime-notifications-teardown)))

(ert-deftest test-chime-notifications-single-time-multiple-intervals-returns-multiple-pairs ()
  "Test that single time with multiple intervals returns multiple notification pairs.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 14:10
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((10 . medium) (5 . medium)))))  ; Two intervals, only 10 matches
                 (result (chime--notifications event)))
            ;; Should return only matching interval
            (should (listp result))
            (should (= 1 (length result))))))
    (test-chime-notifications-teardown)))

(ert-deftest test-chime-notifications-multiple-times-single-interval-returns-matching-pairs ()
  "Test that multiple times with single interval returns matching notifications.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Two events: one at 14:10, one at 14:05
             (event-time-1 (test-time-today-at 14 10))
             (event-time-2 (test-time-today-at 14 5))
             (timestamp-str-1 (test-timestamp-string event-time-1))
             (timestamp-str-2 (test-timestamp-string event-time-2)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str-1 . ,event-time-1)
                                     (,timestamp-str-2 . ,event-time-2)))
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))  ; Only first time matches
                 (result (chime--notifications event)))
            ;; Should return only matching time
            (should (listp result))
            (should (= 1 (length result))))))
    (test-chime-notifications-teardown)))

(ert-deftest test-chime-notifications-multiple-times-multiple-intervals-returns-all-matches ()
  "Test that multiple times and intervals return all matching combinations.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 14:10 and 14:05
             (event-time-1 (test-time-today-at 14 10))
             (event-time-2 (test-time-today-at 14 5))
             (timestamp-str-1 (test-timestamp-string event-time-1))
             (timestamp-str-2 (test-timestamp-string event-time-2)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str-1 . ,event-time-1)
                                     (,timestamp-str-2 . ,event-time-2)))
                          (title . "Test Event")
                          (intervals . ((10 . medium) (5 . medium)))))  ; Both match (10 with first, 5 with second)
                 (result (chime--notifications event)))
            ;; Should return both matching pairs
            (should (listp result))
            (should (= 2 (length result))))))
    (test-chime-notifications-teardown)))

(ert-deftest test-chime-notifications-zero-interval-returns-current-time-match ()
  "Test that zero interval (notify now) works correctly.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at exactly current time
             (event-time (test-time-today-at 14 0))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((0 . high)))))
                 (result (chime--notifications event)))
            ;; Should return one matching pair
            (should (listp result))
            (should (= 1 (length result))))))
    (test-chime-notifications-teardown)))

(ert-deftest test-chime-notifications-filters-day-wide-events ()
  "Test that day-wide events (without time) are filtered out.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Mix of day-wide and timed events
             (event-time (test-time-today-at 14 10))
             (timestamp-str-day (test-timestamp-string event-time t))  ; Day-wide
             (timestamp-str-timed (test-timestamp-string event-time)))  ; Timed
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str-day . ,event-time)  ; Day-wide
                                     (,timestamp-str-timed . ,event-time)))  ; Timed
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            ;; Should return only timed event
            (should (listp result))
            (should (= 1 (length result))))))
    (test-chime-notifications-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-notifications-empty-times-returns-empty-list ()
  "Test that event with no times returns empty list.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let ((now (test-time-today-at 14 0)))
        (with-test-time now
          (let* ((event `((times . (()))
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            (should (listp result))
            (should (= 0 (length result))))))
    (test-chime-notifications-teardown)))

(ert-deftest test-chime-notifications-empty-intervals-returns-empty-list ()
  "Test that event with no intervals returns empty list.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ())))
                 (result (chime--notifications event)))
            (should (listp result))
            (should (= 0 (length result))))))
    (test-chime-notifications-teardown)))

;;; Error Cases

(ert-deftest test-chime-notifications-nil-times-returns-empty-list ()
  "Test that event with nil times returns empty list.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let ((now (test-time-today-at 14 0)))
        (with-test-time now
          (let* ((event `((times . (nil))
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            (should (listp result))
            (should (= 0 (length result))))))
    (test-chime-notifications-teardown)))

(ert-deftest test-chime-notifications-nil-intervals-returns-empty-list ()
  "Test that event with nil intervals returns empty list.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-notifications-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . nil)))
                 (result (chime--notifications event)))
            (should (listp result))
            (should (= 0 (length result))))))
    (test-chime-notifications-teardown)))

(provide 'test-chime-notifications)
;;; test-chime-notifications.el ends here
