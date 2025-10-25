;;; test-chime-timestamp-within-interval-p.el --- Tests for chime--timestamp-within-interval-p -*- lexical-binding: t; -*-

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

;; Unit tests for chime--timestamp-within-interval-p function.
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

(defun test-chime-timestamp-within-interval-p-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-timestamp-within-interval-p-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-timestamp-within-interval-p-exactly-at-interval-returns-t ()
  "Test that timestamp exactly at interval returns t."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:10 (10 minutes from 14:00)
                 (timestamp (encode-time 0 10 14 24 10 2025))
                 (interval 10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-zero-interval-returns-t ()
  "Test that zero interval (notify now) returns t for current time."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 30 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at exactly current time (14:30)
                 (timestamp (encode-time 0 30 14 24 10 2025))
                 (interval 0)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-five-minutes-returns-t ()
  "Test that 5-minute interval works correctly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 25 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:30 (5 minutes from 14:25)
                 (timestamp (encode-time 0 30 14 24 10 2025))
                 (interval 5)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-sixty-minutes-returns-t ()
  "Test that 60-minute (1 hour) interval works correctly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 15:00 (60 minutes from 14:00)
                 (timestamp (encode-time 0 0 15 24 10 2025))
                 (interval 60)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-large-interval-returns-t ()
  "Test that large interval (1 day = 1440 minutes) works correctly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:00 next day (1440 minutes from now)
                 (timestamp (encode-time 0 0 14 25 10 2025))
                 (interval 1440)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-thirty-minutes-returns-t ()
  "Test that 30-minute interval works correctly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 15 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:45 (30 minutes from 14:15)
                 (timestamp (encode-time 0 45 14 24 10 2025))
                 (interval 30)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-timestamp-within-interval-p-one-minute-before-returns-nil ()
  "Test that timestamp 1 minute before interval returns nil."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:09 (9 minutes from 14:00, not 10)
                 (timestamp (encode-time 0 9 14 24 10 2025))
                 (interval 10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should-not result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-one-minute-after-returns-nil ()
  "Test that timestamp 1 minute after interval returns nil."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:11 (11 minutes from 14:00, not 10)
                 (timestamp (encode-time 0 11 14 24 10 2025))
                 (interval 10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should-not result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-crossing-midnight-returns-t ()
  "Test that interval crossing midnight works correctly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 50 23 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 00:00 next day (10 minutes from 23:50)
                 (timestamp (encode-time 0 0 0 25 10 2025))
                 (interval 10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-crossing-day-boundary-returns-t ()
  "Test that interval crossing to next day works correctly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 30 23 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 00:30 next day (60 minutes from 23:30)
                 (timestamp (encode-time 0 30 0 25 10 2025))
                 (interval 60)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-week-interval-returns-t ()
  "Test that very large interval (1 week = 10080 minutes) works."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:00 one week later
                 (timestamp (encode-time 0 0 14 31 10 2025))
                 (interval 10080)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-at-midnight-returns-t ()
  "Test that timestamp at exact midnight works correctly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 50 23 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at midnight (10 minutes from 23:50)
                 (timestamp (encode-time 0 0 0 25 10 2025))
                 (interval 10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

;;; Error Cases

(ert-deftest test-chime-timestamp-within-interval-p-nil-timestamp-returns-nil ()
  "Test that nil timestamp returns nil."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 (timestamp nil)
                 (interval 10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should-not result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-nil-interval-returns-nil ()
  "Test that nil interval returns nil."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 (timestamp (encode-time 0 10 14 24 10 2025))
                 (interval nil)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should-not result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-negative-interval-returns-nil ()
  "Test that negative interval returns nil (past timestamps)."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp 10 minutes in the past (13:50)
                 (timestamp (encode-time 0 50 13 24 10 2025))
                 (interval -10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-invalid-timestamp-returns-nil ()
  "Test that invalid timestamp format returns nil."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 (timestamp "not-a-timestamp")
                 (interval 10)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should-not result))
    (test-chime-timestamp-within-interval-p-teardown)))

(ert-deftest test-chime-timestamp-within-interval-p-float-interval-works ()
  "Test that float interval gets converted properly."
  (test-chime-timestamp-within-interval-p-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ;; Timestamp at 14:10 (10 minutes from 14:00)
                 (timestamp (encode-time 0 10 14 24 10 2025))
                 (interval 10.5)
                 (result (chime--timestamp-within-interval-p timestamp interval)))
        (should result))
    (test-chime-timestamp-within-interval-p-teardown)))

(provide 'test-chime-timestamp-within-interval-p)
;;; test-chime-timestamp-within-interval-p.el ends here
