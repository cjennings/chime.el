;;; test-chime--today.el --- Tests for chime--today -*- lexical-binding: t; -*-

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

;; Unit tests for chime--today function.
;; Tests retrieving beginning of current day timestamp.

;;; Code:

;; Initialize package system for batch mode
(when noninteractive
  (package-initialize))

(require 'ert)
(require 'dash)
(require 'alert)

;; Load chime from parent directory
(load (expand-file-name "../chime.el") nil t)

;;; Setup and Teardown

(defun test-chime--today-setup ()
  "Setup test environment."
  ;; No special setup needed
  )

(defun test-chime--today-teardown ()
  "Teardown test environment."
  ;; No special teardown needed
  )

;;; Normal Cases

(ert-deftest test-chime--today-normal-returns-current-date ()
  "Test that chime--today returns midnight of current day."
  (test-chime--today-setup)
  (unwind-protect
      (let* ((now (encode-time 30 45 14 8 11 2025))  ; 2025-11-08 14:45:30
             (expected (encode-time 0 0 0 8 11 2025))) ; 2025-11-08 00:00:00
        (cl-letf (((symbol-function 'current-time) (lambda () now)))
          (should (equal (chime--today) expected))))
    (test-chime--today-teardown)))

(ert-deftest test-chime--today-normal-truncates-time-component ()
  "Test that chime--today zeros out hour/minute/second."
  (test-chime--today-setup)
  (unwind-protect
      (let* ((now (encode-time 59 59 23 8 11 2025))  ; 2025-11-08 23:59:59
             (result (cl-letf (((symbol-function 'current-time) (lambda () now)))
                       (chime--today)))
             (decoded (decode-time result)))
        ;; Check that hour, minute, second are all 0
        (should (= 0 (decoded-time-second decoded)))
        (should (= 0 (decoded-time-minute decoded)))
        (should (= 0 (decoded-time-hour decoded)))
        ;; Check date is preserved
        (should (= 8 (decoded-time-day decoded)))
        (should (= 11 (decoded-time-month decoded)))
        (should (= 2025 (decoded-time-year decoded))))
    (test-chime--today-teardown)))

;;; Boundary Cases

(ert-deftest test-chime--today-boundary-midnight-returns-correct-day ()
  "Test that chime--today at midnight returns same day."
  (test-chime--today-setup)
  (unwind-protect
      (let* ((now (encode-time 0 0 0 8 11 2025))     ; Already at midnight
             (expected (encode-time 0 0 0 8 11 2025)))
        (cl-letf (((symbol-function 'current-time) (lambda () now)))
          (should (equal (chime--today) expected))))
    (test-chime--today-teardown)))

(provide 'test-chime--today)
;;; test-chime--today.el ends here
