;;; test-chime--time=.el --- Tests for chime--time= -*- lexical-binding: t; -*-

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

;; Unit tests for chime--time= function.
;; Tests timestamp comparison ignoring seconds component.

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

(defun test-chime--time=-setup ()
  "Setup test environment."
  ;; No special setup needed
  )

(defun test-chime--time=-teardown ()
  "Teardown test environment."
  ;; No special teardown needed
  )

;;; Normal Cases

(ert-deftest test-chime--time=-normal-two-equal-times-returns-true ()
  "Test that two timestamps with same day/hour/minute return true."
  (test-chime--time=-setup)
  (unwind-protect
      (let* ((time1 (encode-time 15 30 14 8 11 2025))  ; 2025-11-08 14:30:15
             (time2 (encode-time 45 30 14 8 11 2025))) ; 2025-11-08 14:30:45 (different seconds)
        (should (chime--time= time1 time2)))
    (test-chime--time=-teardown)))

(ert-deftest test-chime--time=-normal-three-equal-times-returns-true ()
  "Test that three timestamps with same day/hour/minute return true."
  (test-chime--time=-setup)
  (unwind-protect
      (let* ((time1 (encode-time 10 45 9 8 11 2025))
             (time2 (encode-time 20 45 9 8 11 2025))
             (time3 (encode-time 55 45 9 8 11 2025)))
        (should (chime--time= time1 time2 time3)))
    (test-chime--time=-teardown)))

(ert-deftest test-chime--time=-normal-two-different-times-returns-nil ()
  "Test that timestamps with different hour/minute return nil."
  (test-chime--time=-setup)
  (unwind-protect
      (let* ((time1 (encode-time 0 30 14 8 11 2025))  ; 14:30
             (time2 (encode-time 0 31 14 8 11 2025))) ; 14:31
        (should-not (chime--time= time1 time2)))
    (test-chime--time=-teardown)))

;;; Boundary Cases

(ert-deftest test-chime--time=-boundary-single-time-returns-true ()
  "Test that single timestamp returns true."
  (test-chime--time=-setup)
  (unwind-protect
      (let ((time (encode-time 0 0 12 8 11 2025)))
        (should (chime--time= time)))
    (test-chime--time=-teardown)))

(ert-deftest test-chime--time=-boundary-empty-list-returns-nil ()
  "Test that empty argument list returns nil."
  (test-chime--time=-setup)
  (unwind-protect
      ;; Empty list has no elements, unique length is 0, not 1
      (should-not (chime--time= ))
    (test-chime--time=-teardown)))

(ert-deftest test-chime--time=-boundary-different-days-same-time-returns-nil ()
  "Test that same time on different days returns nil."
  (test-chime--time=-setup)
  (unwind-protect
      (let* ((time1 (encode-time 0 30 14 8 11 2025))   ; Nov 8
             (time2 (encode-time 0 30 14 9 11 2025)))  ; Nov 9
        (should-not (chime--time= time1 time2)))
    (test-chime--time=-teardown)))

(provide 'test-chime--time=)
;;; test-chime--time=.el ends here
