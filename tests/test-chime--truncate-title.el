;;; test-chime--truncate-title.el --- Tests for chime--truncate-title -*- lexical-binding: t; -*-

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

;; Unit tests for chime--truncate-title function.
;; Tests title truncation with ellipsis based on chime-max-title-length.

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

(defun test-chime--truncate-title-setup ()
  "Setup test environment."
  ;; No special setup needed
  )

(defun test-chime--truncate-title-teardown ()
  "Teardown test environment."
  ;; No special teardown needed
  )

;;; Normal Cases

(ert-deftest test-chime--truncate-title-normal-short-title-unchanged ()
  "Test that short title under max length is unchanged."
  (test-chime--truncate-title-setup)
  (unwind-protect
      (let ((chime-max-title-length 20))
        (should (equal (chime--truncate-title "Short title")
                      "Short title")))
    (test-chime--truncate-title-teardown)))

(ert-deftest test-chime--truncate-title-normal-long-title-truncated-with-ellipsis ()
  "Test that long title is truncated with ... appended."
  (test-chime--truncate-title-setup)
  (unwind-protect
      (let ((chime-max-title-length 15))
        (should (equal (chime--truncate-title "This is a very long title that needs truncation")
                      "This is a ve...")))
    (test-chime--truncate-title-teardown)))

(ert-deftest test-chime--truncate-title-normal-unicode-characters-truncated-correctly ()
  "Test that unicode characters are handled correctly in truncation."
  (test-chime--truncate-title-setup)
  (unwind-protect
      (let ((chime-max-title-length 10))
        (should (equal (chime--truncate-title "Meeting 🎉 with team")
                      "Meeting...")))
    (test-chime--truncate-title-teardown)))

;;; Boundary Cases

(ert-deftest test-chime--truncate-title-boundary-exact-max-length-unchanged ()
  "Test that title exactly at max length is unchanged."
  (test-chime--truncate-title-setup)
  (unwind-protect
      (let ((chime-max-title-length 12))
        (should (equal (chime--truncate-title "Twelve chars")
                      "Twelve chars")))
    (test-chime--truncate-title-teardown)))

(ert-deftest test-chime--truncate-title-boundary-one-char-over-max-truncated ()
  "Test that title one character over max is truncated."
  (test-chime--truncate-title-setup)
  (unwind-protect
      (let ((chime-max-title-length 10))
        (should (equal (chime--truncate-title "Eleven char")
                      "Eleven ...")))
    (test-chime--truncate-title-teardown)))

(ert-deftest test-chime--truncate-title-boundary-empty-string-returns-empty ()
  "Test that empty string returns empty string."
  (test-chime--truncate-title-setup)
  (unwind-protect
      (let ((chime-max-title-length 20))
        (should (equal (chime--truncate-title "")
                      "")))
    (test-chime--truncate-title-teardown)))

;;; Error Cases

(ert-deftest test-chime--truncate-title-error-nil-title-returns-empty ()
  "Test that nil title returns empty string."
  (test-chime--truncate-title-setup)
  (unwind-protect
      (let ((chime-max-title-length 20))
        (should (equal (chime--truncate-title nil)
                      "")))
    (test-chime--truncate-title-teardown)))

(provide 'test-chime--truncate-title)
;;; test-chime--truncate-title.el ends here
