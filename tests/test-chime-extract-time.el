;;; test-chime-extract-time.el --- Tests for chime--extract-time -*- lexical-binding: t; -*-

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

;; Unit tests for chime--extract-time function.
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

(defun test-chime-extract-time-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-extract-time-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-extract-time-scheduled-timestamp-extracted ()
  "Test that SCHEDULED timestamp is extracted correctly."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "<2025-10-24 Fri 14:30>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (= (length result) 1))
              (should (equal (caar result) "<2025-10-24 Fri 14:30>"))
              (should (listp (cdar result)))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-deadline-timestamp-extracted ()
  "Test that DEADLINE timestamp is extracted correctly."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "<2025-10-24 Fri 16:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (= (length result) 1))
              (should (equal (caar result) "<2025-10-24 Fri 16:00>"))
              (should (listp (cdar result)))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-plain-timestamp-extracted ()
  "Test that plain TIMESTAMP is extracted correctly."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Test Event\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "TIMESTAMP"))
                         "<2025-10-24 Fri 10:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (= (length result) 1))
              (should (equal (caar result) "<2025-10-24 Fri 10:00>"))
              (should (listp (cdar result)))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-multiple-timestamps-all-extracted ()
  "Test that multiple timestamps are all extracted."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "<2025-10-24 Fri 14:30>")
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "<2025-10-24 Fri 16:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (= (length result) 2))
              ;; Check both timestamps are present
              (should (--some (equal (car it) "<2025-10-24 Fri 14:30>") result))
              (should (--some (equal (car it) "<2025-10-24 Fri 16:00>") result))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-all-three-timestamp-types-extracted ()
  "Test that all three timestamp types can be extracted together."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Complex Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "<2025-10-24 Fri 09:00>")
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "<2025-10-24 Fri 17:00>")
                        ((and (equal pom marker) (equal property "TIMESTAMP"))
                         "<2025-10-24 Fri 12:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (= (length result) 3))))))
    (test-chime-extract-time-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-extract-time-no-timestamps-returns-empty ()
  "Test that entry with no timestamps returns empty list."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       nil)))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (= (length result) 0))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-only-scheduled-extracted ()
  "Test that only SCHEDULED is extracted when others are missing."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "<2025-10-24 Fri 14:30>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (= (length result) 1))
              (should (equal (caar result) "<2025-10-24 Fri 14:30>"))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-only-deadline-extracted ()
  "Test that only DEADLINE is extracted when others are missing."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "<2025-10-24 Fri 16:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (= (length result) 1))
              (should (equal (caar result) "<2025-10-24 Fri 16:00>"))))))
    (test-chime-extract-time-teardown)))

;;; Error Cases

(ert-deftest test-chime-extract-time-malformed-timestamp-returns-nil-cdr ()
  "Test that malformed timestamps return cons with nil cdr."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "not-a-valid-timestamp")
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "<2025-10-24 Fri 16:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              ;; Should return both, but malformed one has nil cdr
              (should (= (length result) 2))
              ;; Find the malformed timestamp result
              (let ((malformed (--find (equal (car it) "not-a-valid-timestamp") result)))
                (should malformed)
                (should-not (cdr malformed)))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-day-wide-timestamp-returns-nil-cdr ()
  "Test that day-wide timestamps (no time) return cons with nil cdr."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "<2025-10-24 Fri>")  ; Day-wide, no time
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "<2025-10-24 Fri 16:00>")  ; Has time
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              ;; Should return both timestamps
              (should (= (length result) 2))
              ;; Day-wide timestamp has nil cdr
              (let ((day-wide (--find (equal (car it) "<2025-10-24 Fri>") result)))
                (should day-wide)
                (should-not (cdr day-wide)))
              ;; Timed timestamp has valid cdr
              (let ((timed (--find (equal (car it) "<2025-10-24 Fri 16:00>") result)))
                (should timed)
                (should (cdr timed)))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-empty-timestamp-string-returns-nil-cdr ()
  "Test that empty timestamp strings return cons with nil cdr."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "")
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "<2025-10-24 Fri 16:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              ;; Should return both entries
              (should (= (length result) 2))
              ;; Empty string has nil cdr
              (let ((empty (--find (equal (car it) "") result)))
                (should empty)
                (should-not (cdr empty)))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-all-malformed-returns-cons-with-nil-cdrs ()
  "Test that all malformed timestamps return cons with nil cdrs."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "SCHEDULED"))
                         "not-valid")
                        ((and (equal pom marker) (equal property "DEADLINE"))
                         "also-not-valid")
                        ((and (equal pom marker) (equal property "TIMESTAMP"))
                         "<2025-10-24 Fri>")  ; Day-wide
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              ;; Should return 3 entries, all with nil cdr
              (should (= (length result) 3))
              (should (--every (not (cdr it)) result))))))
    (test-chime-extract-time-teardown)))

;;; org-gcal Integration Tests

(ert-deftest test-chime-extract-time-org-gcal-time-range-format ()
  "Test extraction of org-gcal style timestamp with time range.
org-gcal uses format like <2025-10-24 Fri 17:30-18:00> with HH:MM-HH:MM range."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Testing Round Trip\n")
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "TIMESTAMP"))
                         "<2025-10-24 Fri 17:30-18:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (>= (length result) 1))
              ;; Should extract the timestamp string
              (should (equal (caar result) "<2025-10-24 Fri 17:30-18:00>"))
              ;; Should have parsed time value (not nil)
              (should (listp (cdar result)))
              (should (cdar result))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-org-gcal-in-drawer ()
  "Test extraction of timestamp inside org-gcal drawer.
org-gcal stores timestamps in :org-gcal: drawers which should still be detected."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Testing Chime!\n")
        (insert ":PROPERTIES:\n")
        (insert ":ETag:     \"3522688202987102\"\n")
        (insert ":calendar-id: user@example.com\n")
        (insert ":entry-id: abc123/user@example.com\n")
        (insert ":org-gcal-managed: gcal\n")
        (insert ":END:\n")
        (insert ":org-gcal:\n")
        (insert "<2025-10-24 Fri 17:30-18:00>\n")
        (insert ":END:\n")
        (goto-char (point-min))
        (let ((marker (copy-marker (point))))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       (cond
                        ((and (equal pom marker) (equal property "TIMESTAMP"))
                         "<2025-10-24 Fri 17:30-18:00>")
                        (t nil)))))
            (let ((result (chime--extract-time marker)))
              (should (listp result))
              (should (>= (length result) 1))
              (should (equal (caar result) "<2025-10-24 Fri 17:30-18:00>"))
              (should (cdar result))))))
    (test-chime-extract-time-teardown)))

(provide 'test-chime-extract-time)
;;; test-chime-extract-time.el ends here
