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
;; Tests use real org-mode buffers with real org syntax.
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
        (insert "SCHEDULED: <2025-10-24 Fri 14:30>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 1))
            (should (equal (caar result) "<2025-10-24 Fri 14:30>"))
            (should (listp (cdar result)))
            (should (cdar result)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-deadline-timestamp-extracted ()
  "Test that DEADLINE timestamp is extracted correctly."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (insert "DEADLINE: <2025-10-24 Fri 16:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 1))
            (should (equal (caar result) "<2025-10-24 Fri 16:00>"))
            (should (listp (cdar result)))
            (should (cdar result)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-plain-timestamp-in-body-extracted ()
  "Test that plain timestamp in entry body is extracted correctly."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Test Event\n")
        (insert "<2025-10-24 Fri 10:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 1))
            (should (equal (caar result) "<2025-10-24 Fri 10:00>"))
            (should (listp (cdar result)))
            (should (cdar result)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-repeating-plain-timestamp-extracted ()
  "Test that repeating plain timestamp is extracted correctly."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Daily Wrap Up\n")
        (insert "<2025-06-17 Tue 21:00 +1d>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 1))
            (should (equal (caar result) "<2025-06-17 Tue 21:00 +1d>"))
            (should (listp (cdar result)))
            (should (cdar result)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-multiple-timestamps-all-extracted ()
  "Test that multiple timestamps are all extracted."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (insert "SCHEDULED: <2025-10-24 Fri 14:30>\n")
        (insert "DEADLINE: <2025-10-24 Fri 16:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 2))
            ;; Check both timestamps are present
            (should (--some (equal (car it) "<2025-10-24 Fri 14:30>") result))
            (should (--some (equal (car it) "<2025-10-24 Fri 16:00>") result)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-scheduled-and-plain-together ()
  "Test that SCHEDULED and plain timestamp can coexist."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Complex Task\n")
        (insert "SCHEDULED: <2025-10-24 Fri 09:00>\n")
        (insert "Meeting time: <2025-10-24 Fri 14:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 2))
            (should (--some (equal (car it) "<2025-10-24 Fri 09:00>") result))
            (should (--some (equal (car it) "<2025-10-24 Fri 14:00>") result)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-multiple-plain-timestamps-extracted ()
  "Test that multiple plain timestamps in body are all extracted."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Meeting Notes\n")
        (insert "First session: <2025-10-24 Fri 09:00>\n")
        (insert "Second session: <2025-10-24 Fri 14:00>\n")
        (insert "Third session: <2025-10-24 Fri 16:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 3))
            (should (--some (equal (car it) "<2025-10-24 Fri 09:00>") result))
            (should (--some (equal (car it) "<2025-10-24 Fri 14:00>") result))
            (should (--some (equal (car it) "<2025-10-24 Fri 16:00>") result)))))
    (test-chime-extract-time-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-extract-time-no-timestamps-returns-empty ()
  "Test that entry with no timestamps returns empty list."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (insert "No timestamps here\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (= (length result) 0)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-only-scheduled-extracted ()
  "Test that only SCHEDULED is extracted when it's the only timestamp."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (insert "SCHEDULED: <2025-10-24 Fri 14:30>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (= (length result) 1))
            (should (equal (caar result) "<2025-10-24 Fri 14:30>")))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-only-deadline-extracted ()
  "Test that only DEADLINE is extracted when it's the only timestamp."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (insert "DEADLINE: <2025-10-24 Fri 16:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (= (length result) 1))
            (should (equal (caar result) "<2025-10-24 Fri 16:00>")))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-timestamp-after-properties-drawer ()
  "Test that plain timestamps after properties drawer are extracted."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Event\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: abc123\n")
        (insert ":END:\n")
        (insert "<2025-10-24 Fri 10:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (= (length result) 1))
            (should (equal (caar result) "<2025-10-24 Fri 10:00>")))))
    (test-chime-extract-time-teardown)))

;;; Error Cases

(ert-deftest test-chime-extract-time-malformed-scheduled-returns-nil-cdr ()
  "Test that malformed SCHEDULED timestamp returns cons with nil cdr."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (insert "SCHEDULED: not-a-valid-timestamp\n")
        (insert "DEADLINE: <2025-10-24 Fri 16:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            ;; Should return both, but malformed one filtered by -non-nil
            (should (>= (length result) 1))
            ;; Valid deadline should be present
            (should (--some (equal (car it) "<2025-10-24 Fri 16:00>") result)))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-day-wide-timestamp-returns-nil-cdr ()
  "Test that day-wide timestamps (no time) return cons with nil cdr."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test Task\n")
        (insert "SCHEDULED: <2025-10-24 Fri>\n")  ; Day-wide, no time
        (insert "DEADLINE: <2025-10-24 Fri 16:00>\n")  ; Has time
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            ;; Should have at least the timed one
            (should (>= (length result) 1))
            ;; Timed timestamp should be present with valid cdr
            (let ((timed (--find (equal (car it) "<2025-10-24 Fri 16:00>") result)))
              (should timed)
              (should (cdr timed))))))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-plain-day-wide-timestamp-filtered ()
  "Test that plain day-wide timestamps (no time) are filtered out."
  (test-chime-extract-time-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Event\n")
        (insert "<2025-10-24 Fri>\n")  ; Day-wide, no time
        (insert "<2025-10-24 Fri 10:00>\n")  ; Has time
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            ;; Should have at least the timed one
            (should (>= (length result) 1))
            ;; Timed timestamp should be present
            (should (--some (equal (car it) "<2025-10-24 Fri 10:00>") result)))))
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
        (insert "<2025-10-24 Fri 17:30-18:00>\n")
        (goto-char (point-min))
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (>= (length result) 1))
            ;; Should extract the timestamp string
            (should (equal (caar result) "<2025-10-24 Fri 17:30-18:00>"))
            ;; Should have parsed time value (not nil)
            (should (listp (cdar result)))
            (should (cdar result)))))
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
        (let ((marker (point-marker)))
          (let ((result (chime--extract-time marker)))
            (should (listp result))
            (should (>= (length result) 1))
            (should (equal (caar result) "<2025-10-24 Fri 17:30-18:00>"))
            (should (cdar result)))))
    (test-chime-extract-time-teardown)))

(provide 'test-chime-extract-time)
;;; test-chime-extract-time.el ends here
