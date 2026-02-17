;;; test-chime-extract-time.el --- Tests for chime--extract-time -*- lexical-binding: t; -*-

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

;; Tests for chime--extract-time function with source-aware extraction:
;; - org-gcal events: extract ONLY from :org-gcal: drawer
;; - Regular events: prefer SCHEDULED/DEADLINE, fall back to plain timestamps
;; - Prevents duplicate entries when events are rescheduled

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

(defun test-chime-extract-time-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-extract-time-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Tests for org-gcal events

(ert-deftest test-chime-extract-time-gcal-event-from-drawer ()
  "Test that org-gcal events extract timestamps ONLY from :org-gcal: drawer.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp-str (format-time-string "<%Y-%m-%d %a %H:%M-15:00>" time))
             (test-content (format "* Meeting
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
%s
:END:
" timestamp-str))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract the timestamp from :org-gcal: drawer
            (should (= 1 (length times)))
            (should (string-match-p "14:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-gcal-event-ignores-body-timestamps ()
  "Test that org-gcal events ignore plain timestamps in body text.

When an event is rescheduled, old timestamps might remain in the body.
The :org-gcal: drawer has the correct time, so we should ignore body text.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((new-time (test-time-tomorrow-at 14 0))
             (old-time (test-time-today-at 14 0))
             (new-timestamp (test-timestamp-string new-time))
             (old-timestamp (test-timestamp-string old-time))
             (test-content (format "* Meeting
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
%s
:END:
Old time was %s
" new-timestamp old-timestamp))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract ONLY from drawer (tomorrow), ignore body (today)
            (should (= 1 (length times)))
            (should (string-match-p "14:00" (car (car times))))
            ;; Verify it's the new timestamp, not the old one
            (should (string-match-p (format-time-string "%Y-%m-%d" new-time) (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-gcal-event-ignores-scheduled ()
  "Test that org-gcal events ignore SCHEDULED/DEADLINE properties.

For org-gcal events, the :org-gcal: drawer is the source of truth.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((drawer-time (test-time-tomorrow-at 14 0))
             (scheduled-time (test-time-days-from-now 2))
             (drawer-timestamp (test-timestamp-string drawer-time))
             (scheduled-timestamp (test-timestamp-string scheduled-time))
             (test-content (format "* Meeting
SCHEDULED: %s
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
%s
:END:
" scheduled-timestamp drawer-timestamp))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract ONLY from drawer (tomorrow), ignore SCHEDULED (day after)
            (should (= 1 (length times)))
            (should (string-match-p "14:00" (car (car times))))
            (should (string-match-p (format-time-string "%Y-%m-%d" drawer-time) (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-gcal-event-multiple-in-drawer ()
  "Test that org-gcal events extract all timestamps from :org-gcal: drawer.

Some recurring events might have multiple timestamps in the drawer.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((time1 (test-time-tomorrow-at 14 0))
             (time2 (test-time-days-from-now 2 14 0))
             (timestamp1 (test-timestamp-string time1))
             (timestamp2 (test-timestamp-string time2))
             (test-content (format "* Meeting
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
%s
%s
:END:
" timestamp1 timestamp2))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract both timestamps from drawer
            (should (= 2 (length times)))
            (should (string-match-p "14:00" (car (car times))))
            (should (string-match-p "14:00" (car (cadr times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

;;; Tests for regular org events

(ert-deftest test-chime-extract-time-regular-event-scheduled ()
  "Test that regular events extract SCHEDULED timestamp.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-content (format "* Task
SCHEDULED: %s
" timestamp))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract SCHEDULED timestamp
            (should (= 1 (length times)))
            (should (string-match-p "14:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-deadline ()
  "Test that regular events extract DEADLINE timestamp.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 17 0))
             (timestamp (test-timestamp-string time))
             (test-content (format "* Task
DEADLINE: %s
" timestamp))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract DEADLINE timestamp
            (should (= 1 (length times)))
            (should (string-match-p "17:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-plain-timestamps ()
  "Test that regular events extract plain timestamps when no SCHEDULED/DEADLINE.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-content (format "* Meeting notes
Discussed: %s
" timestamp))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract plain timestamp
            (should (= 1 (length times)))
            (should (string-match-p "14:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-scheduled-and-plain ()
  "Test that regular events extract both SCHEDULED and plain timestamps.

SCHEDULED/DEADLINE appear first, then plain timestamps.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((scheduled-time (test-time-tomorrow-at 14 0))
             (plain-time (test-time-days-from-now 2 15 0))
             (scheduled-timestamp (test-timestamp-string scheduled-time))
             (plain-timestamp (format-time-string "<%Y-%m-%d %a %H:%M>" plain-time))
             (test-content (format "* Task
SCHEDULED: %s
Note: also happens %s
" scheduled-timestamp plain-timestamp))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract both: SCHEDULED first, then plain
            (should (= 2 (length times)))
            ;; First should be SCHEDULED
            (should (string-match-p "14:00" (car (car times))))
            ;; Second should be plain at 15:00
            (should (string-match-p "15:00" (car (cadr times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-multiple-plain ()
  "Test that regular events extract all plain timestamps.

REFACTORED: Uses dynamic timestamps"
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((time1 (test-time-tomorrow-at 14 0))
             (time2 (test-time-days-from-now 2 15 0))
             (timestamp1 (test-timestamp-string time1))
             (timestamp2 (test-timestamp-string time2))
             (test-content (format "* Meeting notes
First discussion: %s
Second discussion: %s
" timestamp1 timestamp2))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract both plain timestamps
            (should (= 2 (length times)))
            (should (string-match-p "14:00" (car (car times))))
            (should (string-match-p "15:00" (car (cadr times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(provide 'test-chime-extract-time)
;;; test-chime-extract-time.el ends here
