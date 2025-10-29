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

;;; Setup and Teardown

(defun test-chime-extract-time-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-extract-time-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Tests for org-gcal events

(ert-deftest test-chime-extract-time-gcal-event-from-drawer ()
  "Test that org-gcal events extract timestamps ONLY from :org-gcal: drawer."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Meeting
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
<2025-10-28 Tue 14:00-15:00>
:END:
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract the timestamp from :org-gcal: drawer
            (should (= 1 (length times)))
            (should (string-match-p "2025-10-28.*14:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-gcal-event-ignores-body-timestamps ()
  "Test that org-gcal events ignore plain timestamps in body text.

When an event is rescheduled, old timestamps might remain in the body.
The :org-gcal: drawer has the correct time, so we should ignore body text."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Meeting
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
<2025-10-29 Wed 14:00>
:END:
Old time was <2025-10-28 Tue 14:00>
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract ONLY from drawer (Oct 29), ignore body (Oct 28)
            (should (= 1 (length times)))
            (should (string-match-p "2025-10-29.*14:00" (car (car times))))
            (should-not (string-match-p "2025-10-28" (format "%s" times)))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-gcal-event-ignores-scheduled ()
  "Test that org-gcal events ignore SCHEDULED/DEADLINE properties.

For org-gcal events, the :org-gcal: drawer is the source of truth."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Meeting
SCHEDULED: <2025-10-30 Thu 14:00>
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
<2025-10-29 Wed 14:00>
:END:
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract ONLY from drawer (Oct 29), ignore SCHEDULED (Oct 30)
            (should (= 1 (length times)))
            (should (string-match-p "2025-10-29.*14:00" (car (car times))))
            (should-not (string-match-p "2025-10-30" (format "%s" times)))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-gcal-event-multiple-in-drawer ()
  "Test that org-gcal events extract all timestamps from :org-gcal: drawer.

Some recurring events might have multiple timestamps in the drawer."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Meeting
:PROPERTIES:
:entry-id: abc123@google.com
:END:
:org-gcal:
<2025-10-28 Tue 14:00>
<2025-10-29 Wed 14:00>
:END:
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract both timestamps from drawer
            (should (= 2 (length times)))
            (should (string-match-p "2025-10-28.*14:00" (car (car times))))
            (should (string-match-p "2025-10-29.*14:00" (car (cadr times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

;;; Tests for regular org events

(ert-deftest test-chime-extract-time-regular-event-scheduled ()
  "Test that regular events extract SCHEDULED timestamp."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Task
SCHEDULED: <2025-10-28 Tue 14:00>
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract SCHEDULED timestamp
            (should (= 1 (length times)))
            (should (string-match-p "2025-10-28.*14:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-deadline ()
  "Test that regular events extract DEADLINE timestamp."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Task
DEADLINE: <2025-10-28 Tue 17:00>
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract DEADLINE timestamp
            (should (= 1 (length times)))
            (should (string-match-p "2025-10-28.*17:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-plain-timestamps ()
  "Test that regular events extract plain timestamps when no SCHEDULED/DEADLINE."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Meeting notes
Discussed: <2025-10-28 Tue 14:00>
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract plain timestamp
            (should (= 1 (length times)))
            (should (string-match-p "2025-10-28.*14:00" (car (car times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-scheduled-and-plain ()
  "Test that regular events extract both SCHEDULED and plain timestamps.

SCHEDULED/DEADLINE appear first, then plain timestamps."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Task
SCHEDULED: <2025-10-28 Tue 14:00>
Note: also happens <2025-10-29 Wed 15:00>
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract both: SCHEDULED first, then plain
            (should (= 2 (length times)))
            ;; First should be SCHEDULED (Oct 28)
            (should (string-match-p "2025-10-28.*14:00" (car (car times))))
            ;; Second should be plain (Oct 29)
            (should (string-match-p "2025-10-29.*15:00" (car (cadr times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(ert-deftest test-chime-extract-time-regular-event-multiple-plain ()
  "Test that regular events extract all plain timestamps."
  (test-chime-extract-time-setup)
  (unwind-protect
      (let* ((test-content "* Meeting notes
First discussion: <2025-10-28 Tue 14:00>
Second discussion: <2025-10-29 Wed 15:00>
")
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (times (chime--extract-time marker)))
            ;; Should extract both plain timestamps
            (should (= 2 (length times)))
            (should (string-match-p "2025-10-28.*14:00" (car (car times))))
            (should (string-match-p "2025-10-29.*15:00" (car (cadr times))))))
        (kill-buffer test-buffer))
    (test-chime-extract-time-teardown)))

(provide 'test-chime-extract-time)
;;; test-chime-extract-time.el ends here
