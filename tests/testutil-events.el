;;; testutil-events.el --- Event creation and gathering utilities for tests -*- lexical-binding: t; -*-

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

;; Utilities for creating and gathering events in tests.
;; Reduces duplication across test files that work with org events.
;;
;; Key functions:
;; - test-create-org-event: Create org event content string
;; - test-gather-events-from-content: Create file, gather events, clean up
;; - test-make-event-data: Create event data structure programmatically

;;; Code:

(require 'testutil-general)
(require 'testutil-time)

;;; Event Content Creation

(defun test-create-org-event (title time &optional scheduled-p all-day-p)
  "Create org event content string with TITLE at TIME.
If SCHEDULED-P is non-nil, use SCHEDULED: keyword (default is plain timestamp).
If ALL-DAY-P is non-nil, create all-day event without time component.
Returns formatted org content string.

Examples:
  (test-create-org-event \"Meeting\" (test-time-now))
  => \"* Meeting\\n<2025-01-15 Wed 10:00>\\n\"

  (test-create-org-event \"Call\" (test-time-now) t)
  => \"* TODO Call\\nSCHEDULED: <2025-01-15 Wed 10:00>\\n\"

  (test-create-org-event \"Birthday\" (test-time-now) nil t)
  => \"* Birthday\\n<2025-01-15 Wed>\\n\""
  (let ((timestamp (test-timestamp-string time all-day-p))
        (todo-kw (if scheduled-p "TODO " "")))
    (if scheduled-p
        (format "* %s%s\nSCHEDULED: %s\n" todo-kw title timestamp)
      (format "* %s\n%s\n" title timestamp))))

(defun test-create-org-events (event-specs)
  "Create multiple org events from EVENT-SPECS list.
Each spec is (TITLE TIME &optional SCHEDULED-P ALL-DAY-P).
Returns concatenated org content string.

Example:
  (test-create-org-events
   '((\"Meeting\" ,(test-time-at 0 2 0) t)
     (\"Call\" ,(test-time-at 0 4 0) t)))
  => \"* TODO Meeting\\nSCHEDULED: ...\\n* TODO Call\\nSCHEDULED: ...\\n\""
  (mapconcat (lambda (spec)
               (apply #'test-create-org-event spec))
             event-specs
             "\n"))

;;; Event Gathering

(defun test-gather-events-from-content (content)
  "Create temp org file with CONTENT, gather events using chime--gather-info.
Returns list of event data structures.
Automatically creates and cleans up buffer.

Example:
  (let* ((content (test-create-org-event \"Meeting\" (test-time-now) t))
         (events (test-gather-events-from-content content)))
    (should (= 1 (length events)))
    (should (string= \"Meeting\" (cdr (assoc 'title (car events))))))"
  (let* ((test-file (chime-create-temp-test-file-with-content content))
         (test-buffer (find-file-noselect test-file))
         (events nil))
    (unwind-protect
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ " nil t)
            (beginning-of-line)
            (push (chime--gather-info (point-marker)) events)
            (forward-line 1))
          (nreverse events))
      (kill-buffer test-buffer))))

(defun test-gather-single-event-from-content (content)
  "Like test-gather-events-from-content but returns single event (not list).
Signals error if content contains multiple events.
Useful when test expects exactly one event.

Example:
  (let* ((content (test-create-org-event \"Call\" (test-time-now) t))
         (event (test-gather-single-event-from-content content)))
    (should (string= \"Call\" (cdr (assoc 'title event)))))"
  (let ((events (test-gather-events-from-content content)))
    (unless (= 1 (length events))
      (error "Expected exactly 1 event, found %d" (length events)))
    (car events)))

;;; Event Data Structure Creation

(defun test-make-event-data (title time-alist &optional intervals)
  "Create event data structure programmatically.
TITLE is the event title string.
TIME-ALIST is list of (TIMESTAMP-STR . TIME-OBJECT) cons cells.
INTERVALS is optional list of (MINUTES . SEVERITY) cons cells.

This is useful for creating events without going through org-mode parsing.

Example:
  (let* ((time (test-time-now))
         (ts-str (test-timestamp-string time))
         (event (test-make-event-data
                 \"Meeting\"
                 (list (cons ts-str time))
                 '((10 . medium)))))
    (should (string= \"Meeting\" (cdr (assoc 'title event)))))"
  `((times . ,time-alist)
    (title . ,title)
    (intervals . ,(or intervals '((10 . medium))))))

(defun test-make-simple-event (title time &optional interval-minutes severity)
  "Create simple event data structure with single time and interval.
TITLE is event title.
TIME is the event time (Emacs time object).
INTERVAL-MINUTES defaults to 10.
SEVERITY defaults to 'medium.

Convenience wrapper around test-make-event-data for common case.

Example:
  (let ((event (test-make-simple-event \"Call\" (test-time-now) 5 'high)))
    (should (string= \"Call\" (cdr (assoc 'title event)))))"
  (let* ((ts-str (test-timestamp-string time))
         (interval (or interval-minutes 10))
         (sev (or severity 'medium)))
    (test-make-event-data title
                          (list (cons ts-str time))
                          (list (cons interval sev)))))

;;; Macros for Common Test Patterns

(defmacro with-test-event-file (content &rest body)
  "Create temp org file with CONTENT, execute BODY, clean up.
Binds `test-file' and `test-buffer' in BODY.

Example:
  (with-test-event-file (test-create-org-event \"Meeting\" (test-time-now))
    (with-current-buffer test-buffer
      (goto-char (point-min))
      (should (search-forward \"Meeting\" nil t))))"
  (declare (indent 1))
  `(let* ((test-file (chime-create-temp-test-file-with-content ,content))
          (test-buffer (find-file-noselect test-file)))
     (unwind-protect
         (progn ,@body)
       (kill-buffer test-buffer))))

(defmacro with-gathered-events (content events-var &rest body)
  "Create temp file with CONTENT, gather events into EVENTS-VAR, execute BODY.
Automatically creates file, gathers events, and cleans up.

Example:
  (with-gathered-events (test-create-org-event \"Call\" (test-time-now))
                        events
    (should (= 1 (length events)))
    (should (string= \"Call\" (cdr (assoc 'title (car events))))))"
  (declare (indent 2))
  `(let ((,events-var (test-gather-events-from-content ,content)))
     ,@body))

;;; Setup/Teardown Helpers

(defun test-standard-setup ()
  "Standard setup for tests: create test base dir.
Most tests can use this instead of custom setup function."
  (chime-create-test-base-dir))

(defun test-standard-teardown ()
  "Standard teardown for tests: delete test base dir.
Most tests can use this instead of custom teardown function."
  (chime-delete-test-base-dir))

(defmacro with-test-setup (&rest body)
  "Execute BODY with standard test setup/teardown.
Ensures test base dir is created before and cleaned up after.

Example:
  (ert-deftest test-something ()
    (with-test-setup
      (let ((file (chime-create-temp-test-file)))
        (should (file-exists-p file)))))"
  (declare (indent 0))
  `(progn
     (test-standard-setup)
     (unwind-protect
         (progn ,@body)
       (test-standard-teardown))))

(provide 'testutil-events)
;;; testutil-events.el ends here
