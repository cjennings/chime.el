;;; test-chime-gather-info.el --- Tests for chime--gather-info -*- lexical-binding: t; -*-

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

;; Integration tests for chime--gather-info function.
;; Tests ensure that event information is collected correctly
;; and that titles are properly sanitized to prevent Lisp read
;; syntax errors during async serialization.

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
(require 'testutil-events (expand-file-name "testutil-events.el"))

;;; Setup and Teardown

(defun test-chime-gather-info-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset to default alert intervals
  (setq chime-alert-intervals '((10 . medium))))

(defun test-chime-gather-info-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-gather-info-extracts-all-components ()
  "Test that gather-info extracts times, title, intervals, and marker.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Team Meeting\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should have all required keys
            (should (assoc 'times info))
            (should (assoc 'title info))
            (should (assoc 'intervals info))
            (should (assoc 'marker-file info))
            (should (assoc 'marker-pos info))
            ;; Title should be extracted
            (should (string-equal "Team Meeting" (cdr (assoc 'title info))))
            ;; Intervals should include default alert interval as cons cell
            (should (member '(10 . medium) (cdr (assoc 'intervals info))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-with-balanced-parens-in-title ()
  "Test that balanced parentheses in title are preserved.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Meeting (Team Sync)\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            (should (string-equal "Meeting (Team Sync)" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

;;; Sanitization Cases

(ert-deftest test-chime-gather-info-sanitizes-unmatched-opening-paren ()
  "Test that unmatched opening parenthesis in title is closed.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Meeting (Team Sync\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should add closing paren
            (should (string-equal "Meeting (Team Sync)" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-sanitizes-unmatched-opening-bracket ()
  "Test that unmatched opening bracket in title is closed.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 15 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Review [PR #123\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should add closing bracket
            (should (string-equal "Review [PR #123]" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-sanitizes-unmatched-opening-brace ()
  "Test that unmatched opening brace in title is closed.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 16 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Code Review {urgent\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should add closing brace
            (should (string-equal "Code Review {urgent}" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-sanitizes-multiple-unmatched-delimiters ()
  "Test that multiple unmatched delimiters are all closed.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 17 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Meeting [Team (Sync {Status\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should close all unmatched delimiters
            (should (string-equal "Meeting [Team (Sync {Status})]" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-sanitizes-unmatched-closing-paren ()
  "Test that unmatched closing parenthesis is removed.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Meeting Title)\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should remove extra closing paren
            (should (string-equal "Meeting Title" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

;;; Real-World Bug Cases

(ert-deftest test-chime-gather-info-bug-case-extended-leadership ()
  "Test the actual bug case from vineti.meetings.org.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 13 1))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO 1:01pm CTO/COO XLT (Extended Leadership\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should close the unmatched paren
            (should (string-equal "1:01pm CTO/COO XLT (Extended Leadership)" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-bug-case-spice-cake ()
  "Test the actual bug case from journal/2023-11-22.org.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 18 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Spice Cake (\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should close the unmatched paren
            (should (string-equal "Spice Cake ()" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

;;; Serialization Safety

(ert-deftest test-chime-gather-info-output-serializable-with-unmatched-parens ()
  "Test that gather-info output with unmatched parens can be serialized.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Meeting (Team\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker))
                 ;; Extract just the title for serialization test
                 (title (cdr (assoc 'title info)))
                 ;; Simulate what happens in async serialization
                 (serialized (format "'((title . \"%s\"))" title)))
            ;; Should not signal 'invalid-read-syntax error
            (should (listp (read serialized)))
            ;; Title should be sanitized
            (should (string-equal "Meeting (Team)" title))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-multiple-events-all-serializable ()
  "Test that multiple events with various delimiter issues are all serializable.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (problematic-titles '("Meeting (Team"
                                  "Review [PR"
                                  "Code {Status"
                                  "Event (("
                                  "Task ))"))
             (test-content (mapconcat
                           (lambda (title)
                             (format "* TODO %s\nSCHEDULED: %s\n" title timestamp))
                           problematic-titles
                           "\n"))
             (test-file (chime-create-temp-test-file-with-content test-content))
             (test-buffer (find-file-noselect test-file))
             (all-info '()))
        (with-current-buffer test-buffer
          (org-mode)
          ;; Gather info for all events
          (goto-char (point-min))
          (while (re-search-forward "^\\*\\s-+TODO" nil t)
            (beginning-of-line)
            (let* ((marker (point-marker))
                   (info (chime--gather-info marker)))
              (push info all-info)
              (end-of-line)))
          ;; Try to serialize all titles
          (dolist (info all-info)
            (let* ((title (cdr (assoc 'title info)))
                   (serialized (format "'((title . \"%s\"))" title)))
              ;; Should not signal error
              (should (listp (read serialized))))))
        (kill-buffer test-buffer))
    ))

;;; Edge Cases

(ert-deftest test-chime-gather-info-handles-empty-title ()
  "Test that gather-info handles entries with no title.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should return empty string for nil title
            (should (string-equal "" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-handles-very-long-title-with-delimiters ()
  "Test that gather-info handles very long titles with unmatched delimiters.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (long-title "This is a very long meeting title that contains many words and might wrap in the notification display (Extended Info")
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO %s\nSCHEDULED: %s\n" long-title timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker))
                 (title (cdr (assoc 'title info))))
            ;; Should close the unmatched paren
            (should (string-suffix-p ")" title))
            ;; Should be able to serialize
            (should (listp (read (format "'((title . \"%s\"))" title))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-serializable-without-marker-object ()
  "Test that gather-info returns serializable data without marker object.

This tests the fix for the bug where marker objects from buffers with names
like 'todo.org<jr-estate>' could not be serialized because angle brackets in
the buffer name created invalid Lisp syntax: #<marker ... in todo.org<dir>>

The fix returns marker-file and marker-pos instead of the marker object,
which can be properly serialized regardless of buffer name.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                          (format "* TODO Test Task\nSCHEDULED: %s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))

            ;; Should have marker-file and marker-pos, NOT marker object
            (should (assoc 'marker-file info))
            (should (assoc 'marker-pos info))
            (should-not (assoc 'marker info))

            ;; The file path and position should be correct
            (should (string-equal test-file (cdr (assoc 'marker-file info))))
            (should (numberp (cdr (assoc 'marker-pos info))))
            (should (> (cdr (assoc 'marker-pos info)) 0))

            ;; The entire structure should be serializable via format %S and read
            ;; This simulates what async.el does with the data
            (let* ((serialized (format "%S" info))
                   (deserialized (read serialized)))
              ;; Should deserialize without error
              (should (listp deserialized))
              ;; Should have the same data structure
              (should (string-equal (cdr (assoc 'title deserialized))
                                  (cdr (assoc 'title info))))
              (should (string-equal (cdr (assoc 'marker-file deserialized))
                                  (cdr (assoc 'marker-file info))))
              (should (equal (cdr (assoc 'marker-pos deserialized))
                           (cdr (assoc 'marker-pos info)))))))
        (kill-buffer test-buffer))
    ))

(ert-deftest test-chime-gather-info-special-chars-in-title ()
  "Test that titles with Lisp special characters serialize correctly.

Tests characters that could theoretically cause Lisp read syntax errors:
- Double quotes: string delimiters
- Backslashes: escape characters
- Semicolons: comment start
- Backticks/commas: quasiquote syntax
- Hash symbols: reader macros

These should all be properly escaped by format %S.

REFACTORED: Uses dynamic timestamps"
  (with-test-setup
    (setq chime-alert-intervals '((10 . medium)))
    (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
            (special-titles '(("Quote in \"middle\"" . "Quote in \"middle\"")
                             ("Backslash\\path\\here" . "Backslash\\path\\here")
                             ("Semicolon; not a comment" . "Semicolon; not a comment")
                             ("Backtick `and` comma, here" . "Backtick `and` comma, here")
                             ("Hash #tag and @mention" . "Hash #tag and @mention")
                             ("Mixed: \"foo\\bar;baz`qux#\"" . "Mixed: \"foo\\bar;baz`qux#\""))))
        (dolist (title-pair special-titles)
          (let* ((title (car title-pair))
                 (expected (cdr title-pair))
                 (test-content (format "* TODO %s\nSCHEDULED: %s\n" title timestamp))
                 (test-file (chime-create-temp-test-file-with-content test-content))
                 (test-buffer (find-file-noselect test-file)))
            (with-current-buffer test-buffer
              (org-mode)
              (goto-char (point-min))
              (let* ((marker (point-marker))
                     (info (chime--gather-info marker)))
                ;; Title should be preserved exactly
                (should (string-equal expected (cdr (assoc 'title info))))
                ;; Full structure should serialize/deserialize correctly
                (let* ((serialized (format "%S" info))
                       (deserialized (read serialized)))
                  (should (listp deserialized))
                  (should (string-equal expected (cdr (assoc 'title deserialized)))))))
            (kill-buffer test-buffer))))
    ))

(provide 'test-chime-gather-info)
;;; test-chime-gather-info.el ends here
