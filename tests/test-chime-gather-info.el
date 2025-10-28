;;; test-chime-gather-info.el --- Tests for chime--gather-info -*- lexical-binding: t; -*-

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

;;; Setup and Teardown

(defun test-chime-gather-info-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset to default alert time
  (setq chime-alert-time '(10)))

(defun test-chime-gather-info-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-gather-info-extracts-all-components ()
  "Test that gather-info extracts times, title, intervals, and marker."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Team Meeting\nSCHEDULED: <2025-10-28 Tue 14:00>\n"))
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
            (should (assoc 'marker info))
            ;; Title should be extracted
            (should (string-equal "Team Meeting" (cdr (assoc 'title info))))
            ;; Intervals should include default alert time
            (should (member 10 (cdr (assoc 'intervals info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-with-balanced-parens-in-title ()
  "Test that balanced parentheses in title are preserved."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Meeting (Team Sync)\nSCHEDULED: <2025-10-28 Tue 14:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            (should (string-equal "Meeting (Team Sync)" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

;;; Sanitization Cases

(ert-deftest test-chime-gather-info-sanitizes-unmatched-opening-paren ()
  "Test that unmatched opening parenthesis in title is closed."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Meeting (Team Sync\nSCHEDULED: <2025-10-28 Tue 14:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should add closing paren
            (should (string-equal "Meeting (Team Sync)" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-sanitizes-unmatched-opening-bracket ()
  "Test that unmatched opening bracket in title is closed."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Review [PR #123\nSCHEDULED: <2025-10-28 Tue 15:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should add closing bracket
            (should (string-equal "Review [PR #123]" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-sanitizes-unmatched-opening-brace ()
  "Test that unmatched opening brace in title is closed."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Code Review {urgent\nSCHEDULED: <2025-10-28 Tue 16:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should add closing brace
            (should (string-equal "Code Review {urgent}" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-sanitizes-multiple-unmatched-delimiters ()
  "Test that multiple unmatched delimiters are all closed."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Meeting [Team (Sync {Status\nSCHEDULED: <2025-10-28 Tue 17:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should close all unmatched delimiters
            (should (string-equal "Meeting [Team (Sync {Status})]" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-sanitizes-unmatched-closing-paren ()
  "Test that unmatched closing parenthesis is removed."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Meeting Title)\nSCHEDULED: <2025-10-28 Tue 14:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should remove extra closing paren
            (should (string-equal "Meeting Title" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

;;; Real-World Bug Cases

(ert-deftest test-chime-gather-info-bug-case-extended-leadership ()
  "Test the actual bug case from vineti.meetings.org."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO 1:01pm CTO/COO XLT (Extended Leadership\nSCHEDULED: <2025-10-28 Tue 13:01>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should close the unmatched paren
            (should (string-equal "1:01pm CTO/COO XLT (Extended Leadership)" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-bug-case-spice-cake ()
  "Test the actual bug case from journal/2023-11-22.org."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Spice Cake (\nSCHEDULED: <2025-10-28 Tue 18:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should close the unmatched paren
            (should (string-equal "Spice Cake ()" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

;;; Serialization Safety

(ert-deftest test-chime-gather-info-output-serializable-with-unmatched-parens ()
  "Test that gather-info output with unmatched parens can be serialized."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Meeting (Team\nSCHEDULED: <2025-10-28 Tue 14:00>\n"))
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
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-multiple-events-all-serializable ()
  "Test that multiple events with various delimiter issues are all serializable."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((problematic-titles '("Meeting (Team"
                                  "Review [PR"
                                  "Code {Status"
                                  "Event (("
                                  "Task ))"))
             (test-content (mapconcat
                           (lambda (title)
                             (format "* TODO %s\nSCHEDULED: <2025-10-28 Tue 14:00>\n" title))
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
    (test-chime-gather-info-teardown)))

;;; Edge Cases

(ert-deftest test-chime-gather-info-handles-empty-title ()
  "Test that gather-info handles entries with no title."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO\nSCHEDULED: <2025-10-28 Tue 14:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker)))
            ;; Should return empty string for nil title
            (should (string-equal "" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-handles-very-long-title-with-delimiters ()
  "Test that gather-info handles very long titles with unmatched delimiters."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((long-title "This is a very long meeting title that contains many words and might wrap in the notification display (Extended Info")
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO %s\nSCHEDULED: <2025-10-28 Tue 14:00>\n" long-title)))
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
    (test-chime-gather-info-teardown)))

(ert-deftest test-chime-gather-info-with-custom-notify-property ()
  "Test that gather-info includes custom notification intervals."
  (test-chime-gather-info-setup)
  (unwind-protect
      (let* ((test-file (chime-create-temp-test-file-with-content
                        "* TODO Meeting (Team\n:PROPERTIES:\n:CHIME_NOTIFY_BEFORE: 5 15\n:END:\nSCHEDULED: <2025-10-28 Tue 14:00>\n"))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (let* ((marker (point-marker))
                 (info (chime--gather-info marker))
                 (intervals (cdr (assoc 'intervals info))))
            ;; Should include default (10) plus custom (5, 15)
            (should (member 10 intervals))
            (should (member 5 intervals))
            (should (member 15 intervals))
            ;; Title should still be sanitized
            (should (string-equal "Meeting (Team)" (cdr (assoc 'title info))))))
        (kill-buffer test-buffer))
    (test-chime-gather-info-teardown)))

(provide 'test-chime-gather-info)
;;; test-chime-gather-info.el ends here
