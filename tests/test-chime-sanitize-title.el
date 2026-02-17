;;; test-chime-sanitize-title.el --- Tests for chime--sanitize-title -*- lexical-binding: t; -*-

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

;; Unit tests for chime--sanitize-title function.
;; Tests cover:
;; - Unmatched opening delimiters (parentheses, brackets, braces)
;; - Unmatched closing delimiters
;; - Mixed unmatched delimiters
;; - Already balanced delimiters (no-op)
;; - Nil and empty strings
;; - Real-world bug cases that triggered the issue

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

(defun test-chime-sanitize-title-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-sanitize-title-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases - Already Balanced

(ert-deftest test-chime-sanitize-title-balanced-parens-unchanged ()
  "Test that balanced parentheses are unchanged.

REFACTORED: No timestamps used"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting (Team Sync)")
             (result (chime--sanitize-title title)))
        (should (string-equal title result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-balanced-brackets-unchanged ()
  "Test that balanced brackets are unchanged.

REFACTORED: No timestamps used"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Review [PR #123]")
             (result (chime--sanitize-title title)))
        (should (string-equal title result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-balanced-braces-unchanged ()
  "Test that balanced braces are unchanged.

REFACTORED: No timestamps used"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Code Review {urgent}")
             (result (chime--sanitize-title title)))
        (should (string-equal title result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-mixed-balanced-unchanged ()
  "Test that mixed balanced delimiters are unchanged.

REFACTORED: No timestamps used"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting [Team] (Sync) {Urgent}")
             (result (chime--sanitize-title title)))
        (should (string-equal title result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-nested-balanced-unchanged ()
  "Test that nested balanced delimiters are unchanged.

REFACTORED: No timestamps used"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Review (PR [#123] {urgent})")
             (result (chime--sanitize-title title)))
        (should (string-equal title result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-no-delimiters-unchanged ()
  "Test that titles without delimiters are unchanged.

REFACTORED: No timestamps used"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Simple Meeting Title")
             (result (chime--sanitize-title title)))
        (should (string-equal title result)))
    (test-chime-sanitize-title-teardown)))

;;; Unmatched Opening Delimiters

(ert-deftest test-chime-sanitize-title-unmatched-opening-paren ()
  "Test that unmatched opening parenthesis is closed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "CTO/COO XLT (Extended Leadership")
             (result (chime--sanitize-title title)))
        (should (string-equal "CTO/COO XLT (Extended Leadership)" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-unmatched-opening-paren-at-end ()
  "Test that unmatched opening parenthesis at end is closed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Spice Cake (")
             (result (chime--sanitize-title title)))
        (should (string-equal "Spice Cake ()" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-multiple-unmatched-opening-parens ()
  "Test that multiple unmatched opening parentheses are closed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting (Team (Sync")
             (result (chime--sanitize-title title)))
        (should (string-equal "Meeting (Team (Sync))" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-unmatched-opening-bracket ()
  "Test that unmatched opening bracket is closed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Review [PR #123")
             (result (chime--sanitize-title title)))
        (should (string-equal "Review [PR #123]" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-unmatched-opening-brace ()
  "Test that unmatched opening brace is closed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Code Review {urgent")
             (result (chime--sanitize-title title)))
        (should (string-equal "Code Review {urgent}" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-mixed-unmatched-opening-delimiters ()
  "Test that mixed unmatched opening delimiters are all closed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting [Team (Sync {Urgent")
             (result (chime--sanitize-title title)))
        (should (string-equal "Meeting [Team (Sync {Urgent})]" result)))
    (test-chime-sanitize-title-teardown)))

;;; Unmatched Closing Delimiters

(ert-deftest test-chime-sanitize-title-unmatched-closing-paren ()
  "Test that unmatched closing parenthesis is removed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting Title)")
             (result (chime--sanitize-title title)))
        (should (string-equal "Meeting Title" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-multiple-unmatched-closing-parens ()
  "Test that multiple unmatched closing parentheses are removed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting Title))")
             (result (chime--sanitize-title title)))
        (should (string-equal "Meeting Title" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-unmatched-closing-bracket ()
  "Test that unmatched closing bracket is removed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Review PR]")
             (result (chime--sanitize-title title)))
        (should (string-equal "Review PR" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-unmatched-closing-brace ()
  "Test that unmatched closing brace is removed."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Code Review}")
             (result (chime--sanitize-title title)))
        (should (string-equal "Code Review" result)))
    (test-chime-sanitize-title-teardown)))

;;; Complex Mixed Cases

(ert-deftest test-chime-sanitize-title-opening-and-closing-mixed ()
  "Test title with both unmatched opening and closing delimiters."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting (Team) Extra)")
             (result (chime--sanitize-title title)))
        ;; Should remove the extra closing paren
        (should (string-equal "Meeting (Team) Extra" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-complex-nesting-with-unmatched ()
  "Test complex nested delimiters with some unmatched."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting [Team (Sync] Extra")
             (result (chime--sanitize-title title)))
        ;; The ']' doesn't match the '[' (because '(' is in between)
        ;; So it's removed, and we close the '(' and '[' properly: ')'  and ']'
        (should (string-equal "Meeting [Team (Sync Extra)]" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-all-types-unmatched ()
  "Test with all three delimiter types unmatched."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting (Team [Project {Status")
             (result (chime--sanitize-title title)))
        (should (string-equal "Meeting (Team [Project {Status}])" result)))
    (test-chime-sanitize-title-teardown)))

;;; Edge Cases

(ert-deftest test-chime-sanitize-title-nil-returns-empty-string ()
  "Test that nil title returns empty string."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((result (chime--sanitize-title nil)))
        (should (string-equal "" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-empty-string-unchanged ()
  "Test that empty string is unchanged."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "")
             (result (chime--sanitize-title title)))
        (should (string-equal "" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-only-opening-delimiters ()
  "Test title with only opening delimiters."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "([{")
             (result (chime--sanitize-title title)))
        (should (string-equal "([{}])" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-only-closing-delimiters ()
  "Test title with only closing delimiters."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title ")]}")
             (result (chime--sanitize-title title)))
        (should (string-equal "" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-very-long-title-with-unmatched ()
  "Test very long title with unmatched delimiter."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "This is a very long meeting title that contains many words and might wrap in the notification display (Extended Info")
             (result (chime--sanitize-title title)))
        (should (string-equal "This is a very long meeting title that contains many words and might wrap in the notification display (Extended Info)" result)))
    (test-chime-sanitize-title-teardown)))

;;; Real-World Bug Cases

(ert-deftest test-chime-sanitize-title-bug-case-extended-leadership ()
  "Test the actual bug case from vineti.meetings.org."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "1:01pm             CTO/COO XLT (Extended Leadership")
             (result (chime--sanitize-title title)))
        (should (string-equal "1:01pm             CTO/COO XLT (Extended Leadership)" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-bug-case-spice-cake ()
  "Test the actual bug case from journal/2023-11-22.org."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Spice Cake (")
             (result (chime--sanitize-title title)))
        (should (string-equal "Spice Cake ()" result)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-lisp-serialization-safety ()
  "Test that sanitized title can be safely read by Lisp reader."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((title "Meeting (Team Sync")
             (sanitized (chime--sanitize-title title))
             ;; Simulate what happens in async serialization
             (serialized (format "'((title . \"%s\"))" sanitized)))
        ;; This should not signal an error
        (should (listp (read serialized)))
        (should (string-equal "Meeting (Team Sync)" sanitized)))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-sanitize-title-async-serialization-with-unmatched-parens ()
  "Test that titles with unmatched parens won't break async serialization."
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((problematic-titles '("Meeting (Team"
                                  "Review [PR"
                                  "Code {Status"
                                  "Event (("
                                  "Task ))")))
        (dolist (title problematic-titles)
          (let* ((sanitized (chime--sanitize-title title))
                 (serialized (format "'((title . \"%s\"))" sanitized)))
            ;; Should not signal 'invalid-read-syntax error
            (should (listp (read serialized))))))
    (test-chime-sanitize-title-teardown)))

;;; Integration with chime--extract-title

(ert-deftest test-chime-extract-title-sanitizes-output ()
  "Test that chime--extract-title applies sanitization.

REFACTORED: Uses dynamic timestamps"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO Meeting (Team Sync\n%s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)  ; Enable org-mode
          (goto-char (point-min))
          ;; Search for the heading
          (re-search-forward "^\\* TODO" nil t)
          (beginning-of-line)
          (let* ((marker (point-marker))
                 (title (chime--extract-title marker)))
            ;; Should be sanitized with closing paren added
            (should (string-equal "Meeting (Team Sync)" title))))
        (kill-buffer test-buffer))
    (test-chime-sanitize-title-teardown)))

(ert-deftest test-chime-extract-title-handles-nil ()
  "Test that chime--extract-title handles nil gracefully.

REFACTORED: Uses dynamic timestamps"
  (test-chime-sanitize-title-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (timestamp (test-timestamp-string time))
             (test-file (chime-create-temp-test-file-with-content
                        (format "* TODO\n%s\n" timestamp)))
             (test-buffer (find-file-noselect test-file)))
        (with-current-buffer test-buffer
          (org-mode)  ; Enable org-mode
          (goto-char (point-min))
          ;; Search for the heading
          (re-search-forward "^\\* TODO" nil t)
          (beginning-of-line)
          (let* ((marker (point-marker))
                 (title (chime--extract-title marker)))
            ;; Should return empty string for nil title
            (should (string-equal "" title))))
        (kill-buffer test-buffer))
    (test-chime-sanitize-title-teardown)))

(provide 'test-chime-sanitize-title)
;;; test-chime-sanitize-title.el ends here
