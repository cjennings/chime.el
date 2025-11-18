;;; test-chime-modeline-no-events-text.el --- Tests for chime-modeline-no-events-text customization -*- lexical-binding: t; -*-

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

;; Unit tests for chime-modeline-no-events-text defcustom.
;; Tests the modeline display when no events are within lookahead window.
;;
;; Tests three scenarios:
;; 1. Setting is nil → show nothing in modeline
;; 2. Setting is custom text → show that text
;; 3. Event within lookahead → show event (ignores setting)

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

(defvar test-chime-modeline-no-events-text--orig-lookahead nil)
(defvar test-chime-modeline-no-events-text--orig-tooltip-lookahead nil)
(defvar test-chime-modeline-no-events-text--orig-no-events-text nil)

(defun test-chime-modeline-no-events-text-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Save original values
  (setq test-chime-modeline-no-events-text--orig-lookahead chime-modeline-lookahead-minutes)
  (setq test-chime-modeline-no-events-text--orig-tooltip-lookahead chime-tooltip-lookahead-hours)
  (setq test-chime-modeline-no-events-text--orig-no-events-text chime-modeline-no-events-text)
  ;; Set short lookahead for testing
  (setq chime-modeline-lookahead-minutes 60)  ; 1 hour
  (setq chime-tooltip-lookahead-hours 24))     ; 24 hours

(defun test-chime-modeline-no-events-text-teardown ()
  "Teardown function run after each test."
  ;; Restore original values
  (setq chime-modeline-lookahead-minutes test-chime-modeline-no-events-text--orig-lookahead)
  (setq chime-tooltip-lookahead-hours test-chime-modeline-no-events-text--orig-tooltip-lookahead)
  (setq chime-modeline-no-events-text test-chime-modeline-no-events-text--orig-no-events-text)
  (chime-delete-test-base-dir))

;;; Helper Functions

(defun test-chime-modeline-no-events-text--create-event (title time-offset-hours)
  "Create org content for event with TITLE at TIME-OFFSET-HOURS from now."
  (let* ((event-time (test-time-at 0 time-offset-hours 0))
         (timestamp (test-timestamp-string event-time)))
    (format "* TODO %s\nSCHEDULED: %s\n" title timestamp)))

(defun test-chime-modeline-no-events-text--gather-events (content)
  "Process CONTENT like chime-check does and return events list."
  (let* ((test-file (chime-create-temp-test-file-with-content content))
         (test-buffer (find-file-noselect test-file))
         (events nil))
    (with-current-buffer test-buffer
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (beginning-of-line)
        (let* ((marker (point-marker))
               (info (chime--gather-info marker)))
          (push info events))
        (forward-line 1)))
    (kill-buffer test-buffer)
    (nreverse events)))

(defun test-chime-modeline-no-events-text--update-and-get-modeline (events-content)
  "Create org file with EVENTS-CONTENT, update modeline, return chime-modeline-string."
  (let ((events (test-chime-modeline-no-events-text--gather-events events-content)))
    (chime--update-modeline events)
    ;; Return the modeline string
    chime-modeline-string))

;;; Normal Cases

(ert-deftest test-chime-modeline-no-events-text-normal-nil-setting-no-events-in-lookahead-returns-nil ()
  "Test that nil setting shows nothing when events exist beyond lookahead.

When chime-modeline-no-events-text is nil and events exist beyond
the lookahead window but not within it, the modeline should be nil
(show nothing)."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 3 hours from now (beyond 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Future Event" 3)))
        ;; Set to nil (default)
        (setq chime-modeline-no-events-text nil)
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (null result)))))
    (test-chime-modeline-no-events-text-teardown)))

(ert-deftest test-chime-modeline-no-events-text-normal-custom-text-no-events-in-lookahead-returns-text ()
  "Test that custom text displays when events exist beyond lookahead.

When chime-modeline-no-events-text is \" 🔕\" and events exist beyond
the lookahead window, the modeline should show \" 🔕\"."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 3 hours from now (beyond 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Future Event" 3)))
        ;; Set custom text
        (setq chime-modeline-no-events-text " 🔕")
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (stringp result))
            (should (string-match-p "🔕" result)))))
    (test-chime-modeline-no-events-text-teardown)))

(ert-deftest test-chime-modeline-no-events-text-normal-event-within-lookahead-shows-event ()
  "Test that event within lookahead is shown, ignoring no-events-text.

When an event is within the lookahead window, the modeline should show
the event regardless of chime-modeline-no-events-text setting."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 30 minutes from now (within 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Upcoming Event" 0.5)))
        ;; Set custom text (should be ignored)
        (setq chime-modeline-no-events-text " 🔕")
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (stringp result))
            (should (string-match-p "Upcoming Event" result))
            (should-not (string-match-p "🔕" result)))))
    (test-chime-modeline-no-events-text-teardown)))

(ert-deftest test-chime-modeline-no-events-text-normal-custom-text-has-tooltip ()
  "Test that custom text has tooltip when displayed.

When chime-modeline-no-events-text is displayed, it should have a
help-echo property with the tooltip showing upcoming events."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 3 hours from now (beyond 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Future Event" 3)))
        ;; Set custom text
        (setq chime-modeline-no-events-text " 🔕")
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (stringp result))
            ;; Check for help-echo property (tooltip)
            (should (get-text-property 0 'help-echo result)))))
    (test-chime-modeline-no-events-text-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-modeline-no-events-text-boundary-empty-string-no-events-returns-empty ()
  "Test that empty string setting shows empty string.

When chime-modeline-no-events-text is \"\" (empty string) and events
exist beyond lookahead, the modeline should show empty string."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 3 hours from now (beyond 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Future Event" 3)))
        ;; Set to empty string
        (setq chime-modeline-no-events-text "")
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (stringp result))
            (should (equal "" result)))))
    (test-chime-modeline-no-events-text-teardown)))

(ert-deftest test-chime-modeline-no-events-text-boundary-no-events-at-all-shows-icon ()
  "Test that icon appears even when there are no events at all.

When there are zero events (not just beyond lookahead, but none at all),
the modeline should still show the icon with a helpful tooltip explaining
that there are no events and suggesting to increase the lookahead window."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; No events - empty content
             (content ""))
        ;; Set custom text
        (setq chime-modeline-no-events-text " 🔕")
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            ;; Should show icon (not nil)
            (should result)
            (should (stringp result))
            (should (equal " 🔕" (substring-no-properties result)))
            ;; Should have tooltip explaining no events
            (let ((tooltip (get-text-property 0 'help-echo result)))
              (should tooltip)
              (should (string-match-p "No calendar events" tooltip))
              (should (string-match-p "chime-tooltip-lookahead-hours" tooltip))))))
    (test-chime-modeline-no-events-text-teardown)))

(ert-deftest test-chime-modeline-no-events-text-boundary-very-long-text-displays-correctly ()
  "Test that very long custom text displays correctly.

When chime-modeline-no-events-text is a very long string (50+ chars),
the modeline should show the full string."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 3 hours from now (beyond 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Future Event" 3))
             (long-text " No events within the next hour, but some later today"))
        ;; Set very long text
        (setq chime-modeline-no-events-text long-text)
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (stringp result))
            (should (equal long-text result)))))
    (test-chime-modeline-no-events-text-teardown)))

(ert-deftest test-chime-modeline-no-events-text-boundary-special-characters-emoji-displays ()
  "Test that special characters and emoji display correctly.

When chime-modeline-no-events-text contains emoji and unicode characters,
they should display correctly in the modeline."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 3 hours from now (beyond 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Future Event" 3))
             (emoji-text " 🔕🔔⏰📅"))
        ;; Set emoji text
        (setq chime-modeline-no-events-text emoji-text)
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (stringp result))
            (should (equal emoji-text result)))))
    (test-chime-modeline-no-events-text-teardown)))

;;; Error Cases

(ert-deftest test-chime-modeline-no-events-text-error-whitespace-only-displays-whitespace ()
  "Test that whitespace-only setting displays as-is.

When chime-modeline-no-events-text is \"   \" (whitespace only),
it should display the whitespace without trimming."
  (test-chime-modeline-no-events-text-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 3 hours from now (beyond 1-hour lookahead)
             (content (test-chime-modeline-no-events-text--create-event "Future Event" 3))
             (whitespace-text "   "))
        ;; Set whitespace text
        (setq chime-modeline-no-events-text whitespace-text)
        (with-test-time now
          (let ((result (test-chime-modeline-no-events-text--update-and-get-modeline content)))
            (should (stringp result))
            (should (equal whitespace-text result)))))
    (test-chime-modeline-no-events-text-teardown)))

(provide 'test-chime-modeline-no-events-text)
;;; test-chime-modeline-no-events-text.el ends here
