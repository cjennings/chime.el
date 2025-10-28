;;; test-chime-update-modeline.el --- Tests for chime--update-modeline -*- lexical-binding: t; -*-

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

;; Unit tests for chime--update-modeline function.
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

(defun test-chime-update-modeline-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset modeline settings
  (setq chime-modeline-string nil)
  (setq chime-enable-modeline t)
  (setq chime-modeline-lookahead 30)
  (setq chime-modeline-format " ⏰ %s"))

(defun test-chime-update-modeline-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  (setq chime-modeline-string nil))

;;; Normal Cases

(ert-deftest test-chime-update-modeline-single-event-within-window-updates-modeline ()
  "Test that single event within lookahead window updates modeline."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 ;; Event at 14:10 (10 minutes from now)
                 (event-time (encode-time 0 10 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                          (title . "Team Meeting")))
                 (events (list event)))
        (chime--update-modeline events)
        ;; Should set modeline string
        (should chime-modeline-string)
        (should (stringp chime-modeline-string))
        (should (string-match-p "Team Meeting" chime-modeline-string))
        (should (string-match-p "10 minutes" chime-modeline-string)))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-multiple-events-picks-soonest ()
  "Test that with multiple events, soonest one is shown."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 ;; Event 1 at 14:05 (5 minutes - soonest)
                 (event-time-1 (encode-time 0 5 14 24 10 2025))
                 (event1 `((times . ((("<2025-10-24 Fri 14:05>" . ,event-time-1))))
                           (title . "Standup")))
                 ;; Event 2 at 14:25 (25 minutes)
                 (event-time-2 (encode-time 0 25 14 24 10 2025))
                 (event2 `((times . ((("<2025-10-24 Fri 14:25>" . ,event-time-2))))
                           (title . "Code Review")))
                 (events (list event1 event2)))
        (chime--update-modeline events)
        ;; Should show the soonest event
        (should chime-modeline-string)
        (should (string-match-p "Standup" chime-modeline-string))
        (should-not (string-match-p "Code Review" chime-modeline-string)))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-event-outside-window-no-update ()
  "Test that event outside lookahead window doesn't update modeline."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 ;; Event at 15:10 (70 minutes from now, outside 30 minute window)
                 (event-time (encode-time 0 10 15 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 15:10>" . ,event-time))))
                          (title . "Far Future Event")))
                 (events (list event)))
        (chime--update-modeline events)
        ;; Should NOT set modeline string
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-zero-lookahead-clears-modeline ()
  "Test that zero lookahead clears modeline."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 (event-time (encode-time 0 10 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                          (title . "Team Meeting")))
                 (events (list event)))
        (setq chime-modeline-lookahead 0)
        (chime--update-modeline events)
        ;; Should clear modeline
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-normal-disabled-clears-modeline ()
  "Test that chime-enable-modeline nil clears modeline even with valid event."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 (event-time (encode-time 0 10 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                          (title . "Team Meeting")))
                 (events (list event)))
        (setq chime-enable-modeline nil)
        (chime--update-modeline events)
        ;; Should NOT set modeline string when disabled
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-normal-enabled-updates-modeline ()
  "Test that chime-enable-modeline t allows normal modeline updates."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 (event-time (encode-time 0 10 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                          (title . "Team Meeting")))
                 (events (list event)))
        (setq chime-enable-modeline t)
        (chime--update-modeline events)
        ;; Should set modeline string when enabled
        (should chime-modeline-string)
        (should (string-match-p "Team Meeting" chime-modeline-string)))
    (test-chime-update-modeline-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-update-modeline-no-events-clears-modeline ()
  "Test that no events clears modeline."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 (events '()))
        (chime--update-modeline events)
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-day-wide-events-filtered-out ()
  "Test that day-wide events are filtered out."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 (event-time (encode-time 0 0 0 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri>" . ,event-time))))
                          (title . "All Day Event")))
                 (events (list event)))
        (chime--update-modeline events)
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-event-at-exact-boundary-included ()
  "Test that event at exact lookahead boundary is included."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 ;; Event at 14:30 (exactly 30 minutes, at boundary)
                 (event-time (encode-time 0 30 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:30>" . ,event-time))))
                          (title . "Boundary Event")))
                 (events (list event)))
        (chime--update-modeline events)
        (should chime-modeline-string)
        (should (string-match-p "Boundary Event" chime-modeline-string)))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-boundary-disabled-overrides-lookahead ()
  "Test that chime-enable-modeline nil overrides positive lookahead."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 (event-time (encode-time 0 10 14 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 14:10>" . ,event-time))))
                          (title . "Team Meeting")))
                 (events (list event)))
        ;; Even with positive lookahead, disabled should prevent updates
        (setq chime-enable-modeline nil)
        (setq chime-modeline-lookahead 30)
        (chime--update-modeline events)
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

;;; Error Cases

(ert-deftest test-chime-update-modeline-past-events-not-shown ()
  "Test that past events are not shown in modeline."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* ((mock-time (encode-time 0 0 14 24 10 2025))
                 ((symbol-function 'current-time) (lambda () mock-time))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 ;; Event at 13:50 (10 minutes ago)
                 (event-time (encode-time 0 50 13 24 10 2025))
                 (event `((times . ((("<2025-10-24 Fri 13:50>" . ,event-time))))
                          (title . "Past Event")))
                 (events (list event)))
        (chime--update-modeline events)
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-error-nil-events-handles-gracefully ()
  "Test that nil events parameter doesn't crash."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* (((symbol-function 'current-time) (lambda () (encode-time 0 0 14 24 10 2025)))
                 ((symbol-function 'force-mode-line-update) (lambda ())))
        ;; Should not error with nil events
        (should-not (condition-case nil
                        (progn (chime--update-modeline nil) nil)
                      (error t)))
        ;; Modeline should remain unset or cleared
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-error-invalid-event-structure-handles-gracefully ()
  "Test that invalid event structure doesn't crash."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* (((symbol-function 'current-time) (lambda () (encode-time 0 0 14 24 10 2025)))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 ;; Event missing required fields
                 (invalid-event '((invalid . "structure")))
                 (events (list invalid-event)))
        ;; Should not crash even with invalid events
        (should-not (condition-case nil
                        (progn (chime--update-modeline events) nil)
                      (error t))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-error-event-with-nil-times-handles-gracefully ()
  "Test that event with nil times field doesn't crash."
  (test-chime-update-modeline-setup)
  (unwind-protect
      (cl-letf* (((symbol-function 'current-time) (lambda () (encode-time 0 0 14 24 10 2025)))
                 ((symbol-function 'force-mode-line-update) (lambda ()))
                 (event '((times . nil)
                          (title . "Event with nil times")))
                 (events (list event)))
        ;; Should not crash
        (should-not (condition-case nil
                        (progn (chime--update-modeline events) nil)
                      (error t)))
        ;; Modeline should not be set
        (should-not chime-modeline-string))
    (test-chime-update-modeline-teardown)))

(provide 'test-chime-update-modeline)
;;; test-chime-update-modeline.el ends here
