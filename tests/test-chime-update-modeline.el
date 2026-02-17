;;; test-chime-update-modeline.el --- Tests for chime--update-modeline -*- lexical-binding: t; -*-

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
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Setup and Teardown

(defun test-chime-update-modeline-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset modeline settings
  (setq chime-modeline-string nil)
  (setq chime-enable-modeline t)
  (setq chime-modeline-lookahead-minutes 30)
  (setq chime-modeline-format " ⏰ %s")
  ;; Disable no-events indicator for tests that expect nil modeline
  (setq chime-modeline-no-events-text nil)
  (setq chime-tooltip-lookahead-hours nil)) ; Use modeline lookahead

(defun test-chime-update-modeline-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  (setq chime-modeline-string nil))

;;; Normal Cases

(ert-deftest test-chime-update-modeline-single-event-within-window-updates-modeline ()
  "Test that single event within lookahead window updates modeline.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 14:10 (10 minutes from now)
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Team Meeting")))
                   (events (list event)))
              (chime--update-modeline events)
              ;; Should set modeline string
              (should chime-modeline-string)
              (should (stringp chime-modeline-string))
              (should (string-match-p "Team Meeting" chime-modeline-string))
              (should (string-match-p "10 minutes" chime-modeline-string))))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-multiple-events-picks-soonest ()
  "Test that with multiple events, soonest one is shown.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event 1 at 14:05 (5 minutes - soonest)
             (event-time-1 (test-time-today-at 14 5))
             (timestamp-str-1 (test-timestamp-string event-time-1))
             ;; Event 2 at 14:25 (25 minutes)
             (event-time-2 (test-time-today-at 14 25))
             (timestamp-str-2 (test-timestamp-string event-time-2)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event1 `((times . ((,timestamp-str-1 . ,event-time-1)))
                             (title . "Standup")))
                   (event2 `((times . ((,timestamp-str-2 . ,event-time-2)))
                             (title . "Code Review")))
                   (events (list event1 event2)))
              (chime--update-modeline events)
              ;; Should show the soonest event
              (should chime-modeline-string)
              (should (string-match-p "Standup" chime-modeline-string))
              (should-not (string-match-p "Code Review" chime-modeline-string))))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-event-outside-window-no-update ()
  "Test that event outside lookahead window doesn't update modeline.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 15:10 (70 minutes from now, outside 30 minute window)
             (event-time (test-time-today-at 15 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Far Future Event")))
                   (events (list event)))
              (chime--update-modeline events)
              ;; Should NOT set modeline string
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-zero-lookahead-clears-modeline ()
  "Test that zero lookahead clears modeline.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Team Meeting")))
                   (events (list event)))
              (setq chime-modeline-lookahead-minutes 0)
              (chime--update-modeline events)
              ;; Should clear modeline
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-normal-disabled-clears-modeline ()
  "Test that chime-enable-modeline nil clears modeline even with valid event.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Team Meeting")))
                   (events (list event)))
              (setq chime-enable-modeline nil)
              (chime--update-modeline events)
              ;; Should NOT set modeline string when disabled
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-normal-enabled-updates-modeline ()
  "Test that chime-enable-modeline t allows normal modeline updates.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Team Meeting")))
                   (events (list event)))
              (setq chime-enable-modeline t)
              (chime--update-modeline events)
              ;; Should set modeline string when enabled
              (should chime-modeline-string)
              (should (string-match-p "Team Meeting" chime-modeline-string))))))
    (test-chime-update-modeline-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-update-modeline-no-events-clears-modeline ()
  "Test that no events clears modeline.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let ((now (test-time-today-at 14 0)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let ((events '()))
              (chime--update-modeline events)
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-day-wide-events-filtered-out ()
  "Test that day-wide events are filtered out.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 0 0))
             (timestamp-str (test-timestamp-string event-time t)))  ; Day-wide
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "All Day Event")))
                   (events (list event)))
              (chime--update-modeline events)
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-event-at-exact-boundary-included ()
  "Test that event at exact lookahead boundary is included.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 14:30 (exactly 30 minutes, at boundary)
             (event-time (test-time-today-at 14 30))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Boundary Event")))
                   (events (list event)))
              (chime--update-modeline events)
              (should chime-modeline-string)
              (should (string-match-p "Boundary Event" chime-modeline-string))))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-boundary-disabled-overrides-lookahead ()
  "Test that chime-enable-modeline nil overrides positive lookahead.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Team Meeting")))
                   (events (list event)))
              ;; Even with positive lookahead, disabled should prevent updates
              (setq chime-enable-modeline nil)
              (setq chime-modeline-lookahead-minutes 30)
              (chime--update-modeline events)
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

;;; Error Cases

(ert-deftest test-chime-update-modeline-past-events-not-shown ()
  "Test that past events are not shown in modeline.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event at 13:50 (10 minutes ago)
             (event-time (test-time-today-at 13 50))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Past Event")))
                   (events (list event)))
              (chime--update-modeline events)
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-error-nil-events-handles-gracefully ()
  "Test that nil events parameter doesn't crash.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let ((now (test-time-today-at 14 0)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            ;; Should not error with nil events
            (should-not (condition-case nil
                            (progn (chime--update-modeline nil) nil)
                          (error t)))
            ;; Modeline should remain unset or cleared
            (should-not chime-modeline-string))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-error-invalid-event-structure-handles-gracefully ()
  "Test that invalid event structure doesn't crash.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let ((now (test-time-today-at 14 0)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* (;; Event missing required fields
                   (invalid-event '((invalid . "structure")))
                   (events (list invalid-event)))
              ;; Should not crash even with invalid events
              (should-not (condition-case nil
                              (progn (chime--update-modeline events) nil)
                            (error t)))))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-error-event-with-nil-times-handles-gracefully ()
  "Test that event with nil times field doesn't crash.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let ((now (test-time-today-at 14 0)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event '((times . nil)
                            (title . "Event with nil times")))
                   (events (list event)))
              ;; Should not crash
              (should-not (condition-case nil
                              (progn (chime--update-modeline events) nil)
                            (error t)))
              ;; Modeline should not be set
              (should-not chime-modeline-string)))))
    (test-chime-update-modeline-teardown)))

;;; Upcoming Events State Tests

(ert-deftest test-chime-update-modeline-upcoming-events-populated ()
  "Test that chime--upcoming-events is populated with all events in window.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Three events within 30 minute window
             (event1-time (test-time-today-at 14 5))
             (timestamp-str-1 (test-timestamp-string event1-time))
             (event2-time (test-time-today-at 14 10))
             (timestamp-str-2 (test-timestamp-string event2-time))
             (event3-time (test-time-today-at 14 25))
             (timestamp-str-3 (test-timestamp-string event3-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event1 `((times . ((,timestamp-str-1 . ,event1-time)))
                             (title . "Event 1")
                             (marker . nil)))
                   (event2 `((times . ((,timestamp-str-2 . ,event2-time)))
                             (title . "Event 2")
                             (marker . nil)))
                   (event3 `((times . ((,timestamp-str-3 . ,event3-time)))
                             (title . "Event 3")
                             (marker . nil)))
                   (events (list event1 event2 event3)))
              (chime--update-modeline events)
              ;; Should populate chime--upcoming-events
              (should chime--upcoming-events)
              ;; Should have all 3 events
              (should (= 3 (length chime--upcoming-events)))))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-upcoming-events-sorted ()
  "Test that chime--upcoming-events are sorted by time (soonest first).

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Add events in reverse order
             (event1-time (test-time-today-at 14 25))
             (timestamp-str-1 (test-timestamp-string event1-time))
             (event2-time (test-time-today-at 14 10))
             (timestamp-str-2 (test-timestamp-string event2-time))
             (event3-time (test-time-today-at 14 5))
             (timestamp-str-3 (test-timestamp-string event3-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event1 `((times . ((,timestamp-str-1 . ,event1-time)))
                             (title . "Latest Event")
                             (marker . nil)))
                   (event2 `((times . ((,timestamp-str-2 . ,event2-time)))
                             (title . "Middle Event")
                             (marker . nil)))
                   (event3 `((times . ((,timestamp-str-3 . ,event3-time)))
                             (title . "Soonest Event")
                             (marker . nil)))
                   (events (list event1 event2 event3)))
              (chime--update-modeline events)
              ;; First event should be soonest
              (let* ((first-event (car chime--upcoming-events))
                     (first-event-obj (car first-event))
                     (first-title (cdr (assoc 'title first-event-obj))))
                (should (string= "Soonest Event" first-title)))))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-upcoming-events-cleared-when-disabled ()
  "Test that chime--upcoming-events is cleared when modeline disabled.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Test Event")
                            (marker . nil)))
                   (events (list event)))
              ;; First populate with modeline enabled
              (setq chime-enable-modeline t)
              (chime--update-modeline events)
              (should chime--upcoming-events)
              ;; Now disable modeline
              (setq chime-enable-modeline nil)
              (chime--update-modeline events)
              ;; Should clear chime--upcoming-events
              (should-not chime--upcoming-events)))))
    (test-chime-update-modeline-teardown)))

(ert-deftest test-chime-update-modeline-upcoming-events-only-within-window ()
  "Test that only events within lookahead window are stored.

REFACTORED: Uses dynamic timestamps and with-test-time"
  (test-chime-update-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             ;; Event within window (10 minutes)
             (event1-time (test-time-today-at 14 10))
             (timestamp-str-1 (test-timestamp-string event1-time))
             ;; Event outside window (60 minutes, window is 30)
             (event2-time (test-time-today-at 15 0))
             (timestamp-str-2 (test-timestamp-string event2-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event1 `((times . ((,timestamp-str-1 . ,event1-time)))
                             (title . "Within Window")
                             (marker . nil)))
                   (event2 `((times . ((,timestamp-str-2 . ,event2-time)))
                             (title . "Outside Window")
                             (marker . nil)))
                   (events (list event1 event2)))
              (setq chime-modeline-lookahead-minutes 30)
              (setq chime-tooltip-lookahead-hours 0.5)  ; Also set tooltip lookahead
              (chime--update-modeline events)
              ;; Should only have 1 event (within window)
              (should (= 1 (length chime--upcoming-events)))))))
    (test-chime-update-modeline-teardown)))

(provide 'test-chime-update-modeline)
;;; test-chime-update-modeline.el ends here
