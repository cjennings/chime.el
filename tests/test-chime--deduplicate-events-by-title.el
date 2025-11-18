;;; test-chime--deduplicate-events-by-title.el --- Tests for event deduplication by title -*- lexical-binding: t; -*-

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

;; Unit tests for chime--deduplicate-events-by-title.
;; Tests that recurring events (expanded into multiple instances by org-agenda-list)
;; are deduplicated to show only the soonest occurrence of each title.
;;
;; This fixes bug001: Recurring Events Show Duplicate Entries in Tooltip

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

;; Enable debug mode and load chime
(setq chime-debug t)
(load (expand-file-name "../chime.el") nil t)

;; Load test utilities
(require 'testutil-general (expand-file-name "testutil-general.el"))
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Test Helpers

(defun test-make-event (title)
  "Create a test event object with TITLE."
  `((title . ,title)))

(defun test-make-upcoming-item (title minutes)
  "Create a test upcoming-events item with TITLE and MINUTES until event.
Returns format: (EVENT TIME-INFO MINUTES)"
  (list (test-make-event title)
        '("dummy-time-string" . nil)  ; TIME-INFO (not used in deduplication)
        minutes))

;;; Normal Cases

(ert-deftest test-chime--deduplicate-events-by-title-normal-recurring-daily-keeps-soonest ()
  "Test that recurring daily event keeps only the soonest occurrence."
  (let* ((events (list
                  (test-make-upcoming-item "Daily Standup" 60)    ; 1 hour away
                  (test-make-upcoming-item "Daily Standup" 1500)  ; tomorrow
                  (test-make-upcoming-item "Daily Standup" 2940))) ; day after
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (string= "Daily Standup" (cdr (assoc 'title (car (car result))))))
    (should (= 60 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-normal-multiple-different-events ()
  "Test that different event titles are all preserved."
  (let* ((events (list
                  (test-make-upcoming-item "Meeting A" 30)
                  (test-make-upcoming-item "Meeting B" 60)
                  (test-make-upcoming-item "Meeting C" 90)))
         (result (chime--deduplicate-events-by-title events)))
    (should (= 3 (length result)))
    ;; All three events should be present
    (should (cl-find-if (lambda (item) (string= "Meeting A" (cdr (assoc 'title (car item))))) result))
    (should (cl-find-if (lambda (item) (string= "Meeting B" (cdr (assoc 'title (car item))))) result))
    (should (cl-find-if (lambda (item) (string= "Meeting C" (cdr (assoc 'title (car item))))) result))))

(ert-deftest test-chime--deduplicate-events-by-title-normal-mixed-recurring-and-unique ()
  "Test mix of recurring (duplicated) and unique events."
  (let* ((events (list
                  (test-make-upcoming-item "Daily Wrap Up" 120)   ; 2 hours
                  (test-make-upcoming-item "Team Sync" 180)       ; 3 hours (unique)
                  (test-make-upcoming-item "Daily Wrap Up" 1560)  ; tomorrow
                  (test-make-upcoming-item "Daily Wrap Up" 3000))) ; day after
         (result (chime--deduplicate-events-by-title events)))
    (should (= 2 (length result)))
    ;; Daily Wrap Up should appear once (soonest at 120 minutes)
    (let ((daily-wrap-up (cl-find-if (lambda (item)
                                        (string= "Daily Wrap Up" (cdr (assoc 'title (car item)))))
                                      result)))
      (should daily-wrap-up)
      (should (= 120 (caddr daily-wrap-up))))
    ;; Team Sync should appear once
    (let ((team-sync (cl-find-if (lambda (item)
                                    (string= "Team Sync" (cdr (assoc 'title (car item)))))
                                  result)))
      (should team-sync)
      (should (= 180 (caddr team-sync))))))

;;; Boundary Cases

(ert-deftest test-chime--deduplicate-events-by-title-boundary-empty-list-returns-empty ()
  "Test that empty list returns empty list."
  (let ((result (chime--deduplicate-events-by-title '())))
    (should (null result))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-single-event-returns-same ()
  "Test that single event is returned unchanged."
  (let* ((events (list (test-make-upcoming-item "Solo Event" 45)))
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (string= "Solo Event" (cdr (assoc 'title (car (car result))))))
    (should (= 45 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-all-same-title-keeps-soonest ()
  "Test that when all events have same title, only the soonest is kept."
  (let* ((events (list
                  (test-make-upcoming-item "Recurring Task" 300)
                  (test-make-upcoming-item "Recurring Task" 100)  ; soonest
                  (test-make-upcoming-item "Recurring Task" 500)
                  (test-make-upcoming-item "Recurring Task" 200)))
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (= 100 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-two-events-same-title-keeps-soonest ()
  "Test that with two events of same title, soonest is kept."
  (let* ((events (list
                  (test-make-upcoming-item "Daily Check" 200)
                  (test-make-upcoming-item "Daily Check" 50)))  ; soonest
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (= 50 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-same-title-same-time ()
  "Test events with same title and same time (edge case).
One instance should be kept."
  (let* ((events (list
                  (test-make-upcoming-item "Duplicate Time" 100)
                  (test-make-upcoming-item "Duplicate Time" 100)))
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (= 100 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-zero-minutes ()
  "Test event happening right now (0 minutes away)."
  (let* ((events (list
                  (test-make-upcoming-item "Happening Now" 0)
                  (test-make-upcoming-item "Happening Now" 1440)))  ; tomorrow
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (= 0 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-large-minute-values ()
  "Test with very large minute values (1 year lookahead)."
  (let* ((events (list
                  (test-make-upcoming-item "Annual Review" 60)
                  (test-make-upcoming-item "Annual Review" 525600)))  ; 365 days
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (= 60 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-title-with-special-chars ()
  "Test titles with special characters."
  (let* ((events (list
                  (test-make-upcoming-item "Review: Q1 Report (Draft)" 100)
                  (test-make-upcoming-item "Review: Q1 Report (Draft)" 200)))
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (= 100 (caddr (car result))))))

(ert-deftest test-chime--deduplicate-events-by-title-boundary-empty-title ()
  "Test events with empty string titles."
  (let* ((events (list
                  (test-make-upcoming-item "" 100)
                  (test-make-upcoming-item "" 200)))
         (result (chime--deduplicate-events-by-title events)))
    (should (= 1 (length result)))
    (should (= 100 (caddr (car result))))))

;;; Error Cases

(ert-deftest test-chime--deduplicate-events-by-title-error-nil-input-returns-empty ()
  "Test that nil input returns empty list."
  (let ((result (chime--deduplicate-events-by-title nil)))
    (should (null result))))

(provide 'test-chime--deduplicate-events-by-title)
;;; test-chime--deduplicate-events-by-title.el ends here
