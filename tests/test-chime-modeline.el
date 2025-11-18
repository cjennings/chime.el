;;; test-chime-modeline.el --- Tests for chime modeline and tooltip -*- lexical-binding: t; -*-

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

;; Integration tests for chime modeline and tooltip behavior:
;; - Tests that rescheduled gcal events show correct times
;; - Tests that events don't appear multiple times in tooltip
;; - Tests that tooltip shows events in correct order
;; - Replicates real-world user scenarios

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

(defun test-chime-modeline-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Save original values
  (setq test-chime-modeline--orig-lookahead chime-modeline-lookahead-minutes)
  (setq test-chime-modeline--orig-tooltip-lookahead chime-tooltip-lookahead-hours)
  ;; Set lookahead to 24 hours for testing (both modeline and tooltip)
  (setq chime-modeline-lookahead-minutes 1440)
  (setq chime-tooltip-lookahead-hours 24))

(defun test-chime-modeline-teardown ()
  "Teardown function run after each test."
  ;; Restore original values
  (setq chime-modeline-lookahead-minutes test-chime-modeline--orig-lookahead)
  (setq chime-tooltip-lookahead-hours test-chime-modeline--orig-tooltip-lookahead)
  (chime-delete-test-base-dir))

(defvar test-chime-modeline--orig-lookahead nil)
(defvar test-chime-modeline--orig-tooltip-lookahead nil)

;;; Helper functions

(defun test-chime-modeline--create-gcal-event (title time-str &optional old-time-str)
  "Create test org content for a gcal event.
TITLE is the event title.
TIME-STR is the current time in the :org-gcal: drawer.
OLD-TIME-STR is an optional old time that might remain in the body."
  (concat
   (format "* %s\n" title)
   ":PROPERTIES:\n"
   ":entry-id: test123@google.com\n"
   ":END:\n"
   ":org-gcal:\n"
   (format "%s\n" time-str)
   ":END:\n"
   (when old-time-str
     (format "Old time was: %s\n" old-time-str))))

(defun test-chime-modeline--gather-events (content)
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

(defun test-chime-modeline--count-in-string (regexp string)
  "Count occurrences of REGEXP in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

;;; Tests for org-gcal event rescheduling

(ert-deftest test-chime-modeline-gcal-event-after-reschedule ()
  "Test that rescheduled gcal event shows only NEW time, not old.

Scenario: User moves event in Google Calendar, syncs with org-gcal.
The body might still mention the old time, but modeline should show new time.

REFACTORED: Uses dynamic timestamps via testutil-time.el"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((tomorrow (test-time-tomorrow-at 14 0))
             (today (test-time-today-at 14 0))
             (tomorrow-str (test-timestamp-string tomorrow))
             (today-str (test-timestamp-string today))
             (content (test-chime-modeline--create-gcal-event
                       "Team Meeting"
                       tomorrow-str
                       today-str))
             (events (test-chime-modeline--gather-events content)))
        ;; Should have one event
        (should (= 1 (length events)))

        ;; Event should have only ONE timestamp (from drawer, not body)
        (let* ((event (car events))
               (times (cdr (assoc 'times event))))
          (should (= 1 (length times)))
          ;; times is a list of (timestamp-string . parsed-time) cons cells
          ;; Check the first timestamp string
          (let ((time-str (caar times)))
            (should (string-match-p ".*14:00" time-str))
            (should-not (string-match-p (format-time-string "%Y-%m-%d" today) time-str)))))
    (test-chime-modeline-teardown)))

;;; Tests for modeline deduplication

(ert-deftest test-chime-modeline-no-duplicate-events ()
  "Test that modeline doesn't show the same event multiple times.

Even if an event has multiple timestamps, it should appear only once
in the upcoming events list (with its soonest timestamp).

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (meeting-a-time (test-time-tomorrow-at 14 0))
             (meeting-b-time (test-time-tomorrow-at 15 0))
             (content (concat
                       (test-chime-modeline--create-gcal-event
                        "Meeting A"
                        (test-timestamp-string meeting-a-time))
                       (test-chime-modeline--create-gcal-event
                        "Meeting B"
                        (test-timestamp-string meeting-b-time))))
             (events (test-chime-modeline--gather-events content)))

        ;; Should have two events
        (should (= 2 (length events)))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline with these events
          (chime--update-modeline events)

          ;; Check that upcoming events list has no duplicates
          (should (= 2 (length chime--upcoming-events)))

          ;; Each event should appear exactly once
          (let ((titles (mapcar (lambda (item)
                                 (cdr (assoc 'title (car item))))
                               chime--upcoming-events)))
            (should (member "Meeting A" titles))
            (should (member "Meeting B" titles))
            ;; No duplicate titles
            (should (= 2 (length (delete-dups (copy-sequence titles))))))))
    (test-chime-modeline-teardown)))

;;; Tests for tooltip generation

(ert-deftest test-chime-modeline-tooltip-no-duplicates ()
  "Test that tooltip doesn't show the same event multiple times.
REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (meeting-time (test-time-tomorrow-at 14 0))
             (content (test-chime-modeline--create-gcal-event
                       "Team Meeting"
                       (test-timestamp-string meeting-time)))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Generate tooltip
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            (message "DEBUG: Tooltip content:\n%s" tooltip)

            ;; Tooltip should contain "Team Meeting" exactly once
            (let ((count (test-chime-modeline--count-in-string "Team Meeting" tooltip)))
              (should (= 1 count)))

            ;; "Upcoming Events" header should appear exactly once
            (let ((header-count (test-chime-modeline--count-in-string "Upcoming Events" tooltip)))
              (should (= 1 header-count))))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-modeline-tooltip-correct-order ()
  "Test that tooltip shows events in chronological order (soonest first).
REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (meeting-a-time (test-time-tomorrow-at 14 0))
             (meeting-b-time (test-time-tomorrow-at 15 0))
             (content (concat
                       ;; Later event
                       (test-chime-modeline--create-gcal-event
                        "Meeting B"
                        (test-timestamp-string meeting-b-time))
                       ;; Earlier event
                       (test-chime-modeline--create-gcal-event
                        "Meeting A"
                        (test-timestamp-string meeting-a-time))))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Generate tooltip
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            (message "DEBUG: Tooltip for order test:\n%s" tooltip)

            ;; "Meeting A" should appear before "Meeting B" in tooltip
            (let ((pos-a (string-match "Meeting A" tooltip))
                  (pos-b (string-match "Meeting B" tooltip)))
              (should pos-a)
              (should pos-b)
              (should (< pos-a pos-b))))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-modeline-tooltip-structure ()
  "Test that tooltip has proper structure without duplicates.

Tooltip should have:
- 'Upcoming Events as of...' header (once, at the beginning)
- Date sections (once per date)
- Event listings (once per event)
- No duplicate headers or sections

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (meeting-time (test-time-tomorrow-at 14 0))
             (content (test-chime-modeline--create-gcal-event
                       "Team Meeting"
                       (test-timestamp-string meeting-time)))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Generate tooltip
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            (message "DEBUG: Tooltip structure:\n%s" tooltip)

            ;; Should have exactly one "Upcoming Events" header
            (should (= 1 (test-chime-modeline--count-in-string "Upcoming Events" tooltip)))

            ;; Should start with "Upcoming Events as of" (new header format with timestamp)
            (should (string-match-p "^Upcoming Events as of" tooltip))

            ;; Event should appear exactly once
            (should (= 1 (test-chime-modeline--count-in-string "Team Meeting" tooltip))))))
    (test-chime-modeline-teardown)))

;;; Tests for tooltip max events limit

(ert-deftest test-chime-modeline-tooltip-max-events ()
  "Test that tooltip respects chime-modeline-tooltip-max-events limit.

When there are more events than the max, tooltip should show:
- Only the first N events
- '... and N more' message at the end

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (base-time (test-time-tomorrow-at 14 0))
             (content (concat
                       (test-chime-modeline--create-gcal-event
                        "Event 1"
                        (test-timestamp-string base-time))
                       (test-chime-modeline--create-gcal-event
                        "Event 2"
                        (test-timestamp-string (test-time-tomorrow-at 15 0)))
                       (test-chime-modeline--create-gcal-event
                        "Event 3"
                        (test-timestamp-string (test-time-tomorrow-at 16 0)))
                       (test-chime-modeline--create-gcal-event
                        "Event 4"
                        (test-timestamp-string (test-time-tomorrow-at 17 0)))
                       (test-chime-modeline--create-gcal-event
                        "Event 5"
                        (test-timestamp-string (test-time-tomorrow-at 18 0)))))
             (events (test-chime-modeline--gather-events content))
             (chime-modeline-tooltip-max-events 3))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Generate tooltip
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should show first 3 events
            (should (string-match-p "Event 1" tooltip))
            (should (string-match-p "Event 2" tooltip))
            (should (string-match-p "Event 3" tooltip))

            ;; Should NOT show events 4 and 5
            (should-not (string-match-p "Event 4" tooltip))
            (should-not (string-match-p "Event 5" tooltip))

            ;; Should have "... and 2 more events" message
            (should (string-match-p "\\.\\.\\. and 2 more events" tooltip)))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-modeline-tooltip-max-events-nil ()
  "Test that tooltip shows all events when max-events is nil.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (base-time (test-time-tomorrow-at 14 0))
             (content (concat
                       (test-chime-modeline--create-gcal-event
                        "Event 1"
                        (test-timestamp-string base-time))
                       (test-chime-modeline--create-gcal-event
                        "Event 2"
                        (test-timestamp-string (test-time-tomorrow-at 15 0)))
                       (test-chime-modeline--create-gcal-event
                        "Event 3"
                        (test-timestamp-string (test-time-tomorrow-at 16 0)))))
             (events (test-chime-modeline--gather-events content))
             (chime-modeline-tooltip-max-events nil))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Generate tooltip
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should show all 3 events
            (should (string-match-p "Event 1" tooltip))
            (should (string-match-p "Event 2" tooltip))
            (should (string-match-p "Event 3" tooltip))

            ;; Should NOT have "... and N more" message
            (should-not (string-match-p "\\.\\.\\. and" tooltip)))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-modeline-tooltip-max-events-14-across-week ()
  "Test max-events with 14 events (2/day across 7 days).

Comprehensive test of max-events interaction with multi-day grouping:
- 14 events total (2 per day for 7 days)
- max-events=20: should see all 14 events
- max-events=10: should see 10 events (2/day over 5 days)
- max-events=3: should see 2 for today, 1 for tomorrow

REFACTORED: Uses dynamic timestamps via testutil-time.el"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (content "")
             (events nil))

        (with-test-time now
          ;; Create 14 events: 2 per day for 7 days
          (dotimes (day 7)
            (dotimes (event-num 2)
              (let* ((hours-offset (+ (* day 24) (* event-num 2) 2)) ; 2, 4, 26, 28, etc.
                     (event-time (time-add now (seconds-to-time (* hours-offset 3600))))
                     (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-time))
                     (title (format "Day%d-Event%d" (1+ day) (1+ event-num))))
                (setq content (concat content
                                     (test-chime-modeline--create-gcal-event
                                      title
                                      time-str))))))

          (setq events (test-chime-modeline--gather-events content))

          ;; Should have gathered 14 events
          (should (= 14 (length events)))

          ;; Set lookahead to 10 days (enough to see all events, both modeline and tooltip)
          (setq chime-modeline-lookahead-minutes 240)
          (setq chime-tooltip-lookahead-hours 240)

          ;; Test 1: max-events=20 should show all 14
          (let ((chime-modeline-tooltip-max-events 20))
            (chime--update-modeline events)
            (should (= 14 (length chime--upcoming-events)))

            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              ;; Tooltip structure checks
              (should (string-match-p "^Upcoming Events as of" tooltip))
              (should (= 1 (test-chime-modeline--count-in-string "Upcoming Events" tooltip)))

              ;; All 14 events should appear in tooltip
              (dotimes (day 7)
                (dotimes (event-num 2)
                  (let ((title (format "Day%d-Event%d" (1+ day) (1+ event-num))))
                    (should (string-match-p title tooltip)))))

              ;; Verify chronological order: Day1-Event1 before Day7-Event2
              (let ((pos-first (string-match "Day1-Event1" tooltip))
                    (pos-last (string-match "Day7-Event2" tooltip)))
                (should pos-first)
                (should pos-last)
                (should (< pos-first pos-last)))

              ;; Should NOT have "... and N more"
              (should-not (string-match-p "\\.\\.\\. and" tooltip))))

          ;; Test 2: max-events=10 should show first 10 (5 days)
          (let ((chime-modeline-tooltip-max-events 10))
            (chime--update-modeline events)

            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              ;; Tooltip structure checks
              (should (string-match-p "^Upcoming Events as of" tooltip))
              (should (= 1 (test-chime-modeline--count-in-string "Upcoming Events" tooltip)))

              ;; First 10 events (days 1-5) should appear in tooltip
              (dotimes (day 5)
                (dotimes (event-num 2)
                  (let ((title (format "Day%d-Event%d" (1+ day) (1+ event-num))))
                    (should (string-match-p title tooltip)))))

              ;; Events from days 6-7 should NOT appear in tooltip
              (dotimes (day 2)
                (dotimes (event-num 2)
                  (let ((title (format "Day%d-Event%d" (+ day 6) (1+ event-num))))
                    (should-not (string-match-p title tooltip)))))

              ;; Verify chronological order in tooltip
              (let ((pos-first (string-match "Day1-Event1" tooltip))
                    (pos-last (string-match "Day5-Event2" tooltip)))
                (should pos-first)
                (should pos-last)
                (should (< pos-first pos-last)))

              ;; Tooltip should have "... and 4 more events"
              (should (string-match-p "\\.\\.\\. and 4 more events" tooltip))))

          ;; Test 3: max-events=3 should show 2 today + 1 tomorrow
          (let ((chime-modeline-tooltip-max-events 3))
            (chime--update-modeline events)

            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              ;; Tooltip structure checks
              (should (string-match-p "^Upcoming Events as of" tooltip))
              (should (= 1 (test-chime-modeline--count-in-string "Upcoming Events" tooltip)))

              ;; First 2 events (today) should appear in tooltip
              (should (string-match-p "Day1-Event1" tooltip))
              (should (string-match-p "Day1-Event2" tooltip))

              ;; First event from day 2 (tomorrow) should appear in tooltip
              (should (string-match-p "Day2-Event1" tooltip))

              ;; Second event from day 2 should NOT appear in tooltip
              (should-not (string-match-p "Day2-Event2" tooltip))

              ;; Events from days 3+ should NOT appear in tooltip
              (dotimes (day 5)
                (dotimes (event-num 2)
                  (let ((title (format "Day%d-Event%d" (+ day 3) (1+ event-num))))
                    (should-not (string-match-p title tooltip)))))

              ;; Verify chronological order in tooltip: Day1-Event1 before Day2-Event1
              (let ((pos-day1-e1 (string-match "Day1-Event1" tooltip))
                    (pos-day1-e2 (string-match "Day1-Event2" tooltip))
                    (pos-day2-e1 (string-match "Day2-Event1" tooltip)))
                (should pos-day1-e1)
                (should pos-day1-e2)
                (should pos-day2-e1)
                (should (< pos-day1-e1 pos-day1-e2))
                (should (< pos-day1-e2 pos-day2-e1)))

              ;; Should have "Today," and "Tomorrow," day labels in tooltip
              (should (string-match-p "Today," tooltip))
              (should (string-match-p "Tomorrow," tooltip))

              ;; Tooltip should have "... and 11 more events"
              (should (string-match-p "\\.\\.\\. and 11 more events" tooltip))))))
    (test-chime-modeline-teardown)))

;;; Tests for lookahead window boundaries

(ert-deftest test-chime-modeline-lookahead-exact-limit ()
  "Test that event exactly at lookahead limit appears in modeline.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Create event exactly 60 minutes from now
             (future-time (time-add now (seconds-to-time (* 60 60))))
             (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" future-time))
             (content (test-chime-modeline--create-gcal-event
                       "Event at limit"
                       time-str))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to 60 minutes (both modeline and tooltip)
          (setq chime-modeline-lookahead-minutes 60)
          (setq chime-tooltip-lookahead-hours 1)

          ;; Update modeline
          (chime--update-modeline events)

          ;; Event should appear in upcoming events
          (should (= 1 (length chime--upcoming-events)))
          (should (string-match-p "Event at limit"
                                 (cdr (assoc 'title (car (car chime--upcoming-events))))))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-modeline-lookahead-beyond-limit ()
  "Test that event beyond lookahead limit does NOT appear.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Create event 90 minutes from now (beyond 60 min limit)
             (future-time (time-add now (seconds-to-time (* 90 60))))
             (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" future-time))
             (content (test-chime-modeline--create-gcal-event
                       "Event beyond limit"
                       time-str))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to 60 minutes (both modeline and tooltip)
          (setq chime-modeline-lookahead-minutes 60)
          (setq chime-tooltip-lookahead-hours 1)

          ;; Update modeline
          (chime--update-modeline events)

          ;; Event should NOT appear in upcoming events
          (should (= 0 (length chime--upcoming-events)))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-modeline-past-event-excluded ()
  "Test that past events do NOT appear in modeline.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((past-time (test-time-yesterday-at 14 0))
             (content (test-chime-modeline--create-gcal-event
                       "Past Event"
                       (test-timestamp-string past-time)))
             (events (test-chime-modeline--gather-events content)))

        ;; Update modeline
        (chime--update-modeline events)

        ;; Past event should NOT appear
        (should (= 0 (length chime--upcoming-events))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-tooltip-lookahead-boundary-exactly-5-days ()
  "Test that event exactly 5 days away appears when tooltip lookahead is 5 days.

This tests the inclusive boundary condition at a multi-day scale.
Event at exactly 120 hours with lookahead=120 hours should appear (at boundary).

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event exactly 5 days (120 hours) from now
             (event-time (time-add now (seconds-to-time (* 120 3600))))
             (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-time))
             (content (test-chime-modeline--create-gcal-event
                       "Event at 5 day boundary"
                       time-str))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set tooltip lookahead to 120 hours (exactly 5 days)
          (setq chime-modeline-lookahead-minutes 10080)  ; 7 days (include in modeline)
          (setq chime-tooltip-lookahead-hours 120)       ; Exactly 5 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Event at 120 hours should appear (<= 120 hour boundary, inclusive)
          (should (= 1 (length chime--upcoming-events)))

          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            (should (string-match-p "Event at 5 day boundary" tooltip))
            (should (string-match-p "in 5 days" tooltip)))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-tooltip-lookahead-boundary-crosses-into-window ()
  "Test that event appears when time progression brings it into lookahead window.

Scenario:
- Event exactly 120 hours away
- Tooltip lookahead set to 119 hours (just under 5 days)
- Initially: event should NOT appear (120 > 119, beyond lookahead)
- Time progresses by 2 hours
- Now event is 118 hours away
- After time progression: event SHOULD appear (118 <= 119, within lookahead)

This tests dynamic boundary crossing as time progresses.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event exactly 120 hours from now
             (event-time (time-add now (seconds-to-time (* 120 3600))))
             (event-time-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-time))
             (content (test-chime-modeline--create-gcal-event
                       "Future Event"
                       event-time-str))
             (events (test-chime-modeline--gather-events content)))

        ;; PHASE 1: Initial state - event should NOT appear
        (with-test-time now
          (setq chime-modeline-lookahead-minutes 10080)  ; 7 days (include in modeline)
          (setq chime-tooltip-lookahead-hours 119)       ; Just under 5 days

          (chime--update-modeline events)

          ;; Event at 120 hours should NOT appear (120 > 119, beyond lookahead)
          (should (= 0 (length chime--upcoming-events))))

        ;; PHASE 2: Time progresses by 2 hours
        (let ((later (time-add now (seconds-to-time (* 2 3600)))))  ; +2 hours
          (with-test-time later
            ;; Same lookahead settings
            (setq chime-modeline-lookahead-minutes 10080)
            (setq chime-tooltip-lookahead-hours 119)

            (chime--update-modeline events)

            ;; Now event is 118 hours away, should appear (118 <= 119)
            (should (= 1 (length chime--upcoming-events)))

            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              (should (string-match-p "Future Event" tooltip))
              ;; Should show approximately 4 days 22 hours (118 hours)
              (should (string-match-p "in 4 days 22 hours" tooltip))))))
    (test-chime-modeline-teardown)))

;;; Tests for org-gcal with SCHEDULED edge case

(ert-deftest test-chime-extract-time-gcal-ignores-scheduled ()
  "Test that org-gcal events ignore SCHEDULED and use drawer time.

This is the CRITICAL test for the user's issue: when an event is
rescheduled in Google Calendar, org-gcal updates the :org-gcal: drawer
but might leave SCHEDULED property. We should ONLY use drawer time.

REFACTORED: Uses dynamic timestamps"
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((old-time (test-time-yesterday-at 14 0))
             (new-time (test-time-today-at 16 0))
             (old-timestamp (test-timestamp-string old-time))
             (new-timestamp (test-timestamp-string new-time))
             (content (format "* Team Meeting
SCHEDULED: %s
:PROPERTIES:
:entry-id: test123@google.com
:END:
:org-gcal:
%s
:END:
" old-timestamp new-timestamp))
             (test-file (chime-create-temp-test-file-with-content content))
             (test-buffer (find-file-noselect test-file))
             marker)

        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (setq marker (point-marker)))

        ;; Extract times
        (let ((times (chime--extract-time marker)))
          ;; Should have exactly one timestamp (from drawer, not SCHEDULED)
          (should (= 1 (length times)))

          ;; Should be the drawer time (today 16:00), not SCHEDULED (yesterday 14:00)
          (let ((time-str (caar times)))
            (should (string-match-p "16:00" time-str))
            (should-not (string-match-p "14:00" time-str))))

        (kill-buffer test-buffer))
    (chime-delete-test-base-dir)))

;;; Tests for multiple timestamps deduplication

(ert-deftest test-chime-modeline-multiple-timestamps-shows-soonest ()
  "Test that event with multiple timestamps appears once with soonest time.

Regular org events (not org-gcal) can have multiple plain timestamps.
Modeline should show the event ONCE with its SOONEST upcoming timestamp.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (wed-time (test-time-tomorrow-at 14 0))
             (thu-time (test-time-days-from-now 2))
             (fri-time (test-time-days-from-now 3))
             (content (format "* Weekly Meeting
%s
%s
%s
" (test-timestamp-string wed-time)
  (test-timestamp-string thu-time)
  (test-timestamp-string fri-time)))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Should have exactly ONE entry in upcoming events
          (should (= 1 (length chime--upcoming-events)))

          ;; The entry should be for the soonest time (tomorrow)
          (let* ((item (car chime--upcoming-events))
                 (time-str (car (nth 1 item))))
            (should (string-match-p "14:00" time-str)))

          ;; Tooltip should show the event exactly once
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            (should (= 1 (test-chime-modeline--count-in-string "Weekly Meeting" tooltip))))))
    (test-chime-modeline-teardown)))

;;; Tests for day grouping

(ert-deftest test-chime-modeline-day-grouping-today-tomorrow-future ()
  "Test tooltip groups events by day with correct labels.

Events should be grouped as:
- 'Today, MMM DD' for events in next 24 hours
- 'Tomorrow, MMM DD' for events 24-48 hours away
- 'Weekday, MMM DD' for events beyond 48 hours

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event in 2 hours (today)
             (today-time (time-add now (seconds-to-time (* 2 3600))))
             (today-str (format-time-string "<%Y-%m-%d %a %H:%M>" today-time))
             ;; Event in 26 hours (tomorrow)
             (tomorrow-time (time-add now (seconds-to-time (* 26 3600))))
             (tomorrow-str (format-time-string "<%Y-%m-%d %a %H:%M>" tomorrow-time))
             ;; Event in 50 hours (future)
             (future-time (time-add now (seconds-to-time (* 50 3600))))
             (future-str (format-time-string "<%Y-%m-%d %a %H:%M>" future-time))
             (content (concat
                       (test-chime-modeline--create-gcal-event
                        "Today Event"
                        today-str)
                       (test-chime-modeline--create-gcal-event
                        "Tomorrow Event"
                        tomorrow-str)
                       (test-chime-modeline--create-gcal-event
                        "Future Event"
                        future-str)))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to 72 hours (both modeline and tooltip)
          (setq chime-modeline-lookahead-minutes 4320)
          (setq chime-tooltip-lookahead-hours 72)

          ;; Update modeline
          (chime--update-modeline events)

          ;; Should have 3 events
          (should (= 3 (length chime--upcoming-events)))

          ;; Generate tooltip
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should have "Today," label
            (should (string-match-p "Today," tooltip))

            ;; Should have "Tomorrow," label
            (should (string-match-p "Tomorrow," tooltip))

            ;; Should have a weekday name for future event
            (should (string-match-p (format-time-string "%A," future-time) tooltip))

            ;; All three events should appear
            (should (string-match-p "Today Event" tooltip))
            (should (string-match-p "Tomorrow Event" tooltip))
            (should (string-match-p "Future Event" tooltip)))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-modeline-day-grouping-multiple-same-day ()
  "Test that multiple events on same day are grouped together.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (morning-time (test-time-tomorrow-at 9 0))
             (afternoon-time (test-time-tomorrow-at 14 0))
             (evening-time (test-time-tomorrow-at 18 0))
             (content (concat
                       (test-chime-modeline--create-gcal-event
                        "Morning Event"
                        (test-timestamp-string morning-time))
                       (test-chime-modeline--create-gcal-event
                        "Afternoon Event"
                        (test-timestamp-string afternoon-time))
                       (test-chime-modeline--create-gcal-event
                        "Evening Event"
                        (test-timestamp-string evening-time))))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set lookahead to cover test events (30+ days in future due to test-time offset)
          (setq chime-modeline-lookahead-minutes 64800)  ; 45 days
          (setq chime-tooltip-lookahead-hours 1080)  ; 45 days

          ;; Update modeline
          (chime--update-modeline events)

          ;; Generate tooltip
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should have "Tomorrow, " exactly once (all events same day)
            (should (= 1 (test-chime-modeline--count-in-string "Tomorrow," tooltip)))

            ;; All three events should appear
            (should (string-match-p "Morning Event" tooltip))
            (should (string-match-p "Afternoon Event" tooltip))
            (should (string-match-p "Evening Event" tooltip))

            ;; Events should appear in chronological order
            (let ((pos-morning (string-match "Morning Event" tooltip))
                  (pos-afternoon (string-match "Afternoon Event" tooltip))
                  (pos-evening (string-match "Evening Event" tooltip)))
              (should (< pos-morning pos-afternoon))
              (should (< pos-afternoon pos-evening))))))
    (test-chime-modeline-teardown)))

;;; Tests for tooltip lookahead independence

(ert-deftest test-chime-tooltip-lookahead-hours-independent ()
  "Test that tooltip can show events beyond modeline lookahead.

Scenario: modeline-lookahead=60 (1 hour), tooltip-lookahead=180 (3 hours)
- Event at 30 min: appears in BOTH modeline and tooltip
- Event at 90 min: appears ONLY in tooltip (not modeline)
- Event at 240 min: appears in NEITHER

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event in 30 minutes (within modeline lookahead)
             (event1-time (time-add now (seconds-to-time (* 30 60))))
             (event1-str (format-time-string "<%Y-%m-%d %a %H:%M>" event1-time))
             ;; Event in 90 minutes (beyond modeline, within tooltip)
             (event2-time (time-add now (seconds-to-time (* 90 60))))
             (event2-str (format-time-string "<%Y-%m-%d %a %H:%M>" event2-time))
             ;; Event in 240 minutes (beyond both)
             (event3-time (time-add now (seconds-to-time (* 240 60))))
             (event3-str (format-time-string "<%Y-%m-%d %a %H:%M>" event3-time))
             (content (concat
                       (test-chime-modeline--create-gcal-event
                        "Soon Event"
                        event1-str)
                       (test-chime-modeline--create-gcal-event
                        "Later Event"
                        event2-str)
                       (test-chime-modeline--create-gcal-event
                        "Far Event"
                        event3-str)))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set different lookaheads for modeline vs tooltip
          (setq chime-modeline-lookahead-minutes 60)          ; 1 hour for modeline
          (setq chime-tooltip-lookahead-hours 3) ; 3 hours for tooltip

          ;; Update modeline
          (chime--update-modeline events)

          ;; Modeline should show only the 30-min event
          (should (string-match-p "Soon Event" (or chime-modeline-string "")))
          (should-not (string-match-p "Later Event" (or chime-modeline-string "")))

          ;; Tooltip should show both 30-min and 90-min events, but not 240-min
          (should (= 2 (length chime--upcoming-events)))

          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Tooltip should have event within modeline lookahead
            (should (string-match-p "Soon Event" tooltip))

            ;; Tooltip should have event beyond modeline but within tooltip lookahead
            (should (string-match-p "Later Event" tooltip))

            ;; Tooltip should NOT have event beyond tooltip lookahead
            (should-not (string-match-p "Far Event" tooltip)))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-tooltip-lookahead-hours-default ()
  "Test that tooltip default lookahead (1 year) shows all future events.

The default value effectively means 'show all future events' limited only
by max-events count.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event in 30 minutes
             (event1-time (time-add now (seconds-to-time (* 30 60))))
             (event1-str (format-time-string "<%Y-%m-%d %a %H:%M>" event1-time))
             ;; Event in 90 minutes
             (event2-time (time-add now (seconds-to-time (* 90 60))))
             (event2-str (format-time-string "<%Y-%m-%d %a %H:%M>" event2-time))
             ;; Event in 2 days
             (event3-time (time-add now (seconds-to-time (* 48 3600))))
             (event3-str (format-time-string "<%Y-%m-%d %a %H:%M>" event3-time))
             (content (concat
                       (test-chime-modeline--create-gcal-event
                        "Soon Event"
                        event1-str)
                       (test-chime-modeline--create-gcal-event
                        "Later Event"
                        event2-str)
                       (test-chime-modeline--create-gcal-event
                        "Far Event"
                        event3-str)))
             (events (test-chime-modeline--gather-events content)))

        (with-test-time now
          ;; Set modeline lookahead only (tooltip uses default: 525600 = 1 year)
          (setq chime-modeline-lookahead-minutes 60)
          (setq chime-tooltip-lookahead-hours 8760)  ; Default

          ;; Update modeline
          (chime--update-modeline events)

          ;; Tooltip should see all 3 events (all within 1 year)
          (should (= 3 (length chime--upcoming-events)))

          ;; Modeline should only show first event (within 60 min)
          (should (string-match-p "Soon Event" (or chime-modeline-string "")))

          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; All events should appear in tooltip
            (should (string-match-p "Soon Event" tooltip))
            (should (string-match-p "Later Event" tooltip))
            (should (string-match-p "Far Event" tooltip)))))
    (test-chime-modeline-teardown)))

(ert-deftest test-chime-tooltip-lookahead-hours-larger-shows-more ()
  "Test that larger tooltip lookahead shows more events than modeline.

Real-world scenario: Show next event in modeline if within 2 hours,
but show all events for today (24 hours) in tooltip.

REFACTORED: Uses dynamic timestamps"
  (test-chime-modeline-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (content "")
             (events nil))

        (with-test-time now
          ;; Create 5 events spread across 12 hours
          (dotimes (i 5)
            (let* ((hours-offset (+ 1 (* i 2)))  ; 1, 3, 5, 7, 9 hours
                   (event-time (time-add now (seconds-to-time (* hours-offset 3600))))
                   (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-time))
                   (title (format "Event-%d-hours" hours-offset)))
              (setq content (concat content
                                   (test-chime-modeline--create-gcal-event
                                    title
                                    time-str)))))

          (setq events (test-chime-modeline--gather-events content))

          ;; Set lookaheads: 2 hours for modeline, 12 hours for tooltip
          (setq chime-modeline-lookahead-minutes 120)
          (setq chime-tooltip-lookahead-hours 12)

          ;; Update modeline
          (chime--update-modeline events)

          ;; Modeline should show only first event (within 2 hours)
          (should (string-match-p "Event-1-hours" (or chime-modeline-string "")))
          (should-not (string-match-p "Event-5-hours" (or chime-modeline-string "")))

          ;; Tooltip should show all 5 events (all within 12 hours)
          (should (= 5 (length chime--upcoming-events)))

          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; All events should appear in tooltip
            (should (string-match-p "Event-1-hours" tooltip))
            (should (string-match-p "Event-3-hours" tooltip))
            (should (string-match-p "Event-5-hours" tooltip))
            (should (string-match-p "Event-7-hours" tooltip))
            (should (string-match-p "Event-9-hours" tooltip)))))
    (test-chime-modeline-teardown)))

(provide 'test-chime-modeline)
;;; test-chime-modeline.el ends here
