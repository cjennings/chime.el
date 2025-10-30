;;; test-integration-recurring-events-tooltip.el --- Integration tests for recurring events in tooltip -*- lexical-binding: t; -*-

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

;; Integration tests for bug001: Recurring Events Show Duplicate Entries in Tooltip
;;
;; Tests the complete workflow from org entries with recurring timestamps
;; through org-agenda-list expansion to final tooltip display.
;;
;; Components integrated:
;; - org-agenda-list (org-mode function that expands recurring events)
;; - chime--gather-info (extracts event information from org markers)
;; - chime--deduplicate-events-by-title (deduplicates expanded recurring events)
;; - chime--update-modeline (updates modeline and upcoming events list)
;; - chime--make-tooltip (generates tooltip from upcoming events)
;;
;; Validates:
;; - Recurring events expanded by org-agenda-list are deduplicated correctly
;; - Tooltip shows each recurring event only once (the soonest occurrence)
;; - Multiple different events are all preserved
;; - Mixed recurring and non-recurring events work correctly

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
(setq chime-debug t)
(load (expand-file-name "../chime.el") nil t)

;; Load test utilities
(require 'testutil-general (expand-file-name "testutil-general.el"))
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Setup and Teardown

(defvar test-integration-recurring--orig-agenda-files nil
  "Original org-agenda-files value before test.")

(defvar test-integration-recurring--orig-modeline-lookahead nil
  "Original chime-modeline-lookahead-minutes value.")

(defvar test-integration-recurring--orig-tooltip-lookahead nil
  "Original chime-tooltip-lookahead-hours value.")

(defun test-integration-recurring-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Save original values
  (setq test-integration-recurring--orig-agenda-files org-agenda-files)
  (setq test-integration-recurring--orig-modeline-lookahead chime-modeline-lookahead-minutes)
  (setq test-integration-recurring--orig-tooltip-lookahead chime-tooltip-lookahead-hours)
  ;; Set lookahead to 1 year for testing recurring events
  (setq chime-modeline-lookahead-minutes 525600)  ; 365 days
  (setq chime-tooltip-lookahead-hours 8760))       ; 365 days

(defun test-integration-recurring-teardown ()
  "Teardown function run after each test."
  ;; Restore original values
  (setq org-agenda-files test-integration-recurring--orig-agenda-files)
  (setq chime-modeline-lookahead-minutes test-integration-recurring--orig-modeline-lookahead)
  (setq chime-tooltip-lookahead-hours test-integration-recurring--orig-tooltip-lookahead)
  (chime-delete-test-base-dir))

;;; Helper Functions

(defun test-integration-recurring--create-org-file (content)
  "Create org file with CONTENT and set it as org-agenda-files.
Returns the file path."
  (let* ((base-file (chime-create-temp-test-file "recurring-test-"))
         (org-file (concat base-file ".org")))
    ;; Rename to have .org extension
    (rename-file base-file org-file)
    ;; Write content to the .org file
    (with-temp-buffer
      (insert content)
      (write-file org-file))
    ;; Set as agenda file
    (setq org-agenda-files (list org-file))
    org-file))

(defun test-integration-recurring--run-agenda-and-gather-events (agenda-span)
  "Run org-agenda-list with AGENDA-SPAN and gather event markers.
Returns list of event info gathered from markers."
  (let ((markers nil)
        (events nil)
        (org-buffers nil))
    ;; Remember which buffers are open before agenda
    (setq org-buffers (buffer-list))
    ;; org-agenda-list doesn't return the buffer, it creates "*Org Agenda*"
    (org-agenda-list agenda-span (org-read-date nil nil "today"))
    (with-current-buffer "*Org Agenda*"
      ;; Extract all org-markers from agenda buffer
      (setq markers
            (->> (org-split-string (buffer-string) "\n")
                 (--map (plist-get
                         (org-fix-agenda-info (text-properties-at 0 it))
                         'org-marker))
                 (-non-nil))))
    ;; Gather info for each marker BEFORE killing buffers
    ;; (markers point to the org file buffers, which must stay alive)
    (setq events (-map 'chime--gather-info markers))
    ;; Now kill agenda buffer to clean up
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    events))

(defun test-integration-recurring--count-in-string (regexp string)
  "Count occurrences of REGEXP in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

;;; Normal Cases - Recurring Event Deduplication

(ert-deftest test-integration-recurring-events-tooltip-daily-repeater-shows-once ()
  "Test that daily recurring event appears only once in tooltip.

When org-agenda-list expands a daily recurring event (e.g., +1d) over a
year-long lookahead window, it creates ~365 separate agenda entries.
The tooltip should show only the soonest occurrence, not all 365.

Components integrated:
- org-agenda-list (expands recurring events into separate instances)
- chime--gather-info (extracts info from each expanded marker)
- chime--deduplicate-events-by-title (removes duplicate titles)
- chime--update-modeline (updates upcoming events list)
- chime--make-tooltip (generates tooltip display)

Validates:
- Recurring event expanded 365 times is deduplicated to one entry
- Tooltip shows event title exactly once
- The shown occurrence is the soonest one"
  (test-integration-recurring-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (event-time (test-time-today-at 22 0))  ; 10 PM today
             (timestamp (test-timestamp-repeating event-time "+1d"))
             (content (format "* Daily Wrap Up\n%s\n" timestamp)))

        ;; Create org file with recurring event
        (test-integration-recurring--create-org-file content)

        (with-test-time now
          ;; Run org-agenda-list to expand recurring events (365 days)
          (let* ((events (test-integration-recurring--run-agenda-and-gather-events 365)))

            ;; Verify org-agenda-list expanded the recurring event multiple times
            ;; (should be ~365 instances, one per day)
            (should (> (length events) 300))

            ;; All events should have the same title
            (let ((titles (mapcar (lambda (e) (cdr (assoc 'title e))) events)))
              (should (cl-every (lambda (title) (string= "Daily Wrap Up" title)) titles)))

            ;; Update modeline with these events (this triggers deduplication)
            (chime--update-modeline events)

            ;; The upcoming events list should have only ONE entry
            (should (= 1 (length chime--upcoming-events)))

            ;; Verify it's the right event
            (let* ((item (car chime--upcoming-events))
                   (event (car item))
                   (title (cdr (assoc 'title event))))
              (should (string= "Daily Wrap Up" title)))

            ;; Generate tooltip and verify event appears only once
            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              (should (stringp tooltip))
              ;; Count occurrences of "Daily Wrap Up" in tooltip
              (let ((count (test-integration-recurring--count-in-string
                           "Daily Wrap Up" tooltip)))
                (should (= 1 count)))))))
    (test-integration-recurring-teardown)))

(ert-deftest test-integration-recurring-events-tooltip-weekly-repeater-shows-once ()
  "Test that weekly recurring event appears only once in tooltip.

Weekly repeater (+1w) over 1 year creates ~52 instances.
Should be deduplicated to show only the soonest.

Components integrated:
- org-agenda-list (expands +1w into 52 instances)
- chime--gather-info (processes each instance)
- chime--deduplicate-events-by-title (deduplicates by title)
- chime--update-modeline (updates events list)
- chime--make-tooltip (generates display)

Validates:
- Weekly recurring events are deduplicated correctly
- Tooltip shows only soonest occurrence"
  (test-integration-recurring-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (event-time (test-time-tomorrow-at 14 0))  ; 2 PM tomorrow
             (timestamp (test-timestamp-repeating event-time "+1w"))
             (content (format "* Weekly Team Sync\n%s\n" timestamp)))

        (test-integration-recurring--create-org-file content)

        (with-test-time now
          (let* ((events (test-integration-recurring--run-agenda-and-gather-events 365)))

            ;; Should have ~52 weekly instances
            (should (> (length events) 45))
            (should (< (length events) 60))

            ;; Update modeline
            (chime--update-modeline events)

            ;; Should be deduplicated to one
            (should (= 1 (length chime--upcoming-events)))

            ;; Verify tooltip shows it once
            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              (let ((count (test-integration-recurring--count-in-string
                           "Weekly Team Sync" tooltip)))
                (should (= 1 count)))))))
    (test-integration-recurring-teardown)))

(ert-deftest test-integration-recurring-events-tooltip-mixed-recurring-and-unique ()
  "Test mix of recurring and non-recurring events in tooltip.

When multiple events exist, some recurring and some not, all should
appear but recurring ones should be deduplicated.

Components integrated:
- org-agenda-list (expands only the recurring event)
- chime--gather-info (processes all events)
- chime--deduplicate-events-by-title (deduplicates recurring only)
- chime--update-modeline (updates events list)
- chime--make-tooltip (generates display)

Validates:
- Recurring event appears once
- Non-recurring events all appear
- Total count is correct (recurring deduplicated, others preserved)"
  (test-integration-recurring-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (daily-time (test-time-today-at 22 0))
             (meeting-time (test-time-tomorrow-at 14 0))
             (review-time (test-time-days-from-now 2 15 0))
             (daily-ts (test-timestamp-repeating daily-time "+1d"))
             (meeting-ts (test-timestamp-string meeting-time))
             (review-ts (test-timestamp-string review-time))
             (content (concat
                       (format "* Daily Standup\n%s\n\n" daily-ts)
                       (format "* Project Review\n%s\n\n" meeting-ts)
                       (format "* Client Meeting\n%s\n" review-ts))))

        (test-integration-recurring--create-org-file content)

        (with-test-time now
          (let* ((events (test-integration-recurring--run-agenda-and-gather-events 365)))

            ;; Should have many events due to daily repeater
            (should (> (length events) 300))

            ;; Update modeline (triggers deduplication)
            (chime--update-modeline events)

            ;; Should have exactly 3 events (daily deduplicated + 2 unique)
            (should (= 3 (length chime--upcoming-events)))

            ;; Verify all three titles appear
            (let ((titles (mapcar (lambda (item)
                                   (cdr (assoc 'title (car item))))
                                 chime--upcoming-events)))
              (should (member "Daily Standup" titles))
              (should (member "Project Review" titles))
              (should (member "Client Meeting" titles))
              ;; No duplicates
              (should (= 3 (length (delete-dups (copy-sequence titles))))))

            ;; Verify tooltip shows each event once
            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              (should (= 1 (test-integration-recurring--count-in-string
                           "Daily Standup" tooltip)))
              (should (= 1 (test-integration-recurring--count-in-string
                           "Project Review" tooltip)))
              (should (= 1 (test-integration-recurring--count-in-string
                           "Client Meeting" tooltip)))))))
    (test-integration-recurring-teardown)))

;;; Boundary Cases

(ert-deftest test-integration-recurring-events-tooltip-multiple-different-recurring ()
  "Test multiple different recurring events are all shown once.

When there are multiple recurring events with different titles,
each should appear once in the tooltip.

Components integrated:
- org-agenda-list (expands all recurring events)
- chime--gather-info (processes all expanded instances)
- chime--deduplicate-events-by-title (deduplicates each title separately)
- chime--update-modeline (updates events list)
- chime--make-tooltip (generates display)

Validates:
- Each recurring event title appears exactly once
- Different recurring frequencies handled correctly
- Deduplication works independently for each title"
  (test-integration-recurring-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (daily-time (test-time-today-at 11 0))   ; 11 AM (after 10 AM now)
             (weekly-time (test-time-tomorrow-at 14 0))  ; 2 PM tomorrow
             (daily-ts (test-timestamp-repeating daily-time "+1d"))
             (weekly-ts (test-timestamp-repeating weekly-time "+1w"))
             (content (concat
                       (format "* Daily Standup\n%s\n\n" daily-ts)
                       (format "* Weekly Review\n%s\n" weekly-ts))))

        (test-integration-recurring--create-org-file content)

        (with-test-time now
          (let* ((events (test-integration-recurring--run-agenda-and-gather-events 365)))

            ;; Should have many events (~365 daily + ~52 weekly)
            (should (> (length events) 400))

            ;; Update modeline
            (chime--update-modeline events)

            ;; Should be deduplicated to 2 events
            (should (= 2 (length chime--upcoming-events)))

            ;; Verify tooltip shows each once
            (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
              (should (= 1 (test-integration-recurring--count-in-string
                           "Daily Standup" tooltip)))
              (should (= 1 (test-integration-recurring--count-in-string
                           "Weekly Review" tooltip)))))))
    (test-integration-recurring-teardown)))

(provide 'test-integration-recurring-events-tooltip)
;;; test-integration-recurring-events-tooltip.el ends here
