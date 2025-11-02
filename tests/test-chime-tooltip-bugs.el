;;; test-chime-tooltip-bugs.el --- Tests for tooltip bugs -*- lexical-binding: t; -*-

;; Tests for reported issues:
;; 1. Duplicate "in in" in countdown text
;; 2. Duplicate events in tooltip
;; 3. Missing future events

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'dash)
(require 'alert)
(require 'async)
(require 'org-agenda)
(load (expand-file-name "../chime.el") nil t)
(require 'testutil-general (expand-file-name "testutil-general.el"))
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Setup and Teardown

(defun test-tooltip-bugs-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  (setq test-tooltip-bugs--orig-lookahead chime-modeline-lookahead-minutes)
  (setq test-tooltip-bugs--orig-tooltip-lookahead chime-tooltip-lookahead-hours)
  (setq chime-modeline-lookahead-minutes 1440)
  (setq chime-tooltip-lookahead-hours 8760)) ; 1 year

(defun test-tooltip-bugs-teardown ()
  "Teardown function run after each test."
  (setq chime-modeline-lookahead-minutes test-tooltip-bugs--orig-lookahead)
  (setq chime-tooltip-lookahead-hours test-tooltip-bugs--orig-tooltip-lookahead)
  (chime-delete-test-base-dir))

(defvar test-tooltip-bugs--orig-lookahead nil)
(defvar test-tooltip-bugs--orig-tooltip-lookahead nil)

;;; Helper functions

(defun test-tooltip-bugs--create-gcal-event (title time-str)
  "Create test org content for a gcal event."
  (concat
   (format "* %s\n" title)
   ":PROPERTIES:\n"
   ":entry-id: test@google.com\n"
   ":END:\n"
   ":org-gcal:\n"
   (format "%s\n" time-str)
   ":END:\n"))

(defun test-tooltip-bugs--gather-events (content)
  "Process CONTENT and return events list."
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

(defun test-tooltip-bugs--count-in-string (regexp string)
  "Count occurrences of REGEXP in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

;;; Test 1: No duplicate "in" in countdown text

(ert-deftest test-tooltip-no-duplicate-in ()
  "Test that tooltip doesn't have duplicate 'in in' in countdown.

Issue: Tooltip showed '(in in 1h 4m)' instead of '(in 1h 4m)'."
  (test-tooltip-bugs-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (event-time (time-add now (seconds-to-time (* 90 60)))) ; 90 min from now
             (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-time))
             (content (test-tooltip-bugs--create-gcal-event "Test Event" time-str))
             (events (test-tooltip-bugs--gather-events content)))

        (with-test-time now
          (chime--update-modeline events))
        (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
          ;; Should NOT have "in in"
          (should-not (string-match-p "in in" tooltip))

          ;; Should have single "in" (from format string)
          (should (string-match-p "(in " tooltip))))
    (test-tooltip-bugs-teardown)))

;;; Test 2: No duplicate events in tooltip

(ert-deftest test-tooltip-no-duplicate-events ()
  "Test that same event doesn't appear multiple times in tooltip.

Issue: Events appeared multiple times in tooltip."
  (test-tooltip-bugs-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (event1-time (time-add now (seconds-to-time (* 60 60)))) ; 1 hour
             (event1-str (format-time-string "<%Y-%m-%d %a %H:%M>" event1-time))
             (event2-time (time-add now (seconds-to-time (* 120 60)))) ; 2 hours
             (event2-str (format-time-string "<%Y-%m-%d %a %H:%M>" event2-time))
             (content (concat
                       (test-tooltip-bugs--create-gcal-event "Task 1" event1-str)
                       (test-tooltip-bugs--create-gcal-event "Task 2" event2-str)))
             (events (test-tooltip-bugs--gather-events content)))

        (with-test-time now
          (chime--update-modeline events))

        ;; Should have exactly 2 events
        (should (= 2 (length chime--upcoming-events)))

        (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
          ;; Each event should appear exactly once
          (should (= 1 (test-tooltip-bugs--count-in-string "Task 1" tooltip)))
          (should (= 1 (test-tooltip-bugs--count-in-string "Task 2" tooltip)))))
    (test-tooltip-bugs-teardown)))

;;; Test 3: All future events included (not just first few in file)

(ert-deftest test-tooltip-includes-all-future-events ()
  "Test that tooltip includes all future events, not just first N in file.

Issue: Events later in file were being ignored.
This tests that chime doesn't assume events are in chronological order in file."
  (test-tooltip-bugs-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Create events in NON-chronological order in file
             ;; Far event first, then near events
             (event-far-time (time-add now (seconds-to-time (* 48 3600)))) ; 2 days
             (event-far-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-far-time))
             (event1-time (time-add now (seconds-to-time (* 60 60)))) ; 1 hour
             (event1-str (format-time-string "<%Y-%m-%d %a %H:%M>" event1-time))
             (event2-time (time-add now (seconds-to-time (* 90 60)))) ; 1.5 hours
             (event2-str (format-time-string "<%Y-%m-%d %a %H:%M>" event2-time))
             (content (concat
                       ;; Far event appears FIRST in file
                       (test-tooltip-bugs--create-gcal-event "Task Far" event-far-str)
                       ;; Near events appear AFTER in file
                       (test-tooltip-bugs--create-gcal-event "Task 1" event1-str)
                       (test-tooltip-bugs--create-gcal-event "Task 2" event2-str)))
             (events (test-tooltip-bugs--gather-events content)))

        ;; Mock time to prevent timing-related flakiness
        (with-test-time now
          (chime--update-modeline events))

        ;; Should have all 3 events
        (should (= 3 (length chime--upcoming-events)))

        ;; Events should be in chronological order (not file order)
        (let* ((item1 (nth 0 chime--upcoming-events))
               (item2 (nth 1 chime--upcoming-events))
               (item3 (nth 2 chime--upcoming-events))
               (title1 (cdr (assoc 'title (car item1))))
               (title2 (cdr (assoc 'title (car item2))))
               (title3 (cdr (assoc 'title (car item3)))))
          ;; Should be sorted chronologically
          (should (string= title1 "Task 1"))
          (should (string= title2 "Task 2"))
          (should (string= title3 "Task Far")))

        (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
          ;; All events should appear in tooltip
          (should (string-match-p "Task 1" tooltip))
          (should (string-match-p "Task 2" tooltip))
          (should (string-match-p "Task Far" tooltip))

          ;; Chronological order: Task 1 before Task 2 before Task Far
          (let ((pos1 (string-match "Task 1" tooltip))
                (pos2 (string-match "Task 2" tooltip))
                (pos-far (string-match "Task Far" tooltip)))
            (should (< pos1 pos2))
            (should (< pos2 pos-far)))))
    (test-tooltip-bugs-teardown)))

;;; Test 4: Exact replication of reported duplicate event issue

(ert-deftest test-tooltip-exact-duplicate-bug ()
  "Replicate exact bug: Task 2 appearing twice, Task 3 appearing twice.

From user report:
Task 1 at 9:00 PM (in 1h 4m)
Task 2 at 10:00 PM (in 2h 4m)  <-- appears
Task 3 at 10:00 PM (in 2h 4m)  <-- appears
Task 2 at 10:00 PM (in 2h 4m)  <-- DUPLICATE
Task 4 at 01:00 PM (in 17h 4m)"
  (test-tooltip-bugs-setup)
  (unwind-protect
      (let* ((now (current-time))
             ;; Create exactly the scenario from the report
             (task1-time (time-add now (seconds-to-time (* 64 60)))) ; ~1h 4m
             (task1-str (format-time-string "<%Y-%m-%d %a %H:%M>" task1-time))
             (task2-time (time-add now (seconds-to-time (* 124 60)))) ; ~2h 4m
             (task2-str (format-time-string "<%Y-%m-%d %a %H:%M>" task2-time))
             (task3-time (time-add now (seconds-to-time (* 124 60)))) ; same time as task2
             (task3-str (format-time-string "<%Y-%m-%d %a %H:%M>" task3-time))
             (task4-time (time-add now (seconds-to-time (* 1024 60)))) ; ~17h 4m
             (task4-str (format-time-string "<%Y-%m-%d %a %H:%M>" task4-time))
             (content (concat
                       (test-tooltip-bugs--create-gcal-event "Task 1" task1-str)
                       (test-tooltip-bugs--create-gcal-event "Task 2" task2-str)
                       (test-tooltip-bugs--create-gcal-event "Task 3" task3-str)
                       (test-tooltip-bugs--create-gcal-event "Task 4" task4-str)))
             (events (test-tooltip-bugs--gather-events content)))

        (chime--update-modeline events)

        ;; Should have exactly 4 events
        (should (= 4 (length chime--upcoming-events)))

        (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
          ;; Each event should appear exactly once (no duplicates)
          (should (= 1 (test-tooltip-bugs--count-in-string "Task 1" tooltip)))
          (should (= 1 (test-tooltip-bugs--count-in-string "Task 2" tooltip)))
          (should (= 1 (test-tooltip-bugs--count-in-string "Task 3" tooltip)))
          (should (= 1 (test-tooltip-bugs--count-in-string "Task 4" tooltip)))

          ;; Verify chronological order
          (let ((pos1 (string-match "Task 1" tooltip))
                (pos2 (string-match "Task 2" tooltip))
                (pos3 (string-match "Task 3" tooltip))
                (pos4 (string-match "Task 4" tooltip)))
            (should (< pos1 pos2))
            (should (< pos2 pos4)))))
    (test-tooltip-bugs-teardown)))

;;; Test 5: Missing events that appear later in file

(ert-deftest test-tooltip-missing-later-events ()
  "Replicate exact bug: Tasks 5, 6, 7 missing even though they're in future.

From user report: 'Earlier in the same org-gcal file, I have Tasks 5, 6, 7
and many more in the future.'

This tests the concern: 'I worry chime is somehow assuming events and dates
are always listed in chronological order.'"
  (test-tooltip-bugs-setup)
  (unwind-protect
      (let* ((now (current-time))
             ;; Create scenario where early events appear AFTER later events in file
             ;; This mimics how org-gcal might organize events
             (task5-time (time-add now (seconds-to-time (* 30 3600)))) ; 30 hours (tomorrow)
             (task5-str (format-time-string "<%Y-%m-%d %a %H:%M>" task5-time))
             (task6-time (time-add now (seconds-to-time (* 48 3600)))) ; 48 hours (2 days)
             (task6-str (format-time-string "<%Y-%m-%d %a %H:%M>" task6-time))
             (task7-time (time-add now (seconds-to-time (* 72 3600)))) ; 72 hours (3 days)
             (task7-str (format-time-string "<%Y-%m-%d %a %H:%M>" task7-time))
             (task1-time (time-add now (seconds-to-time (* 2 3600)))) ; 2 hours (soon!)
             (task1-str (format-time-string "<%Y-%m-%d %a %H:%M>" task1-time))
             ;; Put far events FIRST in file, near event LAST
             (content (concat
                       (test-tooltip-bugs--create-gcal-event "Task 5" task5-str)
                       (test-tooltip-bugs--create-gcal-event "Task 6" task6-str)
                       (test-tooltip-bugs--create-gcal-event "Task 7" task7-str)
                       (test-tooltip-bugs--create-gcal-event "Task 1" task1-str)))
             (events (test-tooltip-bugs--gather-events content)))

        (chime--update-modeline events)

        ;; Should have all 4 events
        (should (= 4 (length chime--upcoming-events)))

        ;; Events should be sorted chronologically (not file order)
        (let* ((item1 (nth 0 chime--upcoming-events))
               (item2 (nth 1 chime--upcoming-events))
               (item3 (nth 2 chime--upcoming-events))
               (item4 (nth 3 chime--upcoming-events))
               (title1 (cdr (assoc 'title (car item1))))
               (title2 (cdr (assoc 'title (car item2))))
               (title3 (cdr (assoc 'title (car item3))))
               (title4 (cdr (assoc 'title (car item4)))))
          (should (string= title1 "Task 1")) ; soonest
          (should (string= title2 "Task 5"))
          (should (string= title3 "Task 6"))
          (should (string= title4 "Task 7"))) ; furthest

        (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
          ;; ALL events should be present
          (should (string-match-p "Task 1" tooltip))
          (should (string-match-p "Task 5" tooltip))
          (should (string-match-p "Task 6" tooltip))
          (should (string-match-p "Task 7" tooltip))

          ;; Verify chronological order in tooltip
          (let ((pos1 (string-match "Task 1" tooltip))
                (pos5 (string-match "Task 5" tooltip))
                (pos6 (string-match "Task 6" tooltip))
                (pos7 (string-match "Task 7" tooltip)))
            (should (< pos1 pos5))
            (should (< pos5 pos6))
            (should (< pos6 pos7)))))
    (test-tooltip-bugs-teardown)))

;;; Test 6: Only 5 events shown when many more exist

(ert-deftest test-tooltip-only-first-5-shown ()
  "Replicate bug: Only 5 events shown in tooltip despite having many more.

From user report: Tooltip showed 5 events, then '...and that's all' even
though there were many more future events in the file.

This might be due to default max-events=5."
  (test-tooltip-bugs-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             (content "")
             (events nil))

        ;; Create 10 events
        (dotimes (i 10)
          (let* ((hours-offset (+ 1 i)) ; 1, 2, 3... 10 hours
                 (event-time (time-add now (seconds-to-time (* hours-offset 3600))))
                 (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" event-time))
                 (title (format "Task %d" (1+ i))))
            (setq content (concat content
                                 (test-tooltip-bugs--create-gcal-event title time-str)))))

        (setq events (test-tooltip-bugs--gather-events content))

        ;; Set to default: max-events=5
        (let ((chime-modeline-tooltip-max-events 5))
          (with-test-time now
            (chime--update-modeline events))

          ;; All 10 events should be in upcoming-events
          (should (= 10 (length chime--upcoming-events)))

          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Tooltip should show first 5 events
            (should (string-match-p "Task 1" tooltip))
            (should (string-match-p "Task 2" tooltip))
            (should (string-match-p "Task 3" tooltip))
            (should (string-match-p "Task 4" tooltip))
            (should (string-match-p "Task 5" tooltip))

            ;; Should NOT show tasks 6-10
            (should-not (string-match-p "Task 6" tooltip))
            (should-not (string-match-p "Task 10" tooltip))

            ;; Should show "... and 5 more events"
            (should (string-match-p "\\.\\.\\..*and 5 more events" tooltip)))))
    (test-tooltip-bugs-teardown)))

;;; Test 7: Events at exactly same time

(ert-deftest test-tooltip-events-same-time ()
  "Test events scheduled at exactly the same time.

From user report: Task 2 and Task 3 both at 10:00 PM.
Should both appear, should not duplicate."
  (test-tooltip-bugs-setup)
  (unwind-protect
      (let* ((now (current-time))
             (same-time (time-add now (seconds-to-time (* 120 60)))) ; 2 hours
             (time-str (format-time-string "<%Y-%m-%d %a %H:%M>" same-time))
             (content (concat
                       (test-tooltip-bugs--create-gcal-event "Meeting A" time-str)
                       (test-tooltip-bugs--create-gcal-event "Meeting B" time-str)
                       (test-tooltip-bugs--create-gcal-event "Meeting C" time-str)))
             (events (test-tooltip-bugs--gather-events content)))

        (chime--update-modeline events)

        ;; Should have all 3 events
        (should (= 3 (length chime--upcoming-events)))

        (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
          ;; All 3 events should appear exactly once
          (should (= 1 (test-tooltip-bugs--count-in-string "Meeting A" tooltip)))
          (should (= 1 (test-tooltip-bugs--count-in-string "Meeting B" tooltip)))
          (should (= 1 (test-tooltip-bugs--count-in-string "Meeting C" tooltip)))))
    (test-tooltip-bugs-teardown)))

(provide 'test-chime-tooltip-bugs)
;;; test-chime-tooltip-bugs.el ends here
