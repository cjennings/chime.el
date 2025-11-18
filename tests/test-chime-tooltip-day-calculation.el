;;; test-chime-tooltip-day-calculation.el --- Tests for tooltip day/hour calculation -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive tests for tooltip time-until formatting, especially day/hour calculations.
;;
;; Tests cover:
;; - Boundary cases (23h59m, 24h, 25h)
;; - Midnight boundaries
;; - Multiple days with fractional hours
;; - Exact day boundaries (48h, 72h)
;; - Edge cases that could trigger truncation bugs

;;; Code:

(require 'ert)
(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)
(load (expand-file-name "../chime.el") nil t)
(require 'testutil-time (expand-file-name "testutil-time.el"))
(require 'testutil-general (expand-file-name "testutil-general.el"))

(ert-deftest test-chime-tooltip-day-calculation-fractional-days ()
  "Test that fractional days show both days and hours correctly.

User scenario: Viewing tooltip on Sunday 9pm, sees:
- Tuesday 9pm event: 48 hours = exactly 2 days → 'in 2 days'
- Wednesday 2pm event: 65 hours = 2.7 days → 'in 2 days 17 hours'

This test prevents regression of the integer division truncation bug."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 21 0))  ; Sunday 9pm
             ;; Create events at specific future times
             (tuesday-9pm (time-add now (seconds-to-time (* 48 3600))))   ; +48 hours
             (wednesday-2pm (time-add now (seconds-to-time (* 65 3600)))) ; +65 hours
             (content (format "* Tuesday Event\n<%s>\n* Wednesday Event\n<%s>\n"
                             (format-time-string "<%Y-%m-%d %a %H:%M>" tuesday-9pm)
                             (format-time-string "<%Y-%m-%d %a %H:%M>" wednesday-2pm)))
             (test-file (chime-create-temp-test-file-with-content content))
             (test-buffer (find-file-noselect test-file))
             (events nil))

        ;; Gather events
        (with-current-buffer test-buffer
          (org-mode)
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ " nil t)
            (beginning-of-line)
            (push (chime--gather-info (point-marker)) events)
            (forward-line 1)))
        (kill-buffer test-buffer)
        (setq events (nreverse events))

        ;; Set lookahead to cover events (7 days)
        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          ;; Update modeline and get tooltip
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))

            ;; Verify tooltip contains both events
            (should (string-match-p "Tuesday Event" tooltip))
            (should (string-match-p "Wednesday Event" tooltip))

            ;; Print tooltip for manual inspection
            (message "TOOLTIP CONTENT:\n%s" tooltip)

            ;; AFTER FIX: Tuesday shows "in 2 days", Wednesday shows "in 2 days 17 hours"
            ;; Verify Tuesday shows exactly 2 days (no "hours" in countdown)
            (should (string-match-p "Tuesday Event.*(in 2 days)" tooltip))
            ;; Make sure Tuesday doesn't have hours
            (should-not (string-match-p "Tuesday Event.*hours" tooltip))

            ;; Verify Wednesday shows 2 days AND 17 hours
            (should (string-match-p "Wednesday Event.*(in 2 days 17 hours)" tooltip))

            ;; Verify they show DIFFERENT countdowns
            (let ((tuesday-line (progn
                                 (string-match "Tuesday Event[^\n]*" tooltip)
                                 (match-string 0 tooltip)))
                  (wednesday-line (progn
                                   (string-match "Wednesday Event[^\n]*" tooltip)
                                   (match-string 0 tooltip))))
              (should-not (string= tuesday-line wednesday-line))))))

    (chime-delete-test-base-dir)))

;;; Helper function for creating test events

(defun test-chime-tooltip-day-calculation--create-event-at-hours (base-time title hours-from-now)
  "Create event with TITLE at HOURS-FROM-NOW hours from BASE-TIME.
Returns formatted org content string."
  (let* ((event-time (time-add base-time (seconds-to-time (* hours-from-now 3600)))))
    (format "* %s\n<%s>\n"
            title
            (format-time-string "%Y-%m-%d %a %H:%M" event-time))))

(defun test-chime-tooltip-day-calculation--get-formatted-line (tooltip event-name)
  "Extract the formatted countdown line for EVENT-NAME from TOOLTIP."
  (when (string-match (format "%s[^\n]*" event-name) tooltip)
    (match-string 0 tooltip)))

;;; Boundary Cases - Critical thresholds

(ert-deftest test-chime-tooltip-day-calculation-boundary-exactly-24-hours ()
  "Test event exactly 24 hours away shows 'in 1 day' not hours."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 12 0))
             (content (test-chime-tooltip-day-calculation--create-event-at-hours now "Tomorrow Same Time" 24))
             (test-file (chime-create-temp-test-file-with-content content))
             (events (with-current-buffer (find-file-noselect test-file)
                      (org-mode)
                      (goto-char (point-min))
                      (let ((evs nil))
                        (while (re-search-forward "^\\*+ " nil t)
                          (push (chime--gather-info (point-marker)) evs))
                        (nreverse evs)))))
        (kill-buffer (get-file-buffer test-file))

        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should show "in 1 day" not hours
            (should (string-match-p "(in 1 day)" tooltip))
            (should-not (string-match-p "hours" tooltip)))))
    (chime-delete-test-base-dir)))

(ert-deftest test-chime-tooltip-day-calculation-boundary-23-hours-59-minutes ()
  "Test event 23h59m away shows hours, not days (just under 24h threshold)."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 12 0))
             ;; 23 hours 59 minutes = 1439 minutes = just under 1440
             (event-time (time-add now (seconds-to-time (* 1439 60))))
             (content (format "* Almost Tomorrow\n<%s>\n"
                            (format-time-string "%Y-%m-%d %a %H:%M" event-time)))
             (test-file (chime-create-temp-test-file-with-content content))
             (events (with-current-buffer (find-file-noselect test-file)
                      (org-mode)
                      (goto-char (point-min))
                      (let ((evs nil))
                        (while (re-search-forward "^\\*+ " nil t)
                          (push (chime--gather-info (point-marker)) evs))
                        (nreverse evs)))))
        (kill-buffer (get-file-buffer test-file))

        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should show hours format (< 24 hours)
            (should (string-match-p "hours" tooltip))
            (should-not (string-match-p "days?" tooltip)))))
    (chime-delete-test-base-dir)))

(ert-deftest test-chime-tooltip-day-calculation-boundary-25-hours ()
  "Test event 25 hours away shows 'in 1 day 1 hour'."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 12 0))
             (content (test-chime-tooltip-day-calculation--create-event-at-hours now "Day Plus One" 25))
             (test-file (chime-create-temp-test-file-with-content content))
             (events (with-current-buffer (find-file-noselect test-file)
                      (org-mode)
                      (goto-char (point-min))
                      (let ((evs nil))
                        (while (re-search-forward "^\\*+ " nil t)
                          (push (chime--gather-info (point-marker)) evs))
                        (nreverse evs)))))
        (kill-buffer (get-file-buffer test-file))

        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should show "in 1 day 1 hour"
            (should (string-match-p "(in 1 day 1 hour)" tooltip)))))
    (chime-delete-test-base-dir)))

(ert-deftest test-chime-tooltip-day-calculation-boundary-exactly-48-hours ()
  "Test event exactly 48 hours away shows 'in 2 days' without hours."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 12 0))
             (content (test-chime-tooltip-day-calculation--create-event-at-hours now "Two Days Exact" 48))
             (test-file (chime-create-temp-test-file-with-content content))
             (events (with-current-buffer (find-file-noselect test-file)
                      (org-mode)
                      (goto-char (point-min))
                      (let ((evs nil))
                        (while (re-search-forward "^\\*+ " nil t)
                          (push (chime--gather-info (point-marker)) evs))
                        (nreverse evs)))))
        (kill-buffer (get-file-buffer test-file))

        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events))
                (line (test-chime-tooltip-day-calculation--get-formatted-line
                       (chime--make-tooltip chime--upcoming-events) "Two Days Exact")))
            ;; Should show exactly "in 2 days" with NO hours
            (should (string-match-p "(in 2 days)" tooltip))
            ;; Verify the line doesn't contain "hour" (would be "2 days 0 hours")
            (should-not (string-match-p "hour" line)))))
    (chime-delete-test-base-dir)))

;;; Midnight Boundaries

(ert-deftest test-chime-tooltip-day-calculation-midnight-crossing-shows-correct-days ()
  "Test event crossing midnight boundary calculates days correctly.

Scenario: 11pm now, event at 2am (3 hours later, next calendar day)
Should show hours, not '1 day' since it's only 3 hours away."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 23 0))  ; 11pm
             ;; 3 hours later = 2am next day
             (content (test-chime-tooltip-day-calculation--create-event-at-hours now "Early Morning" 3))
             (test-file (chime-create-temp-test-file-with-content content))
             (events (with-current-buffer (find-file-noselect test-file)
                      (org-mode)
                      (goto-char (point-min))
                      (let ((evs nil))
                        (while (re-search-forward "^\\*+ " nil t)
                          (push (chime--gather-info (point-marker)) evs))
                        (nreverse evs)))))
        (kill-buffer (get-file-buffer test-file))

        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Should show "in 3 hours" not "in 1 day"
            (should (string-match-p "3 hours" tooltip))
            (should-not (string-match-p "days?" tooltip)))))
    (chime-delete-test-base-dir)))

(ert-deftest test-chime-tooltip-day-calculation-midnight-plus-one-day ()
  "Test event at midnight tomorrow (24h exactly) shows '1 day'."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 0 0))  ; Midnight today
             (content (test-chime-tooltip-day-calculation--create-event-at-hours now "Midnight Tomorrow" 24))
             (test-file (chime-create-temp-test-file-with-content content))
             (events (with-current-buffer (find-file-noselect test-file)
                      (org-mode)
                      (goto-char (point-min))
                      (let ((evs nil))
                        (while (re-search-forward "^\\*+ " nil t)
                          (push (chime--gather-info (point-marker)) evs))
                        (nreverse evs)))))
        (kill-buffer (get-file-buffer test-file))

        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            (should (string-match-p "(in 1 day)" tooltip))
            (should-not (string-match-p "hour" tooltip)))))
    (chime-delete-test-base-dir)))

;;; Multiple Events - Verify distinct formatting

(ert-deftest test-chime-tooltip-day-calculation-multiple-events-distinct ()
  "Test multiple events at different fractional-day offsets show distinct times."
  (chime-create-test-base-dir)
  (unwind-protect
      (let* ((now (test-time-today-at 12 0))
             (content (concat
                       (test-chime-tooltip-day-calculation--create-event-at-hours now "Event 1 Day" 24)
                       (test-chime-tooltip-day-calculation--create-event-at-hours now "Event 1.5 Days" 36)
                       (test-chime-tooltip-day-calculation--create-event-at-hours now "Event 2 Days" 48)
                       (test-chime-tooltip-day-calculation--create-event-at-hours now "Event 2.75 Days" 66)))
             (test-file (chime-create-temp-test-file-with-content content))
             (events (with-current-buffer (find-file-noselect test-file)
                      (org-mode)
                      (goto-char (point-min))
                      (let ((evs nil))
                        (while (re-search-forward "^\\*+ " nil t)
                          (push (chime--gather-info (point-marker)) evs))
                        (nreverse evs)))))
        (kill-buffer (get-file-buffer test-file))

        (setq chime-modeline-lookahead-minutes 10080)
        (setq chime-tooltip-lookahead-hours 168)

        (with-test-time now
          (chime--update-modeline events)
          (let ((tooltip (chime--make-tooltip chime--upcoming-events)))
            ;; Verify each event shows correctly
            (should (string-match-p "Event 1 Day.*(in 1 day)" tooltip))
            (should (string-match-p "Event 1.5 Days.*(in 1 day 12 hours)" tooltip))
            (should (string-match-p "Event 2 Days.*(in 2 days)" tooltip))
            (should (string-match-p "Event 2.75 Days.*(in 2 days 18 hours)" tooltip))

            ;; Verify they're all different
            (let ((lines (split-string tooltip "\n")))
              (let ((countdowns (cl-remove-if-not
                                (lambda (line) (string-match-p "Event.*day" line))
                                lines)))
                ;; Should have 4 distinct countdown lines
                (should (= 4 (length countdowns)))
                ;; All should be unique
                (should (= 4 (length (delete-dups (copy-sequence countdowns))))))))))
    (chime-delete-test-base-dir)))

(provide 'test-chime-tooltip-day-calculation)
;;; test-chime-tooltip-day-calculation.el ends here
