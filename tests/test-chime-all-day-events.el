;;; test-chime-all-day-events.el --- Tests for all-day event handling -*- lexical-binding: t; -*-

;; Tests for:
;; - All-day event detection
;; - Tooltip display configuration (chime-tooltip-show-all-day-events)
;; - Day-wide notifications
;; - Advance notice notifications

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'dash)
(require 'org-agenda)
(load (expand-file-name "../chime.el") nil t)
(require 'testutil-general (expand-file-name "testutil-general.el"))
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Helper Functions

(defun test-allday--create-event (title &optional timestamp-str has-time)
  "Create a test event with TITLE and TIMESTAMP-STR.
If HAS-TIME is t, timestamp includes time component."
  (let* ((ts-str (or timestamp-str
                    (if has-time
                        "<2025-11-15 Sat 10:00-11:00>"
                      "<2025-11-15 Sat>")))
         (parsed-time (when has-time
                       (chime--timestamp-parse ts-str))))
    `((title . ,title)
      (times . ((,ts-str . ,parsed-time)))
      (intervals . ((0 15 30)))
      (marker-file . "/tmp/test.org")
      (marker-pos . 1))))

;;; Tests: All-day event detection

(ert-deftest test-chime-has-timestamp-with-time ()
  "Test that timestamps with time component are detected.

REFACTORED: Uses dynamic timestamps"
  (let ((time1 (test-time-tomorrow-at 10 0))
        (time2 (test-time-tomorrow-at 9 30)))
    (should (chime--has-timestamp (test-timestamp-string time1)))
    (should (chime--has-timestamp (test-timestamp-string time1)))
    (should (chime--has-timestamp (format-time-string "<%Y-%m-%d %a %H:%M-%H:%M>" time2)))))

(ert-deftest test-chime-has-timestamp-without-time ()
  "Test that all-day timestamps (no time) are correctly identified.

REFACTORED: Uses dynamic timestamps"
  (let ((time1 (test-time-tomorrow-at 0 0))
        (time2 (test-time-days-from-now 10))
        (time3 (test-time-days-from-now 30)))
    (should-not (chime--has-timestamp (test-timestamp-string time1 t)))
    (should-not (chime--has-timestamp (test-timestamp-string time2 t)))
    (should-not (chime--has-timestamp (test-timestamp-string time3 t)))))

(ert-deftest test-chime-event-has-day-wide-timestamp ()
  "Test detection of events with all-day timestamps.

REFACTORED: Uses dynamic timestamps"
  (let* ((all-day-time (test-time-days-from-now 10))
         (timed-time (test-time-tomorrow-at 10 0))
         (all-day-event (test-allday--create-event "Birthday" (test-timestamp-string all-day-time t) nil))
         (timed-event (test-allday--create-event "Meeting" (test-timestamp-string timed-time) t)))
    (should (chime-event-has-any-day-wide-timestamp all-day-event))
    (should-not (chime-event-has-any-day-wide-timestamp timed-event))))

;;; Tests: Advance notice window

(ert-deftest test-chime-advance-notice-nil ()
  "Test that advance notice is disabled when chime-day-wide-advance-notice is nil.

TIME RELATIONSHIPS:
  Current time: TODAY at 10:00 AM
  Event: TOMORROW (all-day)
  Setting: chime-day-wide-advance-notice = nil

EXPECTED: Should NOT be in advance notice window (disabled)

REFACTORED: Uses dynamic timestamps via testutil-time.el"
  (let* ((now (test-time-now))
         (tomorrow (test-time-tomorrow-at 0 0))
         (tomorrow-timestamp (test-timestamp-string tomorrow t))
         (chime-day-wide-advance-notice nil)
         (event (test-allday--create-event "Birthday Tomorrow" tomorrow-timestamp nil)))
    (with-test-time now
      (should-not (chime-event-within-advance-notice-window event)))))

(ert-deftest test-chime-advance-notice-tomorrow ()
  "Test advance notice for event tomorrow when set to 1 day.

TIME RELATIONSHIPS:
  Current time: TODAY at 10:00 AM
  Event: TOMORROW (all-day)
  Setting: chime-day-wide-advance-notice = 1

EXPECTED: Should be in advance notice window

REFACTORED: Uses dynamic timestamps via testutil-time.el"
  (let* ((now (test-time-now))
         (tomorrow (test-time-tomorrow-at 0 0))
         (tomorrow-timestamp (test-timestamp-string tomorrow t))
         (chime-day-wide-advance-notice 1)
         (event (test-allday--create-event "Birthday Tomorrow" tomorrow-timestamp nil)))
    (with-test-time now
      (should (chime-event-within-advance-notice-window event)))))

(ert-deftest test-chime-advance-notice-two-days ()
  "Test advance notice for event in 2 days when set to 2 days.

TIME: TODAY, Event: 2 DAYS FROM NOW, advance=2
EXPECTED: Should be in window
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (two-days (test-time-days-from-now 2))
         (timestamp (test-timestamp-string two-days t))
         (chime-day-wide-advance-notice 2)
         (event (test-allday--create-event "Birthday in 2 days" timestamp nil)))
    (with-test-time now
      (should (chime-event-within-advance-notice-window event)))))

(ert-deftest test-chime-advance-notice-too-far-future ()
  "Test that events beyond advance notice window are not included.

TIME: TODAY, Event: 5 DAYS FROM NOW, advance=1
EXPECTED: Should NOT be in window (too far)
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (five-days (test-time-days-from-now 5))
         (timestamp (test-timestamp-string five-days t))
         (chime-day-wide-advance-notice 1)
         (event (test-allday--create-event "Birthday in 5 days" timestamp nil)))
    (with-test-time now
      (should-not (chime-event-within-advance-notice-window event)))))

(ert-deftest test-chime-advance-notice-today-not-included ()
  "Test that today's events are not in advance notice window.

TIME: TODAY, Event: TODAY, advance=1
EXPECTED: Should NOT be in window (today is handled separately)
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (today-timestamp (test-timestamp-string now t))
         (chime-day-wide-advance-notice 1)
         (event (test-allday--create-event "Birthday Today" today-timestamp nil)))
    (with-test-time now
      ;; Today's event should NOT be in advance notice window
      ;; It should be handled by regular day-wide logic
      (should-not (chime-event-within-advance-notice-window event)))))

(ert-deftest test-chime-advance-notice-timed-events-ignored ()
  "Test that timed events are not included in advance notice.

TIME: TODAY, Event: TOMORROW with time, advance=1
EXPECTED: Should NOT be in window (only all-day events qualify)
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (tomorrow (test-time-tomorrow-at 10 0))
         (timestamp (test-timestamp-string tomorrow))
         (chime-day-wide-advance-notice 1)
         (event (test-allday--create-event "Meeting Tomorrow" timestamp t)))
    (with-test-time now
      ;; Timed events should not trigger advance notices
      (should-not (chime-event-within-advance-notice-window event)))))

;;; Tests: Day-wide notification text

(ert-deftest test-chime-day-wide-notification-today ()
  "Test notification text for all-day event today.
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (today-timestamp (test-timestamp-string now t))
         (chime-day-wide-advance-notice nil)
         (event (test-allday--create-event "Blake's Birthday" today-timestamp nil)))
    (with-test-time now
      (let ((text (chime--day-wide-notification-text event)))
        (should (string-match-p "Blake's Birthday is due or scheduled today" text))))))

(ert-deftest test-chime-day-wide-notification-tomorrow ()
  "Test notification text for all-day event tomorrow with advance notice.
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (tomorrow (test-time-tomorrow-at 0 0))
         (tomorrow-timestamp (test-timestamp-string tomorrow t))
         (chime-day-wide-advance-notice 1)
         (event (test-allday--create-event "Blake's Birthday" tomorrow-timestamp nil)))
    (with-test-time now
      (let ((text (chime--day-wide-notification-text event)))
        (should (string-match-p "Blake's Birthday is tomorrow" text))))))

(ert-deftest test-chime-day-wide-notification-in-2-days ()
  "Test notification text for all-day event in 2 days with advance notice.
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (two-days (test-time-days-from-now 2))
         (timestamp (test-timestamp-string two-days t))
         (chime-day-wide-advance-notice 2)
         (event (test-allday--create-event "Blake's Birthday" timestamp nil)))
    (with-test-time now
      (let ((text (chime--day-wide-notification-text event)))
        (should (string-match-p "Blake's Birthday is in 2 days" text))))))

(ert-deftest test-chime-day-wide-notification-in-N-days ()
  "Test notification text for all-day event in N days with advance notice.
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (five-days (test-time-days-from-now 5))
         (timestamp (test-timestamp-string five-days t))
         (chime-day-wide-advance-notice 5)
         (event (test-allday--create-event "Conference" timestamp nil)))
    (with-test-time now
      (let ((text (chime--day-wide-notification-text event)))
        (should (string-match-p "Conference is in [0-9]+ days" text))))))

;;; Tests: Display as day-wide event

(ert-deftest test-chime-display-as-day-wide-event-today ()
  "Test that all-day events today are displayed as day-wide.
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (today-timestamp (test-timestamp-string now t))
         (chime-day-wide-advance-notice nil)
         (event (test-allday--create-event "Birthday Today" today-timestamp nil)))
    (with-test-time now
      (should (chime-display-as-day-wide-event event)))))

(ert-deftest test-chime-display-as-day-wide-event-tomorrow-with-advance ()
  "Test that all-day events tomorrow are displayed when advance notice is enabled.
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (tomorrow (test-time-tomorrow-at 0 0))
         (timestamp (test-timestamp-string tomorrow t))
         (chime-day-wide-advance-notice 1)
         (event (test-allday--create-event "Birthday Tomorrow" timestamp nil)))
    (with-test-time now
      (should (chime-display-as-day-wide-event event)))))

(ert-deftest test-chime-display-as-day-wide-event-tomorrow-without-advance ()
  "Test that all-day events tomorrow are NOT displayed without advance notice.
REFACTORED: Uses dynamic timestamps"
  (let* ((now (test-time-now))
         (tomorrow (test-time-tomorrow-at 0 0))
         (timestamp (test-timestamp-string tomorrow t))
         (chime-day-wide-advance-notice nil)
         (event (test-allday--create-event "Birthday Tomorrow" timestamp nil)))
    (with-test-time now
      (should-not (chime-display-as-day-wide-event event)))))

;;; Tests: Tooltip configuration

;; Note: These tests verify the logic exists, but full integration testing
;; requires the modeline update function which is async. See integration tests.

(ert-deftest test-chime-tooltip-config-exists ()
  "Test that chime-tooltip-show-all-day-events customization exists."
  (should (boundp 'chime-tooltip-show-all-day-events))
  (should (booleanp chime-tooltip-show-all-day-events)))

(ert-deftest test-chime-day-wide-alert-times-default ()
  "Test that chime-day-wide-alert-times has correct default."
  (should (boundp 'chime-day-wide-alert-times))
  (should (equal chime-day-wide-alert-times '("08:00"))))

(ert-deftest test-chime-day-wide-advance-notice-default ()
  "Test that chime-day-wide-advance-notice has correct default."
  (should (boundp 'chime-day-wide-advance-notice))
  (should (null chime-day-wide-advance-notice)))

(provide 'test-chime-all-day-events)
;;; test-chime-all-day-events.el ends here
