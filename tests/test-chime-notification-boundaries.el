;;; test-chime-notification-boundaries.el --- Boundary tests for notification intervals -*- lexical-binding: t; -*-

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

;; Comprehensive boundary tests for notification interval filtering.
;; Tests exact matching behavior, multi-day intervals, and edge cases.
;; Parallels the tooltip lookahead boundary tests but for notifications.

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

(defun test-chime-notification-boundaries-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir))

(defun test-chime-notification-boundaries-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Exact Matching Tests

(ert-deftest test-chime-notification-boundary-exact-10-minutes ()
  "Test that notification fires exactly at 10-minute interval.

Event at 14:10, interval 10 minutes, current time 14:00.
Should match: current_time + 10 minutes = 14:10 = event_time"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            ;; Should match: event is exactly 10 minutes away
            (should (= 1 (length result))))))
    (test-chime-notification-boundaries-teardown)))

(ert-deftest test-chime-notification-boundary-9-minutes-no-match ()
  "Test that notification does NOT fire at 9 minutes (1 minute before interval).

Event at 14:10, interval 10 minutes, current time 14:01.
Should NOT match: current_time + 10 minutes = 14:11 ≠ 14:10"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 1))  ; 1 minute past the hour
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            ;; Should NOT match: event is 9 minutes away, not 10
            (should (= 0 (length result))))))
    (test-chime-notification-boundaries-teardown)))

(ert-deftest test-chime-notification-boundary-11-minutes-no-match ()
  "Test that notification does NOT fire at 11 minutes (1 minute after interval).

Event at 14:11, interval 10 minutes, current time 14:00.
Should NOT match: current_time + 10 minutes = 14:10 ≠ 14:11"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 11))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            ;; Should NOT match: event is 11 minutes away, not 10
            (should (= 0 (length result))))))
    (test-chime-notification-boundaries-teardown)))

;;; Multi-Day Interval Tests

(ert-deftest test-chime-notification-boundary-4-days-exact ()
  "Test notification for event exactly 4 days away.

This tests the user's reported bug: event on Saturday (4 days from Tuesday).
With default 10-minute interval, should NOT match.
Event at Saturday 10:00am, current time Tuesday 10:00am, interval 10 minutes.
Should NOT match: Tuesday 10:00 + 10 min = Tuesday 10:10 ≠ Saturday 10:00"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event 4 days from now at same time
             (event-time (time-add now (seconds-to-time (* 4 24 3600))))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Saturday Event")
                          (intervals . ((10 . medium)))))  ; Default 10 minutes
                 (result (chime--notifications event)))
            ;; Should NOT match: event is 4 days away, interval is 10 minutes
            (should (= 0 (length result))))))
    (test-chime-notification-boundaries-teardown)))

(ert-deftest test-chime-notification-boundary-4-days-with-4-day-interval ()
  "Test notification for event 4 days away with 4-day interval.

Event at Saturday 10:00am, current time Tuesday 10:00am, interval 5760 minutes (4 days).
Should match: Tuesday 10:00 + 4 days = Saturday 10:00 = event_time"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event exactly 4 days from now
             (event-time (time-add now (seconds-to-time (* 4 24 3600))))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Saturday Event")
                          (intervals . ((5760 . medium)))))  ; 4 days = 5760 minutes
                 (result (chime--notifications event)))
            ;; Should match: event is exactly 4 days away, interval is 4 days
            (should (= 1 (length result))))))
    (test-chime-notification-boundaries-teardown)))

(ert-deftest test-chime-notification-boundary-1-week-interval ()
  "Test notification for event exactly 1 week away with 1-week interval.

Event 7 days from now, interval 10080 minutes (7 days).
Should match: current_time + 7 days = event_time"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-now))
             ;; Event exactly 7 days from now
             (event-time (time-add now (seconds-to-time (* 7 24 3600))))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Next Week Event")
                          (intervals . ((10080 . medium)))))  ; 7 days = 10080 minutes
                 (result (chime--notifications event)))
            ;; Should match: event is exactly 7 days away, interval is 7 days
            (should (= 1 (length result))))))
    (test-chime-notification-boundaries-teardown)))

;;; Cross-Month Boundary Tests (Testing for day-of-month matching bug)

(ert-deftest test-chime-notification-boundary-same-day-different-month ()
  "Test that events on same day-of-month but different months do NOT match.

This tests for the bug in chime--time= that only compares day:hour:minute.
Event on Nov 18 at 10:00, current time Oct 18 at 10:00 minus 10 minutes.
Should NOT match even though day-of-month (18) is the same."
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* (;; Oct 18, 2024 at 9:50am
             (now (encode-time 0 50 9 18 10 2024))
             ;; Nov 18, 2024 at 10:00am (31 days + 10 minutes later)
             (event-time (encode-time 0 0 10 18 11 2024))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Next Month Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            ;; Should NOT match: different months, even though day-of-month matches
            (should (= 0 (length result))))))
    (test-chime-notification-boundaries-teardown)))

(ert-deftest test-chime-notification-boundary-same-day-different-year ()
  "Test that events on same day/month but different years do NOT match.

Event on Nov 18, 2025 at 10:00, current time Nov 18, 2024 at 9:50.
Should NOT match even though month and day-of-month are the same."
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* (;; Nov 18, 2024 at 9:50am
             (now (encode-time 0 50 9 18 11 2024))
             ;; Nov 18, 2025 at 10:00am (1 year + 10 minutes later)
             (event-time (encode-time 0 0 10 18 11 2025))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Next Year Event")
                          (intervals . ((10 . medium)))))
                 (result (chime--notifications event)))
            ;; Should NOT match: different years
            (should (= 0 (length result))))))
    (test-chime-notification-boundaries-teardown)))

;;; Cross-Day Boundary Tests

(ert-deftest test-chime-notification-boundary-crosses-midnight ()
  "Test notification that crosses midnight boundary.

Event at 00:10 (next day), current time 23:50 (today), interval 20 minutes.
Should match: 23:50 + 20 minutes = 00:10 next day"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 23 50))
             ;; 20 minutes later crosses midnight
             (event-time (time-add now (seconds-to-time (* 20 60))))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Midnight Event")
                          (intervals . ((20 . medium)))))
                 (result (chime--notifications event)))
            ;; Should match: event is exactly 20 minutes away
            (should (= 1 (length result))))))
    (test-chime-notification-boundaries-teardown)))

(ert-deftest test-chime-notification-boundary-just-before-midnight-no-match ()
  "Test that notification doesn't fire just before crossing midnight.

Event at 00:10 (next day), current time 23:51 (today), interval 20 minutes.
Should NOT match: 23:51 + 20 minutes = 00:11 ≠ 00:10"
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 23 51))
             ;; 19 minutes later
             (event-time (time-add now (seconds-to-time (* 19 60))))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Midnight Event")
                          (intervals . ((20 . medium)))))
                 (result (chime--notifications event)))
            ;; Should NOT match: event is 19 minutes away, not 20
            (should (= 0 (length result))))))
    (test-chime-notification-boundaries-teardown)))

;;; Multiple Interval Tests

(ert-deftest test-chime-notification-boundary-multiple-intervals-one-matches ()
  "Test that only matching intervals trigger notifications.

Event at 14:10, current time 14:00, intervals 10 and 20 minutes.
Should match only the 10-minute interval."
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((10 . medium) (20 . high)))))
                 (result (chime--notifications event)))
            ;; Should match only 10-minute interval
            (should (= 1 (length result))))))
    (test-chime-notification-boundaries-teardown)))

(ert-deftest test-chime-notification-boundary-multiple-intervals-all-match ()
  "Test that multiple matching intervals all trigger.

Event at 14:20, current time 14:00, intervals 10 and 20 minutes.
At 14:00, only 20-minute interval should match.
At 14:10, only 10-minute interval should match."
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 20))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Event")
                          (intervals . ((10 . medium) (20 . high)))))
                 (result (chime--notifications event)))
            ;; At 14:00, event is 20 minutes away, so only 20-minute interval matches
            (should (= 1 (length result)))))

        ;; Now test at 14:10 (10 minutes before event)
        (let ((now-2 (test-time-today-at 14 10)))
          (with-test-time now-2
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Test Event")
                            (intervals . ((10 . medium) (20 . high)))))
                   (result (chime--notifications event)))
              ;; At 14:10, event is 10 minutes away, so only 10-minute interval matches
              (should (= 1 (length result)))))))
    (test-chime-notification-boundaries-teardown)))

;;; Escalating Notification Tests

(ert-deftest test-chime-notification-boundary-escalating-intervals ()
  "Test escalating notifications: 1 day, 1 hour, 10 minutes before event.

Simulates the common use case of multiple notifications at different times."
  (test-chime-notification-boundaries-setup)
  (unwind-protect
      (let* ((base-time (test-time-today-at 10 0))
             (event-time (time-add base-time (seconds-to-time (* 24 3600))))  ; 1 day later
             (timestamp-str (test-timestamp-string event-time)))

        ;; Test 1: 24 hours before (1 day interval)
        (with-test-time base-time
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Important Event")
                          (intervals . ((1440 . low) (60 . medium) (10 . high)))))
                 (result (chime--notifications event)))
            ;; Should match 1440-minute (1 day) interval
            (should (= 1 (length result)))))

        ;; Test 2: 1 hour before (60-minute interval)
        (let ((time-1h-before (time-add event-time (seconds-to-time (* -60 60)))))
          (with-test-time time-1h-before
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Important Event")
                            (intervals . ((1440 . low) (60 . medium) (10 . high)))))
                   (result (chime--notifications event)))
              ;; Should match 60-minute interval
              (should (= 1 (length result))))))

        ;; Test 3: 10 minutes before (10-minute interval)
        (let ((time-10m-before (time-add event-time (seconds-to-time (* -10 60)))))
          (with-test-time time-10m-before
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Important Event")
                            (intervals . ((1440 . low) (60 . medium) (10 . high)))))
                   (result (chime--notifications event)))
              ;; Should match 10-minute interval
              (should (= 1 (length result)))))))
    (test-chime-notification-boundaries-teardown)))

(provide 'test-chime-notification-boundaries)
;;; test-chime-notification-boundaries.el ends here
