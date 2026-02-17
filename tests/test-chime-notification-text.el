;;; test-chime-notification-text.el --- Tests for chime--notification-text -*- lexical-binding: t; -*-

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

;; Unit tests for chime--notification-text function.
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

(defun test-chime-notification-text-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset display format to default
  (setq chime-display-time-format-string "%I:%M %p")
  ;; Reset notification text format to default
  (setq chime-notification-text-format "%t at %T (%u)")
  ;; Reset time-left formats to defaults
  (setq chime-time-left-format-at-event "right now")
  (setq chime-time-left-format-short "in %M")
  (setq chime-time-left-format-long "in %H %M")
  ;; Reset title truncation to default (no truncation)
  (setq chime-max-title-length nil))

(defun test-chime-notification-text-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-notification-text-standard-event-formats-correctly ()
  "Test that standard event formats correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Team Meeting")))
             (result (chime--notification-text str-interval event)))
        ;; Should format: "Team Meeting at 02:30 PM (in X minutes)"
        (should (stringp result))
        (should (string-match-p "Team Meeting" result))
        (should (string-match-p "02:30 PM" result))
        (should (string-match-p "in 10 minutes" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-morning-time-formats-with-am ()
  "Test that morning time uses AM.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 9 15))
             (str-interval (cons (test-timestamp-string time) '(5 . medium)))
             (event '((title . "Standup")))
             (result (chime--notification-text str-interval event)))
        (should (string-match-p "Standup" result))
        (should (string-match-p "09:15 AM" result))
        (should (string-match-p "in 5 minutes" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-midnight-formats-correctly ()
  "Test that midnight time formats correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 0 0))
             (str-interval (cons (test-timestamp-string time) '(30 . medium)))
             (event '((title . "Midnight Event")))
             (result (chime--notification-text str-interval event)))
        (should (string-match-p "Midnight Event" result))
        (should (string-match-p "12:00 AM" result))
        (should (string-match-p "in 30 minutes" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-noon-formats-correctly ()
  "Test that noon time formats correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 12 0))
             (str-interval (cons (test-timestamp-string time) '(15 . medium)))
             (event '((title . "Lunch")))
             (result (chime--notification-text str-interval event)))
        (should (string-match-p "Lunch" result))
        (should (string-match-p "12:00 PM" result))
        (should (string-match-p "in 15 minutes" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-zero-minutes-shows-right-now ()
  "Test that zero minutes shows 'right now'.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 0))
             (str-interval (cons (test-timestamp-string time) '(0 . high)))
             (event '((title . "Current Event")))
             (result (chime--notification-text str-interval event)))
        (should (string-match-p "Current Event" result))
        (should (string-match-p "02:00 PM" result))
        (should (string-match-p "right now" result)))
    (test-chime-notification-text-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-notification-text-very-long-title-included ()
  "Test that very long titles are included in full.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 15 45))
             (str-interval (cons (test-timestamp-string time) '(20 . medium)))
             (long-title "This is a very long event title that contains many words and might wrap in the notification display")
             (event `((title . ,long-title)))
             (result (chime--notification-text str-interval event)))
        ;; Should include the full title
        (should (string-match-p long-title result))
        (should (string-match-p "03:45 PM" result))
        (should (string-match-p "in 20 minutes" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-title-with-special-characters ()
  "Test that titles with special characters work correctly.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 16 30))
             (str-interval (cons (test-timestamp-string time) '(5 . medium)))
             (event '((title . "Review: Alice's PR #123 (urgent!)")))
             (result (chime--notification-text str-interval event)))
        (should (string-match-p "Review: Alice's PR #123 (urgent!)" result))
        (should (string-match-p "04:30 PM" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-custom-time-format ()
  "Test that custom time format string is respected.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Meeting")))
             (chime-display-time-format-string "%H:%M")  ; 24-hour format
             (result (chime--notification-text str-interval event)))
        ;; Should use 24-hour format
        (should (string-match-p "Meeting" result))
        (should (string-match-p "14:30" result))
        (should-not (string-match-p "PM" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-large-interval-shows-hours ()
  "Test that large intervals show hours.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 18 0))
             (str-interval (cons (test-timestamp-string time) '(120 . low)))  ; 2 hours
             (event '((title . "Evening Event")))
             (result (chime--notification-text str-interval event)))
        (should (string-match-p "Evening Event" result))
        (should (string-match-p "06:00 PM" result))
        ;; Should show hours format
        (should (string-match-p "in 2 hours" result)))
    (test-chime-notification-text-teardown)))

;;; Error Cases

(ert-deftest test-chime-notification-text-empty-title-shows-empty ()
  "Test that empty title still generates output.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "")))
             (result (chime--notification-text str-interval event)))
        ;; Should still format, even with empty title
        (should (stringp result))
        (should (string-match-p "02:30 PM" result))
        (should (string-match-p "in 10 minutes" result)))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-missing-title-shows-nil ()
  "Test that missing title shows nil in output.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '())  ; No title
             (result (chime--notification-text str-interval event)))
        ;; Should still generate output with nil title
        (should (stringp result))
        (should (string-match-p "02:30 PM" result))
        (should (string-match-p "in 10 minutes" result)))
    (test-chime-notification-text-teardown)))

;;; Custom Format Cases

(ert-deftest test-chime-notification-text-custom-title-only ()
  "Test custom format showing title only.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Team Meeting")))
             (chime-notification-text-format "%t"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-equal "Team Meeting" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-custom-title-and-time-no-countdown ()
  "Test custom format with title and time, no countdown.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Team Meeting")))
             (chime-notification-text-format "%t at %T"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-equal "Team Meeting at 02:30 PM" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-custom-title-and-countdown-no-time ()
  "Test custom format with title and countdown, no time.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Team Meeting")))
             (chime-notification-text-format "%t (%u)"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-equal "Team Meeting (in 10 minutes)" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-custom-separator ()
  "Test custom format with custom separator.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Team Meeting")))
             (chime-notification-text-format "%t - %T"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-equal "Team Meeting - 02:30 PM" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-custom-order-time-first ()
  "Test custom format with time before title.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Team Meeting")))
             (chime-notification-text-format "%T: %t"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-equal "02:30 PM: Team Meeting" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-custom-compact-format ()
  "Test custom compact format.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Meeting")))
             (chime-notification-text-format "%t@%T"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-equal "Meeting@02:30 PM" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-custom-with-compact-time-left ()
  "Test custom format with compact time-left format.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Meeting")))
             (chime-notification-text-format "%t (%u)")
             (chime-time-left-format-short "in %mm"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-equal "Meeting (in 10m)" result))))
    (test-chime-notification-text-teardown)))

;;; Time Format Cases

(ert-deftest test-chime-notification-text-24-hour-time-format ()
  "Test 24-hour time format (14:30).

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Meeting")))
             (chime-display-time-format-string "%H:%M"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "14:30" result))
          (should-not (string-match-p "PM" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-12-hour-no-space-before-ampm ()
  "Test 12-hour format without space before AM/PM (02:30PM).

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Meeting")))
             (chime-display-time-format-string "%I:%M%p"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "02:30PM" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-lowercase-ampm ()
  "Test 12-hour format with lowercase am/pm (02:30 pm).

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Meeting")))
             (chime-display-time-format-string "%I:%M %P"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "02:30 pm" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-24-hour-morning ()
  "Test 24-hour format for morning time (09:15).

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 9 15))
             (str-interval (cons (test-timestamp-string time) '(5 . medium)))
             (event '((title . "Standup")))
             (chime-display-time-format-string "%H:%M"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "09:15" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-24-hour-midnight ()
  "Test 24-hour format for midnight (00:00).

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 0 0))
             (str-interval (cons (test-timestamp-string time) '(30 . medium)))
             (event '((title . "Midnight")))
             (chime-display-time-format-string "%H:%M"))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "00:00" result))))
    (test-chime-notification-text-teardown)))

;;; Title Truncation Cases

(ert-deftest test-chime-notification-text-truncate-nil-no-truncation ()
  "Test that nil chime-max-title-length shows full title.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Very Long Meeting Title That Goes On And On")))
             (chime-max-title-length nil))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "Very Long Meeting Title That Goes On And On" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-truncate-25-chars ()
  "Test truncation to 25 characters.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Very Long Meeting Title That Goes On")))
             (chime-max-title-length 25))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "Very Long Meeting Titl\\.\\.\\." result))
          (should-not (string-match-p "That Goes On" result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-truncate-15-chars ()
  "Test truncation to 15 characters.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Very Long Meeting Title")))
             (chime-max-title-length 15))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "Very Long Me\\.\\.\\." result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-truncate-10-chars ()
  "Test truncation to 10 characters.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Very Long Title")))
             (chime-max-title-length 10))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "Very Lo\\.\\.\\." result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-truncate-short-title-unchanged ()
  "Test that short titles are not truncated.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Short")))
             (chime-max-title-length 25))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "Short" result))
          (should-not (string-match-p "\\.\\.\\." result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-truncate-exact-length-unchanged ()
  "Test that title exactly at max length is not truncated.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '((title . "Exactly Twenty-Five C")))  ; 21 chars
             (chime-max-title-length 21))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "Exactly Twenty-Five C" result))
          (should-not (string-match-p "\\.\\.\\." result))))
    (test-chime-notification-text-teardown)))

(ert-deftest test-chime-notification-text-truncate-nil-title-handled ()
  "Test that nil title is handled gracefully with truncation enabled.

REFACTORED: Uses dynamic timestamps"
  (test-chime-notification-text-setup)
  (unwind-protect
      (let* ((time (test-time-tomorrow-at 14 30))
             (str-interval (cons (test-timestamp-string time) '(10 . medium)))
             (event '())  ; No title
             (chime-max-title-length 25))
        (let ((result (chime--notification-text str-interval event)))
          (should (stringp result))
          (should (string-match-p "02:30 PM" result))))
    (test-chime-notification-text-teardown)))

(provide 'test-chime-notification-text)
;;; test-chime-notification-text.el ends here
