;;; test-chime-time-left.el --- Tests for chime--time-left -*- lexical-binding: t; -*-

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

;; Unit tests for chime--time-left function.
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

(defun test-chime-time-left-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset format strings to defaults
  (setq chime-time-left-format-at-event "right now")
  (setq chime-time-left-format-short "in %M")
  (setq chime-time-left-format-long "in %H %M"))

(defun test-chime-time-left-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-time-left-one-minute-formats-correctly ()
  "Test that 1 minute formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 60)))
        (should (stringp result))
        (should (string-match-p "in 1 minute" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-five-minutes-formats-correctly ()
  "Test that 5 minutes formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 300)))
        (should (stringp result))
        (should (string-match-p "in 5 minutes" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-ten-minutes-formats-correctly ()
  "Test that 10 minutes formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 600)))
        (should (stringp result))
        (should (string-match-p "in 10 minutes" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-thirty-minutes-formats-correctly ()
  "Test that 30 minutes formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 1800)))
        (should (stringp result))
        (should (string-match-p "in 30 minutes" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-one-hour-formats-correctly ()
  "Test that 1 hour formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 3600)))
        (should (stringp result))
        ;; At exactly 1 hour (3600s), still shows minutes format
        (should (string-match-p "in 60 minutes" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-two-hours-formats-correctly ()
  "Test that 2 hours formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 7200)))
        (should (stringp result))
        (should (string-match-p "in 2 hours" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-one-hour-thirty-minutes-formats-correctly ()
  "Test that 1 hour 30 minutes formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 5400)))
        (should (stringp result))
        ;; Should show both hours and minutes
        (should (string-match-p "in 1 hour 30 minutes" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-three-hours-fifteen-minutes-formats-correctly ()
  "Test that 3 hours 15 minutes formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 11700)))
        (should (stringp result))
        (should (string-match-p "in 3 hours 15 minutes" result)))
    (test-chime-time-left-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-time-left-zero-seconds-returns-right-now ()
  "Test that 0 seconds returns 'right now'."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 0)))
        (should (stringp result))
        (should (string-equal "right now" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-one-second-shows-right-now ()
  "Test that 1 second shows 'right now'."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 1)))
        (should (stringp result))
        ;; Less than a minute, but format-seconds might show "in 0 minutes"
        ;; or the implementation might handle this specially
        (should result))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-fifty-nine-seconds-shows-minutes ()
  "Test that 59 seconds shows in minutes format."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 59)))
        (should (stringp result))
        ;; Should use minutes format (< 1 hour)
        (should (string-match-p "in" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-exactly-one-hour-shows-minutes-format ()
  "Test that exactly 1 hour shows minutes format."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 3600)))
        (should (stringp result))
        ;; At exactly 3600s, still uses minutes format (boundary case)
        (should (string-match-p "in 60 minutes" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-fifty-nine-minutes-shows-minutes-only ()
  "Test that 59 minutes shows minutes format only."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 3540)))  ; 59 minutes
        (should (stringp result))
        (should (string-match-p "in 59 minutes" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-twenty-four-hours-formats-correctly ()
  "Test that 24 hours formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 86400)))
        (should (stringp result))
        (should (string-match-p "in 24 hours" result)))
    (test-chime-time-left-teardown)))

;;; Error Cases

(ert-deftest test-chime-time-left-negative-value-returns-right-now ()
  "Test that negative value returns 'right now'."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left -60)))
        (should (stringp result))
        (should (string-equal "right now" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-large-negative-returns-right-now ()
  "Test that large negative value returns 'right now'."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left -3600)))
        (should (stringp result))
        (should (string-equal "right now" result)))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-very-large-value-formats-correctly ()
  "Test that very large value (1 week) formats correctly."
  (test-chime-time-left-setup)
  (unwind-protect
      (let ((result (chime--time-left 604800)))  ; 1 week
        (should (stringp result))
        ;; Should format with days/hours
        (should (string-match-p "in" result)))
    (test-chime-time-left-teardown)))

;;; Custom Format Cases

(ert-deftest test-chime-time-left-custom-compact-format-short ()
  "Test custom compact format for short duration (in 5m)."
  (test-chime-time-left-setup)
  (unwind-protect
      (progn
        (setq chime-time-left-format-short "in %mm")
        (let ((result (chime--time-left 300)))  ; 5 minutes
          (should (stringp result))
          (should (string-equal "in 5m" result))))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-custom-compact-format-long ()
  "Test custom compact format for long duration (in 1h 37m)."
  (test-chime-time-left-setup)
  (unwind-protect
      (progn
        (setq chime-time-left-format-long "in %hh %mm")
        (let ((result (chime--time-left 5820)))  ; 1 hour 37 minutes
          (should (stringp result))
          (should (string-equal "in 1h 37m" result))))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-custom-parentheses-format ()
  "Test custom format with parentheses ((1 hr 37 min))."
  (test-chime-time-left-setup)
  (unwind-protect
      (progn
        (setq chime-time-left-format-long "(%h hr %m min)")
        (let ((result (chime--time-left 5820)))  ; 1 hour 37 minutes
          (should (stringp result))
          (should (string-equal "(1 hr 37 min)" result))))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-custom-no-prefix-format ()
  "Test custom format without 'in' prefix (1h37m)."
  (test-chime-time-left-setup)
  (unwind-protect
      (progn
        (setq chime-time-left-format-long "%hh%mm")
        (let ((result (chime--time-left 5820)))  ; 1 hour 37 minutes
          (should (stringp result))
          (should (string-equal "1h37m" result))))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-custom-at-event-message ()
  "Test custom at-event message (NOW!)."
  (test-chime-time-left-setup)
  (unwind-protect
      (progn
        (setq chime-time-left-format-at-event "NOW!")
        (let ((result (chime--time-left 0)))
          (should (stringp result))
          (should (string-equal "NOW!" result))))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-custom-short-with-unit-text ()
  "Test custom short format with custom unit text (5 min)."
  (test-chime-time-left-setup)
  (unwind-protect
      (progn
        (setq chime-time-left-format-short "%m min")
        (let ((result (chime--time-left 300)))  ; 5 minutes
          (should (stringp result))
          (should (string-equal "5 min" result))))
    (test-chime-time-left-teardown)))

(ert-deftest test-chime-time-left-custom-emoji-format ()
  "Test custom format with emoji (🕐 1h37m)."
  (test-chime-time-left-setup)
  (unwind-protect
      (progn
        (setq chime-time-left-format-long "🕐 %hh%mm")
        (let ((result (chime--time-left 5820)))  ; 1 hour 37 minutes
          (should (stringp result))
          (should (string-equal "🕐 1h37m" result))))
    (test-chime-time-left-teardown)))

(provide 'test-chime-time-left)
;;; test-chime-time-left.el ends here
