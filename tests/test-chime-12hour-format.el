;;; test-chime-12hour-format.el --- Tests for 12-hour am/pm timestamp parsing -*- lexical-binding: t; -*-

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

;; Tests for 12-hour am/pm timestamp format support.
;; Verifies that chime correctly parses timestamps like:
;; - <2025-11-05 Wed 1:30pm>
;; - <2025-11-05 Wed 1:30 PM>
;; - <2025-11-05 Wed 12:00pm>
;; - <2025-11-05 Wed 12:00am>

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

;;; Tests for chime--convert-12hour-to-24hour

(ert-deftest test-12hour-convert-1pm-to-13 ()
  "Test that 1pm converts to hour 13."
  (should (= 13 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 1:30pm>" 1))))

(ert-deftest test-12hour-convert-1pm-uppercase-to-13 ()
  "Test that 1PM (uppercase) converts to hour 13."
  (should (= 13 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 1:30PM>" 1))))

(ert-deftest test-12hour-convert-1pm-with-space-to-13 ()
  "Test that 1 PM (with space) converts to hour 13."
  (should (= 13 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 1:30 PM>" 1))))

(ert-deftest test-12hour-convert-11pm-to-23 ()
  "Test that 11pm converts to hour 23."
  (should (= 23 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 11:59pm>" 11))))

(ert-deftest test-12hour-convert-12pm-noon-stays-12 ()
  "Test that 12pm (noon) stays as hour 12."
  (should (= 12 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 12:00pm>" 12))))

(ert-deftest test-12hour-convert-12am-midnight-to-0 ()
  "Test that 12am (midnight) converts to hour 0."
  (should (= 0 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 12:00am>" 12))))

(ert-deftest test-12hour-convert-1am-stays-1 ()
  "Test that 1am stays as hour 1."
  (should (= 1 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 1:00am>" 1))))

(ert-deftest test-12hour-convert-11am-stays-11 ()
  "Test that 11am stays as hour 11."
  (should (= 11 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 11:59am>" 11))))

(ert-deftest test-12hour-convert-24hour-format-unchanged ()
  "Test that 24-hour format (no am/pm) is unchanged."
  (should (= 13 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 13:30>" 13))))

(ert-deftest test-12hour-convert-24hour-0-unchanged ()
  "Test that 24-hour format hour 0 (midnight) is unchanged."
  (should (= 0 (chime--convert-12hour-to-24hour "<2025-11-05 Wed 00:00>" 0))))

;;; Tests for chime--timestamp-parse with 12-hour format

(ert-deftest test-12hour-parse-1-30pm ()
  "Test parsing 1:30pm returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 1:30pm>")))
    (should result)
    ;; Check that hour is 13 (1pm)
    (let ((time-decoded (decode-time result)))
      (should (= 13 (decoded-time-hour time-decoded)))
      (should (= 30 (decoded-time-minute time-decoded)))
      (should (= 5 (decoded-time-day time-decoded)))
      (should (= 11 (decoded-time-month time-decoded)))
      (should (= 2025 (decoded-time-year time-decoded))))))

(ert-deftest test-12hour-parse-2-00pm ()
  "Test parsing 2:00PM (uppercase) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 2:00PM>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 14 (decoded-time-hour time-decoded)))
      (should (= 0 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-3-45-pm-with-space ()
  "Test parsing 3:45 PM (with space) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 3:45 PM>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 15 (decoded-time-hour time-decoded)))
      (should (= 45 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-11-59pm ()
  "Test parsing 11:59pm (last minute before midnight) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 11:59pm>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 23 (decoded-time-hour time-decoded)))
      (should (= 59 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-12-00pm-noon ()
  "Test parsing 12:00pm (noon) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 12:00pm>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 12 (decoded-time-hour time-decoded)))
      (should (= 0 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-12-30pm ()
  "Test parsing 12:30pm (afternoon) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 12:30pm>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 12 (decoded-time-hour time-decoded)))
      (should (= 30 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-12-00am-midnight ()
  "Test parsing 12:00am (midnight) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 12:00am>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 0 (decoded-time-hour time-decoded)))
      (should (= 0 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-12-30am ()
  "Test parsing 12:30am (after midnight) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 12:30am>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 0 (decoded-time-hour time-decoded)))
      (should (= 30 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-1-00am ()
  "Test parsing 1:00am returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 1:00am>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 1 (decoded-time-hour time-decoded)))
      (should (= 0 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-11-59am ()
  "Test parsing 11:59am (last minute before noon) returns correct time list."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 11:59am>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 11 (decoded-time-hour time-decoded)))
      (should (= 59 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-24hour-still-works ()
  "Test that 24-hour format (13:30) still works correctly."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 13:30>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 13 (decoded-time-hour time-decoded)))
      (should (= 30 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-24hour-midnight ()
  "Test that 24-hour format 00:00 (midnight) still works correctly."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 00:00>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 0 (decoded-time-hour time-decoded)))
      (should (= 0 (decoded-time-minute time-decoded))))))

(ert-deftest test-12hour-parse-24hour-23-59 ()
  "Test that 24-hour format 23:59 still works correctly."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 23:59>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 23 (decoded-time-hour time-decoded)))
      (should (= 59 (decoded-time-minute time-decoded))))))

;;; Mixed case and whitespace variations

(ert-deftest test-12hour-parse-mixed-case-Pm ()
  "Test parsing with mixed case Pm."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 3:30Pm>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 15 (decoded-time-hour time-decoded))))))

(ert-deftest test-12hour-parse-mixed-case-pM ()
  "Test parsing with mixed case pM."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 3:30pM>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 15 (decoded-time-hour time-decoded))))))

(ert-deftest test-12hour-parse-multiple-spaces ()
  "Test parsing with multiple spaces before am/pm."
  (let ((result (chime--timestamp-parse "<2025-11-05 Wed 3:30  pm>")))
    (should result)
    (let ((time-decoded (decode-time result)))
      (should (= 15 (decoded-time-hour time-decoded))))))

(provide 'test-chime-12hour-format)
;;; test-chime-12hour-format.el ends here
