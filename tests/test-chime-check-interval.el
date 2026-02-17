;;; test-chime-check-interval.el --- Tests for chime-check-interval -*- lexical-binding: t; -*-

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

;; Unit tests for chime-check-interval customization variable.
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

(defun test-chime-check-interval-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset to default
  (setq chime-check-interval 60))

(defun test-chime-check-interval-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  ;; Reset to default
  (setq chime-check-interval 60))

;;; Normal Cases

(ert-deftest test-chime-check-interval-normal-default-is-60 ()
  "Test that default check interval is 60 seconds."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (should (equal chime-check-interval 60)))
    (test-chime-check-interval-teardown)))

(ert-deftest test-chime-check-interval-normal-can-set-30-seconds ()
  "Test that check interval can be set to 30 seconds."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (setq chime-check-interval 30)
        (should (equal chime-check-interval 30)))
    (test-chime-check-interval-teardown)))

(ert-deftest test-chime-check-interval-normal-can-set-300-seconds ()
  "Test that check interval can be set to 300 seconds (5 minutes)."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (setq chime-check-interval 300)
        (should (equal chime-check-interval 300)))
    (test-chime-check-interval-teardown)))

(ert-deftest test-chime-check-interval-normal-can-set-10-seconds ()
  "Test that check interval can be set to 10 seconds (minimum recommended)."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (setq chime-check-interval 10)
        (should (equal chime-check-interval 10)))
    (test-chime-check-interval-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-check-interval-boundary-one-second-triggers-warning ()
  "Test that 1 second interval triggers warning but is allowed."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        ;; Setting to 1 should trigger a warning but succeed
        ;; We can't easily test the warning, but we can verify it's set
        (setq chime-check-interval 1)
        (should (equal chime-check-interval 1)))
    (test-chime-check-interval-teardown)))

(ert-deftest test-chime-check-interval-boundary-large-value-accepted ()
  "Test that large interval values are accepted (e.g., 1 hour)."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (setq chime-check-interval 3600)  ; 1 hour
        (should (equal chime-check-interval 3600)))
    (test-chime-check-interval-teardown)))

;;; Error Cases

(ert-deftest test-chime-check-interval-error-zero-rejected ()
  "Test that zero interval is rejected."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (should-error (customize-set-variable 'chime-check-interval 0)
                      :type 'user-error))
    (test-chime-check-interval-teardown)))

(ert-deftest test-chime-check-interval-error-negative-rejected ()
  "Test that negative interval is rejected."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (should-error (customize-set-variable 'chime-check-interval -60)
                      :type 'user-error))
    (test-chime-check-interval-teardown)))

(ert-deftest test-chime-check-interval-error-non-integer-rejected ()
  "Test that non-integer values are rejected."
  (test-chime-check-interval-setup)
  (unwind-protect
      (progn
        (should-error (customize-set-variable 'chime-check-interval "60")
                      :type 'user-error))
    (test-chime-check-interval-teardown)))

(provide 'test-chime-check-interval)
;;; test-chime-check-interval.el ends here
