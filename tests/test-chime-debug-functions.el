;;; test-chime-debug-functions.el --- Tests for chime debug functions -*- lexical-binding: t; -*-

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

;; Tests for debug functions: chime--debug-dump-events, chime--debug-dump-tooltip,
;; and chime--debug-config. These tests verify that debug functions work correctly
;; and handle edge cases gracefully.

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
(require 'chime-debug (expand-file-name "../chime-debug.el"))

;; Load test utilities
(require 'testutil-general (expand-file-name "testutil-general.el"))
(require 'testutil-time (expand-file-name "testutil-time.el"))

;;; Setup and Teardown

(defun test-chime-debug-functions-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Clear upcoming events
  (setq chime--upcoming-events nil))

(defun test-chime-debug-functions-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  (setq chime--upcoming-events nil))

;;; Tests for chime--debug-dump-events

(ert-deftest test-chime-debug-dump-events-normal-with-events ()
  "Test that chime--debug-dump-events dumps events to *Messages* buffer."
  (test-chime-debug-functions-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time))
             (minutes 10))
        (with-test-time now
          ;; Set up chime--upcoming-events as if chime--update-modeline populated it
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Meeting")))
                 (event-item (list event (cons timestamp-str event-time) minutes)))
            (setq chime--upcoming-events (list event-item))
            ;; Clear messages buffer to check output
            (with-current-buffer (get-buffer-create "*Messages*")
              (let ((inhibit-read-only t))
                (erase-buffer)))
            ;; Call debug function
            (chime--debug-dump-events)
            ;; Verify output in *Messages* buffer
            (with-current-buffer "*Messages*"
              (let ((content (buffer-string)))
                (should (string-match-p "=== Chime Debug: Upcoming Events" content))
                (should (string-match-p "Test Meeting" content))
                (should (string-match-p "=== End Chime Debug ===" content)))))))
    (test-chime-debug-functions-teardown)))

(ert-deftest test-chime-debug-dump-events-boundary-no-events ()
  "Test that chime--debug-dump-events handles no events gracefully."
  (test-chime-debug-functions-setup)
  (unwind-protect
      (progn
        ;; Ensure no events
        (setq chime--upcoming-events nil)
        ;; Should not error
        (should-not (condition-case nil
                        (progn (chime--debug-dump-events) nil)
                      (error t))))
    (test-chime-debug-functions-teardown)))

;;; Tests for chime--debug-dump-tooltip

(ert-deftest test-chime-debug-dump-tooltip-normal-with-events ()
  "Test that chime--debug-dump-tooltip dumps tooltip to *Messages* buffer."
  (test-chime-debug-functions-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time))
             (minutes 10))
        (with-test-time now
          ;; Set up chime--upcoming-events
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Test Meeting")))
                 (event-item (list event (cons timestamp-str event-time) minutes)))
            (setq chime--upcoming-events (list event-item))
            ;; Clear messages buffer
            (with-current-buffer (get-buffer-create "*Messages*")
              (let ((inhibit-read-only t))
                (erase-buffer)))
            ;; Call debug function
            (chime--debug-dump-tooltip)
            ;; Verify output in *Messages* buffer
            (with-current-buffer "*Messages*"
              (let ((content (buffer-string)))
                (should (string-match-p "=== Chime Debug: Tooltip Content ===" content))
                (should (string-match-p "Test Meeting" content))
                (should (string-match-p "=== End Chime Debug ===" content)))))))
    (test-chime-debug-functions-teardown)))

(ert-deftest test-chime-debug-dump-tooltip-boundary-no-events ()
  "Test that chime--debug-dump-tooltip handles no events gracefully."
  (test-chime-debug-functions-setup)
  (unwind-protect
      (progn
        ;; Ensure no events
        (setq chime--upcoming-events nil)
        ;; Should not error
        (should-not (condition-case nil
                        (progn (chime--debug-dump-tooltip) nil)
                      (error t))))
    (test-chime-debug-functions-teardown)))

;;; Tests for chime--debug-config

(ert-deftest test-chime-debug-config-normal-dumps-config ()
  "Test that chime--debug-config dumps configuration to *Messages* buffer."
  (test-chime-debug-functions-setup)
  (unwind-protect
      (let ((chime-enable-modeline t)
            (chime-modeline-lookahead-minutes 60)
            (chime-alert-intervals '((10 . medium)))
            (org-agenda-files '("/tmp/test.org")))
        ;; Clear messages buffer
        (with-current-buffer (get-buffer-create "*Messages*")
          (let ((inhibit-read-only t))
            (erase-buffer)))
        ;; Call debug function
        (chime--debug-config)
        ;; Verify output in *Messages* buffer
        (with-current-buffer "*Messages*"
          (let ((content (buffer-string)))
            (should (string-match-p "=== Chime Debug: Configuration ===" content))
            (should (string-match-p "Mode enabled:" content))
            (should (string-match-p "chime-enable-modeline:" content))
            (should (string-match-p "chime-modeline-lookahead-minutes:" content))
            (should (string-match-p "chime-alert-intervals:" content))
            (should (string-match-p "Org agenda files" content))
            (should (string-match-p "=== End Chime Debug ===" content)))))
    (test-chime-debug-functions-teardown)))

(ert-deftest test-chime-debug-config-boundary-no-agenda-files ()
  "Test that chime--debug-config handles empty org-agenda-files."
  (test-chime-debug-functions-setup)
  (unwind-protect
      (let ((org-agenda-files '()))
        ;; Clear messages buffer
        (with-current-buffer (get-buffer-create "*Messages*")
          (let ((inhibit-read-only t))
            (erase-buffer)))
        ;; Should not error
        (should-not (condition-case nil
                        (progn (chime--debug-config) nil)
                      (error t)))
        ;; Verify output mentions 0 files
        (with-current-buffer "*Messages*"
          (let ((content (buffer-string)))
            (should (string-match-p "Org agenda files (0)" content)))))
    (test-chime-debug-functions-teardown)))

;;; Integration tests

(ert-deftest test-chime-debug-all-functions-work-together ()
  "Test that all three debug functions can be called sequentially."
  (test-chime-debug-functions-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time))
             (minutes 10)
             (chime-enable-modeline t)
             (org-agenda-files '("/tmp/test.org")))
        (with-test-time now
          ;; Set up events
          (let* ((event `((times . ((,timestamp-str . ,event-time)))
                          (title . "Integration Test Meeting")))
                 (event-item (list event (cons timestamp-str event-time) minutes)))
            (setq chime--upcoming-events (list event-item))
            ;; Call all three debug functions - should not error
            (should-not (condition-case nil
                            (progn
                              (chime--debug-dump-events)
                              (chime--debug-dump-tooltip)
                              (chime--debug-config)
                              nil)
                          (error t))))))
    (test-chime-debug-functions-teardown)))

(provide 'test-chime-debug-functions)
;;; test-chime-debug-functions.el ends here
