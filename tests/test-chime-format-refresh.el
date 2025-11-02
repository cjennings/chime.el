;;; test-chime-format-refresh.el --- Test format changes are picked up on refresh -*- lexical-binding: t; -*-

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

;; Tests to verify that changing configuration variables and calling
;; refresh functions picks up the new values.

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

(defun test-chime-format-refresh-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset to defaults
  (setq chime-modeline-string nil)
  (setq chime-enable-modeline t)
  (setq chime-modeline-lookahead-minutes 30)
  (setq chime-modeline-format " ⏰ %s")
  (setq chime-notification-text-format "%t at %T (%u)"))

(defun test-chime-format-refresh-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  (setq chime-modeline-string nil))

;;; Tests

(ert-deftest test-chime-format-refresh-update-modeline-picks-up-format-change ()
  "Test that chime--update-modeline picks up changed chime-notification-text-format."
  (test-chime-format-refresh-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Meeting")))
                   (events (list event)))
              ;; First update with format "%t at %T (%u)"
              (chime--update-modeline events)
              (should (string-match-p "Meeting at" chime-modeline-string))
              (should (string-match-p "(in" chime-modeline-string))

              ;; Change format to "%t %u" (no time, no parentheses)
              (setq chime-notification-text-format "%t %u")

              ;; Update again - should pick up new format
              (chime--update-modeline events)
              (should (string-match-p "Meeting in" chime-modeline-string))
              (should-not (string-match-p "Meeting at" chime-modeline-string))
              (should-not (string-match-p "(in" chime-modeline-string))))))
    (test-chime-format-refresh-teardown)))

(ert-deftest test-chime-format-refresh-title-only-format ()
  "Test changing format to title-only."
  (test-chime-format-refresh-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Important Meeting")))
                   (events (list event)))
              ;; Start with default format
              (chime--update-modeline events)
              (should (string-match-p "Important Meeting" chime-modeline-string))
              (should (string-match-p "at\\|in" chime-modeline-string))

              ;; Change to title only
              (setq chime-notification-text-format "%t")

              ;; Update - should show title only
              (chime--update-modeline events)
              ;; The modeline string will have the format applied, then wrapped with chime-modeline-format
              ;; chime-modeline-format is " ⏰ %s", so the title will be in there
              (should (string-match-p "Important Meeting" chime-modeline-string))
              ;; After format-spec, the raw text is just "Important Meeting"
              ;; Wrapped with " ⏰ %s" it becomes " ⏰ Important Meeting"
              ;; So we should NOT see time or countdown
              (should-not (string-match-p "at [0-9]" chime-modeline-string))
              (should-not (string-match-p "in [0-9]" chime-modeline-string))))))
    (test-chime-format-refresh-teardown)))

(ert-deftest test-chime-format-refresh-custom-separator ()
  "Test changing format with custom separator."
  (test-chime-format-refresh-setup)
  (unwind-protect
      (let* ((now (test-time-today-at 14 0))
             (event-time (test-time-today-at 14 10))
             (timestamp-str (test-timestamp-string event-time)))
        (with-test-time now
          (cl-letf (((symbol-function 'force-mode-line-update) (lambda (&optional _))))
            (let* ((event `((times . ((,timestamp-str . ,event-time)))
                            (title . "Review PR")))
                   (events (list event)))
              ;; Start with default "at"
              (chime--update-modeline events)
              (should (string-match-p "at" chime-modeline-string))

              ;; Change to custom separator " - "
              (setq chime-notification-text-format "%t - %T")

              ;; Update - should show custom separator
              (chime--update-modeline events)
              (should (string-match-p "Review PR - " chime-modeline-string))
              (should-not (string-match-p " at " chime-modeline-string))))))
    (test-chime-format-refresh-teardown)))

(provide 'test-chime-format-refresh)
;;; test-chime-format-refresh.el ends here
