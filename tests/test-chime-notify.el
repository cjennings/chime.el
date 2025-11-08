;;; test-chime-notify.el --- Tests for chime--notify -*- lexical-binding: t; -*-

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

;; Unit tests for chime--notify function.
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

(defun test-chime-notify-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset notification settings
  (setq chime-notification-title "Agenda")
  (setq chime-notification-icon nil)
  (setq chime-extra-alert-plist nil)
  (setq chime-play-sound t)
  ;; Use a simple test path for sound file
  (setq chime-sound-file "/tmp/test-chime.wav"))

(defun test-chime-notify-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir))

;;; Normal Cases

(ert-deftest test-chime-notify-plays-sound-when-enabled-and-file-exists ()
  "Test that sound is played when enabled and file exists."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((sound-played nil)
            (alert-called nil)
            (alert-message nil))
        (cl-letf* ((chime-play-sound t)
                   (chime-sound-file (expand-file-name "test-sound.wav" chime-test-base-dir))
                   ;; Mock file-exists-p to return t
                   ((symbol-function 'file-exists-p) (lambda (file) t))
                   ;; Mock play-sound-file to track if called
                   ((symbol-function 'play-sound-file)
                    (lambda (file)
                      (setq sound-played t)))
                   ;; Mock alert to track if called
                   ((symbol-function 'alert)
                    (lambda (msg &rest args)
                      (setq alert-called t)
                      (setq alert-message msg))))
          (chime--notify "Team Meeting at 02:10 PM")
          ;; Should play sound
          (should sound-played)
          ;; Should show alert
          (should alert-called)
          (should (equal alert-message "Team Meeting at 02:10 PM"))))
    (test-chime-notify-teardown)))

(ert-deftest test-chime-notify-uses-beep-when-no-sound-file-specified ()
  "Test that no sound is played when chime-sound-file is nil."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((beep-called nil)
            (alert-called nil))
        (cl-letf* ((chime-play-sound t)
                   (chime-sound-file nil)
                   ;; Mock beep to track if called
                   ((symbol-function 'beep)
                    (lambda () (setq beep-called t)))
                   ;; Mock alert
                   ((symbol-function 'alert)
                    (lambda (msg &rest args) (setq alert-called t))))
          (chime--notify "Standup in 5 minutes")
          ;; Should NOT call beep (no sound when chime-sound-file is nil)
          (should-not beep-called)
          ;; Should still show alert
          (should alert-called)))
    (test-chime-notify-teardown)))

(ert-deftest test-chime-notify-no-sound-when-disabled ()
  "Test that no sound is played when chime-play-sound is nil."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((sound-played nil)
            (beep-called nil)
            (alert-called nil))
        (cl-letf* ((chime-play-sound nil)
                   ((symbol-function 'play-sound-file)
                    (lambda (file) (setq sound-played t)))
                   ((symbol-function 'beep)
                    (lambda () (setq beep-called t)))
                   ((symbol-function 'alert)
                    (lambda (msg &rest args) (setq alert-called t))))
          (chime--notify "Daily Standup")
          ;; Should NOT play sound or beep
          (should-not sound-played)
          (should-not beep-called)
          ;; Should still show alert
          (should alert-called)))
    (test-chime-notify-teardown)))

(ert-deftest test-chime-notify-passes-correct-parameters-to-alert ()
  "Test that alert is called with correct parameters."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((alert-params nil))
        (cl-letf* ((chime-play-sound nil)
                   (chime-notification-title "Custom Title")
                   (chime-notification-icon "/path/to/icon.png")
                   (chime-extra-alert-plist '(:persistent t))
                   ;; Mock alert to capture parameters
                   ((symbol-function 'alert)
                    (lambda (msg &rest args) (setq alert-params args))))
          ;; Pass cons cell (message . severity) to chime--notify
          (chime--notify (cons "Test Event" 'high))
          ;; Verify alert was called with correct parameters
          (should (equal (plist-get alert-params :title) "Custom Title"))
          (should (equal (plist-get alert-params :icon) "/path/to/icon.png"))
          (should (equal (plist-get alert-params :severity) 'high))
          (should (equal (plist-get alert-params :category) 'chime))
          ;; Extra plist should be merged in
          (should (equal (plist-get alert-params :persistent) t))))
    (test-chime-notify-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-notify-empty-message-still-notifies ()
  "Test that empty message still triggers notification."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((alert-called nil)
            (alert-message nil))
        (cl-letf* ((chime-play-sound nil)
                   ((symbol-function 'alert)
                    (lambda (msg &rest args)
                      (setq alert-called t)
                      (setq alert-message msg))))
          (chime--notify "")
          ;; Should still call alert, even with empty message
          (should alert-called)
          (should (equal alert-message ""))))
    (test-chime-notify-teardown)))

(ert-deftest test-chime-notify-no-sound-file-when-file-doesnt-exist ()
  "Test that no sound is played when file doesn't exist."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((sound-played nil)
            (alert-called nil))
        (cl-letf* ((chime-play-sound t)
                   (chime-sound-file "/nonexistent/path/sound.wav")
                   ;; Mock file-exists-p to return nil
                   ((symbol-function 'file-exists-p) (lambda (file) nil))
                   ((symbol-function 'play-sound-file)
                    (lambda (file) (setq sound-played t)))
                   ((symbol-function 'alert)
                    (lambda (msg &rest args) (setq alert-called t))))
          (chime--notify "Test Event")
          ;; Should NOT play sound
          (should-not sound-played)
          ;; Should still show alert
          (should alert-called)))
    (test-chime-notify-teardown)))

;;; Error Cases

(ert-deftest test-chime-notify-handles-sound-playback-error-gracefully ()
  "Test that errors in sound playback don't prevent notification."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((alert-called nil))
        (cl-letf* ((chime-play-sound t)
                   (chime-sound-file "/some/file.wav")
                   ;; Mock file-exists-p to return t
                   ((symbol-function 'file-exists-p) (lambda (file) t))
                   ;; Mock play-sound-file to throw error
                   ((symbol-function 'play-sound-file)
                    (lambda (file) (error "Sound playback failed")))
                   ((symbol-function 'alert)
                    (lambda (msg &rest args) (setq alert-called t))))
          ;; Should not throw error
          (should-not (condition-case nil
                          (progn (chime--notify "Test Event") nil)
                        (error t)))
          ;; Should still show alert despite sound error
          (should alert-called)))
    (test-chime-notify-teardown)))

(ert-deftest test-chime-notify-handles-beep-error-gracefully ()
  "Test that errors in beep don't prevent notification."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((alert-called nil))
        (cl-letf* ((chime-play-sound t)
                   (chime-sound-file nil)
                   ;; Mock beep to throw error
                   ((symbol-function 'beep)
                    (lambda () (error "Beep failed")))
                   ((symbol-function 'alert)
                    (lambda (msg &rest args) (setq alert-called t))))
          ;; Should not throw error
          (should-not (condition-case nil
                          (progn (chime--notify "Test Event") nil)
                        (error t)))
          ;; Should still show alert despite beep error
          (should alert-called)))
    (test-chime-notify-teardown)))

(ert-deftest test-chime-notify-error-nil-message-handles-gracefully ()
  "Test that nil message parameter doesn't crash."
  (test-chime-notify-setup)
  (unwind-protect
      (let ((alert-called nil))
        (cl-letf* ((chime-play-sound nil)
                   ((symbol-function 'alert)
                    (lambda (msg &rest args) (setq alert-called t))))
          ;; Should not error with nil message
          (should-not (condition-case nil
                          (progn (chime--notify nil) nil)
                        (error t)))
          ;; Alert should still be called
          (should alert-called)))
    (test-chime-notify-teardown)))

(provide 'test-chime-notify)
;;; test-chime-notify.el ends here
