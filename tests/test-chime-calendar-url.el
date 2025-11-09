;;; test-chime-calendar-url.el --- Tests for calendar URL feature -*- lexical-binding: t; -*-

;;; Code:

;; Initialize package system for batch mode
(when noninteractive
  (package-initialize))

(require 'ert)
(require 'dash)
(require 'alert)

;; Load chime from parent directory
(load (expand-file-name "../chime.el") nil t)

;;; Tests for chime--open-calendar-url

(ert-deftest test-chime-open-calendar-url-opens-when-set ()
  "Test that chime--open-calendar-url calls browse-url when URL is set."
  (let ((chime-calendar-url "https://calendar.google.com")
        (url-opened nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq url-opened url))))
      (chime--open-calendar-url)
      (should (equal url-opened "https://calendar.google.com")))))

(ert-deftest test-chime-open-calendar-url-does-nothing-when-nil ()
  "Test that chime--open-calendar-url does nothing when URL is nil."
  (let ((chime-calendar-url nil)
        (browse-url-called nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (_url) (setq browse-url-called t))))
      (chime--open-calendar-url)
      (should-not browse-url-called))))

;;; Tests for chime--jump-to-first-event

(ert-deftest test-chime-jump-to-first-event-jumps-to-event ()
  "Test that chime--jump-to-first-event jumps to first event in list."
  (let* ((event1 '((title . "Event 1")
                   (marker-file . "/tmp/test.org")
                   (marker-pos . 100)))
         (event2 '((title . "Event 2")
                   (marker-file . "/tmp/test.org")
                   (marker-pos . 200)))
         (chime--upcoming-events `((,event1 ("time1" . time1) 10)
                                   (,event2 ("time2" . time2) 20)))
         (jumped-to-event nil))
    (cl-letf (((symbol-function 'chime--jump-to-event)
               (lambda (event) (setq jumped-to-event event))))
      (chime--jump-to-first-event)
      (should (equal jumped-to-event event1)))))

(ert-deftest test-chime-jump-to-first-event-does-nothing-when-empty ()
  "Test that chime--jump-to-first-event does nothing when no events."
  (let ((chime--upcoming-events nil)
        (jump-called nil))
    (cl-letf (((symbol-function 'chime--jump-to-event)
               (lambda (_event) (setq jump-called t))))
      (chime--jump-to-first-event)
      (should-not jump-called))))

(provide 'test-chime-calendar-url)
;;; test-chime-calendar-url.el ends here
