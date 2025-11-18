;;; test-chime-validation-retry.el --- Tests for chime validation retry logic -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for chime's configuration validation retry mechanism.
;;
;; Tests cover the graceful retry behavior when org-agenda-files is not
;; immediately available (e.g., loaded asynchronously via idle timer).
;;
;; The retry mechanism allows chime to wait for org-agenda-files to be
;; populated before showing configuration errors, providing a better UX
;; for users with async initialization code.
;;
;; Components tested:
;; - chime--validation-retry-count tracking
;; - chime-validation-max-retries configuration
;; - chime-check validation retry logic
;; - chime--stop retry counter reset
;; - Message display behavior (waiting vs error)

;;; Code:

(require 'ert)

;; Initialize package system to make installed packages available in batch mode
(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

;; Load chime from parent directory (which will load its dependencies)
(load (expand-file-name "../chime.el") nil t)

;;; Setup and Teardown

(defvar test-chime-validation-retry--original-max-retries nil
  "Original value of chime-validation-max-retries for restoration.")

(defvar test-chime-validation-retry--original-agenda-files nil
  "Original value of org-agenda-files for restoration.")

(defun test-chime-validation-retry-setup ()
  "Set up test environment before each test."
  ;; Save original values
  (setq test-chime-validation-retry--original-max-retries chime-validation-max-retries)
  (setq test-chime-validation-retry--original-agenda-files org-agenda-files)

  ;; Reset validation state
  (setq chime--validation-done nil)
  (setq chime--validation-retry-count 0)

  ;; Set predictable defaults
  (setq chime-validation-max-retries 3))

(defun test-chime-validation-retry-teardown ()
  "Clean up test environment after each test."
  ;; Restore original values
  (setq chime-validation-max-retries test-chime-validation-retry--original-max-retries)
  (setq org-agenda-files test-chime-validation-retry--original-agenda-files)

  ;; Reset validation state
  (setq chime--validation-done nil)
  (setq chime--validation-retry-count 0))

;;; Normal Cases - Retry Behavior

(ert-deftest test-chime-validation-retry-normal-first-failure-shows-waiting ()
  "Test first validation failure shows waiting message, not error.

When org-agenda-files is empty on the first check, chime should show
a friendly waiting message instead of immediately displaying the full
error. This accommodates async org-agenda-files initialization."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Empty org-agenda-files to trigger validation failure
        (setq org-agenda-files nil)

        ;; Capture message output
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ;; Mock fetch to prevent actual agenda processing
                    ((symbol-function 'chime--fetch-and-process)
                     (lambda (callback) nil)))

            ;; Call chime-check
            (chime-check)

            ;; Should show waiting message
            (should (= chime--validation-retry-count 1))
            (should-not chime--validation-done)
            (should (cl-some (lambda (msg)
                              (string-match-p "Waiting for org-agenda-files" msg))
                            messages))
            ;; Should NOT show error message
            (should-not (cl-some (lambda (msg)
                                  (string-match-p "Configuration errors detected" msg))
                                messages)))))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-normal-success-resets-counter ()
  "Test successful validation after retry resets counter to zero.

When validation succeeds on a retry attempt, the retry counter should
be reset to 0, allowing fresh retry attempts if validation fails again
later (e.g., after mode restart)."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Simulate one failed attempt
        (setq chime--validation-retry-count 1)

        ;; Set valid org-agenda-files
        (setq org-agenda-files '("/tmp/test.org"))

        ;; Mock fetch to prevent actual agenda processing
        (cl-letf (((symbol-function 'chime--fetch-and-process)
                   (lambda (callback) nil)))

          ;; Call chime-check - should succeed
          (chime-check)

          ;; Counter should be reset
          (should (= chime--validation-retry-count 0))
          ;; Validation marked as done
          (should chime--validation-done)))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-normal-multiple-retries-increment ()
  "Test multiple validation failures increment counter correctly.

Each validation failure should increment the retry counter by 1,
allowing the system to track how many retries have been attempted."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Empty org-agenda-files
        (setq org-agenda-files nil)

        ;; Mock fetch
        (cl-letf (((symbol-function 'chime--fetch-and-process)
                   (lambda (callback) nil))
                  ((symbol-function 'message)
                   (lambda (&rest args) nil)))

          ;; First attempt
          (chime-check)
          (should (= chime--validation-retry-count 1))

          ;; Second attempt
          (chime-check)
          (should (= chime--validation-retry-count 2))

          ;; Third attempt
          (chime-check)
          (should (= chime--validation-retry-count 3))))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-normal-successful-validation-proceeds ()
  "Test successful validation proceeds with event checking.

When validation passes, chime-check should proceed to fetch and
process events normally."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Valid org-agenda-files
        (setq org-agenda-files '("/tmp/test.org"))

        ;; Track if fetch was called
        (let ((fetch-called nil))
          (cl-letf (((symbol-function 'chime--fetch-and-process)
                     (lambda (callback)
                       (setq fetch-called t))))

            ;; Call chime-check
            (chime-check)

            ;; Should proceed to fetch
            (should fetch-called)
            (should chime--validation-done)
            (should (= chime--validation-retry-count 0)))))
    (test-chime-validation-retry-teardown)))

;;; Boundary Cases - Edge Conditions

(ert-deftest test-chime-validation-retry-boundary-max-retries-zero ()
  "Test max-retries=0 shows error immediately without retrying.

When chime-validation-max-retries is set to 0, validation failures
should immediately show the full error message without any retry
attempts."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Set max retries to 0
        (setq chime-validation-max-retries 0)

        ;; Empty org-agenda-files
        (setq org-agenda-files nil)

        ;; Capture message output
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'chime--fetch-and-process)
                     (lambda (callback) nil)))

            ;; Call chime-check
            (chime-check)

            ;; Counter incremented
            (should (= chime--validation-retry-count 1))
            ;; Should show error, not waiting message
            (should (cl-some (lambda (msg)
                              (string-match-p "Configuration errors detected" msg))
                            messages))
            (should-not (cl-some (lambda (msg)
                                  (string-match-p "Waiting for" msg))
                                messages)))))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-boundary-max-retries-one ()
  "Test max-retries=1 allows one retry before showing error.

First attempt should show waiting message, second attempt should
show full error."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Set max retries to 1
        (setq chime-validation-max-retries 1)

        ;; Empty org-agenda-files
        (setq org-agenda-files nil)

        (cl-letf (((symbol-function 'chime--fetch-and-process)
                   (lambda (callback) nil)))

          ;; First attempt - should show waiting
          (let ((messages nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (chime-check)
              (should (= chime--validation-retry-count 1))
              (should (cl-some (lambda (msg)
                                (string-match-p "Waiting for" msg))
                              messages))))

          ;; Second attempt - should show error
          (let ((messages nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (chime-check)
              (should (= chime--validation-retry-count 2))
              (should (cl-some (lambda (msg)
                                (string-match-p "Configuration errors detected" msg))
                              messages))))))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-boundary-exactly-at-threshold ()
  "Test behavior exactly at max-retries threshold.

The (retry_count + 1)th attempt should show the error message."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Default max retries = 3
        (setq chime-validation-max-retries 3)
        (setq org-agenda-files nil)

        (cl-letf (((symbol-function 'chime--fetch-and-process)
                   (lambda (callback) nil)))

          ;; Attempts 1-3: waiting messages
          (dotimes (_ 3)
            (let ((messages nil))
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args) messages))))
                (chime-check)
                (should (cl-some (lambda (msg)
                                  (string-match-p "Waiting for" msg))
                                messages)))))

          ;; Attempt 4: should show error
          (let ((messages nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (chime-check)
              (should (= chime--validation-retry-count 4))
              (should (cl-some (lambda (msg)
                                (string-match-p "Configuration errors detected" msg))
                              messages))))))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-boundary-stop-resets-counter ()
  "Test chime--stop resets retry counter to zero.

When chime-mode is stopped, the retry counter should be reset to
allow fresh retry attempts on next start."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Simulate some failed attempts
        (setq chime--validation-retry-count 5)
        (setq chime--validation-done nil)

        ;; Call stop
        (chime--stop)

        ;; Counter should be reset
        (should (= chime--validation-retry-count 0))
        (should-not chime--validation-done))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-boundary-empty-agenda-files ()
  "Test empty org-agenda-files list triggers retry.

An empty list should be treated the same as nil - both should
trigger validation failure and retry."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        ;; Empty list (not nil)
        (setq org-agenda-files '())

        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'chime--fetch-and-process)
                     (lambda (callback) nil)))

            ;; Should trigger retry
            (chime-check)
            (should (= chime--validation-retry-count 1))
            (should (cl-some (lambda (msg)
                              (string-match-p "Waiting for" msg))
                            messages)))))
    (test-chime-validation-retry-teardown)))

;;; Error Cases - Failure Scenarios

(ert-deftest test-chime-validation-retry-error-exceeding-max-shows-full-error ()
  "Test exceeding max retries shows full error with details.

After max retries exceeded, the full validation error should be
displayed with all error details in the *Messages* buffer."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        (setq chime-validation-max-retries 2)
        (setq org-agenda-files nil)

        (cl-letf (((symbol-function 'chime--fetch-and-process)
                   (lambda (callback) nil)))

          ;; Exhaust retries
          (dotimes (_ 3)
            (let ((messages nil))
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (push (apply #'format format-string args) messages))))
                (chime-check))))

          ;; Verify error message on next attempt
          (let ((messages nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (chime-check)
              ;; Should show error message (detailed error with retry count goes to *Messages* buffer via chime--log-silently)
              (should (cl-some (lambda (msg)
                                (string-match-p "Configuration errors detected" msg))
                              messages))))))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-error-persistent-failure ()
  "Test validation failure persisting through all retries.

If org-agenda-files remains empty through all retry attempts,
validation should never be marked as done."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        (setq chime-validation-max-retries 3)
        (setq org-agenda-files nil)

        (cl-letf (((symbol-function 'chime--fetch-and-process)
                   (lambda (callback) nil))
                  ((symbol-function 'message)
                   (lambda (&rest args) nil)))

          ;; Multiple attempts, all failing
          (dotimes (_ 10)
            (chime-check)
            ;; Should never mark as done
            (should-not chime--validation-done))

          ;; Counter keeps incrementing
          (should (= chime--validation-retry-count 10))))
    (test-chime-validation-retry-teardown)))

(ert-deftest test-chime-validation-retry-error-counter-large-value ()
  "Test retry counter handles large values without overflow.

The retry counter should continue incrementing correctly even with
many retry attempts, ensuring no integer overflow issues."
  (test-chime-validation-retry-setup)
  (unwind-protect
      (progn
        (setq chime-validation-max-retries 1000)
        (setq org-agenda-files nil)

        (cl-letf (((symbol-function 'chime--fetch-and-process)
                   (lambda (callback) nil))
                  ((symbol-function 'message)
                   (lambda (&rest args) nil)))

          ;; Many attempts
          (dotimes (i 100)
            (chime-check)
            (should (= chime--validation-retry-count (1+ i))))

          ;; Should still be counting correctly
          (should (= chime--validation-retry-count 100))))
    (test-chime-validation-retry-teardown)))

(provide 'test-chime-validation-retry)
;;; test-chime-validation-retry.el ends here
