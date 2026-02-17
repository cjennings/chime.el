;;; test-chime-validate-configuration.el --- Tests for chime-validate-configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Craig Jennings

;;; Commentary:

;; Unit tests for chime-validate-configuration function.
;; Tests validation of chime's runtime environment and configuration.
;;
;; Test categories:
;; - Normal Cases: Valid configurations that should pass
;; - Boundary Cases: Edge conditions (single file, empty strings, etc.)
;; - Error Cases: Invalid configurations that should fail
;;
;; External dependencies mocked:
;; - file-exists-p (file I/O)
;; - require (package loading)
;; - display-warning (UI side effect)
;;
;; NOT mocked:
;; - Validation logic itself
;; - org-agenda-files variable (use let to set test values)

;;; Code:

(when noninteractive
  (package-initialize))

(require 'ert)
(require 'dash)
(require 'org-agenda)
(load (expand-file-name "../chime.el") nil t)
(require 'cl-lib)

;;; Setup and Teardown

(defun test-chime-validate-configuration-setup ()
  "Set up test environment before each test."
  ;; No persistent state to set up - each test uses let-bindings
  nil)

(defun test-chime-validate-configuration-teardown ()
  "Clean up test environment after each test."
  ;; No cleanup needed - let-bindings automatically unwind
  nil)

;;; Test Helper Functions

(defun test-chime-validate-configuration--has-error-p (issues)
  "Return t if ISSUES contains at least one :error severity item."
  (cl-some (lambda (issue) (eq (car issue) :error)) issues))

(defun test-chime-validate-configuration--has-warning-p (issues)
  "Return t if ISSUES contains at least one :warning severity item."
  (cl-some (lambda (issue) (eq (car issue) :warning)) issues))

(defun test-chime-validate-configuration--count-issues (issues severity)
  "Count number of ISSUES with given SEVERITY (:error, :warning, or :info)."
  (length (cl-remove-if-not (lambda (i) (eq (car i) severity)) issues)))

;;; Normal Cases - Valid Configurations

(ert-deftest test-chime-validate-configuration-normal-valid-config-returns-nil ()
  "Test validation passes with valid org-agenda-files and all dependencies loaded."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/tmp/inbox.org" "/tmp/work.org"))
        (chime-enable-modeline t)
        (global-mode-string '("")))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (should (null (chime-validate-configuration)))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-normal-multiple-files-returns-nil ()
  "Test validation passes with multiple org-agenda files that all exist."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/tmp/one.org" "/tmp/two.org" "/tmp/three.org"))
        (chime-enable-modeline t)
        (global-mode-string '("")))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (should (null (chime-validate-configuration)))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-normal-modeline-disabled-skips-check ()
  "Test validation skips global-mode-string check when modeline is disabled."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/tmp/inbox.org"))
        (chime-enable-modeline nil))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'display-warning) (lambda (&rest _) nil))
              ((symbol-function 'boundp)
               (lambda (sym) (not (eq sym 'global-mode-string))))) ; Only global-mode-string unbound
      (should (null (chime-validate-configuration)))))
  (test-chime-validate-configuration-teardown))

;;; Boundary Cases - Edge Conditions

(ert-deftest test-chime-validate-configuration-boundary-single-file-returns-nil ()
  "Test validation passes with exactly one org-agenda file."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/tmp/single.org"))
        (chime-enable-modeline t)
        (global-mode-string '("")))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (should (null (chime-validate-configuration)))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-boundary-some-files-missing-returns-warning ()
  "Test validation warns when some but not all files are missing."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/exists.org" "/missing.org" "/also-missing.org"))
        (chime-enable-modeline t)
        (global-mode-string '("")))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (f) (string= f "/exists.org")))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should (= 1 (length issues)))
        (should (eq :warning (caar issues)))
        (should (string-match-p "2 org-agenda-files don't exist" (cadar issues)))
        (should (string-match-p "/missing.org" (cadar issues)))
        (should (string-match-p "/also-missing.org" (cadar issues))))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-boundary-all-files-missing-returns-warning ()
  "Test validation warns when all org-agenda files are missing."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/missing1.org" "/missing2.org"))
        (chime-enable-modeline t)
        (global-mode-string '("")))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should (= 1 (length issues)))
        (should (eq :warning (caar issues)))
        (should (string-match-p "2 org-agenda-files don't exist" (cadar issues))))))
  (test-chime-validate-configuration-teardown))

;;; Error Cases - Invalid Configurations

(ert-deftest test-chime-validate-configuration-error-nil-org-agenda-files-returns-error ()
  "Test validation returns error when org-agenda-files is nil."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files nil)
        (chime-enable-modeline t))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should issues)
        (should (test-chime-validate-configuration--has-error-p issues))
        (should (string-match-p "not set or empty" (cadar issues))))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-error-empty-list-returns-error ()
  "Test validation returns error when org-agenda-files is empty list."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '())
        (chime-enable-modeline t))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should issues)
        (should (eq :error (caar issues)))
        (should (string-match-p "not set or empty" (cadar issues))))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-error-unbound-org-agenda-files-returns-error ()
  "Test validation returns error when org-agenda-files variable is not bound."
  (test-chime-validate-configuration-setup)
  (cl-letf (((symbol-function 'boundp)
             (lambda (sym) (not (eq sym 'org-agenda-files))))
            ((symbol-function 'display-warning) (lambda (&rest _) nil)))
    (let ((issues (chime-validate-configuration)))
      (should issues)
      (should (test-chime-validate-configuration--has-error-p issues))
      (should (string-match-p "not set or empty" (cadar issues)))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-error-non-list-org-agenda-files-returns-error ()
  "Test validation returns error when org-agenda-files is not a list."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files "/tmp/inbox.org") ; string instead of list
        (chime-enable-modeline t))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should issues)
        (should (test-chime-validate-configuration--has-error-p issues)))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-error-org-agenda-not-loadable-returns-error ()
  "Test validation returns error when org-agenda cannot be loaded."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/tmp/test.org"))
        (chime-enable-modeline t)
        (global-mode-string '("")))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'require)
               (lambda (feature &optional _ _)
                 (if (eq feature 'org-agenda) nil t)))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should issues)
        (should (cl-some (lambda (i)
                          (and (eq (car i) :error)
                               (string-match-p "org-agenda" (cadr i))))
                        issues)))))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-error-multiple-errors-returns-all ()
  "Test validation returns all errors when multiple issues exist."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files nil) ; Error 1: nil org-agenda-files
        (chime-enable-modeline t))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional _ _)
                 (if (eq feature 'org-agenda) nil t))) ; Error 2: can't load org-agenda
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should (>= (test-chime-validate-configuration--count-issues issues :error) 2)))))
  (test-chime-validate-configuration-teardown))

;;; Warning Cases - Non-Critical Issues

(ert-deftest test-chime-validate-configuration-warning-missing-global-mode-string-returns-warning ()
  "Test validation warns when global-mode-string is not available but modeline is enabled."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/tmp/inbox.org"))
        (chime-enable-modeline t))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'boundp)
               (lambda (sym) (not (eq sym 'global-mode-string))))
              ((symbol-function 'display-warning) (lambda (&rest _) nil)))
      (let ((issues (chime-validate-configuration)))
        (should (test-chime-validate-configuration--has-warning-p issues))
        (should (string-match-p "global-mode-string not available" (cadar issues))))))
  (test-chime-validate-configuration-teardown))

;;; Interactive Behavior Tests

(ert-deftest test-chime-validate-configuration-interactive-calls-display-warning ()
  "Test validation displays warnings when called interactively."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files nil)
        (warning-called nil)
        (chime-enable-modeline t))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warning-called t)))
              ((symbol-function 'called-interactively-p) (lambda (_) t)))
      (chime-validate-configuration)
      (should warning-called)))
  (test-chime-validate-configuration-teardown))

(ert-deftest test-chime-validate-configuration-interactive-success-shows-message ()
  "Test validation shows success message when called interactively with valid config."
  (test-chime-validate-configuration-setup)
  (let ((org-agenda-files '("/tmp/inbox.org"))
        (message-shown nil)
        (chime-enable-modeline t)
        (global-mode-string '("")))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'require) (lambda (_ &optional _ _) t))
              ((symbol-function 'message)
               (lambda (fmt &rest _)
                 (when (string-match-p "validation checks passed" fmt)
                   (setq message-shown t))))
              ((symbol-function 'called-interactively-p) (lambda (_) t)))
      (chime-validate-configuration)
      (should message-shown)))
  (test-chime-validate-configuration-teardown))

(provide 'test-chime-validate-configuration)
;;; test-chime-validate-configuration.el ends here
