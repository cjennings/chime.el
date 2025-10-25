;;; test-chime-apply-whitelist.el --- Tests for chime--apply-whitelist -*- lexical-binding: t; -*-

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

;; Unit tests for chime--apply-whitelist function.
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

(defun test-chime-apply-whitelist-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset whitelist settings
  (setq chime-keyword-whitelist nil)
  (setq chime-tags-whitelist nil)
  (setq chime-predicate-whitelist nil))

(defun test-chime-apply-whitelist-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  (setq chime-keyword-whitelist nil)
  (setq chime-tags-whitelist nil)
  (setq chime-predicate-whitelist nil))

(defun test-chime-create-org-marker (keyword tags)
  "Create a marker pointing to an org entry with KEYWORD and TAGS.
Returns a marker with mocked org-entry-get and chime--get-tags."
  (with-temp-buffer
    (org-mode)
    (insert (format "* %s Test Entry\n" (or keyword "")))
    (let ((marker (point-marker)))
      ;; Mock org-entry-get to return the keyword
      (cl-letf (((symbol-function 'org-entry-get)
                 (lambda (pom property &optional inherit literal-nil)
                   (when (and (equal pom marker) (equal property "TODO"))
                     keyword)))
                ((symbol-function 'chime--get-tags)
                 (lambda (pom)
                   (when (equal pom marker)
                     tags))))
        marker))))

;;; Normal Cases

(ert-deftest test-chime-apply-whitelist-nil-whitelist-returns-all-markers ()
  "Test that nil whitelist returns all markers unchanged."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (let* ((chime-keyword-whitelist nil)
             (chime-tags-whitelist nil)
             (markers (list (make-marker) (make-marker) (make-marker)))
             (result (chime--apply-whitelist markers)))
        ;; Should return all markers when whitelist is nil
        (should (equal (length result) 3)))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-keyword-whitelist-filters-correctly ()
  "Test that keyword whitelist filters markers correctly."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (let ((marker1 (copy-marker (point))))
          (insert "* DONE Task 2\n")
          (let ((marker2 (copy-marker (point))))
            (insert "* TODO Task 3\n")
            (let ((marker3 (copy-marker (point)))
                  (chime-keyword-whitelist '("TODO")))
              ;; Mock org-entry-get to return appropriate keywords
              (cl-letf (((symbol-function 'org-entry-get)
                         (lambda (pom property &optional inherit literal-nil)
                           (when (equal property "TODO")
                             (cond
                              ((equal pom marker1) "TODO")
                              ((equal pom marker2) "DONE")
                              ((equal pom marker3) "TODO")
                              (t nil))))))
                (let ((result (chime--apply-whitelist (list marker1 marker2 marker3))))
                  ;; Should only return TODO markers
                  (should (= (length result) 2))
                  (should (member marker1 result))
                  (should-not (member marker2 result))
                  (should (member marker3 result))))))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-tags-whitelist-filters-correctly ()
  "Test that tags whitelist filters markers correctly."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Task 1\n")
        (let ((marker1 (copy-marker (point))))
          (insert "* Task 2\n")
          (let ((marker2 (copy-marker (point))))
            (insert "* Task 3\n")
            (let ((marker3 (copy-marker (point)))
                  (chime-tags-whitelist '("work")))
              ;; Mock chime--get-tags to return appropriate tags
              (cl-letf (((symbol-function 'chime--get-tags)
                         (lambda (pom)
                           (cond
                            ((equal pom marker1) '("work" "urgent"))
                            ((equal pom marker2) '("personal"))
                            ((equal pom marker3) '("work"))
                            (t nil)))))
                (let ((result (chime--apply-whitelist (list marker1 marker2 marker3))))
                  ;; Should only return markers with "work" tag
                  (should (= (length result) 2))
                  (should (member marker1 result))
                  (should-not (member marker2 result))
                  (should (member marker3 result))))))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-keyword-and-tags-whitelist-uses-or-logic ()
  "Test that both keyword and tags whitelists use OR logic."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (let ((marker1 (copy-marker (point))))
          (insert "* DONE Task 2\n")
          (let ((marker2 (copy-marker (point))))
            (insert "* NEXT Task 3\n")
            (let ((marker3 (copy-marker (point)))
                  (chime-keyword-whitelist '("TODO"))
                  (chime-tags-whitelist '("urgent")))
              ;; Mock functions
              (cl-letf (((symbol-function 'org-entry-get)
                         (lambda (pom property &optional inherit literal-nil)
                           (when (equal property "TODO")
                             (cond
                              ((equal pom marker1) "TODO")
                              ((equal pom marker2) "DONE")
                              ((equal pom marker3) "NEXT")
                              (t nil)))))
                        ((symbol-function 'chime--get-tags)
                         (lambda (pom)
                           (cond
                            ((equal pom marker1) nil)
                            ((equal pom marker2) '("urgent"))
                            ((equal pom marker3) nil)
                            (t nil)))))
                (let ((result (chime--apply-whitelist (list marker1 marker2 marker3))))
                  ;; Should return marker1 (TODO keyword) and marker2 (urgent tag)
                  (should (= (length result) 2))
                  (should (member marker1 result))
                  (should (member marker2 result))
                  (should-not (member marker3 result))))))))
    (test-chime-apply-whitelist-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-apply-whitelist-empty-markers-list-returns-empty ()
  "Test that empty markers list returns empty."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (let ((chime-keyword-whitelist '("TODO"))
            (result (chime--apply-whitelist '())))
        (should (equal result '())))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-single-item-whitelist-works ()
  "Test that single-item whitelist works correctly."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (let ((marker1 (copy-marker (point))))
          (insert "* DONE Task 2\n")
          (let ((marker2 (copy-marker (point)))
                (chime-keyword-whitelist '("TODO")))
            (cl-letf (((symbol-function 'org-entry-get)
                       (lambda (pom property &optional inherit literal-nil)
                         (when (equal property "TODO")
                           (cond
                            ((equal pom marker1) "TODO")
                            ((equal pom marker2) "DONE")
                            (t nil))))))
              (let ((result (chime--apply-whitelist (list marker1 marker2))))
                (should (= (length result) 1))
                (should (member marker1 result)))))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-no-matching-markers-returns-empty ()
  "Test that no matching markers returns empty list."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* DONE Task 1\n")
        (let ((marker1 (copy-marker (point))))
          (insert "* CANCELLED Task 2\n")
          (let ((marker2 (copy-marker (point)))
                (chime-keyword-whitelist '("TODO")))
            (cl-letf (((symbol-function 'org-entry-get)
                       (lambda (pom property &optional inherit literal-nil)
                         (when (equal property "TODO")
                           (cond
                            ((equal pom marker1) "DONE")
                            ((equal pom marker2) "CANCELLED")
                            (t nil))))))
              (let ((result (chime--apply-whitelist (list marker1 marker2))))
                (should (equal result '())))))))
    (test-chime-apply-whitelist-teardown)))

;;; Error Cases

(ert-deftest test-chime-apply-whitelist-handles-nil-keyword-gracefully ()
  "Test that nil keyword in marker is handled gracefully."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Entry without TODO keyword\n")
        (let ((marker1 (copy-marker (point)))
              (chime-keyword-whitelist '("TODO")))
          (cl-letf (((symbol-function 'org-entry-get)
                     (lambda (pom property &optional inherit literal-nil)
                       nil)))
            (let ((result (chime--apply-whitelist (list marker1))))
              ;; Should filter out marker with nil keyword
              (should (equal result '()))))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-handles-nil-tags-gracefully ()
  "Test that nil tags in marker is handled gracefully."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Entry without tags\n")
        (let ((marker1 (copy-marker (point)))
              (chime-tags-whitelist '("work")))
          (cl-letf (((symbol-function 'chime--get-tags)
                     (lambda (pom) nil)))
            (let ((result (chime--apply-whitelist (list marker1))))
              ;; Should filter out marker with nil tags
              (should (equal result '()))))))
    (test-chime-apply-whitelist-teardown)))

(provide 'test-chime-apply-whitelist)
;;; test-chime-apply-whitelist.el ends here
