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
;; Tests use real org-mode buffers with real org syntax.
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
        (insert "* DONE Task 2\n")
        (insert "* TODO Task 3\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker))
                  (chime-keyword-whitelist '("TODO")))
              (let ((result (chime--apply-whitelist (list marker1 marker2 marker3))))
                ;; Should only keep TODO markers
                (should (= (length result) 2))
                (should (member marker1 result))
                (should-not (member marker2 result))
                (should (member marker3 result)))))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-tags-whitelist-filters-correctly ()
  "Test that tags whitelist filters markers correctly."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Task 1                                            :urgent:\n")
        (insert "* Task 2                                             :normal:\n")
        (insert "* Task 3                                            :urgent:\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker))
                  (chime-tags-whitelist '("urgent")))
              (let ((result (chime--apply-whitelist (list marker1 marker2 marker3))))
                ;; Should only keep markers with "urgent" tag
                (should (= (length result) 2))
                (should (member marker1 result))
                (should-not (member marker2 result))
                (should (member marker3 result)))))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-keyword-and-tags-whitelist-uses-or-logic ()
  "Test that both keyword and tags whitelists use OR logic."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (insert "* DONE Task 2\n")
        (insert "* NEXT Task 3                                        :urgent:\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker))
                  (chime-keyword-whitelist '("TODO"))
                  (chime-tags-whitelist '("urgent")))
              (let ((result (chime--apply-whitelist (list marker1 marker2 marker3))))
                ;; Should keep marker1 (TODO) and marker3 (urgent tag)
                (should (= (length result) 2))
                (should (member marker1 result))
                (should-not (member marker2 result))
                (should (member marker3 result)))))))
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
        (insert "* DONE Task 2\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker))
                (chime-keyword-whitelist '("TODO")))
            (let ((result (chime--apply-whitelist (list marker1 marker2))))
              (should (= (length result) 1))
              (should (member marker1 result))))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-no-matching-markers-returns-empty ()
  "Test that whitelist with no matching markers returns empty list."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* DONE Task 1\n")
        (insert "* DONE Task 2\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker))
                (chime-keyword-whitelist '("TODO")))
            (let ((result (chime--apply-whitelist (list marker1 marker2))))
              (should (equal result '()))))))
    (test-chime-apply-whitelist-teardown)))

;;; Error Cases

(ert-deftest test-chime-apply-whitelist-handles-nil-keyword-gracefully ()
  "Test that nil keyword in marker is handled gracefully."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Entry without TODO keyword\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker))
              (chime-keyword-whitelist '("TODO")))
          (let ((result (chime--apply-whitelist (list marker1))))
            ;; Should filter out marker with nil keyword (not in whitelist)
            (should (= (length result) 0)))))
    (test-chime-apply-whitelist-teardown)))

(ert-deftest test-chime-apply-whitelist-handles-nil-tags-gracefully ()
  "Test that nil tags in marker is handled gracefully."
  (test-chime-apply-whitelist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Entry without tags\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker))
              (chime-tags-whitelist '("urgent")))
          (let ((result (chime--apply-whitelist (list marker1))))
            ;; Should filter out marker with nil tags (not in whitelist)
            (should (= (length result) 0)))))
    (test-chime-apply-whitelist-teardown)))

(provide 'test-chime-apply-whitelist)
;;; test-chime-apply-whitelist.el ends here
