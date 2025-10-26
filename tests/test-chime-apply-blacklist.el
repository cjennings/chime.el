;;; test-chime-apply-blacklist.el --- Tests for chime--apply-blacklist -*- lexical-binding: t; -*-

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

;; Unit tests for chime--apply-blacklist function.
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

(defun test-chime-apply-blacklist-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset blacklist settings
  (setq chime-keyword-blacklist nil)
  (setq chime-tags-blacklist nil)
  (setq chime-predicate-blacklist nil))

(defun test-chime-apply-blacklist-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  (setq chime-keyword-blacklist nil)
  (setq chime-tags-blacklist nil)
  (setq chime-predicate-blacklist nil))

;;; Normal Cases

(ert-deftest test-chime-apply-blacklist-nil-blacklist-returns-all-markers ()
  "Test that nil blacklist returns all markers unchanged."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (let* ((chime-keyword-blacklist nil)
             (chime-tags-blacklist nil)
             (markers (list (make-marker) (make-marker) (make-marker)))
             (result (chime--apply-blacklist markers)))
        ;; Should return all markers when blacklist is nil
        (should (equal (length result) 3)))
    (test-chime-apply-blacklist-teardown)))

(ert-deftest test-chime-apply-blacklist-keyword-blacklist-filters-correctly ()
  "Test that keyword blacklist filters out markers correctly."
  (test-chime-apply-blacklist-setup)
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
                  (chime-keyword-blacklist '("DONE")))
              (let ((result (chime--apply-blacklist (list marker1 marker2 marker3))))
                ;; Should filter out DONE marker
                (should (= (length result) 2))
                (should (member marker1 result))
                (should-not (member marker2 result))
                (should (member marker3 result)))))))
    (test-chime-apply-blacklist-teardown)))

(ert-deftest test-chime-apply-blacklist-tags-blacklist-filters-correctly ()
  "Test that tags blacklist filters out markers correctly."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Task 1                                          :work:urgent:\n")
        (insert "* Task 2                                             :personal:\n")
        (insert "* Task 3                                                 :work:\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker))
                  (chime-tags-blacklist '("personal")))
              (let ((result (chime--apply-blacklist (list marker1 marker2 marker3))))
                ;; Should filter out marker with "personal" tag
                (should (= (length result) 2))
                (should (member marker1 result))
                (should-not (member marker2 result))
                (should (member marker3 result)))))))
    (test-chime-apply-blacklist-teardown)))

(ert-deftest test-chime-apply-blacklist-keyword-and-tags-blacklist-uses-or-logic ()
  "Test that both keyword and tags blacklists use OR logic."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (insert "* DONE Task 2\n")
        (insert "* NEXT Task 3                                        :archive:\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker))
                  (chime-keyword-blacklist '("DONE"))
                  (chime-tags-blacklist '("archive")))
              (let ((result (chime--apply-blacklist (list marker1 marker2 marker3))))
                ;; Should filter out marker2 (DONE) and marker3 (archive tag)
                (should (= (length result) 1))
                (should (member marker1 result))
                (should-not (member marker2 result))
                (should-not (member marker3 result)))))))
    (test-chime-apply-blacklist-teardown)))

(ert-deftest test-chime-apply-blacklist-multiple-keywords-filters-all ()
  "Test that multiple keywords in blacklist filters all matching."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (insert "* DONE Task 2\n")
        (insert "* DONE Task 3\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker))
                  (chime-keyword-blacklist '("DONE")))
              (let ((result (chime--apply-blacklist (list marker1 marker2 marker3))))
                ;; Should only keep TODO marker, filter out both DONE markers
                (should (= (length result) 1))
                (should (member marker1 result)))))))
    (test-chime-apply-blacklist-teardown)))

;;; Boundary Cases

(ert-deftest test-chime-apply-blacklist-empty-markers-list-returns-empty ()
  "Test that empty markers list returns empty."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (let ((chime-keyword-blacklist '("DONE"))
            (result (chime--apply-blacklist '())))
        (should (equal result '())))
    (test-chime-apply-blacklist-teardown)))

(ert-deftest test-chime-apply-blacklist-single-item-blacklist-works ()
  "Test that single-item blacklist works correctly."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (insert "* DONE Task 2\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker))
                (chime-keyword-blacklist '("DONE")))
            (let ((result (chime--apply-blacklist (list marker1 marker2))))
              (should (= (length result) 1))
              (should (member marker1 result))))))
    (test-chime-apply-blacklist-teardown)))

(ert-deftest test-chime-apply-blacklist-all-markers-blacklisted-returns-empty ()
  "Test that blacklisting all markers returns empty list."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* DONE Task 1\n")
        (let ((marker1 (point-marker)))
          (insert "* DONE Task 2\n")
          (let ((marker2 (point-marker))
                (chime-keyword-blacklist '("DONE")))
            (let ((result (chime--apply-blacklist (list marker1 marker2))))
              (should (equal result '()))))))
    (test-chime-apply-blacklist-teardown)))

;;; Error Cases

(ert-deftest test-chime-apply-blacklist-handles-nil-keyword-gracefully ()
  "Test that nil keyword in marker is handled gracefully."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Entry without TODO keyword\n")
        (let ((marker1 (point-marker))
              (chime-keyword-blacklist '("DONE")))
          (let ((result (chime--apply-blacklist (list marker1))))
            ;; Should keep marker with nil keyword (not in blacklist)
            (should (= (length result) 1)))))
    (test-chime-apply-blacklist-teardown)))

(ert-deftest test-chime-apply-blacklist-handles-nil-tags-gracefully ()
  "Test that nil tags in marker is handled gracefully."
  (test-chime-apply-blacklist-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Entry without tags\n")
        (let ((marker1 (point-marker))
              (chime-tags-blacklist '("archive")))
          (let ((result (chime--apply-blacklist (list marker1))))
            ;; Should keep marker with nil tags (not in blacklist)
            (should (= (length result) 1)))))
    (test-chime-apply-blacklist-teardown)))

(provide 'test-chime-apply-blacklist)
;;; test-chime-apply-blacklist.el ends here
