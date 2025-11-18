;;; test-chime-whitelist-blacklist-conflicts.el --- Tests for whitelist/blacklist conflicts -*- lexical-binding: t; -*-

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

;; Tests for conflicts when the same keyword or tag appears in both
;; whitelist and blacklist.  These tests verify which takes precedence.
;;
;; Current implementation: whitelist is applied first, then blacklist.
;; Therefore, blacklist wins in conflicts - items matching both lists
;; are filtered out.

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

(defun test-chime-conflicts-setup ()
  "Setup function run before each test."
  (chime-create-test-base-dir)
  ;; Reset all whitelist/blacklist settings
  (setq chime-keyword-whitelist nil)
  (setq chime-tags-whitelist nil)
  (setq chime-predicate-whitelist nil)
  (setq chime-keyword-blacklist nil)
  (setq chime-tags-blacklist nil)
  (setq chime-predicate-blacklist nil))

(defun test-chime-conflicts-teardown ()
  "Teardown function run after each test."
  (chime-delete-test-base-dir)
  (setq chime-keyword-whitelist nil)
  (setq chime-tags-whitelist nil)
  (setq chime-predicate-whitelist nil)
  (setq chime-keyword-blacklist nil)
  (setq chime-tags-blacklist nil)
  (setq chime-predicate-blacklist nil))

;;; Keyword Conflict Tests

(ert-deftest test-chime-conflict-same-keyword-in-both-lists ()
  "Test behavior when same keyword is in both whitelist and blacklist.
Current behavior: blacklist wins (item is filtered out)."
  (test-chime-conflicts-setup)
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
                  (chime-keyword-whitelist '("TODO" "DONE"))
                  (chime-keyword-blacklist '("DONE")))  ; DONE in both lists
              ;; Apply both filters (simulating what happens in chime--gather-timestamps)
              (let* ((after-whitelist (chime--apply-whitelist (list marker1 marker2 marker3)))
                     (after-blacklist (chime--apply-blacklist after-whitelist)))
                ;; Whitelist should keep all three (all are TODO or DONE)
                (should (= (length after-whitelist) 3))
                (should (member marker1 after-whitelist))
                (should (member marker2 after-whitelist))
                (should (member marker3 after-whitelist))
                ;; Blacklist should then remove DONE (marker2)
                ;; So only TODO items (markers 1 and 3) should remain
                (should (= (length after-blacklist) 2))
                (should (member marker1 after-blacklist))
                (should-not (member marker2 after-blacklist))
                (should (member marker3 after-blacklist)))))))
    (test-chime-conflicts-teardown)))

(ert-deftest test-chime-conflict-all-keywords-in-both-lists ()
  "Test behavior when all keywords are in both whitelist and blacklist.
Current behavior: blacklist wins, all items filtered out."
  (test-chime-conflicts-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1\n")
        (insert "* DONE Task 2\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker))
                (chime-keyword-whitelist '("TODO" "DONE"))
                (chime-keyword-blacklist '("TODO" "DONE")))
            (let* ((after-whitelist (chime--apply-whitelist (list marker1 marker2)))
                   (after-blacklist (chime--apply-blacklist after-whitelist)))
              ;; Whitelist should keep both
              (should (= (length after-whitelist) 2))
              ;; Blacklist should remove both
              (should (= (length after-blacklist) 0))))))
    (test-chime-conflicts-teardown)))

;;; Tag Conflict Tests

(ert-deftest test-chime-conflict-same-tag-in-both-lists ()
  "Test behavior when same tag is in both whitelist and blacklist.
Current behavior: blacklist wins (item is filtered out)."
  (test-chime-conflicts-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* Task 1                                            :urgent:\n")
        (insert "* Task 2                                             :normal:\n")
        (insert "* Task 3                                         :important:\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker))
                  (chime-tags-whitelist '("urgent" "important"))
                  (chime-tags-blacklist '("urgent")))  ; urgent in both lists
              (let* ((after-whitelist (chime--apply-whitelist (list marker1 marker2 marker3)))
                     (after-blacklist (chime--apply-blacklist after-whitelist)))
                ;; Whitelist should keep urgent and important (markers 1 and 3)
                (should (= (length after-whitelist) 2))
                (should (member marker1 after-whitelist))
                (should (member marker3 after-whitelist))
                ;; Blacklist should then remove urgent (marker1)
                ;; So only important (marker3) should remain
                (should (= (length after-blacklist) 1))
                (should-not (member marker1 after-blacklist))
                (should (member marker3 after-blacklist)))))))
    (test-chime-conflicts-teardown)))

;;; Mixed Keyword and Tag Conflict Tests

(ert-deftest test-chime-conflict-keyword-whitelisted-tag-blacklisted ()
  "Test when item has whitelisted keyword but blacklisted tag.
Current behavior: blacklist wins (OR logic means tag match filters it out)."
  (test-chime-conflicts-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1                                        :urgent:\n")
        (insert "* DONE Task 2                                         :normal:\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker))
                (chime-keyword-whitelist '("TODO"))
                (chime-tags-blacklist '("urgent")))
            (let* ((after-whitelist (chime--apply-whitelist (list marker1 marker2)))
                   (after-blacklist (chime--apply-blacklist after-whitelist)))
              ;; Whitelist should keep TODO (marker1)
              (should (= (length after-whitelist) 1))
              (should (member marker1 after-whitelist))
              ;; Blacklist should remove items with urgent tag (marker1)
              (should (= (length after-blacklist) 0))))))
    (test-chime-conflicts-teardown)))

(ert-deftest test-chime-conflict-tag-whitelisted-keyword-blacklisted ()
  "Test when item has whitelisted tag but blacklisted keyword.
Current behavior: blacklist wins (OR logic means keyword match filters it out)."
  (test-chime-conflicts-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1                                        :urgent:\n")
        (insert "* DONE Task 2                                         :normal:\n")
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker))
                (chime-tags-whitelist '("urgent"))
                (chime-keyword-blacklist '("TODO")))
            (let* ((after-whitelist (chime--apply-whitelist (list marker1 marker2)))
                   (after-blacklist (chime--apply-blacklist after-whitelist)))
              ;; Whitelist should keep urgent tag (marker1)
              (should (= (length after-whitelist) 1))
              (should (member marker1 after-whitelist))
              ;; Blacklist should remove items with TODO keyword (marker1)
              (should (= (length after-blacklist) 0))))))
    (test-chime-conflicts-teardown)))

;;; Complex Conflict Tests

(ert-deftest test-chime-conflict-multiple-items-partial-conflicts ()
  "Test multiple items with some having conflicts and some not.
Current behavior: only items with conflicts are filtered out."
  (test-chime-conflicts-setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task 1                                        :urgent:\n")   ; whitelisted keyword, blacklisted tag
        (insert "* DONE Task 2                                         :normal:\n")  ; not whitelisted
        (insert "* TODO Task 3                                        :urgent:\n")   ; whitelisted keyword, blacklisted tag
        (insert "* TODO Task 4                                         :normal:\n")  ; whitelisted keyword, ok tag
        (goto-char (point-min))
        (let ((marker1 (point-marker)))
          (forward-line 1)
          (let ((marker2 (point-marker)))
            (forward-line 1)
            (let ((marker3 (point-marker)))
              (forward-line 1)
              (let ((marker4 (point-marker))
                    (chime-keyword-whitelist '("TODO"))
                    (chime-tags-blacklist '("urgent")))
                (let* ((after-whitelist (chime--apply-whitelist (list marker1 marker2 marker3 marker4)))
                       (after-blacklist (chime--apply-blacklist after-whitelist)))
                  ;; Whitelist should keep TODO (markers 1, 3, 4)
                  (should (= (length after-whitelist) 3))
                  (should (member marker1 after-whitelist))
                  (should (member marker3 after-whitelist))
                  (should (member marker4 after-whitelist))
                  ;; Blacklist should remove items with urgent tag (markers 1, 3)
                  ;; Only marker4 (TODO with normal tag) should remain
                  (should (= (length after-blacklist) 1))
                  (should (member marker4 after-blacklist))
                  (should-not (member marker1 after-blacklist))
                  (should-not (member marker3 after-blacklist))))))))
    (test-chime-conflicts-teardown)))

(provide 'test-chime-whitelist-blacklist-conflicts)
;;; test-chime-whitelist-blacklist-conflicts.el ends here
