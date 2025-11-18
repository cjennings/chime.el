;;; test-convert-org-contacts-birthdays.el --- Tests for convert-org-contacts-birthdays.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

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

;; Unit tests for convert-org-contacts-birthdays.el
;; Tests the conversion of org-contacts BIRTHDAY properties to plain timestamps.

;;; Code:

;; Initialize package system for batch mode
(when noninteractive
  (package-initialize))

(require 'ert)

;; Load the conversion utility from parent directory
(load (expand-file-name "../convert-org-contacts-birthdays.el") nil t)

;;; Tests for birthday parsing

(ert-deftest test-convert-normal-parse-birthday-with-year-returns-year-month-day ()
  "Test parsing YYYY-MM-DD format returns (YEAR MONTH DAY)."
  (let ((result (chime--parse-birthday "2000-03-15")))
    (should (equal result '(2000 3 15)))))

(ert-deftest test-convert-normal-parse-birthday-without-year-returns-nil-month-day ()
  "Test parsing MM-DD format returns (nil MONTH DAY)."
  (let ((result (chime--parse-birthday "03-15")))
    (should (equal result '(nil 3 15)))))

(ert-deftest test-convert-normal-parse-birthday-december-returns-correct-month ()
  "Test parsing December date returns month 12."
  (let ((result (chime--parse-birthday "1985-12-25")))
    (should (equal result '(1985 12 25)))))

(ert-deftest test-convert-normal-parse-birthday-january-returns-correct-month ()
  "Test parsing January date returns month 1."
  (let ((result (chime--parse-birthday "01-01")))
    (should (equal result '(nil 1 1)))))

(ert-deftest test-convert-error-parse-birthday-invalid-format-signals-error ()
  "Test parsing invalid format signals user-error."
  (should-error (chime--parse-birthday "2000/03/15") :type 'user-error)
  (should-error (chime--parse-birthday "March 15, 2000") :type 'user-error)
  (should-error (chime--parse-birthday "15-03-2000") :type 'user-error))

;;; Tests for birthday formatting

(ert-deftest test-convert-normal-format-birthday-timestamp-with-year-returns-yearly-repeater ()
  "Test formatting with year returns yearly repeating timestamp."
  (let ((result (chime--format-birthday-timestamp 2000 3 15)))
    ;; Should contain year, date, and +1y repeater
    (should (string-match-p "<2000-03-15 [A-Za-z]\\{3\\} \\+1y>" result))))

(ert-deftest test-convert-normal-format-birthday-timestamp-without-year-uses-current-year ()
  "Test formatting without year uses current year."
  (let* ((current-year (nth 5 (decode-time)))
         (result (chime--format-birthday-timestamp nil 3 15)))
    ;; Should contain current year
    (should (string-match-p (format "<%d-03-15 [A-Za-z]\\{3\\} \\+1y>" current-year) result))))

(ert-deftest test-convert-boundary-format-birthday-timestamp-leap-day-returns-valid-timestamp ()
  "Test formatting February 29 (leap day) returns valid timestamp."
  (let ((result (chime--format-birthday-timestamp 2000 2 29)))
    (should (string-match-p "<2000-02-29 [A-Za-z]\\{3\\} \\+1y>" result))))

(ert-deftest test-convert-boundary-format-birthday-timestamp-december-31-returns-valid-timestamp ()
  "Test formatting December 31 returns valid timestamp."
  (let ((result (chime--format-birthday-timestamp 2000 12 31)))
    (should (string-match-p "<2000-12-31 [A-Za-z]\\{3\\} \\+1y>" result))))

;;; Tests for backup file creation

(ert-deftest test-convert-normal-backup-file-creates-timestamped-backup ()
  "Test that backup file is created with timestamp."
  (let* ((temp-file (make-temp-file "test-contacts"))
         (backup-path nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Test Contact\n:PROPERTIES:\n:BIRTHDAY: 2000-01-01\n:END:\n"))
          (setq backup-path (chime--backup-contacts-file temp-file))
          (should (file-exists-p backup-path))
          (should (string-match-p "\\.backup-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{6\\}$" backup-path)))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (when (and backup-path (file-exists-p backup-path)) (delete-file backup-path)))))

(ert-deftest test-convert-normal-backup-file-preserves-content ()
  "Test that backup file contains exact copy of original."
  (let* ((temp-file (make-temp-file "test-contacts"))
         (content "* Test Contact\n:PROPERTIES:\n:BIRTHDAY: 2000-01-01\n:END:\n")
         (backup-path nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert content))
          (setq backup-path (chime--backup-contacts-file temp-file))
          (with-temp-buffer
            (insert-file-contents backup-path)
            (should (string= (buffer-string) content))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (when (and backup-path (file-exists-p backup-path)) (delete-file backup-path)))))

;;; Tests for timestamp insertion

(ert-deftest test-convert-normal-insert-timestamp-after-drawer-adds-timestamp ()
  "Test that timestamp is inserted after properties drawer."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Contact\n:PROPERTIES:\n:EMAIL: test@example.com\n:BIRTHDAY: 2000-03-15\n:END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\* ")
    (beginning-of-line)
    (chime--insert-birthday-timestamp-after-drawer "2000-03-15")
    (should (string-match-p "<2000-03-15 [A-Za-z]\\{3\\} \\+1y>" (buffer-string)))))

(ert-deftest test-convert-normal-insert-timestamp-preserves-properties ()
  "Test that inserting timestamp doesn't modify properties."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Contact\n:PROPERTIES:\n:EMAIL: test@example.com\n:BIRTHDAY: 2000-03-15\n:END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\* ")
    (beginning-of-line)
    (chime--insert-birthday-timestamp-after-drawer "2000-03-15")
    (should (string-match-p ":EMAIL: test@example.com" (buffer-string)))
    (should (string-match-p ":BIRTHDAY: 2000-03-15" (buffer-string)))))

(ert-deftest test-convert-boundary-insert-timestamp-without-year-uses-current ()
  "Test that MM-DD format uses current year."
  (let ((current-year (nth 5 (decode-time))))
    (with-temp-buffer
      (org-mode)
      (insert "* Test Contact\n:PROPERTIES:\n:BIRTHDAY: 03-15\n:END:\n")
      (goto-char (point-min))
      (re-search-forward "^\\* ")
      (beginning-of-line)
      (chime--insert-birthday-timestamp-after-drawer "03-15")
      (should (string-match-p (format "<%d-03-15" current-year) (buffer-string))))))

;;; Tests for contact entry processing

(ert-deftest test-convert-normal-process-contact-with-birthday-returns-true ()
  "Test that processing contact with birthday returns t."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Contact\n:PROPERTIES:\n:BIRTHDAY: 2000-03-15\n:END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\* ")
    (beginning-of-line)
    (should (eq t (chime--process-contact-entry)))))

(ert-deftest test-convert-normal-process-contact-without-birthday-returns-nil ()
  "Test that processing contact without birthday returns nil."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Contact\n:PROPERTIES:\n:EMAIL: test@example.com\n:END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\* ")
    (beginning-of-line)
    (should (null (chime--process-contact-entry)))))

;;; Tests for file conversion

(ert-deftest test-convert-normal-convert-file-multiple-contacts-returns-correct-count ()
  "Test converting file with multiple contacts returns correct count."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: 1985-03-15\n:END:\n\n")
            (insert "* Bob Baker\n:PROPERTIES:\n:BIRTHDAY: 1990-07-22\n:END:\n\n")
            (insert "* Carol Chen\n:PROPERTIES:\n:BIRTHDAY: 12-25\n:END:\n"))
          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))
            (should (= count 3))
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-boundary-convert-file-no-contacts-returns-zero ()
  "Test converting file with no contacts returns zero."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "#+TITLE: Empty File\n\nSome text but no contacts.\n"))
          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))
            (should (= count 0))
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-boundary-convert-file-contacts-without-birthdays-returns-zero ()
  "Test converting file with contacts but no birthdays returns zero."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:EMAIL: alice@example.com\n:END:\n\n")
            (insert "* Bob Baker\n:PROPERTIES:\n:EMAIL: bob@example.com\n:END:\n"))
          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))
            (should (= count 0))
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-boundary-convert-file-contact-without-properties-drawer ()
  "Test converting contact without properties drawer is skipped."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\nJust some text, no properties.\n"))
          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))
            (should (= count 0))
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-boundary-convert-file-preserves-existing-timestamps ()
  "Test that existing timestamps in contact body are not modified."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: 1985-03-15\n:END:\n")
            (insert "Meeting scheduled <2025-11-15 Sat>\n"))
          (chime--convert-contacts-file-in-place temp-file)
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should have birthday timestamp
              (should (string-match-p "<1985-03-15 [A-Za-z]\\{3\\} \\+1y>" content))
              ;; Should still have meeting timestamp
              (should (string-match-p "<2025-11-15 Sat>" content))
              ;; Should have exactly 2 timestamps
              (should (= 2 (how-many "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (point-min) (point-max)))))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (let ((backup (concat temp-file ".backup-")))
        (dolist (file (directory-files (file-name-directory temp-file) t (regexp-quote (file-name-nondirectory backup))))
          (delete-file file))))))

(ert-deftest test-convert-normal-convert-file-timestamp-inserted-after-drawer ()
  "Test that timestamp is inserted immediately after properties drawer."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: 1985-03-15\n:END:\n")
            (insert "Some notes about Alice.\n"))
          (chime--convert-contacts-file-in-place temp-file)
          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Timestamp should come after :END: but before notes
              (should (string-match-p ":END:\n<1985-03-15 [A-Za-z]\\{3\\} \\+1y>\nSome notes" content)))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (let ((backup (concat temp-file ".backup-")))
        (dolist (file (directory-files (file-name-directory temp-file) t (regexp-quote (file-name-nondirectory backup))))
          (delete-file file))))))

;;; Tests for year extraction

(ert-deftest test-convert-normal-extract-birthday-year-with-year-returns-year ()
  "Test extracting year from YYYY-MM-DD returns the year."
  (let ((result (chime--extract-birthday-year "2000-03-15")))
    (should (equal result 2000))))

(ert-deftest test-convert-normal-extract-birthday-year-without-year-returns-nil ()
  "Test extracting year from MM-DD returns nil."
  (let ((result (chime--extract-birthday-year "03-15")))
    (should (null result))))

(ert-deftest test-convert-boundary-extract-birthday-year-very-old-date-returns-year ()
  "Test extracting year from very old date (1900s) returns the year."
  (let ((result (chime--extract-birthday-year "1920-01-01")))
    (should (equal result 1920))))

(ert-deftest test-convert-boundary-extract-birthday-year-future-date-returns-year ()
  "Test extracting year from future date returns the year."
  (let ((result (chime--extract-birthday-year "2100-12-31")))
    (should (equal result 2100))))

;;; Edge Case Tests

(ert-deftest test-convert-edge-duplicate-timestamps-not-added ()
  "Test that running conversion twice doesn't add duplicate timestamps."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: 2000-01-01\n:END:\n"))

          ;; Run conversion once
          (let ((backup1 (cdr (chime--convert-contacts-file-in-place temp-file))))
            ;; Clean up first backup to avoid file-already-exists error
            (when (file-exists-p backup1) (delete-file backup1))

            ;; Run conversion again
            (let ((backup2 (cdr (chime--convert-contacts-file-in-place temp-file))))

              (with-temp-buffer
                (insert-file-contents temp-file)
                (let ((content (buffer-string)))
                  ;; Should have exactly one birthday timestamp
                  (should (= 1 (how-many "<2000-01-01 [A-Za-z]\\{3\\} \\+1y>" (point-min) (point-max))))))

              (when (file-exists-p backup2) (delete-file backup2)))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-edge-malformed-properties-drawer-missing-end ()
  "Test handling of properties drawer missing :END:."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: 2000-01-01\nMissing END tag\n"))

          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))
            ;; Should handle gracefully (likely returns 0 if drawer malformed)
            (should (numberp count))
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-edge-empty-birthday-property ()
  "Test handling of empty BIRTHDAY property value."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: \n:END:\n"))

          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))
            ;; Should skip empty birthday
            (should (= count 0))
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-edge-whitespace-in-birthday ()
  "Test handling of whitespace in BIRTHDAY property."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY:   2000-01-01  \n:END:\n"))

          (chime--convert-contacts-file-in-place temp-file)

          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Should handle whitespace and create timestamp
              (should (string-match-p "<2000-01-01 [A-Za-z]\\{3\\} \\+1y>" content)))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (let ((backup (concat temp-file ".backup-")))
        (dolist (file (directory-files (file-name-directory temp-file) t (regexp-quote (file-name-nondirectory backup))))
          (delete-file file))))))

(ert-deftest test-convert-edge-very-large-file ()
  "Test conversion of file with many contacts (performance test)."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (dotimes (i 100)
              (insert (format "* Contact %d\n:PROPERTIES:\n:BIRTHDAY: 1990-01-01\n:END:\n\n" i))))

          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))
            (should (= count 100))
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

;;; Integration Tests

(ert-deftest test-convert-integration-full-workflow-with-realistic-contacts ()
  "Integration test with realistic contacts file structure."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "#+TITLE: My Contacts\n#+STARTUP: overview\n\n")
            (insert "* Alice Anderson\n")
            (insert ":PROPERTIES:\n")
            (insert ":EMAIL: alice@example.com\n")
            (insert ":PHONE: 555-0101\n")
            (insert ":BIRTHDAY: 1985-03-15\n")
            (insert ":ADDRESS: 123 Main St\n")
            (insert ":END:\n")
            (insert "Met at conference 2023.\n\n")
            (insert "* Bob Baker\n")
            (insert ":PROPERTIES:\n")
            (insert ":EMAIL: bob@example.com\n")
            (insert ":BIRTHDAY: 1990-07-22\n")
            (insert ":END:\n\n")
            (insert "* Carol Chen\n")
            (insert ":PROPERTIES:\n")
            (insert ":EMAIL: carol@example.com\n")
            (insert ":END:\n")
            (insert "No birthday for Carol.\n"))

          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (count (car result))
                 (backup-file (cdr result)))

            ;; Verify conversion count
            (should (= count 2))

            ;; Verify backup exists
            (should (file-exists-p backup-file))

            ;; Verify converted file content
            (with-temp-buffer
              (insert-file-contents temp-file)
              (let ((content (buffer-string)))
                ;; File header preserved
                (should (string-search "#+TITLE: My Contacts" content))

                ;; Alice's properties preserved
                (should (string-search ":EMAIL: alice@example.com" content))
                (should (string-search ":ADDRESS: 123 Main St" content))
                (should (string-search ":BIRTHDAY: 1985-03-15" content))

                ;; Alice's birthday timestamp added
                (should (string-match-p "<1985-03-15 [A-Za-z]\\{3\\} \\+1y>" content))

                ;; Alice's notes preserved
                (should (string-search "Met at conference 2023" content))

                ;; Bob's birthday timestamp added
                (should (string-match-p "<1990-07-22 [A-Za-z]\\{3\\} \\+1y>" content))

                ;; Carol has no timestamp added
                (goto-char (point-min))
                (re-search-forward "\\* Carol Chen")
                (let ((carol-section-start (point))
                      (carol-section-end (or (re-search-forward "^\\* " nil t) (point-max))))
                  (goto-char carol-section-start)
                  (should-not (re-search-forward "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" carol-section-end t)))))

            ;; Clean up backup
            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-integration-mixed-birthday-formats ()
  "Integration test with both YYYY-MM-DD and MM-DD formats."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org"))
        (current-year (nth 5 (decode-time))))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: 1985-03-15\n:END:\n\n")
            (insert "* Bob Baker\n:PROPERTIES:\n:BIRTHDAY: 07-04\n:END:\n"))

          (chime--convert-contacts-file-in-place temp-file)

          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; Alice has year from property
              (should (string-match-p "<1985-03-15" content))
              ;; Bob uses current year
              (should (string-match-p (format "<%d-07-04" current-year) content)))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (let ((backup (concat temp-file ".backup-")))
        (dolist (file (directory-files (file-name-directory temp-file) t (regexp-quote (file-name-nondirectory backup))))
          (delete-file file))))))

(ert-deftest test-convert-integration-file-with-existing-content-preserved ()
  "Integration test verifying all existing content is preserved."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "#+TITLE: Contacts\n")
            (insert "#+FILETAGS: :contacts:\n\n")
            (insert "Some introductory text.\n\n")
            (insert "* Alice Anderson\n")
            (insert ":PROPERTIES:\n")
            (insert ":BIRTHDAY: 2000-01-01\n")
            (insert ":END:\n")
            (insert "** Notes\n")
            (insert "Some nested content.\n")
            (insert "*** Deep nested\n")
            (insert "More content.\n\n")
            (insert "* Final Section\n")
            (insert "Closing remarks.\n"))

          (chime--convert-contacts-file-in-place temp-file)

          (with-temp-buffer
            (insert-file-contents temp-file)
            (let ((content (buffer-string)))
              ;; All content preserved
              (should (string-search "Some introductory text" content))
              (should (string-search "** Notes" content))
              (should (string-search "Some nested content" content))
              (should (string-search "*** Deep nested" content))
              (should (string-search "Closing remarks" content))
              (should (string-search "#+FILETAGS: :contacts:" content)))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (let ((backup (concat temp-file ".backup-")))
        (dolist (file (directory-files (file-name-directory temp-file) t (regexp-quote (file-name-nondirectory backup))))
          (delete-file file))))))

;;; Workflow Tests (Backup → Convert → Verify)

(ert-deftest test-convert-workflow-backup-created-before-modification ()
  "Test that backup file is created before original is modified."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org"))
        (original-content "* Alice Anderson\n:PROPERTIES:\n:BIRTHDAY: 2000-01-01\n:END:\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert original-content))

          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (backup-file (cdr result)))

            ;; Backup exists
            (should (file-exists-p backup-file))

            ;; Backup contains original content (unmodified)
            (with-temp-buffer
              (insert-file-contents backup-file)
              (should (string= (buffer-string) original-content)))

            ;; Original file is modified
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string-match-p "<2000-01-01 [A-Za-z]\\{3\\} \\+1y>" (buffer-string))))

            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-workflow-backup-content-matches-original ()
  "Test that backup file contains exact copy of original content."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org"))
        (original-content "#+TITLE: Contacts\n\n* Alice\n:PROPERTIES:\n:BIRTHDAY: 1985-03-15\n:END:\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert original-content))

          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (backup-file (cdr result)))

            ;; Backup content matches original
            (with-temp-buffer
              (insert-file-contents backup-file)
              (should (string= (buffer-string) original-content)))

            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-workflow-original-modified-with-timestamps ()
  "Test that original file is modified with timestamps after conversion."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice\n:PROPERTIES:\n:BIRTHDAY: 1985-03-15\n:END:\n")
            (insert "* Bob\n:PROPERTIES:\n:BIRTHDAY: 1990-07-22\n:END:\n"))

          ;; Get original content
          (let ((original-content nil))
            (with-temp-buffer
              (insert-file-contents temp-file)
              (setq original-content (buffer-string)))

            ;; Should not have timestamps before conversion
            (should-not (string-match-p "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} \\+1y>" original-content))

            ;; Convert
            (let* ((result (chime--convert-contacts-file-in-place temp-file))
                   (count (car result))
                   (backup-file (cdr result)))

              (should (= count 2))

              ;; Modified content should have timestamps
              (with-temp-buffer
                (insert-file-contents temp-file)
                (let ((modified-content (buffer-string)))
                  (should (string-match-p "<1985-03-15 [A-Za-z]\\{3\\} \\+1y>" modified-content))
                  (should (string-match-p "<1990-07-22 [A-Za-z]\\{3\\} \\+1y>" modified-content))))

              (when (file-exists-p backup-file) (delete-file backup-file)))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-workflow-rollback-via-backup ()
  "Test that conversion can be rolled back by restoring from backup."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org"))
        (original-content "* Alice\n:PROPERTIES:\n:BIRTHDAY: 2000-01-01\n:END:\n"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert original-content))

          ;; Convert
          (let* ((result (chime--convert-contacts-file-in-place temp-file))
                 (backup-file (cdr result)))

            ;; Verify file was modified
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string-match-p "<2000-01-01 [A-Za-z]\\{3\\} \\+1y>" (buffer-string))))

            ;; Rollback by copying backup over original
            (copy-file backup-file temp-file t)

            ;; Verify rollback worked
            (with-temp-buffer
              (insert-file-contents temp-file)
              (let ((content (buffer-string)))
                (should (string= content original-content))
                (should-not (string-match-p "<2000-01-01 [A-Za-z]\\{3\\} \\+1y>" content))))

            (when (file-exists-p backup-file) (delete-file backup-file))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(ert-deftest test-convert-workflow-multiple-backups-distinct-timestamps ()
  "Test that multiple conversions create backups with distinct timestamps."
  (let ((temp-file (make-temp-file "test-contacts" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Alice\n:PROPERTIES:\n:BIRTHDAY: 2000-01-01\n:END:\n"))

          ;; First conversion
          (let ((backup1 (cdr (chime--convert-contacts-file-in-place temp-file))))
            ;; Small delay to ensure different timestamp
            (sleep-for 1)
            ;; Second conversion
            (let ((backup2 (cdr (chime--convert-contacts-file-in-place temp-file))))

              ;; Backup files should have different names
              (should-not (string= backup1 backup2))

              ;; Both should exist
              (should (file-exists-p backup1))
              (should (file-exists-p backup2))

              (when (file-exists-p backup1) (delete-file backup1))
              (when (file-exists-p backup2) (delete-file backup2)))))
      (when (file-exists-p temp-file) (delete-file temp-file)))))

(provide 'test-convert-org-contacts-birthdays)
;;; test-convert-org-contacts-birthdays.el ends here
