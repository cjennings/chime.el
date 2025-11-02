;;; test-chime-org-contacts.el --- Tests for chime-org-contacts.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Craig Jennings

;; Author: Craig Jennings <c@cjennings.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Unit and integration tests for chime-org-contacts.el

;;; Code:

;; Initialize package system for batch mode
(when noninteractive
  (package-initialize))

(require 'ert)
(require 'org)
(require 'org-capture)

;; Load the module being tested
(let ((module-file (expand-file-name "../chime-org-contacts.el"
                                     (file-name-directory (or load-file-name buffer-file-name)))))
  (load module-file nil t))

;;; Unit Tests - chime-org-contacts--parse-birthday

(ert-deftest test-chime-org-contacts-parse-birthday-full-format ()
  "Test parsing YYYY-MM-DD format."
  (let ((result (chime-org-contacts--parse-birthday "2000-01-01")))
    (should (equal result '(2000 1 1))))

  (let ((result (chime-org-contacts--parse-birthday "1985-03-15")))
    (should (equal result '(1985 3 15))))

  (let ((result (chime-org-contacts--parse-birthday "2024-12-31")))
    (should (equal result '(2024 12 31)))))

(ert-deftest test-chime-org-contacts-parse-birthday-partial-format ()
  "Test parsing MM-DD format uses current year."
  (let ((current-year (nth 5 (decode-time))))
    (let ((result (chime-org-contacts--parse-birthday "03-15")))
      (should (equal result (list current-year 3 15))))

    (let ((result (chime-org-contacts--parse-birthday "12-31")))
      (should (equal result (list current-year 12 31))))))

(ert-deftest test-chime-org-contacts-parse-birthday-leap-year ()
  "Test parsing leap year date."
  (let ((result (chime-org-contacts--parse-birthday "2024-02-29")))
    (should (equal result '(2024 2 29)))))

(ert-deftest test-chime-org-contacts-parse-birthday-invalid-format ()
  "Test that invalid formats return nil."
  (should (null (chime-org-contacts--parse-birthday "2000/01/01")))
  (should (null (chime-org-contacts--parse-birthday "1-1-2000")))
  (should (null (chime-org-contacts--parse-birthday "Jan 1, 2000")))
  (should (null (chime-org-contacts--parse-birthday "not a date"))))

(ert-deftest test-chime-org-contacts-parse-birthday-empty-input ()
  "Test that empty input returns nil."
  (should (null (chime-org-contacts--parse-birthday ""))))

(ert-deftest test-chime-org-contacts-parse-birthday-boundary-dates ()
  "Test boundary dates (start/end of year, end of months)."
  (should (equal (chime-org-contacts--parse-birthday "2025-01-01") '(2025 1 1)))
  (should (equal (chime-org-contacts--parse-birthday "2025-12-31") '(2025 12 31)))
  (should (equal (chime-org-contacts--parse-birthday "2025-11-30") '(2025 11 30))))

;;; Unit Tests - chime-org-contacts--format-timestamp

(ert-deftest test-chime-org-contacts-format-timestamp-basic ()
  "Test basic timestamp formatting."
  (let ((timestamp (chime-org-contacts--format-timestamp 2025 1 1)))
    (should (string-match-p "^<2025-01-01 [A-Za-z]\\{3\\} \\+1y>$" timestamp))))

(ert-deftest test-chime-org-contacts-format-timestamp-day-of-week ()
  "Test that day of week matches the date."
  ;; 2025-01-01 is a Wednesday
  (let ((timestamp (chime-org-contacts--format-timestamp 2025 1 1)))
    (should (string-match-p "Wed" timestamp)))

  ;; 2024-02-29 is a Thursday (leap year)
  (let ((timestamp (chime-org-contacts--format-timestamp 2024 2 29)))
    (should (string-match-p "Thu" timestamp))))

(ert-deftest test-chime-org-contacts-format-timestamp-all-months ()
  "Test formatting for all months."
  (dolist (month '(1 2 3 4 5 6 7 8 9 10 11 12))
    (let ((timestamp (chime-org-contacts--format-timestamp 2025 month 1)))
      (should (string-match-p (format "^<2025-%02d-01 [A-Za-z]\\{3\\} \\+1y>$" month) timestamp)))))

(ert-deftest test-chime-org-contacts-format-timestamp-repeater ()
  "Test that +1y repeater is always included."
  (let ((timestamp (chime-org-contacts--format-timestamp 2025 3 15)))
    (should (string-match-p "\\+1y>" timestamp))))

;;; Unit Tests - chime-org-contacts--insert-timestamp-after-drawer

(ert-deftest test-chime-org-contacts-insert-timestamp-when-none-exists ()
  "Test inserting timestamp when none exists."
  (with-temp-buffer
    (org-mode)
    (insert "* Contact\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    (insert ":END:\n")
    (goto-char (point-min))

    (chime-org-contacts--insert-timestamp-after-drawer "<2000-01-01 Wed +1y>")

    (let ((content (buffer-string)))
      (should (string-match-p "<2000-01-01 Wed \\+1y>" content))
      (should (string-match-p ":END:\n<2000-01-01" content)))))

(ert-deftest test-chime-org-contacts-insert-timestamp-skips-when-exists ()
  "Test that insertion is skipped when timestamp already exists."
  (with-temp-buffer
    (org-mode)
    (insert "* Contact\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    (insert ":END:\n")
    (insert "<2000-01-01 Wed +1y>\n")
    (goto-char (point-min))

    (chime-org-contacts--insert-timestamp-after-drawer "<2000-01-01 Wed +1y>")

    ;; Should have exactly one timestamp
    (should (= 1 (how-many "<2000-01-01 [A-Za-z]\\{3\\} \\+1y>" (point-min) (point-max))))))

(ert-deftest test-chime-org-contacts-insert-timestamp-handles-whitespace ()
  "Test handling of whitespace around :END:."
  (with-temp-buffer
    (org-mode)
    (insert "* Contact\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    (insert "  :END:  \n")  ; Whitespace before and after
    (goto-char (point-min))

    (chime-org-contacts--insert-timestamp-after-drawer "<2000-01-01 Wed +1y>")

    (should (string-match-p "<2000-01-01 Wed \\+1y>" (buffer-string)))))

(ert-deftest test-chime-org-contacts-insert-timestamp-preserves-content ()
  "Test that insertion doesn't modify other content."
  (with-temp-buffer
    (org-mode)
    (insert "* Contact\n")
    (insert ":PROPERTIES:\n")
    (insert ":EMAIL: test@example.com\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    (insert ":END:\n")
    (insert "Some notes about the contact.\n")
    (goto-char (point-min))

    (let ((original-content (buffer-substring (point-min) (point-max))))
      (chime-org-contacts--insert-timestamp-after-drawer "<2000-01-01 Wed +1y>")

      (should (string-search ":EMAIL: test@example.com" (buffer-string)))
      (should (string-search "Some notes about the contact" (buffer-string))))))

(ert-deftest test-chime-org-contacts-insert-timestamp-missing-end ()
  "Test handling when :END: is missing (malformed drawer)."
  (with-temp-buffer
    (org-mode)
    (insert "* Contact\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    ;; No :END:
    (goto-char (point-min))

    (chime-org-contacts--insert-timestamp-after-drawer "<2000-01-01 Wed +1y>")

    ;; Should not insert when :END: is missing
    (should-not (string-match-p "<2000-01-01" (buffer-string)))))

;;; Integration Tests - chime-org-contacts--finalize-birthday-timestamp

(ert-deftest test-chime-org-contacts-finalize-adds-timestamp-full-date ()
  "Test finalize adds timestamp for YYYY-MM-DD birthday."
  (with-temp-buffer
    (org-mode)
    (insert "* Alice Anderson\n")
    (insert ":PROPERTIES:\n")
    (insert ":EMAIL: alice@example.com\n")
    (insert ":BIRTHDAY: 1985-03-15\n")
    (insert ":END:\n")
    (goto-char (point-min))

    (let ((org-capture-plist '(:key "C")))
      (chime-org-contacts--finalize-birthday-timestamp)

      (let ((content (buffer-string)))
        (should (string-match-p "<1985-03-15 [A-Za-z]\\{3\\} \\+1y>" content))))))

(ert-deftest test-chime-org-contacts-finalize-adds-timestamp-partial-date ()
  "Test finalize adds timestamp for MM-DD birthday."
  (let ((current-year (nth 5 (decode-time))))
    (with-temp-buffer
      (org-mode)
      (insert "* Bob Baker\n")
      (insert ":PROPERTIES:\n")
      (insert ":BIRTHDAY: 07-04\n")
      (insert ":END:\n")
      (goto-char (point-min))

      (let ((org-capture-plist '(:key "C")))
        (chime-org-contacts--finalize-birthday-timestamp)

        (let ((content (buffer-string)))
          (should (string-match-p (format "<%d-07-04 [A-Za-z]\\{3\\} \\+1y>" current-year) content)))))))

(ert-deftest test-chime-org-contacts-finalize-skips-when-no-birthday ()
  "Test finalize does nothing when :BIRTHDAY: property missing."
  (with-temp-buffer
    (org-mode)
    (insert "* Carol Chen\n")
    (insert ":PROPERTIES:\n")
    (insert ":EMAIL: carol@example.com\n")
    (insert ":END:\n")
    (goto-char (point-min))

    (let ((original-content (buffer-string))
          (org-capture-plist '(:key "C")))
      (chime-org-contacts--finalize-birthday-timestamp)

      ;; Content should be unchanged
      (should (string= (buffer-string) original-content)))))

(ert-deftest test-chime-org-contacts-finalize-skips-empty-birthday ()
  "Test finalize skips empty birthday values."
  (with-temp-buffer
    (org-mode)
    (insert "* David Davis\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: \n")
    (insert ":END:\n")
    (goto-char (point-min))

    (let ((original-content (buffer-string))
          (org-capture-plist '(:key "C")))
      (chime-org-contacts--finalize-birthday-timestamp)

      (should (string= (buffer-string) original-content)))))

(ert-deftest test-chime-org-contacts-finalize-only-runs-for-correct-key ()
  "Test finalize only runs for configured capture key."
  (with-temp-buffer
    (org-mode)
    (insert "* Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":BIRTHDAY: 2000-01-01\n")
    (insert ":END:\n")
    (goto-char (point-min))

    (let ((original-content (buffer-string))
          (org-capture-plist '(:key "t")))  ; Different key
      (chime-org-contacts--finalize-birthday-timestamp)

      ;; Should not insert timestamp
      (should (string= (buffer-string) original-content)))))

;;; Integration Tests - chime-org-contacts--setup-capture-template

(ert-deftest test-chime-org-contacts-setup-adds-template-when-file-set ()
  "Test that template is added when file is set."
  (let ((chime-org-contacts-file "/tmp/test-contacts.org")
        (org-capture-templates nil))

    (chime-org-contacts--setup-capture-template)

    (should org-capture-templates)
    (should (assoc "C" org-capture-templates))))

(ert-deftest test-chime-org-contacts-setup-skips-when-file-nil ()
  "Test that template is not added when file is nil."
  (let ((chime-org-contacts-file nil)
        (org-capture-templates nil))

    (chime-org-contacts--setup-capture-template)

    (should-not org-capture-templates)))

(ert-deftest test-chime-org-contacts-setup-template-structure ()
  "Test that added template has correct structure."
  (let ((chime-org-contacts-file "/tmp/test-contacts.org")
        (chime-org-contacts-capture-key "C")
        (chime-org-contacts-heading "Contacts")
        (org-capture-templates nil))

    (chime-org-contacts--setup-capture-template)

    (let ((template (assoc "C" org-capture-templates)))
      (should (string= (nth 1 template) "Contact (chime)"))
      (should (eq (nth 2 template) 'entry))
      (should (equal (nth 3 template) '(file+headline chime-org-contacts-file "Contacts"))))))

(ert-deftest test-chime-org-contacts-setup-uses-custom-key ()
  "Test that template uses custom capture key."
  (let ((chime-org-contacts-file "/tmp/test-contacts.org")
        (chime-org-contacts-capture-key "K")
        (org-capture-templates nil))

    (chime-org-contacts--setup-capture-template)

    (should (assoc "K" org-capture-templates))
    (should-not (assoc "C" org-capture-templates))))

(provide 'test-chime-org-contacts)
;;; test-chime-org-contacts.el ends here
