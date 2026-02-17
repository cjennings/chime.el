;;; convert-org-contacts-birthdays.el --- Convert org-contacts birthdays to plain timestamps -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;; Author: Craig Jennings <c@cjennings.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: calendar, org-mode
;; URL: https://github.com/cjennings/chime.el

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

;; This utility converts org-contacts files IN-PLACE, adding birthday timestamps.
;;
;; Problem:
;; When using chime.el with org-contacts diary sexps like
;; %%(org-contacts-anniversaries), the async subprocess needs org-contacts
;; loaded or you get "Bad sexp" errors.
;;
;; Solution:
;; Convert your contacts.org file in-place by adding plain yearly repeating
;; timestamps after each contact's properties drawer. The :BIRTHDAY: property
;; is kept for vCard export compatibility.
;;
;; Usage:
;;   M-x chime-convert-contacts-in-place RET ~/org/contacts.org RET
;;
;; Input:  Contact with :BIRTHDAY: property
;;   * Alice Anderson
;;   :PROPERTIES:
;;   :EMAIL: alice@example.com
;;   :BIRTHDAY: 1985-03-15
;;   :END:
;;
;; Output: Same contact with added timestamp
;;   * Alice Anderson
;;   :PROPERTIES:
;;   :EMAIL: alice@example.com
;;   :BIRTHDAY: 1985-03-15
;;   :END:
;;   <1985-03-15 Fri +1y>
;;
;; After conversion:
;; 1. Comment out %%(org-contacts-anniversaries) from schedule.org
;; 2. Birthdays appear in agenda via plain timestamps
;; 3. Chime works without errors
;; 4. vCard export still works

;;; Code:

(require 'org)

(defun chime--extract-birthday-year (birthday-string)
  "Extract year from BIRTHDAY-STRING, handling various formats.
Returns nil if no year is present or if year should be ignored.
Handles formats like:
  2000-03-15
  03-15
  1985-12-25"
  (when (string-match "^\\([0-9]\\{4\\}\\)-[0-9]\\{2\\}-[0-9]\\{2\\}$" birthday-string)
    (string-to-number (match-string 1 birthday-string))))

(defun chime--parse-birthday (birthday-string)
  "Parse BIRTHDAY-STRING into (YEAR MONTH DAY) list.
YEAR may be nil if not present in the string.
Handles formats:
  2000-03-15 → (2000 3 15)
  03-15      → (nil 3 15)
  1985-12-25 → (1985 12 25)"
  (cond
   ;; Format: YYYY-MM-DD
   ((string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" birthday-string)
    (list (string-to-number (match-string 1 birthday-string))
          (string-to-number (match-string 2 birthday-string))
          (string-to-number (match-string 3 birthday-string))))
   ;; Format: MM-DD
   ((string-match "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" birthday-string)
    (list nil
          (string-to-number (match-string 1 birthday-string))
          (string-to-number (match-string 2 birthday-string))))
   (t
    (user-error "Cannot parse birthday format: %s (expected YYYY-MM-DD or MM-DD)" birthday-string))))

(defun chime--format-birthday-timestamp (year month day)
  "Format birthday as org timestamp.
If YEAR is nil, uses current year.
Returns yearly repeating timestamp like <2026-03-15 Sun +1y>."
  (let* ((use-year (or year (nth 5 (decode-time))))
         (time (encode-time 0 0 0 day month use-year))
         (dow (format-time-string "%a" time))
         (date-str (format "%04d-%02d-%02d" use-year month day)))
    (format "<%s %s +1y>" date-str dow)))

(defun chime--backup-contacts-file (contacts-file)
  "Create timestamped backup of CONTACTS-FILE.
Returns the backup file path."
  (let* ((backup-name (format "%s.backup-%s"
                             contacts-file
                             (format-time-string "%Y-%m-%d-%H%M%S")))
         (backup-path (expand-file-name backup-name)))
    (copy-file contacts-file backup-path)
    backup-path))

(defun chime--insert-birthday-timestamp-after-drawer (birthday-value)
  "Insert birthday timestamp after current properties drawer.
BIRTHDAY-VALUE is the value from :BIRTHDAY: property (YYYY-MM-DD or MM-DD).
Point should be at the heading with the properties drawer.
Does not insert if a yearly repeating timestamp already exists."
  (let* ((parsed (chime--parse-birthday birthday-value))
         (year (nth 0 parsed))
         (month (nth 1 parsed))
         (day (nth 2 parsed))
         (timestamp (chime--format-birthday-timestamp year month day))
         (heading-end (save-excursion (outline-next-heading) (point))))
    ;; Find end of properties drawer
    (when (re-search-forward "^[ \t]*:END:[ \t]*$" heading-end t)
      (let ((drawer-end (point)))
        ;; Check if a yearly repeating timestamp already exists between drawer end and next heading
        (goto-char drawer-end)
        (unless (re-search-forward "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*\\+1y>" heading-end t)
          ;; No existing yearly timestamp found, insert new one
          (goto-char drawer-end)
          (end-of-line)
          (insert "\n" timestamp))))))

(defun chime--process-contact-entry ()
  "Process current contact entry, adding birthday timestamp if needed.
Returns t if birthday was added, nil otherwise.
Point should be at the heading."
  (let ((birthday-value (org-entry-get (point) "BIRTHDAY")))
    (when (and birthday-value
               (not (string-blank-p birthday-value)))
      (chime--insert-birthday-timestamp-after-drawer birthday-value)
      t)))

(defun chime--convert-contacts-file-in-place (contacts-file)
  "Convert CONTACTS-FILE in-place, adding birthday timestamps.
Creates a backup first. Returns number of birthdays converted."
  (let ((count 0)
        (backup-file (chime--backup-contacts-file contacts-file)))
    (with-current-buffer (find-file-noselect contacts-file)
      (save-excursion
        (goto-char (point-min))
        ;; Process each heading
        (while (re-search-forward "^\\* " nil t)
          (beginning-of-line)
          (when (chime--process-contact-entry)
            (setq count (1+ count)))
          (outline-next-heading)))
      (save-buffer))
    (cons count backup-file)))

;;;###autoload
(defun chime-convert-contacts-in-place (contacts-file)
  "Convert org-contacts file IN-PLACE, adding birthday timestamps.

SAFETY: Creates timestamped backup before modifying the file.

What this does:
- Creates backup: contacts.org.backup-YYYY-MM-DD-HHMMSS
- For each contact with :BIRTHDAY: property:
  - Adds plain timestamp after properties drawer: <YYYY-MM-DD DayOfWeek +1y>
  - KEEPS the :BIRTHDAY: property (for vCard export)
- Writes changes to original file

After conversion:
1. Comment out %%(org-contacts-anniversaries) in your schedule file
2. Birthdays will appear in agenda via plain timestamps
3. Chime will work without errors
4. vCard export still works via :BIRTHDAY: property

Example:
  Before:
    * Alice Anderson
    :PROPERTIES:
    :EMAIL: alice@example.com
    :BIRTHDAY: 1985-03-15
    :END:

  After:
    * Alice Anderson
    :PROPERTIES:
    :EMAIL: alice@example.com
    :BIRTHDAY: 1985-03-15
    :END:
    <1985-03-15 Fri +1y>

Example usage:
  M-x chime-convert-contacts-in-place RET ~/org/contacts.org RET"
  (interactive "fContacts file to convert: ")

  (let ((contacts-file (expand-file-name contacts-file)))
    ;; Validate file exists
    (unless (file-exists-p contacts-file)
      (user-error "File does not exist: %s" contacts-file))

    ;; Safety confirmation
    (unless (yes-or-no-p
             (format "This will MODIFY %s (backup will be created). Continue? "
                     contacts-file))
      (user-error "Conversion cancelled"))

    ;; Perform conversion
    (let* ((result (chime--convert-contacts-file-in-place contacts-file))
           (count (car result))
           (backup-file (cdr result)))

      (if (= count 0)
          (progn
            (message "No birthdays found to convert in %s" contacts-file)
            (message "Backup created at: %s (you may want to delete it)" backup-file))
        (message "Converted %d birthday%s in %s"
                 count
                 (if (= count 1) "" "s")
                 contacts-file)
        (message "Backup saved to: %s" backup-file)
        (message "Next steps:")
        (message "  1. Comment out: %%(org-contacts-anniversaries)")
        (message "  2. Run M-x chime-check to verify")
        (message "  3. Delete backup if conversion looks good")))))

(provide 'convert-org-contacts-birthdays)
;;; convert-org-contacts-birthdays.el ends here
