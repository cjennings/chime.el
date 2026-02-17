;;; chime-org-contacts.el --- Optional org-contacts integration for chime -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Craig Jennings

;; Author: Craig Jennings <c@cjennings.net>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: calendar, org-mode, contacts
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

;; Optional org-contacts integration for chime.el
;;
;; This module provides an org-capture template that automatically inserts
;; birthday timestamps when creating new contacts. This complements the
;; chime conversion script (convert-org-contacts-birthdays.el) which handles
;; existing contacts.
;;
;; Usage:
;;   (setq chime-org-contacts-file "~/org/contacts.org")
;;
;; This will:
;; - Add an org-capture template (default key: "C")
;; - Automatically insert yearly repeating timestamps for birthdays
;; - Enable birthdays to appear in org-agenda without org-contacts loaded
;;
;; The integration is disabled by default. Set `chime-org-contacts-file'
;; to enable it.

;;; Code:

(require 'org)
(require 'org-capture)

;;; Customization

(defgroup chime-org-contacts nil
  "Org-contacts integration for chime.el."
  :group 'chime
  :prefix "chime-org-contacts-")

(defcustom chime-org-contacts-file nil
  "Path to org-contacts file for birthday timestamp integration.

When nil, org-contacts capture integration is disabled.

When set to a file path, chime will add an org-capture template
that automatically inserts birthday timestamps for new contacts,
enabling them to appear in org-agenda without requiring org-contacts
to be loaded in the async subprocess.

Example:
  (setq chime-org-contacts-file \"~/org/contacts.org\")"
  :type '(choice (const :tag "Disabled" nil)
                 (file :tag "Contacts file path"))
  :group 'chime-org-contacts)

(defcustom chime-org-contacts-capture-key "C"
  "Key binding for chime org-contacts capture template.

This is the key you press after invoking org-capture (C-c c by default).
Change this if you already have a capture template using \"C\"."
  :type 'string
  :group 'chime-org-contacts)

(defcustom chime-org-contacts-heading "Contacts"
  "Heading under which to file new contacts.

New contacts will be filed under this heading in `chime-org-contacts-file'."
  :type 'string
  :group 'chime-org-contacts)

;;; Implementation

(defun chime-org-contacts--parse-birthday (birthday-string)
  "Parse BIRTHDAY-STRING into (YEAR MONTH DAY) list.
YEAR may be current year if not present in the string.
Returns nil if parsing fails."
  (cond
   ;; Format: YYYY-MM-DD
   ((string-match "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" birthday-string)
    (list (string-to-number (match-string 1 birthday-string))
          (string-to-number (match-string 2 birthday-string))
          (string-to-number (match-string 3 birthday-string))))
   ;; Format: MM-DD (use current year)
   ((string-match "^\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" birthday-string)
    (list (nth 5 (decode-time))
          (string-to-number (match-string 1 birthday-string))
          (string-to-number (match-string 2 birthday-string))))
   (t nil)))

(defun chime-org-contacts--format-timestamp (year month day)
  "Format YEAR MONTH DAY as yearly repeating org timestamp."
  (let* ((time (encode-time 0 0 0 day month year))
         (dow (format-time-string "%a" time)))
    (format "<%04d-%02d-%02d %s +1y>" year month day dow)))

(defun chime-org-contacts--insert-timestamp-after-drawer (timestamp)
  "Insert TIMESTAMP after properties drawer if not already present."
  (let ((heading-end (save-excursion (outline-next-heading) (point))))
    (when (re-search-forward "^[ \t]*:END:[ \t]*$" heading-end t)
      (let ((end-pos (point)))
        ;; Only insert if no yearly timestamp already exists
        (unless (save-excursion
                  (goto-char end-pos)
                  (re-search-forward "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*\\+1y>" heading-end t))
          (goto-char end-pos)
          (end-of-line)
          (insert "\n" timestamp))))))

(defun chime-org-contacts--finalize-birthday-timestamp ()
  "Add yearly repeating timestamp after properties drawer if BIRTHDAY is set.

This function is called during org-capture finalization to automatically
insert a plain timestamp for birthdays, enabling them to appear in org-agenda
without requiring org-contacts to be loaded in the async subprocess."
  (when (string= (plist-get org-capture-plist :key) chime-org-contacts-capture-key)
    (save-excursion
      (goto-char (point-min))
      (let ((birthday (org-entry-get (point) "BIRTHDAY")))
        (when (and birthday (not (string-blank-p birthday)))
          (let ((parsed (chime-org-contacts--parse-birthday birthday)))
            (when parsed
              (let* ((year (nth 0 parsed))
                     (month (nth 1 parsed))
                     (day (nth 2 parsed))
                     (timestamp (chime-org-contacts--format-timestamp year month day)))
                (chime-org-contacts--insert-timestamp-after-drawer timestamp)))))))))

(defun chime-org-contacts--setup-capture-template ()
  "Add org-capture template for contacts with birthday timestamps.

This template will only be added if:
1. `chime-org-contacts-file' is set
2. `org-capture-templates' is available
3. The capture key is not already in use (warns if it is)"
  (when (and chime-org-contacts-file
             (boundp 'org-capture-templates))
    ;; Check if key is already in use
    (when (assoc chime-org-contacts-capture-key org-capture-templates)
      (warn "chime-org-contacts: Capture key \"%s\" already in use. Change `chime-org-contacts-capture-key' to use a different key."
            chime-org-contacts-capture-key))

    ;; Add the capture template
    (add-to-list 'org-capture-templates
                 `(,chime-org-contacts-capture-key
                   "Contact (chime)"
                   entry
                   (file+headline chime-org-contacts-file ,chime-org-contacts-heading)
                   "* %^{Name}
:PROPERTIES:
:EMAIL: %^{Email}
:PHONE: %^{Phone}
:ADDRESS: %^{Address}
:BIRTHDAY: %^{Birthday (YYYY-MM-DD or MM-DD)}
:NICKNAME: %^{Nickname}
:COMPANY: %^{Company}
:TITLE: %^{Title}
:WEBSITE: %^{Website}
:NOTE: %^{Note}
:END:
Added: %U"
                   :prepare-finalize chime-org-contacts--finalize-birthday-timestamp))))

;;; Activation

;; Set up the capture template when org-capture is loaded,
;; but only if chime-org-contacts-file is configured
(with-eval-after-load 'org-capture
  (when chime-org-contacts-file
    (chime-org-contacts--setup-capture-template)))

(provide 'chime-org-contacts)
;;; chime-org-contacts.el ends here
