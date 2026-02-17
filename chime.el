;;; chime.el --- CHIME Heralds Imminent Events -*- lexical-binding: t -*-

;; Copyright (C) 2017 Artem Khramov
;; Copyright (C) 2024-2025 Craig Jennings

;; Current Author/Maintainer: Craig Jennings <c@cjennings.net>
;; Original Author: Artem Khramov <akhramov+emacs@pm.me>
;; Created: 6 Jan 2017
;; Version: 0.6.0
;; Package-Requires: ((alert "1.2") (async "1.9.3") (dash "2.18.0") (emacs "26.1"))
;; Keywords: notification alert org org-agenda agenda calendar chime sound
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

;; CHIME (CHIME Heralds Imminent Events) - Customizable org-agenda notifications
;;
;; This package provides visual and audible notifications for upcoming org-agenda
;; events with modeline display of the next upcoming event.
;;
;; Features:
;; - Visual notifications with customizable alert times
;; - Audible chime sound when notifications appear
;; - Modeline display of next upcoming event
;; - Support for SCHEDULED, DEADLINE, and plain timestamps
;; - Repeating timestamp support (+1w, .+1d, ++1w)
;; - Async background checking (runs every minute)
;;
;; Quick Start:
;; (require 'chime)
;; (setq chime-alert-intervals '((5 . medium) (0 . high)))  ; 5 min before and at event time
;; (chime-mode 1)
;;
;; Manual check: M-x chime-check
;;
;; Notification intervals and severity can be customized globally via
;; `chime-alert-intervals'.
;;
;; Filter notifications using `chime-keyword-whitelist' and
;; `chime-keyword-blacklist' variables.
;;
;; See README.org for complete documentation.

;;; Code:

(require 'dash)
(require 'alert)
(require 'async)
(require 'org-agenda)
(require 'org-duration)
(require 'cl-lib)

;; Declare functions from chime-debug.el (loaded conditionally)
(declare-function chime-debug-monitor-event-loading "chime-debug")

(defgroup chime nil
  "Chime customization options."
  :group 'org)

(defcustom chime-alert-intervals '((10 . medium))
  "Alert intervals with severity levels for upcoming events.
Each element is a cons cell (MINUTES . SEVERITY) where:
- MINUTES: Number of minutes before event to notify (0 = at event time)
- SEVERITY: Alert urgency level (high, medium, or low)

Example configurations:
  ;; Single notification at event time with high urgency
  \\='((0 . high))

  ;; Multiple notifications with escalating urgency
  \\='((60 . low)      ;; 1 hour before: low urgency
    (30 . low)      ;; 30 min before: low urgency
    (10 . medium)   ;; 10 min before: medium urgency
    (0  . high))    ;; At event: high urgency

  ;; Same severity for all notifications
  \\='((15 . medium) (5 . medium) (0 . medium))

Each interval's severity affects how the notification is displayed
by your system's notification daemon."
  :package-version '(chime . "0.7.0")
  :group 'chime
  :type '(repeat (cons (integer :tag "Minutes before event")
                       (symbol :tag "Severity")))
  :set (lambda (symbol value)
         (unless (listp value)
           (user-error "chime-alert-intervals must be a list of cons cells, got: %S" value))
         (dolist (interval value)
           (unless (consp interval)
             (user-error "Each interval must be a cons cell (MINUTES . SEVERITY), got: %S" interval))
           (let ((minutes (car interval))
                 (severity (cdr interval)))
             (unless (integerp minutes)
               (user-error "Alert time must be an integer, got: %S" minutes))
             (when (< minutes 0)
               (user-error "Alert time cannot be negative, got: %d" minutes))
             (unless (memq severity '(high medium low))
               (user-error "Severity must be high, medium, or low, got: %S" severity))))
         (set-default symbol value)))

(defcustom chime-check-interval 60
  "How often to check for upcoming events, in seconds.
Chime will poll your agenda files at this interval to check for
notifications. Lower values make notifications more responsive but
increase system load. Higher values reduce polling overhead but may
delay notifications slightly.

Minimum recommended value: 10 seconds.
Default: 60 seconds (1 minute).

Note: Changes take effect after restarting chime-mode."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'integer
  :set (lambda (symbol value)
         (unless (integerp value)
           (user-error "Check interval must be an integer, got: %S" value))
         (when (< value 10)
           (warn "chime-check-interval: Values below 10 seconds may cause excessive polling and system load"))
         (when (<= value 0)
           (user-error "Check interval must be positive, got: %d" value))
         (set-default symbol value)))

(defcustom chime-notification-title "Agenda"
  "Notifications title."
  :package-version '(chime . "0.1.0")
  :group 'chime
  :type 'string)

(defcustom chime-notification-icon nil
  "Path to notification icon file."
  :package-version '(chime . "0.4.1")
  :group 'chime
  :type 'string)

(defcustom chime-keyword-whitelist nil
  "Receive notifications for these keywords only.
Leave this variable blank if you do not want to filter anything."
  :package-version '(chime . "0.2.2")
  :group 'chime
  :type '(repeat string))

(defcustom chime-keyword-blacklist nil
  "Never receive notifications for these keywords."
  :package-version '(chime . "0.2.2")
  :group 'chime
  :type '(repeat string))

(defcustom chime-tags-whitelist nil
  "Receive notifications for these tags only.
Leave this variable blank if you do not want to filter anything."
  :package-version '(chime . "0.3.1")
  :group 'chime
  :type '(repeat string))

(defcustom chime-tags-blacklist nil
  "Never receive notifications for these tags."
  :package-version '(chime . "0.3.1")
  :group 'chime
  :type '(repeat string))

(defcustom chime-display-time-format-string "%I:%M %p"
  "Format string for displaying event times.
Passed to `format-time-string' when displaying notification times.
Uses standard time format codes:
  %I - Hour (01-12, 12-hour format)
  %H - Hour (00-23, 24-hour format)
  %M - Minutes (00-59)
  %p - AM/PM designation (uppercase)
  %P - am/pm designation (lowercase)

Common formats:
  \"%I:%M %p\" -> \"02:30 PM\" (12-hour with AM/PM, default)
  \"%H:%M\"    -> \"14:30\" (24-hour)
  \"%I:%M%p\"  -> \"02:30PM\" (12-hour, no space before AM/PM)
  \"%l:%M %p\" -> \" 2:30 PM\" (12-hour, space-padded hour)

Note: Avoid using seconds (%S) as chime polls once per minute."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type 'string
  :set (lambda (symbol value)
         (when (and value (stringp value) (string-match-p "%S" value))
           (warn "chime-display-time-format-string: Using seconds (%%S) is not recommended as chime polls once per minute"))
         (set-default symbol value)))

(defcustom chime-time-left-format-at-event "right now"
  "Format string for when event time has arrived (0 or negative seconds).
This is a literal string with no format codes."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'string)

(defcustom chime-time-left-format-short "in %M"
  "Format string for times under 1 hour.
Uses `format-seconds' codes:
  %m - minutes as number only (e.g., \"37\")
  %M - minutes with unit name (e.g., \"37 minutes\")

Examples:
  \"in %M\"      -> \"in 37 minutes\"
  \"in %mm\"     -> \"in 37m\"
  \"%m min\"     -> \"37 min\""
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'string)

(defcustom chime-time-left-format-long "in %H %M"
  "Format string for times 1 hour or longer.
Uses `format-seconds' codes:
  %h - hours as number only (e.g., \"1\")
  %H - hours with unit name (e.g., \"1 hour\")
  %m - minutes as number only (e.g., \"37\")
  %M - minutes with unit name (e.g., \"37 minutes\")

Examples:
  \"in %H %M\"       -> \"in 1 hour 37 minutes\"
  \"in %hh %mm\"     -> \"in 1h 37m\"
  \"(%h hr %m min)\" -> \"(1 hr 37 min)\"
  \"%hh%mm\"         -> \"1h37m\""
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'string)

(defcustom chime-predicate-whitelist nil
  "Receive notifications for events matching these predicates only.
Each function should take an event POM and return non-nil iff that event should
trigger a notification. Leave this variable blank if you do not want to filter
anything."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type '(function))

(defcustom chime-additional-environment-regexes nil
  "Additional regular expressions for async environment injection.
These regexes are provided to `async-inject-environment' before
running the async command to check notifications."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type '(string))

(defcustom chime-predicate-blacklist
  '(chime-done-keywords-predicate)
  "Never receive notifications for events matching these predicates.
Each function should take an event POM and return non-nil iff that event should
not trigger a notification."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type '(function))

(defcustom chime-extra-alert-plist nil
  "Additional arguments that should be passed to invocations of `alert'."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type 'plist)

(defcustom chime-day-wide-alert-times '("08:00")
  "List of time strings for day-wide event alerts.
Each string specifies a time of day when day-wide events should trigger.
Defaults to 08:00 (morning reminder for all-day events happening today).
Set to nil to disable all-day event notifications entirely.

Example: \\='(\"08:00\" \"17:00\") for morning and evening reminders."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(repeat string))

(defcustom chime-show-any-overdue-with-day-wide-alerts t
  "Show any overdue TODO items along with day wide alerts whenever they are shown."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type 'boolean)

(defcustom chime-day-wide-advance-notice nil
  "Number of days before all-day events to show advance notifications.
When nil, only notify on the day of the event.
When 1, also notify the day before at `chime-day-wide-alert-times'.
When 2, notify two days before, etc.

Useful for events requiring preparation, such as birthdays (buying gifts)
or multi-day conferences (packing, travel arrangements).

Note: This only affects notifications, not tooltip/modeline display.

Example: With value 1 and alert times \\='(\"08:00\"), you'll get:
  - \"Blake's birthday is tomorrow\" at 08:00 the day before
  - \"Blake's birthday is today\" at 08:00 on the day"
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(choice (const :tag "Same day only" nil)
                 (integer :tag "Days in advance")))

(defcustom chime-tooltip-show-all-day-events t
  "Whether to show all-day events in the tooltip.
When nil, all-day events (birthdays, multi-day conferences, etc.) are
hidden from the tooltip but can still trigger notifications.
When t, all-day events appear in the tooltip for planning purposes.

All-day events are never shown in the modeline (only in tooltip).

This is useful for seeing upcoming birthdays, holidays, and multi-day
events without cluttering the modeline with non-time-sensitive items."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'boolean)

(defcustom chime-enable-modeline t
  "Whether to display upcoming events in the modeline.
When nil, chime will not modify the modeline at all."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'boolean)

(defcustom chime-modeline-lighter " 🔔"
  "Text to display in the modeline when chime-mode is enabled.
This is the mode lighter that appears in the modeline to indicate
chime-mode is active."
  :package-version '(chime . "0.7.0")
  :group 'chime
  :type 'string)

(defcustom chime-modeline-lookahead-minutes 60
  "Minutes ahead to look for next event to display in modeline.
Should be larger than notification alert times for advance awareness.
Set to 0 to disable modeline display.
This setting only takes effect when `chime-enable-modeline' is non-nil."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(integer :tag "Minutes"))

(defcustom chime-modeline-format " ⏰ %s"
  "Format string for modeline display.
%s will be replaced with the event description (time and title)."
  :package-version '(chime . "0.5.1")
  :group 'chime
  :type 'string)

(defcustom chime-calendar-url nil
  "URL to your calendar for browser access.
When set, left-clicking the modeline icon/text opens this URL in your
browser.  Right-clicking jumps to the next event in your org file.

Set this to your calendar's web interface, such as:
  - Google Calendar: \"https://calendar.google.com\"
  - Outlook: \"https://outlook.office.com/calendar\"
  - Custom calendar URL

When nil (default), left-click does nothing."
  :package-version '(chime . "0.7.0")
  :group 'chime
  :type '(choice (const :tag "No calendar URL" nil)
                 (string :tag "Calendar URL")))

(defcustom chime-tooltip-lookahead-hours 8760
  "Hours ahead to look for events in tooltip.
Separate from modeline lookahead window.
Default is 8760 hours (1 year), showing all future events.
The actual number of events shown is limited by
`chime-modeline-tooltip-max-events'.

Set to a smaller value to limit tooltip by time as well as count.
Example: Set to 24 to show only today's and tomorrow's events,
or keep at default to show next N events regardless of distance."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(integer :tag "Hours"))

(defcustom chime-modeline-tooltip-max-events 5
  "Maximum number of events to show in modeline tooltip.
Set to nil to show all events within tooltip lookahead window."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(choice (integer :tag "Maximum events")
                 (const :tag "Show all" nil)))

(defcustom chime-modeline-no-events-text " ⏰"
  "Text to display in modeline when no events are within lookahead window.
Shows an alarm icon by default.
When nil, nothing is shown in the modeline when no upcoming events.
When a string, that text is displayed.

This only applies when events exist beyond the lookahead window.
If there are no events at all, the modeline is always empty.

Examples:
  \" ⏰\"         - Alarm icon (default)
  \" 🔕\"           - Muted bell emoji
  nil              - Show nothing (clean modeline)
  \" No events\"   - Show text message"
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(choice (const :tag "Show nothing" nil)
                 (string :tag "Custom text")))

(defcustom chime-notification-text-format "%t at %T (%u)"
  "Format string for notification text display.
Available placeholders:
  %t - Event title
  %T - Event time (formatted per `chime-display-time-format-string')
  %u - Time until event (formatted per time-left format settings)

Examples:
  \"%t at %T (%u)\"  -> \"Team Meeting at 02:30 PM (in 10 minutes)\" (default)
  \"%t at %T\"       -> \"Team Meeting at 02:30 PM\" (no countdown)
  \"%t (%u)\"        -> \"Team Meeting (in 10 minutes)\" (no time)
  \"%t - %T\"        -> \"Team Meeting - 02:30 PM\" (custom separator)
  \"%t\"             -> \"Team Meeting\" (title only)"
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'string)

(defcustom chime-max-title-length nil
  "Maximum length for event titles in notifications.
When non-nil, truncate titles longer than this value with \"...\".
When nil, show full title without truncation.

This affects ONLY the event title (%t in `chime-notification-text-format'),
NOT the icon, time, or countdown. The icon is part of
`chime-modeline-format' and is added separately.

Examples (assuming format \"%t (%u)\" and icon \" ⏰ \"):
  nil  -> \" ⏰ Very Long Meeting Title That Goes On ( in 10m)\"
  25   -> \" ⏰ Very Long Meeting Titl... ( in 10m)\"
  15   -> \" ⏰ Very Long Me... ( in 10m)\"
  10   -> \" ⏰ Very Lo... ( in 10m)\"

The limit includes the \"...\" suffix (3 chars), so a limit of 15
means up to 12 chars of title plus \"...\".

Minimum recommended value: 10 characters."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(choice (const :tag "No truncation (show full title)" nil)
                 (integer :tag "Maximum title length"))
  :set (lambda (symbol value)
         (when (and value (integerp value) (< value 5))
           (warn "chime-max-title-length: Values below 5 may produce illegible titles"))
         (set-default symbol value)))

(defcustom chime-tooltip-header-format "Upcoming Events as of %a %b %d %Y @ %I:%M %p"
  "Format string for tooltip header showing current date/time.
Uses `format-time-string' codes.
See Info node `(elisp)Time Parsing' for details.

Common format codes:
  %a - Abbreviated weekday (Mon, Tue, ...)
  %A - Full weekday name (Monday, Tuesday, ...)
  %b - Abbreviated month (Jan, Feb, ...)
  %B - Full month name (January, February, ...)
  %d - Day of month, zero-padded (01-31)
  %e - Day of month, space-padded ( 1-31)
  %Y - Four-digit year (2025)
  %I - Hour (01-12, 12-hour format)
  %H - Hour (00-23, 24-hour format)
  %M - Minute (00-59)
  %p - AM/PM indicator

Default: \"Upcoming Events as of %a %b %d %Y @ %I:%M %p\"
Result: \"Upcoming Events as of Tue Nov 04 2025 @ 08:25 PM\""
  :package-version '(chime . "0.7.0")
  :group 'chime
  :type 'string)

(defcustom chime-play-sound t
  "Whether to play a sound when notifications are displayed.
When non-nil, plays the sound file specified in `chime-sound-file'."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'boolean)

(defcustom chime-sound-file
  (expand-file-name "sounds/chime.wav"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to sound file to play when notifications are displayed.
Defaults to the bundled chime.wav file.
Set to nil to disable sound completely (no sound file, no beep).
Should be an absolute path to a .wav, .au, or other sound file
supported by your system."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(choice (const :tag "No sound" nil)
                 (file :tag "Sound file path")))

(defcustom chime-startup-delay 10
  "Seconds to wait before first event check after chime-mode is enabled.
This delay allows org-agenda-files and related infrastructure to finish
loading before chime attempts to check for events.

Default of 10 seconds works well for most configurations. Adjust if:
- You have custom org-agenda-files setup that takes longer to initialize
- You want faster startup (reduce to 5) and know org is ready
- You see \"found 0 events\" messages (increase to 15 or 20)

Set to 0 to check immediately (not recommended unless you're sure
org-agenda-files is populated at startup)."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'integer
  :set (lambda (symbol value)
         (unless (and (integerp value) (>= value 0))
           (user-error "chime-startup-delay must be a non-negative integer, got: %s" value))
         (set-default symbol value)))

(defcustom chime-debug nil
  "Enable debug functions for troubleshooting chime behavior.
When non-nil, loads chime-debug.el which provides:
- `chime--debug-dump-events' - Show all stored upcoming events
- `chime--debug-dump-tooltip' - Show tooltip content
- `chime--debug-config' - Show complete configuration dump
- `chime-debug-monitor-event-loading' - Monitor event loading timing

These functions write detailed information to the *Messages* buffer
without cluttering the echo area.

When enabled, automatically monitors event loading to help diagnose
timing issues where the modeline takes a while to populate after
Emacs startup.

Set to t to enable debug functions:
  (setq chime-debug t)
  (require \\='chime)"
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'boolean)

;; Load debug functions if enabled
(when chime-debug
  (require 'chime-debug
           (expand-file-name "chime-debug.el"
                             (file-name-directory (or load-file-name buffer-file-name)))
           t))

;; Load org-contacts integration if configured
;; Note: The actual template setup happens in chime-org-contacts.el
;; when org-capture is loaded, so users can defer org loading
(with-eval-after-load 'org-capture
  (when (and (boundp 'chime-org-contacts-file)
             chime-org-contacts-file)
    (require 'chime-org-contacts
             (expand-file-name "chime-org-contacts.el"
                               (file-name-directory (or load-file-name buffer-file-name)))
             t)))

(defvar chime--timer nil
  "Timer value.")

(defvar chime--process nil
  "Currently-running async process.")

(defvar chime--agenda-buffer-name "*chime-agenda*"
  "Name for temporary \\='org-agenda\\=' buffer.")

(defvar chime--last-check-time (seconds-to-time 0)
  "Last time checked for events.")

(defvar chime--upcoming-events nil
  "List of upcoming events with full data for tooltip and clicking.
Each event includes marker, title, times, and intervals.")

(defvar chime--validation-done nil
  "Whether configuration validation has been performed.
Validation runs on the first call to `chime-check', after `chime-startup-delay'
has elapsed. This gives startup hooks time to populate org-agenda-files.")

(defvar chime--validation-retry-count 0
  "Number of times validation has failed and been retried.
Reset to 0 when validation succeeds. Used to provide graceful retry
behavior for users with async org-agenda-files initialization.")

(defcustom chime-validation-max-retries 3
  "Maximum number of times to retry validation before showing error.
When org-agenda-files is empty on startup, chime will retry validation
on each check cycle (every `chime-check-interval' seconds) until either:
  - Validation succeeds (org-agenda-files is populated)
  - This retry limit is exceeded (error is shown)

This accommodates users with async initialization code that populates
org-agenda-files after a delay (e.g., via idle timers).

Set to 0 to show errors immediately without retrying.
Default is 3 retries (with 30-60s check intervals, this gives ~1.5-3 minutes
for org-agenda-files to be populated)."
  :type 'integer
  :group 'chime)

(defvar chime-modeline-string nil
  "Modeline string showing next upcoming event.")
;;;###autoload(put 'chime-modeline-string 'risky-local-variable t)
(put 'chime-modeline-string 'risky-local-variable t)

(defun chime--time= (&rest list)
  "Compare timestamps.
Comparison is performed by converted each element of LIST onto string
in order to ignore seconds."
  (->> list
       (--map (format-time-string "%Y-%m-%d %H:%M" it))
       (-uniq)
       (length)
       (= 1)))

(defun chime--today ()
  "Get the timestamp for the beginning of current day."
  (apply 'encode-time
         (append '(0 0 0) (nthcdr 3 (decode-time (current-time))))))

(defun chime--timestamp-within-interval-p (timestamp interval)
  "Check whether TIMESTAMP is within notification INTERVAL.
Returns non-nil if TIMESTAMP matches current time plus INTERVAL minutes.
Returns nil if TIMESTAMP or INTERVAL is invalid."
  (and timestamp
       interval
       (numberp interval)
       ;; Validate timestamp is a proper time value (accepts list, integer, or float)
       (or (listp timestamp) (numberp timestamp))
       (chime--time=
        (time-add (current-time) (seconds-to-time (* 60 interval)))
        timestamp)))

(defun chime--notifications (event)
  "Get notifications for given EVENT.
Returns a list of time information interval pairs.
Each pair is ((TIMESTAMP . TIME-VALUE) (MINUTES . SEVERITY))."
  (->> (list
        (chime--filter-day-wide-events (cdr (assoc 'times event)))
        (cdr (assoc 'intervals event)))
         (apply '-table-flat (lambda (ts int) (list ts int)))
         ;; When no values are provided for table flat, we get the second values
         ;; paired with nil.
         (--filter (not (null (car it))))
         ;; Extract minutes from (minutes . severity) cons for time matching
         (--filter (chime--timestamp-within-interval-p (cdar it) (car (cadr it))))))

(defun chime--has-timestamp (s)
  "Check if S contain a timestamp with a time component.
Returns non-nil only if the timestamp includes HH:MM time information."
  (and s
       (stringp s)
       (string-match org-ts-regexp0 s)
       (match-beginning 7)))

(defun chime--filter-day-wide-events (times)
  "Filter TIMES list to include only events with timestamps."
  (--filter (chime--has-timestamp (car it)) times))

(defun chime--time-left (seconds)
  "Human-friendly representation for SECONDS.
Format is controlled by `chime-time-left-format-at-event',
`chime-time-left-format-short', and `chime-time-left-format-long'."
  (-> seconds
       (pcase
         ((pred (>= 0)) chime-time-left-format-at-event)
         ((pred (>= 3600)) chime-time-left-format-short)
         (_ chime-time-left-format-long))

       (format-seconds seconds)))

(defun chime--get-hh-mm-from-org-time-string (time-string)
  "Convert given org time-string TIME-STRING into string with \\='hh:mm\\=' format."
  (format-time-string
   chime-display-time-format-string
   (encode-time (org-parse-time-string time-string))))

(defun chime--truncate-title (title)
  "Truncate TITLE to `chime-max-title-length' if set.
Returns the truncated title with \"...\" appended if truncated,
or the original title if no truncation is needed.
Returns empty string if TITLE is nil."
  (let ((title-str (or title "")))
    (if (and chime-max-title-length
             (integerp chime-max-title-length)
             (> chime-max-title-length 0)
             (> (length title-str) chime-max-title-length))
        (concat (substring title-str 0 (max 0 (- chime-max-title-length 3))) "...")
      title-str)))

(defun chime--notification-text (str-interval event)
  "For given STR-INTERVAL list and EVENT get notification wording.
STR-INTERVAL is (TIMESTAMP-STRING . (MINUTES . SEVERITY)).
Format is controlled by `chime-notification-text-format'.
Title is truncated per `chime-max-title-length' if set."
  (let* ((title (cdr (assoc 'title event)))
         (minutes (car (cdr str-interval))))
    (format-spec chime-notification-text-format
                 `((?t . ,(chime--truncate-title title))
                   (?T . ,(chime--get-hh-mm-from-org-time-string (car str-interval)))
                   (?u . ,(chime--time-left (* 60 minutes)))))))

(defun chime-get-minutes-into-day (time)
  "Get minutes elapsed since midnight for TIME string."
  (org-duration-to-minutes (org-get-time-of-day time t)))

(defun chime-get-hours-minutes-from-time (time-string)
  "Extract hours and minutes from TIME-STRING.
Returns a list of (HOURS MINUTES)."
  (let ((total-minutes (truncate (chime-get-minutes-into-day time-string))))
    (list (/ total-minutes 60)
          (mod total-minutes 60))))

(defun chime-set-hours-minutes-for-time (time hours minutes)
  "Set HOURS and MINUTES for TIME, preserving date components."
  (cl-destructuring-bind (_s _m _h day month year dow dst utcoff) (decode-time time)
    (encode-time 0 minutes hours day month year dow dst utcoff)))

(defun chime-current-time-matches-time-of-day-string (time-of-day-string)
  "Check if current time matches TIME-OF-DAY-STRING."
  (let ((now (current-time)))
    (chime--time=
     now
     (apply 'chime-set-hours-minutes-for-time
            now
            (chime-get-hours-minutes-from-time time-of-day-string)))))

(defun chime-current-time-is-day-wide-time ()
  "Check if current time matches any day-wide alert time."
  (--any (chime-current-time-matches-time-of-day-string it)
         chime-day-wide-alert-times))

(defun chime-day-wide-notifications (events)
  "Generate notification texts for day-wide EVENTS.
Returns a list of (MESSAGE . SEVERITY) cons cells with \\='medium severity."
  (->> events
       (-filter 'chime-display-as-day-wide-event)
       (-map 'chime--day-wide-notification-text)
       (-uniq)
       ;; Wrap messages in cons cells with default 'medium' severity
       (--map (cons it 'medium))))

(defun chime-display-as-day-wide-event (event)
  "Check if EVENT should be displayed as a day-wide event.
Considers both events happening today and advance notices for future events.

When `chime-show-any-overdue-with-day-wide-alerts' is t (default):
  - Shows overdue TODO items (timed events that passed)
  - Shows all-day events from today or earlier

When nil:
  - Shows only today's events (both timed and all-day)
  - Hides overdue items from past days"
  (or
   ;; Events happening today or in the past
   (and (chime-event-has-any-passed-time event)
        (or chime-show-any-overdue-with-day-wide-alerts
            ;; When overdue alerts disabled, only show today's events
            (chime-event-is-today event)))
   ;; Advance notice for upcoming all-day events
   (and chime-day-wide-advance-notice
        (chime-event-has-any-day-wide-timestamp event)
        (chime-event-within-advance-notice-window event))))

(defun chime-event-has-any-day-wide-timestamp (event)
  "Check if EVENT has any day-wide (no time component) timestamps."
  (--any (not (chime--has-timestamp (car it)))
         (cdr (assoc 'times event))))

(defun chime-event-within-advance-notice-window (event)
  "Check if EVENT has any day-wide timestamps within advance notice window.
Returns t if any all-day timestamp is between tomorrow and N days from now,
where N is `chime-day-wide-advance-notice'."
  (when chime-day-wide-advance-notice
    (let* ((now (current-time))
           ;; Calculate time range: start of tomorrow to end of N days from now
           (window-end (time-add now (seconds-to-time
                                      (* 86400 (1+ chime-day-wide-advance-notice)))))
           (all-times (cdr (assoc 'times event))))
      (--any
       (when-let* ((timestamp-str (car it))
                   ;; Only check all-day events (those without time component)
                   (is-all-day (not (chime--has-timestamp timestamp-str)))
                   ;; Parse the date portion even without time
                   (parsed (org-parse-time-string timestamp-str))
                   ;; Use nth accessors for Emacs 26 compatibility
                   (year (nth 5 parsed))
                   (month (nth 4 parsed))
                   (day (nth 3 parsed)))
         ;; Convert to time at start of day (00:00:00)
         (let ((event-time (encode-time 0 0 0 day month year)))
           ;; Check if event is within the advance notice window
           (and (time-less-p now event-time)           ;; Event is in future
                (time-less-p event-time window-end)))) ;; Event is within window
       all-times))))

(defun chime-event-has-any-passed-time (event)
  "Check if EVENT has any timestamps in the past or today.
For all-day events, checks if the date is today or earlier."
  (--any
   (let ((timestamp-str (car it))
         (parsed-time (cdr it)))
     (if parsed-time
         ;; Timed event: check if time has passed
         (time-less-p parsed-time (current-time))
       ;; All-day event: check if date is today or earlier
       (when-let* ((parsed (org-parse-time-string timestamp-str))
                   (year (nth 5 parsed))
                   (month (nth 4 parsed))
                   (day (nth 3 parsed)))
         (let* ((event-date (encode-time 0 0 0 day month year))
                (today-start (let ((now (decode-time (current-time))))
                               (encode-time 0 0 0
                                           (decoded-time-day now)
                                           (decoded-time-month now)
                                           (decoded-time-year now)))))
           (not (time-less-p today-start event-date))))))
   (cdr (assoc 'times event))))

(defun chime-event-is-today (event)
  "Check if EVENT has any timestamps that are specifically today (not past days).
For all-day events, checks if the date is exactly today.
For timed events, checks if the time is today (past or future)."
  (--any
   (let ((timestamp-str (car it))
         (parsed-time (cdr it)))
     (if parsed-time
         ;; Timed event: check if it's today (could be future time today)
         (let* ((decoded (decode-time parsed-time))
                (event-day (decoded-time-day decoded))
                (event-month (decoded-time-month decoded))
                (event-year (decoded-time-year decoded))
                (today (decode-time))
                (today-day (decoded-time-day today))
                (today-month (decoded-time-month today))
                (today-year (decoded-time-year today)))
           (and (= event-day today-day)
                (= event-month today-month)
                (= event-year today-year)))
       ;; All-day event: check if date is exactly today
       (when-let* ((parsed (org-parse-time-string timestamp-str))
                   (year (nth 5 parsed))
                   (month (nth 4 parsed))
                   (day (nth 3 parsed)))
         (let* ((event-date (encode-time 0 0 0 day month year))
                (today-start (let ((now (decode-time (current-time))))
                               (encode-time 0 0 0
                                           (decoded-time-day now)
                                           (decoded-time-month now)
                                           (decoded-time-year now)))))
           (time-equal-p event-date today-start)))))
   (cdr (assoc 'times event))))

(defun chime--day-wide-notification-text (event)
  "Generate notification text for day-wide EVENT.
Handles both same-day events and advance notices."
  (let* ((title (cdr (assoc 'title event)))
         (all-times (cdr (assoc 'times event)))
         (is-today (chime-event-has-any-passed-time event))
         (is-advance-notice (and chime-day-wide-advance-notice
                                (chime-event-within-advance-notice-window event))))
    (cond
     ;; Event is today
     (is-today
      (format "%s is due or scheduled today" title))
     ;; Event is within advance notice window
     (is-advance-notice
      ;; Calculate days until event
      (let* ((now (current-time))
             (days-until
              (-min
               (--map
                (when-let* ((timestamp-str (car it))
                           (is-all-day (not (chime--has-timestamp timestamp-str)))
                           (parsed (org-parse-time-string timestamp-str))
                           ;; Use nth accessors for Emacs 26 compatibility
                           (year (nth 5 parsed))
                           (month (nth 4 parsed))
                           (day (nth 3 parsed)))
                  (let* ((event-time (encode-time 0 0 0 day month year))
                         (seconds-until (time-subtract event-time now))
                         (days (/ (float-time seconds-until) 86400.0)))
                    (ceiling days)))
                all-times))))
        (cond
         ((= days-until 1)
          (format "%s is tomorrow" title))
         ((= days-until 2)
          (format "%s is in 2 days" title))
         (t
          (format "%s is in %d days" title days-until)))))
     ;; Fallback (shouldn't happen)
     (t
      (format "%s is due or scheduled today" title)))))

(defun chime--check-event (event)
  "Get notifications for given EVENT.
Returns a list of (MESSAGE . SEVERITY) cons cells."
  (->> (chime--notifications event)
       (--map (let* ((notif it)
                     (timestamp-str (caar notif))
                     (interval-cons (cadr notif))  ; (minutes . severity)
                     (severity (cdr interval-cons))
                     (message (chime--notification-text
                              `(,timestamp-str . ,interval-cons)
                              event)))
                (cons message severity)))))

(defun chime--jump-to-event (event)
  "Jump to EVENT's org entry in its file.
Reconstructs marker from serialized file path and position."
  (interactive)
  (when-let* ((file (cdr (assoc 'marker-file event)))
              (pos (cdr (assoc 'marker-pos event))))
    (when (file-exists-p file)
      (find-file file)
      (goto-char pos)
      ;; Use org-fold-show-entry (Org 9.6+) if available, fallback to org-show-entry
      (if (fboundp 'org-fold-show-entry)
          (org-fold-show-entry)
        (with-no-warnings
          (org-show-entry))))))

(defun chime--open-calendar-url ()
  "Open calendar URL in browser if `chime-calendar-url' is set."
  (interactive)
  (when chime-calendar-url
    (browse-url chime-calendar-url)))

(defun chime--jump-to-first-event ()
  "Jump to first event in `chime--upcoming-events' list."
  (interactive)
  (when-let* ((first-event (car chime--upcoming-events))
              (event (car first-event)))
    (chime--jump-to-event event)))

(defun chime--format-event-for-tooltip (event-time-str minutes-until title)
  "Format a single event line for tooltip display.
EVENT-TIME-STR is the time string, MINUTES-UNTIL is minutes until event,
TITLE is the event title."
  (let ((time-display (chime--get-hh-mm-from-org-time-string event-time-str))
        (countdown (cond
                    ((< minutes-until 1440) ;; Less than 24 hours
                     (format "(%s)" (chime--time-left (* minutes-until 60))))
                    (t
                     ;; 24+ hours: show days and hours
                     (let* ((days (truncate (/ minutes-until 1440)))
                            (remaining-minutes (truncate (mod minutes-until 1440)))
                            (hours (truncate (/ remaining-minutes 60))))
                       (if (> hours 0)
                           (format "(in %d day%s %d hour%s)"
                                   days (if (= days 1) "" "s")
                                   hours (if (= hours 1) "" "s"))
                         (format "(in %d day%s)"
                                 days (if (= days 1) "" "s"))))))))
    (format "%s at %s %s" title time-display countdown)))

(defun chime--group-events-by-day (upcoming-events)
  "Group UPCOMING-EVENTS by day.
Returns an alist of (DATE-STRING . EVENTS-LIST)."
  (let ((grouped '())
        (now (current-time)))
    (dolist (item upcoming-events)
      (let* ((event-time (cdr (nth 1 item)))
             (_minutes-until (nth 2 item))
             ;; Get date components for calendar day comparison
             (now-decoded (decode-time now))
             (event-decoded (decode-time event-time)))
        (when event-decoded
          (let* ((now-day (decoded-time-day now-decoded))
                 (now-month (decoded-time-month now-decoded))
                 (now-year (decoded-time-year now-decoded))
                 (event-day (decoded-time-day event-decoded))
                 (event-month (decoded-time-month event-decoded))
                 (event-year (decoded-time-year event-decoded))
                 ;; Check if same calendar day (not just < 24 hours)
                 (same-day-p (and (= now-day event-day)
                                 (= now-month event-month)
                                 (= now-year event-year)))
                 ;; Check if tomorrow (next calendar day)
                 (tomorrow-decoded (decode-time (time-add now (days-to-time 1))))
                 (tomorrow-p (and (= event-day (decoded-time-day tomorrow-decoded))
                                 (= event-month (decoded-time-month tomorrow-decoded))
                                 (= event-year (decoded-time-year tomorrow-decoded))))
                 (date-string (cond
                               (same-day-p
                                (format-time-string "Today, %b %d" now))
                               (tomorrow-p
                                (format-time-string "Tomorrow, %b %d"
                                                   (time-add now (days-to-time 1))))
                               (t ;; Future days
                                (format-time-string "%A, %b %d" event-time)))))
            (let ((day-group (assoc date-string grouped)))
              (if day-group
                  (setcdr day-group (append (cdr day-group) (list item)))
                (push (cons date-string (list item)) grouped)))))))
    (nreverse grouped)))

(defun chime--make-tooltip (upcoming-events)
  "Generate tooltip text showing UPCOMING-EVENTS grouped by day."
  (if (null upcoming-events)
      nil
    (let* ((max-events (or chime-modeline-tooltip-max-events (length upcoming-events)))
           (events-to-show (seq-take upcoming-events max-events))
           (remaining (- (length upcoming-events) (length events-to-show)))
           (grouped (chime--group-events-by-day events-to-show))
           (header (concat (format-time-string chime-tooltip-header-format) "\n"))
           (lines (list header)))
      ;; Build tooltip text
      (dolist (day-group grouped)
        (let ((date-str (car day-group))
              (day-events (cdr day-group)))
          (push (format "\n%s:\n" date-str) lines)
          (push "─────────────\n" lines)
          (dolist (item day-events)
            (let* ((event (car item))
                   (event-time-str (car (nth 1 item)))
                   (minutes-until (nth 2 item))
                   (title (cdr (assoc 'title event))))
              (push (format "%s\n"
                           (chime--format-event-for-tooltip
                            event-time-str minutes-until title))
                    lines)))))
      ;; Add "... and N more" if needed
      (when (> remaining 0)
        (push (format "\n... and %d more event%s"
                     remaining
                     (if (> remaining 1) "s" ""))
              lines))
      (apply #'concat (nreverse lines)))))

(defun chime--make-no-events-tooltip (lookahead-minutes)
  "Generate tooltip text when no events exist within LOOKAHEAD-MINUTES."
  (let* ((hours (/ lookahead-minutes 60))
         (days (/ hours 24))
         (timeframe (cond
                     ((>= days 7) (format "%d days" days))
                     ((>= hours 24) (format "%.1f days" (/ hours 24.0)))
                     ((>= hours 1) (format "%d hours" hours))
                     (t (format "%d minutes" lookahead-minutes))))
         (header (format-time-string chime-tooltip-header-format))
         (increase-var "chime-tooltip-lookahead-hours"))
    (concat header "\n"
            "─────────────────────\n"
            (format "No calendar events in\nthe next %s.\n\n" timeframe)
            (format "Increase `%s`\nto expand scope.\n\n" increase-var)
            "Left-click: Open calendar")))

(defun chime--propertize-modeline-string (text soonest-event)
  "Add tooltip and click handlers to modeline TEXT for SOONEST-EVENT.
Left-click opens calendar URL (if set), right-click jumps to event."
  (if (null chime--upcoming-events)
      text
    (let ((map (make-sparse-keymap))
          (tooltip (chime--make-tooltip chime--upcoming-events)))
      ;; Left-click: open calendar URL
      (define-key map [mode-line mouse-1] #'chime--open-calendar-url)
      ;; Right-click: jump to event
      (define-key map [mode-line mouse-3]
        (lambda ()
          (interactive)
          (chime--jump-to-event soonest-event)))
      (propertize text
                  'help-echo tooltip
                  'mouse-face 'mode-line-highlight
                  'local-map map))))

(defun chime--deduplicate-events-by-title (upcoming-events)
  "Deduplicate UPCOMING-EVENTS by title, keeping soonest occurrence.

UPCOMING-EVENTS should be a list where each element is
\(EVENT TIME-INFO MINUTES).
Returns a new list with only the soonest occurrence of each
unique title.

This prevents recurring events from appearing multiple times in
the tooltip when `org-agenda-list' expands them into separate
event objects."
  (let ((title-hash (make-hash-table :test 'equal)))
    (dolist (item upcoming-events)
      (let* ((event (car item))
             (title (cdr (assoc 'title event)))
             (minutes (caddr item))
             (existing (gethash title title-hash)))
        ;; Only keep if this is the first occurrence or soonest so far
        (when (or (not existing)
                  (< minutes (caddr existing)))
          (puthash title item title-hash))))
    (hash-table-values title-hash)))

(defun chime--find-soonest-time-in-window (times now lookahead-minutes)
  "Find soonest time from TIMES list within LOOKAHEAD-MINUTES from NOW.
TIMES is a list of (TIME-STRING . TIME-OBJECT) cons cells.
Returns (TIME-STRING . TIME-OBJECT MINUTES-UNTIL) or nil if none found."
  (let ((soonest-time-info nil)
        (soonest-minutes nil))
    (dolist (time-info times)
      (when-let* ((time-str (car time-info))
                  (event-time (cdr time-info))
                  (seconds-until (- (float-time event-time) (float-time now)))
                  (minutes-until (/ seconds-until 60)))
        (when (and (> minutes-until 0)
                   (<= minutes-until lookahead-minutes))
          (when (or (not soonest-minutes)
                    (< minutes-until soonest-minutes))
            (setq soonest-minutes minutes-until)
            (setq soonest-time-info time-info)))))
    (when soonest-time-info
      (list (car soonest-time-info) (cdr soonest-time-info) soonest-minutes))))

(defun chime--build-upcoming-events-list (events now tooltip-lookahead-minutes show-all-day-p)
  "Build list of upcoming events within TOOLTIP-LOOKAHEAD-MINUTES from NOW.
EVENTS is the list of events to process.
If SHOW-ALL-DAY-P is non-nil, include all-day events in the list.
Returns sorted, deduplicated list of (EVENT TIME-INFO MINUTES-UNTIL) tuples."
  (let ((upcoming '()))
    ;; Collect events with their soonest timestamp within tooltip window
    (dolist (event events)
      (let* ((all-times (cdr (assoc 'times event)))
             (times-for-tooltip (if show-all-day-p
                                    all-times
                                  (chime--filter-day-wide-events all-times)))
             (soonest (chime--find-soonest-time-in-window
                      times-for-tooltip now tooltip-lookahead-minutes)))
        (when soonest
          (push (list event (cons (nth 0 soonest) (nth 1 soonest)) (nth 2 soonest))
                upcoming))))
    ;; Sort by time (soonest first)
    (setq upcoming (sort upcoming (lambda (a b) (< (nth 2 a) (nth 2 b)))))
    ;; Deduplicate by title - keep only soonest occurrence
    (setq upcoming (chime--deduplicate-events-by-title upcoming))
    ;; Re-sort after deduplication
    (sort upcoming (lambda (a b) (< (nth 2 a) (nth 2 b))))))

(defun chime--find-soonest-modeline-event (events now modeline-lookahead-minutes)
  "Find soonest timed event for modeline from EVENTS within MODELINE-LOOKAHEAD-MINUTES.
NOW is the current time.
Returns (EVENT TIME-STR MINUTES-UNTIL EVENT-TEXT) or nil if none found."
  (let ((soonest-event nil)
        (soonest-event-text nil)
        (soonest-minutes nil)
        (soonest-time-info nil))
    (dolist (event events)
      (let* ((all-times (cdr (assoc 'times event)))
             ;; Always filter all-day events for modeline (need specific time)
             (times-for-modeline (chime--filter-day-wide-events all-times))
             (soonest (chime--find-soonest-time-in-window
                      times-for-modeline now modeline-lookahead-minutes)))
        (when soonest
          (let ((minutes (nth 2 soonest)))
            (when (or (not soonest-minutes)
                      (< minutes soonest-minutes))
              (setq soonest-minutes minutes)
              (setq soonest-event event)
              (setq soonest-time-info (cons (nth 0 soonest) (nth 1 soonest)))
              (setq soonest-event-text
                    (chime--notification-text
                     `(,(car soonest-time-info) . (,soonest-minutes . medium))
                     event)))))))
    (when soonest-event
      (list soonest-event (car soonest-time-info) soonest-minutes soonest-event-text))))

(defun chime--update-modeline (events)
  "Update modeline with next upcoming event from EVENTS.
Orchestrates filtering, finding soonest event, and updating display.
Shows soonest event within `chime-modeline-lookahead-minutes' in modeline.
Tooltip shows events within `chime-tooltip-lookahead-hours' hours."
  (if (or (not chime-enable-modeline)
          (not chime-modeline-lookahead-minutes)
          (zerop chime-modeline-lookahead-minutes))
      (progn
        (setq chime-modeline-string nil)
        (setq chime--upcoming-events nil))
    (let* ((now (current-time))
           (tooltip-lookahead-minutes (if chime-tooltip-lookahead-hours
                                          (* chime-tooltip-lookahead-hours 60)
                                        chime-modeline-lookahead-minutes))
           ;; Build list of upcoming events for tooltip
           (upcoming (chime--build-upcoming-events-list
                     events now tooltip-lookahead-minutes
                     chime-tooltip-show-all-day-events))
           ;; Find soonest event for modeline display
           (soonest-modeline (chime--find-soonest-modeline-event
                             events now chime-modeline-lookahead-minutes)))
      ;; Store upcoming events for tooltip
      (setq chime--upcoming-events upcoming)
      ;; Format and set modeline string
      (setq chime-modeline-string
            (if soonest-modeline
                ;; Show soonest event in modeline
                (chime--propertize-modeline-string
                 (format chime-modeline-format (nth 3 soonest-modeline))
                 (nth 0 soonest-modeline))
              ;; Show icon when no event in modeline window
              (when chime-modeline-no-events-text
                (let ((map (make-sparse-keymap))
                      (tooltip-text (if upcoming
                                        (chime--make-tooltip upcoming)
                                      (chime--make-no-events-tooltip tooltip-lookahead-minutes))))
                  (define-key map [mode-line mouse-1] #'chime--open-calendar-url)
                  (when upcoming
                    (define-key map [mode-line mouse-3] #'chime--jump-to-first-event))
                  (propertize chime-modeline-no-events-text
                             'help-echo tooltip-text
                             'mouse-face 'mode-line-highlight
                             'local-map map)))))
      ;; Force update ALL windows/modelines
      (force-mode-line-update t))))

(defun chime--get-tags (marker)
  "Retrieve tags of MARKER."
  (-> (org-entry-get marker "TAGS")
      (or "")
      (org-split-string  ":")))

(defun chime--whitelist-predicates ()
  "Return list of whitelist predicate functions.
Combines keyword, tag, and custom predicate whitelists."
  (->> `([,chime-keyword-whitelist
          (lambda (it)
            (-contains-p chime-keyword-whitelist
                         (org-with-point-at it (org-get-todo-state))))]

         [,chime-tags-whitelist
          (lambda (it)
            (-intersection chime-tags-whitelist
                           (chime--get-tags it)))]

         [,chime-predicate-whitelist
          (lambda (marker)
            (--some? (funcall it marker) chime-predicate-whitelist))])
       (--filter (aref it 0))
       (--map (aref it 1))))

(defun chime--blacklist-predicates ()
  "Return list of blacklist predicate functions.
Combines keyword, tag, and custom predicate blacklists."
  (->> `([,chime-keyword-blacklist
          (lambda (it)
            (-contains-p chime-keyword-blacklist
                         (org-with-point-at it (org-get-todo-state))))]

         [,chime-tags-blacklist
          (lambda (it)
            (-intersection chime-tags-blacklist
                           (chime--get-tags it)))]

         [,chime-predicate-blacklist
          (lambda (marker)
            (--some? (funcall it marker) chime-predicate-blacklist))])
       (--filter (aref it 0))
       (--map (aref it 1))))

(defun chime-done-keywords-predicate (marker)
  "Check if entry at MARKER has a done keyword."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (member (nth 2 (org-heading-components)) org-done-keywords))))

(defun chime--apply-whitelist (markers)
  "Apply whitelist to MARKERS."
  (-if-let (whitelist-predicates (chime--whitelist-predicates))
      (-> (apply '-orfn whitelist-predicates)
          (-filter markers))
    markers))

(defun chime--apply-blacklist (markers)
  "Apply blacklist to MARKERS."
  (-if-let (blacklist-predicates (chime--blacklist-predicates))
      (-> (apply '-orfn blacklist-predicates)
          (-remove markers))
    markers))

(defconst chime-default-environment-regex
  (macroexpand
   `(rx string-start
        (or ,@(mapcar (lambda (literal) (list 'literal literal))
                (list
                 "org-agenda-files"
                 "load-path"
                 "org-todo-keywords"
                 "chime-alert-intervals"
                 "chime-keyword-whitelist"
                 "chime-keyword-blacklist"
                 "chime-tags-whitelist"
                 "chime-tags-blacklist"
                 "chime-predicate-whitelist"
                 "chime-predicate-blacklist")))
        string-end)))


(defun chime-environment-regex ()
  "Generate regex for environment variables to inject into async process."
  (macroexpand
   `(rx (or
         ,@(mapcar (lambda (regexp) (list 'regexp regexp))
                   (cons chime-default-environment-regex
                         chime-additional-environment-regexes))))))

(defun chime--retrieve-events ()
  "Get events from agenda view."
  `(lambda ()
    (setf org-agenda-use-time-grid nil)
    (setf org-agenda-compact-blocks t)
    ,(async-inject-variables (chime-environment-regex))

    (package-initialize)
    (require 'chime)

    ;; Load optional dependencies for org-mode diary sexps
    ;; Many users have sexp entries like %%(org-contacts-anniversaries) in their
    ;; org files, which generate dynamic agenda entries. These sexps are evaluated
    ;; when org-agenda-list runs, so the required packages must be loaded in this
    ;; async subprocess. We use (require ... nil t) to avoid errors if packages
    ;; aren't installed - the sexp will simply fail gracefully with a "Bad sexp"
    ;; warning that won't break event retrieval.
    (require 'org-contacts nil t)

    ;; Calculate agenda span based on max lookahead (convert to days, round up)
    ;; Use the larger of modeline-lookahead (minutes) and tooltip-lookahead (hours) to ensure
    ;; we fetch enough events for both. Add 1 day buffer to account for partial days.
    (let* ((tooltip-lookahead-minutes (if chime-tooltip-lookahead-hours
                                           (* chime-tooltip-lookahead-hours 60)
                                         chime-modeline-lookahead-minutes))
           (max-lookahead-minutes (max chime-modeline-lookahead-minutes tooltip-lookahead-minutes))
           (max-lookahead-days (ceiling (/ max-lookahead-minutes 1440.0)))
           (agenda-span (+ max-lookahead-days 1)))
      (org-agenda-list agenda-span (org-read-date nil nil "today")))

    (->> (org-split-string (buffer-string) "\n")
         (--map (plist-get
                 (org-fix-agenda-info (text-properties-at 0 it))
                 'org-marker))
         (-non-nil)
         (chime--apply-whitelist)
         (chime--apply-blacklist)
         (-map 'chime--gather-info))))

(defun chime--notify (msg-severity)
  "Notify about an event using `alert' library.
MSG-SEVERITY is a cons cell (MESSAGE . SEVERITY) where MESSAGE is the
notification text and SEVERITY is one of high, medium, or low."
  (let* ((event-msg (if (consp msg-severity) (car msg-severity) msg-severity))
         (severity (if (consp msg-severity) (cdr msg-severity) 'medium)))
    ;; Play sound if enabled and sound file is specified
    (when (and chime-play-sound chime-sound-file)
      (condition-case err
          (when (file-exists-p chime-sound-file)
            (play-sound-file chime-sound-file))
        (error
         (message "chime: Failed to play sound: %s"
                  (error-message-string err)))))
    ;; Show visual notification
    (apply
     'alert event-msg
     :icon chime-notification-icon
     :title chime-notification-title
     :severity severity
     :category 'chime
     chime-extra-alert-plist)))

(defun chime--convert-12hour-to-24hour (timestamp hour)
  "Convert HOUR from 12-hour to 24-hour format based on TIMESTAMP's am/pm suffix.
TIMESTAMP is the original timestamp string (e.g., \"<2025-11-05 Wed 1:30pm>\").
HOUR is the hour value from org-parse-time-string (1-12 for 12-hour format).

Returns converted hour in 24-hour format (0-23):
- 12pm → 12 (noon)
- 1-11pm → 13-23 (add 12)
- 12am → 0 (midnight)
- 1-11am → 1-11 (no change)
- No am/pm → HOUR unchanged (24-hour format)"
  (let ((is-pm (string-match-p "[0-9]:[0-9]\\{2\\}[[:space:]]*pm" (downcase timestamp)))
        (is-am (string-match-p "[0-9]:[0-9]\\{2\\}[[:space:]]*am" (downcase timestamp))))
    (cond
     ;; 12pm = 12:00 (noon), don't add 12
     ((and is-pm (= hour 12)) 12)
     ;; 1-11pm: add 12 to get 13-23
     (is-pm (+ hour 12))
     ;; 12am = 00:00 (midnight)
     ((and is-am (= hour 12)) 0)
     ;; 1-11am or 24-hour format: use as-is
     (t hour))))

(defun chime--timestamp-parse (timestamp)
  "Parse TIMESTAMP and return time in list-of-integer format.
Returns nil if parsing fails or timestamp is malformed."
  (condition-case err
      (when (and timestamp
                 (stringp timestamp)
                 (not (string-empty-p timestamp))
                 ;; Validate angle bracket format
                 (string-match-p "<.*>" timestamp)
                 ;; Ensure timestamp has time component (HH:MM format)
                 (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}" timestamp))
        (let ((parsed (org-parse-time-string timestamp))
              (today (format-time-string "<%Y-%m-%d>")))
          (when (and parsed
                     (decoded-time-hour parsed)
                     (decoded-time-minute parsed))
            ;; Validate date components are in reasonable ranges
            (let* ((month (decoded-time-month parsed))
                   (day (decoded-time-day parsed))
                   (raw-hour (decoded-time-hour parsed))
                   (minute (decoded-time-minute parsed))
                   ;; Convert 12-hour am/pm format to 24-hour format
                   (hour (chime--convert-12hour-to-24hour timestamp raw-hour)))
              (when (and month day hour minute
                         (>= month 1) (<= month 12)
                         (>= day 1) (<= day 31)
                         (>= hour 0) (<= hour 23)
                         (>= minute 0) (<= minute 59))
                ;; seconds-to-time returns also milliseconds and nanoseconds so we
                ;; have to "trim" the list
                (butlast
                 (seconds-to-time
                  (time-add
                   ;; we get the cycled absolute day (not hour and minutes)
                   (org-time-from-absolute (org-closest-date timestamp today 'past))
                   ;; so we have to add the minutes too
                   (+ (* hour 3600)
                      (* minute 60))))
                 2))))))
    (error
     (message "chime: Failed to parse timestamp '%s': %s"
              timestamp (error-message-string err))
     nil)))

(defun chime--extract-time (marker)
  "Extract timestamps from MARKER using source-aware extraction.

For org-gcal events (those with :entry-id: property):
  - Extract ONLY from :org-gcal: drawer
    (ignores SCHEDULED/DEADLINE and body text)
  - This prevents showing stale timestamps after rescheduling

For regular org events:
  - Prefer SCHEDULED and DEADLINE from properties
  - Fall back to plain timestamps in entry body

Timestamps are extracted as cons cells:
\(org-formatted-string . parsed-time)."
  (org-with-point-at marker
    (let ((is-gcal-event (org-entry-get marker "entry-id")))
      (if is-gcal-event
          ;; org-gcal event: extract ONLY from :org-gcal: drawer
          (let ((timestamps nil))
            (save-excursion
              (org-back-to-heading t)
              ;; Search for :org-gcal: drawer
              (when (re-search-forward "^[ \t]*:org-gcal:"
                                      (save-excursion (org-end-of-subtree t) (point))
                                      t)
                (let ((drawer-start (point))
                      (drawer-end (save-excursion
                                   (if (re-search-forward "^[ \t]*:END:"
                                                         (save-excursion (org-end-of-subtree t) (point))
                                                         t)
                                       (match-beginning 0)
                                     (point)))))
                  ;; Extract timestamps within drawer boundaries
                  (goto-char drawer-start)
                  (while (re-search-forward org-ts-regexp drawer-end t)
                    (let ((timestamp-str (match-string 0)))
                      (push (cons timestamp-str
                                 (chime--timestamp-parse timestamp-str))
                            timestamps))))))
            (-non-nil (nreverse timestamps)))
        ;; Regular org event: prefer SCHEDULED/DEADLINE, fall back to plain timestamps
        (let ((property-timestamps
               ;; Extract SCHEDULED and DEADLINE from properties
               (-non-nil
                (--map
                 (let ((org-timestamp (org-entry-get marker it)))
                   (and org-timestamp
                        (cons org-timestamp
                              (chime--timestamp-parse org-timestamp))))
                 '("DEADLINE" "SCHEDULED"))))
              (plain-timestamps
               ;; Extract plain timestamps from entry body
               ;; Skip planning lines (SCHEDULED, DEADLINE, CLOSED) to avoid duplicates
               (let ((timestamps nil))
                 (save-excursion
                   ;; Skip heading and planning lines, but NOT other drawers (nil arg)
                   (org-end-of-meta-data nil)
                   (let ((start (point))
                         (end (save-excursion (org-end-of-subtree t) (point))))
                     ;; Only search if there's content after metadata
                     (when (< start end)
                       (goto-char start)
                       ;; Search for timestamps until end of entry
                       (while (re-search-forward org-ts-regexp end t)
                         (let ((timestamp-str (match-string 0)))
                           (push (cons timestamp-str
                                      (chime--timestamp-parse timestamp-str))
                                 timestamps))))))
                 (nreverse timestamps))))
          ;; Combine property and plain timestamps, removing duplicates and nils
          (-non-nil (append property-timestamps plain-timestamps)))))))

(defun chime--sanitize-title (title)
  "Sanitize TITLE to prevent Lisp read syntax errors during async serialization.
Balances unmatched parentheses, brackets, and braces by adding matching pairs.
Returns sanitized title or empty string if TITLE is nil."
  (if (not title)
      ""
    (let ((chars (string-to-list title))
          (stack '())  ; Stack to track opening delimiters in order
          (result '()))
      ;; Process each character
      (dolist (char chars)
        (cond
         ;; Opening delimiters - add to stack and result
         ((memq char '(?\( ?\[ ?\{))
          (push char stack)
          (push char result))
         ;; Closing delimiters - check if they match
         ((eq char ?\))
          (if (and stack (eq (car stack) ?\())
              (progn
                (pop stack)
                (push char result))
            ;; Unmatched closing paren - skip it
            nil))
         ((eq char ?\])
          (if (and stack (eq (car stack) ?\[))
              (progn
                (pop stack)
                (push char result))
            ;; Unmatched closing bracket - skip it
            nil))
         ((eq char ?\})
          (if (and stack (eq (car stack) ?\{))
              (progn
                (pop stack)
                (push char result))
            ;; Unmatched closing brace - skip it
            nil))
         ;; Regular characters - add to result
         (t
          (push char result))))
      ;; Add closing delimiters for any remaining opening delimiters
      (dolist (opener stack)
        (cond
         ((eq opener ?\() (push ?\) result))
         ((eq opener ?\[) (push ?\] result))
         ((eq opener ?\{) (push ?\} result))))
      ;; Convert back to string (reverse because we built it backwards)
      (concat (nreverse result)))))

(defun chime--extract-title (marker)
  "Extract event title from MARKER.
MARKER acts like the event's identifier.
Title is sanitized to prevent Lisp read syntax errors."
  (org-with-point-at marker
    (-let (((_lvl _reduced-lvl _todo _priority title _tags)
            (org-heading-components)))
      (chime--sanitize-title title))))

(defun chime--gather-info (marker)
  "Collect information about an event.
MARKER acts like event's identifier.
Returns file path and position instead of marker object for proper
async serialization (markers can't be serialized across processes,
especially when buffer names contain angle brackets)."
  `((times . ,(chime--extract-time marker))
    (title . ,(chime--extract-title marker))
    (intervals . ,chime-alert-intervals)
    (marker-file . ,(buffer-file-name (marker-buffer marker)))
    (marker-pos . ,(marker-position marker))))

;;;###autoload
(defun chime-validate-configuration ()
  "Validate chime's runtime environment and configuration.
Returns a list of (SEVERITY MESSAGE) pairs, or nil if all checks pass.
SEVERITY is one of: :error :warning :info

Checks performed:
- org-agenda-files is set and non-empty
- org-agenda-files exist on disk
- org-agenda package is loadable
- global-mode-string available (for modeline display)

When called interactively, displays results via message/warning system.
When called programmatically, returns structured validation results."
  (interactive)
  (let ((issues '()))

    ;; Critical: org-agenda-files must be set and non-empty
    (unless (and (boundp 'org-agenda-files)
                 org-agenda-files
                 (listp org-agenda-files)
                 (> (length org-agenda-files) 0))
      (push '(:error "org-agenda-files is not set or empty.\nChime cannot check for events without org files to monitor.\n\nSet org-agenda-files in your config:\n  (setq org-agenda-files '(\"~/org/inbox.org\" \"~/org/work.org\"))")
            issues))

    ;; Warning: Check if files actually exist
    (when (and (boundp 'org-agenda-files)
               org-agenda-files
               (listp org-agenda-files))
      (let ((missing-files
             (cl-remove-if #'file-exists-p org-agenda-files)))
        (when missing-files
          (push `(:warning ,(format "%d org-agenda-files don't exist:\n  %s\n\nChime will skip these files during event checks."
                                   (length missing-files)
                                   (mapconcat #'identity missing-files "\n  ")))
                issues))))

    ;; Check org-agenda is loadable
    (unless (require 'org-agenda nil t)
      (push '(:error "Cannot load org-agenda.\nEnsure org-mode is installed and available in load-path.")
            issues))

    ;; Check modeline support (if enabled)
    (when (and chime-enable-modeline
               (not (boundp 'global-mode-string)))
      (push '(:warning "global-mode-string not available.\nModeline display may not work in this Emacs version.")
            issues))

    ;; Display results if interactive
    (when (called-interactively-p 'any)
      (if (null issues)
          (message "Chime: ✓ All validation checks passed!")
        ;; Show errors and warnings
        (let ((errors (cl-remove-if-not (lambda (i) (eq (car i) :error)) issues))
              (warnings (cl-remove-if-not (lambda (i) (eq (car i) :warning)) issues)))
          (when errors
            (dolist (err errors)
              (display-warning 'chime (cadr err) :error)))
          (when warnings
            (dolist (warn warnings)
              (display-warning 'chime (cadr warn) :warning))))))

    ;; Return issues for programmatic use
    issues))

(defun chime--stop ()
  "Stop the notification timer and cancel any in-progress check."
  (-some-> chime--timer (cancel-timer))
  (when chime--process
    (interrupt-process chime--process)
    (setq chime--process nil))
  ;; Reset validation state so it runs again on next start
  (setq chime--validation-done nil)
  (setq chime--validation-retry-count 0))

(defun chime--start ()
  "Start the notification timer.  Cancel old one, if any.
Timer interval is controlled by `chime-check-interval'.
First check runs after `chime-startup-delay' seconds to allow
org-agenda-files to load.

Configuration validation happens on the first `chime-check' call,
after the startup delay has elapsed.  This gives startup hooks time
to populate org-agenda-files."
  (chime--stop)

  ;; Wait chime-startup-delay seconds before first check
  ;; This allows org-agenda-files and related infrastructure to finish loading
  (when (featurep 'chime-debug)
    (chime--log-silently "Chime: Scheduling first check in %d seconds" chime-startup-delay))

  ;; Schedule repeating timer: first run at t=chime-startup-delay, then every chime-check-interval
  (--> (run-at-time chime-startup-delay chime-check-interval 'chime-check)
       (setf chime--timer it)))

(defun chime--process-notifications (events)
  "Process EVENTS and send notifications for upcoming items.
Handles both regular event notifications and day-wide alerts."
  (-each
      (->> events
           (-map 'chime--check-event)
           (-flatten)
           (-uniq))
    'chime--notify)
  (when (chime-current-time-is-day-wide-time)
    (mapc 'chime--notify
          (chime-day-wide-notifications events))))

(defun chime--fetch-and-process (callback)
  "Asynchronously fetch events from agenda and invoke CALLBACK with them.
Manages async process state and last-check-time internally.
Does nothing if a check is already in progress."
  (unless (and chime--process
               (process-live-p chime--process))
    (setq chime--process
          (let ((default-directory user-emacs-directory)
                (async-prompt-for-password nil)
                (async-process-noquery-on-exit t))
            (async-start
             (chime--retrieve-events)
             (lambda (events)
               (setq chime--process nil)
               (setq chime--last-check-time (current-time))
               ;; Handle errors from async process
               (condition-case err
                   (progn
                     ;; Check if events is an error signal from async process
                     (if (and (listp events)
                              (eq (car events) 'async-signal))
                         (progn
                           ;; Async process returned an error
                           (when (featurep 'chime-debug)
                             (chime--debug-log-async-error (cdr events)))
                           (chime--log-silently "Chime: Async error: %s"
                                               (error-message-string (cdr events)))
                           (message "Chime: Event check failed - see *Messages* for details"))
                       ;; Success - process events normally
                       (when (featurep 'chime-debug)
                         (chime--debug-log-async-complete events))
                       (funcall callback events)))
                 (error
                  ;; Error occurred in callback processing
                  (when (featurep 'chime-debug)
                    (chime--debug-log-async-error err))
                  (chime--log-silently "Chime: Error processing events: %s"
                                      (error-message-string err))
                  (message "Chime: Error processing events - see *Messages* for details")))))))))

(defun chime--log-silently (format-string &rest args)
  "Append formatted message to *Messages* buffer without echoing.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (apply #'format format-string args))
      (unless (bolp) (insert "\n")))))

;;;###autoload
(cl-defun chime-check ()
  "Parse agenda view and notify about upcoming events.

Do nothing if a check is already in progress in the background.

On the first call after `chime-mode' is enabled, validates the runtime
configuration.  This happens after `chime-startup-delay', giving startup
hooks time to populate org-agenda-files.  If validation fails, logs an
error and skips the check."
  (interactive)

  ;; Validate configuration on first check only
  (unless chime--validation-done
    (let ((issues (chime-validate-configuration)))
      (if (cl-some (lambda (i) (eq (car i) :error)) issues)
          (progn
            ;; Critical errors found - increment retry counter
            (setq chime--validation-retry-count (1+ chime--validation-retry-count))

            ;; Check if we've exceeded max retries
            (if (> chime--validation-retry-count chime-validation-max-retries)
                ;; Max retries exceeded - show full error
                (let ((errors (cl-remove-if-not (lambda (i) (eq (car i) :error)) issues)))
                  (chime--log-silently "Chime: Configuration validation failed with %d error(s) after %d retries:"
                                       (length errors)
                                       chime--validation-retry-count)
                  (dolist (err errors)
                    (chime--log-silently "")
                    (chime--log-silently "ERROR: %s" (cadr err)))
                  (message "Chime: Configuration errors detected (see *Messages* buffer for details)"))
              ;; Still within retry limit - show friendly waiting message
              (message "Chime: Waiting for org-agenda-files to load... (attempt %d/%d)"
                       chime--validation-retry-count
                       chime-validation-max-retries))

            ;; Don't mark validation as done - will retry on next check
            ;; in case dependencies load later
            ;; Don't proceed with check
            (cl-return-from chime-check nil))
        ;; No errors - mark validation as done and reset retry counter
        (setq chime--validation-done t)
        (setq chime--validation-retry-count 0))))

  ;; Validation passed or already done - proceed with check
  (chime--fetch-and-process
   (lambda (events)
     (chime--process-notifications events)
     (chime--update-modeline events))))

;;;###autoload
(defun chime-refresh-modeline ()
  "Update modeline display with latest events without sending notifications.

Useful after external calendar sync operations (e.g., org-gcal-sync).
Does nothing if a check is already in progress in the background."
  (interactive)
  (chime--fetch-and-process
   (lambda (events)
     (chime--update-modeline events))))

;;;###autoload
(define-minor-mode chime-mode
  "Toggle org notifications globally.
When enabled parses your agenda once a minute and emits notifications
if needed."
  :global
  :lighter chime-modeline-lighter
  (if chime-mode
      (progn
        (chime--start)
        ;; Add modeline string to global-mode-string
        (when (and chime-enable-modeline
                   (> chime-modeline-lookahead-minutes 0))
          (if global-mode-string
              (add-to-list 'global-mode-string 'chime-modeline-string 'append)
            (setq global-mode-string '("" chime-modeline-string)))))
    (progn
      (chime--stop)
      ;; Remove modeline string from global-mode-string
      (setq global-mode-string
            (delq 'chime-modeline-string global-mode-string))
      (setq chime-modeline-string nil)
      ;; Force update ALL windows/modelines, not just current buffer
      (force-mode-line-update t))))

;; Automatically enable debug features when debug mode is on
;; Only enable in the main Emacs process, not in async subprocesses.
;; We detect async context by checking if this is an interactive session.
;; Async child processes run in batch mode with noninteractive=t.
(when (and chime-debug
           (not noninteractive))
  (chime-debug-monitor-event-loading)
  (chime-debug-enable-async-monitoring))

(provide 'chime)
;;; chime.el ends here
