;;; chime.el --- CHIME Heralds Imminent Events -*- lexical-binding: t -*-

;; Copyright (C) 2017 Artem Khramov
;; Copyright (C) 2024 Craig Jennings

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
;; - Per-event notification customization
;; - Async background checking (runs every minute)
;;
;; Quick Start:
;; (require 'chime)
;; (setq chime-alert-time '(5 0))  ; 5 min before and at event time
;; (chime-mode 1)
;;
;; Manual check: M-x chime-check
;;
;; Notification times can be customized globally via `chime-alert-time'
;; or per-event using the `CHIME_NOTIFY_BEFORE` property.
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

(defgroup chime nil
  "Chime customization options."
  :group 'org)

(defcustom chime-alert-time '(10)
  "Time in minutes to get a notification about upcoming event.
Can be a single integer or a list of integers. Each value represents
minutes before the event. Use 0 to notify at event time. Cannot be
negative."
  :package-version '(chime . "0.1.0")
  :group 'chime
  :type '(choice (integer :tag "Notify once")
                 (repeat integer))
  :set (lambda (symbol value)
         (let ((values (if (listp value) value (list value))))
           (dolist (v values)
             (unless (integerp v)
               (user-error "Alert time must be an integer, got: %S" v))
             (when (< v 0)
               (user-error "Alert time cannot be negative, got: %d" v)))
           (set-default symbol value))))

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

(defcustom chime-alert-times-property "CHIME_NOTIFY_BEFORE"
  "Property name for per-event notification times.
Use this property in your agenda files to add additional
notifications to an event."
  :package-version '(chime . "0.1.0")
  :group 'chime
  :type 'string)

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

(defcustom chime-alert-severity 'medium
  "Severity of the alert.
Options: \\='high \\='medium \\='low"
  :package-version '(chime . "0.3.1")
  :group 'chime
  :type 'symbol
  :options '(high medium low))

(defcustom chime-extra-alert-plist nil
  "Additional arguments that should be passed to invocations of `alert'."
  :package-version "v0.5.0"
  :group 'chime
  :type 'plist)

(defcustom chime-day-wide-alert-times nil
  "List of time strings for day-wide event alerts.
Each string specifies a time of day when day-wide events should trigger."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type 'string)

(defcustom chime-show-any-overdue-with-day-wide-alerts t
  "Show any overdue TODO items along with day wide alerts whenever they are shown."
  :package-version '(chime . "0.5.0")
  :group 'chime
  :type 'string)

(defcustom chime-enable-modeline t
  "Whether to display upcoming events in the modeline.
When nil, chime will not modify the modeline at all."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type 'boolean)

(defcustom chime-modeline-lookahead 30
  "Minutes ahead to look for next event to display in modeline.
Should be larger than notification alert times for advance awareness.
Set to 0 to disable modeline display.
This setting only takes effect when `chime-enable-modeline' is non-nil."
  :package-version '(chime . "0.5.1")
  :group 'chime
  :type 'integer)

(defcustom chime-modeline-format " ⏰ %s"
  "Format string for modeline display.
%s will be replaced with the event description (time and title)."
  :package-version '(chime . "0.5.1")
  :group 'chime
  :type 'string)

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
Set to nil to use Emacs default beep instead.
Should be an absolute path to a .wav, .au, or other sound file
supported by your system."
  :package-version '(chime . "0.6.0")
  :group 'chime
  :type '(choice (const :tag "Use system beep" nil)
                 (file :tag "Sound file path")))

(defvar chime--timer nil
  "Timer value.")

(defvar chime--process nil
  "Currently-running async process.")

(defvar chime--agenda-buffer-name "*org wild notifier affairs*"
  "Name for temporary \\='org-agenda\\=' buffer.")

(defvar chime--last-check-time (seconds-to-time 0)
  "Last time checked for events.")

(defvar chime--next-event nil
  "Next upcoming event for modeline display.
Stored as (EVENT-MSG . MINUTES-UNTIL) or nil if no upcoming event.")

(defvar chime-modeline-string nil
  "Modeline string showing next upcoming event.")
;;;###autoload(put 'chime-modeline-string 'risky-local-variable t)
(put 'chime-modeline-string 'risky-local-variable t)

(defun chime--time= (&rest list)
  "Compare timestamps.
Comparison is performed by converted each element of LIST onto string
in order to ignore seconds."
  (->> list
       (--map (format-time-string "%d:%H:%M" it))
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
       ;; Validate timestamp is a proper time value (list of integers)
       (listp timestamp)
       (chime--time=
        (time-add (current-time) (seconds-to-time (* 60 interval)))
        timestamp)))

(defun chime--notifications (event)
  "Get notifications for given EVENT.
Returns a list of time information interval pairs."
  (->> (list
        (chime--filter-day-wide-events (cadr (assoc 'times event)))
        (cdr (assoc 'intervals event)))
         (apply '-table-flat (lambda (ts int) (list ts int)))
         ;; When no values are provided for table flat, we get the second values
         ;; paired with nil.
         (--filter (not (null (car it))))
         (--filter (chime--timestamp-within-interval-p (cdar it) (cadr it)))))

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

(defun chime--notification-text (str-interval event)
  "For given STR-INTERVAL list and EVENT get notification wording.
Format is controlled by `chime-notification-text-format'."
  (format-spec chime-notification-text-format
               `((?t . ,(or (cdr (assoc 'title event)) ""))
                 (?T . ,(chime--get-hh-mm-from-org-time-string (car str-interval)))
                 (?u . ,(chime--time-left (* 60 (cdr str-interval)))))))

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
  "Generate notification texts for day-wide EVENTS."
  (->> events
       (-filter 'chime-display-as-day-wide-event)
       (-map 'chime--day-wide-notification-text)
       (-uniq)))

(defun chime-display-as-day-wide-event (event)
  "Check if EVENT should be displayed as a day-wide event.
`chime-event-has-any-passed-time' is a requirement regardless of
whether `chime-show-any-overdue-with-day-wide-alerts' is set,
because the events list can include events scheduled tomorrow.
We only want to alert for things scheduled today."
  (and (chime-event-has-any-passed-time event)
      (or chime-show-any-overdue-with-day-wide-alerts
           (chime-event-has-any-day-wide-timestamp event))))

(defun chime-event-has-any-day-wide-timestamp (event)
  "Check if EVENT has any day-wide (no time component) timestamps."
  (--any (not (chime--has-timestamp (car it)))
         (car (cdr (assoc 'times event)))))

(defun chime-event-has-any-passed-time (event)
  "Check if EVENT has any timestamps in the past."
  (--any (time-less-p (cdr it) (current-time))
         (car (cdr (assoc 'times event )))))

(defun chime--day-wide-notification-text (event)
  "For given STR-INTERVAL list and EVENT get notification wording."
  (format "%s is due or scheduled today"
          (cdr (assoc 'title event))))

(defun chime--check-event (event)
  "Get notifications for given EVENT.
Returns a list of notification messages"
  (->> (chime--notifications event)
       (--map (chime--notification-text `(,(caar it) . ,(cadr it)) event))))

(defun chime--update-modeline (events)
  "Update modeline with next upcoming event from EVENTS.
Only shows events within `chime-modeline-lookahead' minutes."
  (if (or (not chime-enable-modeline)
          (not chime-modeline-lookahead)
          (zerop chime-modeline-lookahead))
      (setq chime-modeline-string nil)
    (let ((soonest-event nil)
          (soonest-minutes nil)
          (now (current-time)))
      ;; Find soonest event within lookahead window
      (dolist (event events)
        (let* ((all-times (cadr (assoc 'times event)))
               (times (chime--filter-day-wide-events all-times)))
          (dolist (time-info times)
            (when-let* ((time-str (car time-info))
                        (event-time (cdr time-info))
                        ;; Calculate minutes until event
                        (seconds-until (- (float-time event-time) (float-time now)))
                        (minutes-until (/ seconds-until 60)))
              ;; Only consider future events within lookahead window
              (when (and (> minutes-until 0)
                         (<= minutes-until chime-modeline-lookahead)
                         (or (not soonest-minutes)
                             (< minutes-until soonest-minutes)))
                (setq soonest-minutes minutes-until)
                (setq soonest-event
                      (chime--notification-text
                       `(,time-str . ,minutes-until) event)))))))
      ;; Format and set modeline string
      (setq chime-modeline-string
            (when soonest-event
              (format chime-modeline-format soonest-event)))
      (force-mode-line-update))))

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
                 "chime-alert-time"
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

    (org-agenda-list 2 (org-read-date nil nil "today"))

    (->> (org-split-string (buffer-string) "\n")
         (--map (plist-get
                 (org-fix-agenda-info (text-properties-at 0 it))
                 'org-marker))
         (-non-nil)
         (chime--apply-whitelist)
         (chime--apply-blacklist)
         (-map 'chime--gather-info))))

(defun chime--notify (event-msg)
  "Notify about an event using `alert' library.
EVENT-MSG is a string representation of the event."
  ;; Play sound if enabled
  (when chime-play-sound
    (condition-case err
        (if chime-sound-file
            (when (file-exists-p chime-sound-file)
              (play-sound-file chime-sound-file))
          ;; Use default Emacs bell/beep if no file specified
          (beep))
      (error
       (message "chime: Failed to play sound: %s"
                (error-message-string err)))))
  ;; Show visual notification
  (apply
   'alert event-msg
   :icon chime-notification-icon
   :title chime-notification-title
   :severity chime-alert-severity
   :category 'chime
   chime-extra-alert-plist))

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
            (let ((month (decoded-time-month parsed))
                  (day (decoded-time-day parsed))
                  (hour (decoded-time-hour parsed))
                  (minute (decoded-time-minute parsed)))
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
  "Extract timestamps from MARKER.
Extracts SCHEDULED and DEADLINE from properties, plus any plain
timestamps found in the entry body.
Timestamps are extracted as cons cells.  car holds org-formatted
string, cdr holds time in list-of-integer format."
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
         (org-with-point-at marker
           (let ((timestamps nil))
             (save-excursion
               ;; Skip heading and planning lines, but NOT other drawers (nil arg)
               ;; This allows extraction from :org-gcal: and similar drawers
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
             (nreverse timestamps)))))
    ;; Combine property and plain timestamps, removing duplicates and nils
    (-non-nil (append property-timestamps plain-timestamps))))

(defun chime--extract-title (marker)
  "Extract event title from MARKER.
MARKER acts like the event's identifier."
  (org-with-point-at marker
    (-let (((_lvl _reduced-lvl _todo _priority title _tags)
            (org-heading-components)))
      title)))

(defun chime--extract-notication-intervals (marker)
  "Extract notification intervals from the event's properties.
MARKER acts like the event's identifier.  Resulting list also contains
standard notification interval (`chime-alert-time')."
  `(,@(-flatten (list chime-alert-time))
    ,@(-map 'string-to-number
           (org-entry-get-multivalued-property
            marker
            chime-alert-times-property))))

(defun chime--gather-info (marker)
  "Collect information about an event.
MARKER acts like event's identifier."
  `((times . (,(chime--extract-time marker)))
    (title . ,(chime--extract-title marker))
    (intervals . ,(chime--extract-notication-intervals marker))))

(defun chime--stop ()
  "Stop the notification timer and cancel any in-progress check."
  (-some-> chime--timer (cancel-timer))
  (when chime--process
    (interrupt-process chime--process)
    (setq chime--process nil)))

(defun chime--start ()
  "Start the notification timer.  Cancel old one, if any.
Timer interval is controlled by `chime-check-interval'.
Runs an immediate check for smoother experience."
  (chime--stop)
  (chime-check)

  (--> (time-add (current-time) chime-check-interval)
       (run-at-time it chime-check-interval 'chime-check)
       (setf chime--timer it)))

(defun chime--check-events (events)
  "Process EVENTS and send notifications for upcoming items.
Clears the async process flag, sends notifications for matching
events, handles day-wide alerts, and updates the modeline."
  (setq chime--process nil)
  ;; Handle notifications
  (-each
      (->> events
           (-map 'chime--check-event)
           (-flatten)
           (-uniq))
    'chime--notify)
  (when (chime-current-time-is-day-wide-time)
    (mapc 'chime--notify
          (chime-day-wide-notifications events)))
  ;; Update modeline with next upcoming event
  (chime--update-modeline events)
  (setq chime--last-check-time (current-time)))

;;;###autoload
(defun chime-check ()
  "Parse agenda view and notify about upcoming events.

Do nothing if a check is already in progress in the background."
  (interactive)
  (unless (and chime--process
               (process-live-p chime--process))
    (setq chime--process
          (let ((default-directory user-emacs-directory)
                (async-prompt-for-password nil)
                (async-process-noquery-on-exit t))
            (async-start
             (chime--retrieve-events)
             'chime--check-events)))))

;;;###autoload
(define-minor-mode chime-mode
  "Toggle org notifications globally.
When enabled parses your agenda once a minute and emits notifications
if needed."
  :global
  :lighter "Chime"
  (if chime-mode
      (progn
        (chime--start)
        ;; Add modeline string to global-mode-string
        (when (and chime-enable-modeline
                   (> chime-modeline-lookahead 0))
          (if global-mode-string
              (add-to-list 'global-mode-string 'chime-modeline-string 'append)
            (setq global-mode-string '("" chime-modeline-string)))))
    (progn
      (chime--stop)
      ;; Remove modeline string from global-mode-string
      (setq global-mode-string
            (delq 'chime-modeline-string global-mode-string))
      (setq chime-modeline-string nil)
      (force-mode-line-update))))

(provide 'chime)

;;; chime.el ends here
