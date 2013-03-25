;;; ical-event.el --- Calendar Event Object

;; Copyright (C) 2013  Jan Tatarik

;; Author: Jan Tatarik <Jan.Tatarik@gmail.com>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
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

;; 

;;; Code:


(require 'icalendar)
(require 'eieio)
(require 'cl)

;; TODO: most fields optional, especially when handling different kinds of
;; methods

(defclass cal-event ()
  ((organizer :initarg :organizer
              :accessor organizer
              :type string)
   (summary :initarg :summary
            :accessor summary
            :initform ""
            :type (or null string))
   (description :initarg :description
                :accessor description
                :initform ""
                :type (or null string))
   (location :initarg :location
             :accessor location
             :initform ""
             :type (or null string))
   (start :initarg :start
          :accessor start
          :initform ""
          :type (or null string))
   (end :initarg :end
        :accessor end
        :initform ""
        :type (or null string))
   (recur :initarg :recur
          :accessor recur
          :initform ""
          :type (or null string))
   (uid :initarg :uid
        :accessor uid
        :type string)
   (method :initarg :method
           :accessor method
           :initform "PUBLISH"
           :type (or null string))
   (rsvp :initarg :rsvp
         :accessor rsvp
         :initform nil
         :type (or null boolean)))
  "iCalendar event class")

(defclass cal-event-request (cal-event)
  nil
  "Request iCalendar event")

(defclass cal-event-cancel (cal-event)
  nil
  "Cancel iCalendar event")

(defmethod cancel-event-p ((event cal-event))
  (with-slots (method) event
    (and method (string= method "CANCEL"))))

(defmethod request-event-p ((event cal-event))
  (with-slots (method) event
    (and method (string= method "REQUEST"))))

(defmethod recurring-p ((event cal-event))
  "Returns `t' if EVENT is recurring."
  (not (null (recur event))))

(defmethod recurring-freq ((event cal-event))
  "Returns recurring frequency for EVENT."
  (let ((rrule (recur event)))
    (string-match "FREQ=\\([[:alpha:]]+\\)" rrule)
    (match-string 1 rrule)))

(defmethod recurring-interval ((event cal-event))
  "Returns recurring interval for EVENT."
  (let ((rrule (recur event))
        (default-interval 1))

    (string-match "INTERVAL=\\([[:digit:]]+\\)" rrule)
    (or (match-string 1 rrule)
        default-interval)))

(defmethod start-time ((event cal-event))
  "Returns time value of the EVENT start date."
  (date-to-time (start event)))

(defmethod end-time ((event cal-event))
  "Returns time value of the EVENT end date."
  (date-to-time (end event)))


(defun icalendar-decode-datefield (event field zone-map &optional date-style)
  (let* ((calendar-date-style (or date-style 'european))
         (date (icalendar--get-event-property event field))
         (date-zone (icalendar--find-time-zone
                        (icalendar--get-event-property-attributes
                         event field)
                        zone-map))
         (date-decoded (icalendar--decode-isodatetime date nil date-zone)))

    (concat (icalendar--datetime-to-iso-date date-decoded "-")
            " "
            (icalendar--datetime-to-colontime date-decoded))))

(defun find-attendee-by-name-or-email (ical name-or-email)
  (let* ((event (car (icalendar--all-events ical)))
         (details (caddr event)))
    (cl-flet ((attendee-name (att)
                (plist-get (cadr att) 'CN))
              (attendee-email (att)
                (replace-regexp-in-string "^.*MAILTO:" "" (caddr att))))

      (cl-find-if (lambda (x)
                    (and (eq (car x) 'ATTENDEE)
                         (or (member (attendee-name x) name-or-email)
                             (cl-find-if (lambda (z)
                                           ;; FIXME: cache (attendee-email)
                                           (string-match z (attendee-email x)))
                                         name-or-email))))
                  details))))


(defun icalendar->ical (ical &optional attendee-name-or-email)
  (let* ((event (car (icalendar--all-events ical)))
         (zone-map (icalendar--convert-all-timezones ical))
         (organizer (replace-regexp-in-string
                     "^.*MAILTO:" ""
                     (icalendar--get-event-property event 'ORGANIZER)))
         (prop-map '((summary . SUMMARY)
                     (description . DESCRIPTION)
                     (location . LOCATION)
                     (recur . RRULE)
                     (uid . UID)))
         (method (third (assoc 'METHOD (third (car (nreverse ical))))))
         (attendee (when attendee-name-or-email
                     (find-attendee-by-name-or-email ical attendee-name-or-email)))
         (args (list :method method
                     :organizer organizer
                     :start (icalendar-decode-datefield event 'DTSTART zone-map)
                     :end (icalendar-decode-datefield event 'DTEND zone-map)
                     :rsvp (string= (plist-get (cadr attendee) 'RSVP)
                                    "TRUE")))
         (event-class (pcase method
                        ("REQUEST" 'cal-event-request)
                        ("CANCEL" 'cal-event-cancel)
                        (_ 'cal-event))))

    (cl-labels ((map-property (prop)
                              (let ((value (icalendar--get-event-property event prop)))
                                (when value
                                  ;; ugly, but cannot get
                                  ;;replace-regexp-in-string work with "\\" as
                                  ;;REP, plus we should also handle "\\;"
                                  (replace-regexp-in-string
                                   "\\\\," ","
                                   (replace-regexp-in-string
                                    "\\\\n" "\n" (substring-no-properties value))))))
                (accumulate-args (mapping)
                                 (destructuring-bind (slot . ical-property) mapping
                                   (setq args (append (list
                                                       (intern (concat ":" (symbol-name slot)))
                                                       (map-property ical-property))
                                                      args)))))

      (mapc #'accumulate-args prop-map)
      (apply 'make-instance event-class args))))

(defun ical-from-buffer (buf &optional attendee-name-or-email)
  (let ((ical (with-current-buffer (icalendar--get-unfolded-buffer (get-buffer buf))
                (goto-char (point-min))
                (icalendar--read-element nil nil))))

    (when ical
      (icalendar->ical ical attendee-name-or-email))))


(defun build-reply-event-body (event status identity)
  (let ((summary-status (capitalize (symbol-name status)))
        (attendee-status (upcase (symbol-name status)))
        reply-event-lines)
    (cl-labels ((update-summary (line)
                (if (string-match "^[^:]+:" line)
                 (replace-match (format "\\&%s: " summary-status) t nil line)
                 line))
              (update-dtstamp ()
                (format-time-string "DTSTAMP:%Y%m%dT%H%M%SZ" nil t))
              (attendee-matches-identity (line)
                (cl-find-if (lambda (name)
                              (string-match name line))
                            identity))
              (update-attendee-status (line)
                (when (and (attendee-matches-identity line)
                           (string-match "\\(PARTSTAT=\\)[^;]+" line))
                  (replace-match (format "\\1%s" attendee-status) t nil line)))
              (process-event-line (line)
                 (when (string-match "^\\([^;:]+\\)" line)
                  (let* ((key (match-string 0 line))
                         ;; NOTE: not all of the below fields are mandatory,
                         ;; but they are present in MS Exchange replies. Need
                         ;; to test with minimalistic setup, too.
                         (new-line (pcase key
                                     ("ATTENDEE" (update-attendee-status line))
                                     ("ORGANIZER" line)
                                     ("SUMMARY" (update-summary line))
                                     ("DTSTART" line)
                                     ("DTEND" line)
                                     ("LOCATION" line)
                                     ("DTSTAMP" (update-dtstamp))
                                     ("DURATION" line)
                                     ("SEQUENCE" line)
                                     ("RECURRENCE-ID" line)
                                     ("UID" line)
                                     (_ nil))))
                    (when new-line
                      (push new-line reply-event-lines))))))

      (mapc #'process-event-line
            (split-string event "\n"))

      (unless (cl-find-if (lambda (x) (string-match "^ATTENDEE" x))
                          reply-event-lines)
        (error "Could not find an event attendee matching given identity"))

      (concat
       "BEGIN:VEVENT\n"
       (mapconcat #'identity (nreverse reply-event-lines) "\n")
       "\nEND:VEVENT\n"))))

(defun event-to-reply (buf status identity)
  "Build a calendar event reply for request contained in BUF.
The reply will have STATUS (accepted, tentative, declined).
The reply will be composed for attendees matching IDENTITY."
  (cl-flet ((extract-block (blockname)
               (save-excursion
                 (let ((block-start-re (format "^BEGIN:%s" blockname))
                       (block-end-re (format "^END:%s" blockname))
                       start)
                   (when (re-search-forward block-start-re nil t)
                     (setq start (line-beginning-position))
                     (re-search-forward block-end-re)
                     (buffer-substring-no-properties start (line-beginning-position 2)))))))

    (let (zone event)
      (with-current-buffer (icalendar--get-unfolded-buffer (get-buffer buf))
        (goto-char (point-min))
        (setq zone (extract-block "VTIMEZONE")
              event (extract-block "VEVENT")))

      (when event
        (concat
         "BEGIN:VCALENDAR\n"
         "METHOD:REPLY\n"
         zone
         (build-reply-event-body event status identity)
         "END:VCALENDAR\n")))))


(provide 'ical-event)
;;; ical-event.el ends here
