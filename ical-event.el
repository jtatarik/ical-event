;;; ical-event.el --- iCalendar Event Object

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

(defclass ical-event ()
  ((organizer :initarg :organizer
              :accessor ical-event:organizer
              :type string)
   (summary :initarg :summary
            :accessor summary
            :initform ""
            :type (or null string))
   (description :initarg :description
                :accessor ical-event:description
                :initform ""
                :type (or null string))
   (location :initarg :location
             :accessor ical-event:location
             :initform ""
             :type (or null string))
   (start :initarg :start
          :accessor ical-event:start
          :initform ""
          :type (or null string))
   (end :initarg :end
        :accessor ical-event:end
        :initform ""
        :type (or null string))
   (recur :initarg :recur
          :accessor ical-event:recur
          :initform ""
          :type (or null string))
   (uid :initarg :uid
        :accessor ical-event:uid
        :type string)
   (method :initarg :method
           :accessor ical-event:method
           :initform "PUBLISH"
           :type (or null string))
   (rsvp :initarg :rsvp
         :accessor ical-event:rsvp
         :initform nil
         :type (or null boolean)))
  "iCalendar Event class")

(defclass ical-event-request (ical-event)
  nil
  "iCalendar Request Event class")

(defclass ical-event-cancel (ical-event)
  nil
  "iCalendar Cancel Event class")

(defmethod ical-event:recurring-p ((event ical-event))
  "Returns `t' if EVENT is recurring."
  (not (null (ical-event:recur event))))

(defmethod ical-event:recurring-freq ((event ical-event))
  "Returns recurring frequency for EVENT."
  (let ((rrule (ical-event:recur event)))
    (string-match "FREQ=\\([[:alpha:]]+\\)" rrule)
    (match-string 1 rrule)))

(defmethod ical-event:recurring-interval ((event ical-event))
  "Returns recurring interval for EVENT."
  (let ((rrule (ical-event:recur event))
        (default-interval 1))

    (string-match "INTERVAL=\\([[:digit:]]+\\)" rrule)
    (or (match-string 1 rrule)
        default-interval)))

(defmethod ical-event:start-time ((event ical-event))
  "Returns time value of the EVENT start date."
  (date-to-time (ical-event:start event)))

(defmethod ical-event:end-time ((event ical-event))
  "Returns time value of the EVENT end date."
  (date-to-time (ical-event:end event)))


(defun ical-event--decode-datefield (ical field zone-map &optional date-style)
  (let* ((calendar-date-style (or date-style 'european))
         (date (icalendar--get-event-property ical field))
         (date-zone (icalendar--find-time-zone
                        (icalendar--get-event-property-attributes
                         ical field)
                        zone-map))
         (date-decoded (icalendar--decode-isodatetime date nil date-zone)))

    (concat (icalendar--datetime-to-iso-date date-decoded "-")
            " "
            (icalendar--datetime-to-colontime date-decoded))))

(defun ical-event--find-attendee (ical name-or-email)
  (let* ((event (car (icalendar--all-events ical)))
         (event-props (caddr event)))
    (cl-labels ((attendee-name (att)
                  (plist-get (cadr att) 'CN))
                (attendee-email (att)
                  (replace-regexp-in-string "^.*MAILTO:" "" (caddr att)))
                (attendee-prop-matches (prop)
                   (and (eq (car prop) 'ATTENDEE)
                        (or (member (attendee-name prop) name-or-email)
                            (let ((att-email (attendee-email prop)))
                              (cl-find-if (lambda (email)
                                            (string-match email att-email))
                                          name-or-email))))))

      (cl-find-if #'attendee-prop-matches event-props))))


(defun icalendar->ical-event (ical &optional attendee-name-or-email)
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
                     (ical-event--find-attendee ical attendee-name-or-email)))
         (args (list :method method
                     :organizer organizer
                     :start (ical-event--decode-datefield event 'DTSTART zone-map)
                     :end (ical-event--decode-datefield event 'DTEND zone-map)
                     :rsvp (string= (plist-get (cadr attendee) 'RSVP)
                                    "TRUE")))
         (event-class (pcase method
                        ("REQUEST" 'ical-event-request)
                        ("CANCEL" 'ical-event-cancel)
                        (_ 'ical-event))))

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

(defun ical-event-from-buffer (buf &optional attendee-name-or-email)
  (let ((ical (with-current-buffer (icalendar--get-unfolded-buffer (get-buffer buf))
                (goto-char (point-min))
                (icalendar--read-element nil nil))))

    (when ical
      (icalendar->ical-event ical attendee-name-or-email))))


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
                  (cl-find-if (lambda (name) (string-match-p name line))
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
                                       ("SUMMARY" (update-summary line))
                                       ("DTSTAMP" (update-dtstamp))
                                       ((or "ORGANIZER" "DTSTART" "DTEND"
                                            "LOCATION" "DURATION" "SEQUENCE"
                                            "RECURRENCE-ID" "UID") line)
                                       (_ nil))))
                      (when new-line
                        (push new-line reply-event-lines))))))

      (mapc #'process-event-line (split-string event "\n"))

      (unless (cl-find-if (lambda (x) (string-match "^ATTENDEE" x))
                          reply-event-lines)
        (error "Could not find an event attendee matching given identity"))

      (mapconcat #'identity `("BEGIN:VEVENT"
                              ,@(nreverse reply-event-lines)
                              "END:VEVENT\n")
                 "\n"))))

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
         "PRODID:Gnus\n"
         "VERSION:2.0\n"
         zone
         (build-reply-event-body event status identity)
         "END:VCALENDAR\n")))))


(provide 'ical-event)
;;; ical-event.el ends here
