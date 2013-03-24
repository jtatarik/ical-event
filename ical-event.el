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
                              (string-match "\\(?:MAILTO:\\)?\\(.+\\)" (caddr att))
                              (match-string 1 (caddr att))))

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
         (prop-map '((organizer . ORGANIZER)
                     (summary . SUMMARY)
                     (description . DESCRIPTION)
                     (location . LOCATION)
                     (recur . RRULE)
                     (uid . UID)))
         (method (third (assoc 'METHOD (third (car (nreverse ical))))))
         (attendee (when attendee-name-or-email
                     (find-attendee-by-name-or-email ical attendee-name-or-email)))
         (args (list :method method
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


(provide 'ical-event)
;;; ical-event.el ends here
