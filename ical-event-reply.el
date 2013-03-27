;;; ical-event-reply.el --- Build iCalendar reply to event request

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

(require 'ical-event)

(defun ical-event--build-reply-event-body (event status identity)
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
                              "END:VEVENT")
                 "\n"))))

(defun ical-event-reply-from-buffer (buf status identity)
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
                    (buffer-substring-no-properties start (line-end-position)))))))

    (let (zone event)
      (with-current-buffer (icalendar--get-unfolded-buffer (get-buffer buf))
        (goto-char (point-min))
        (setq zone (extract-block "VTIMEZONE")
              event (extract-block "VEVENT")))

      (when event
        (let ((contents (list "BEGIN:VCALENDAR"
                              "METHOD:REPLY"
                              "PRODID:Gnus"
                              "VERSION:2.0"
                              zone
                              (ical-event--build-reply-event-body event status identity)
                              "END:VCALENDAR")))

          (mapconcat #'identity (delq nil contents) "\n"))))))

(provide 'ical-event-reply)
;;; ical-event-reply.el ends here
