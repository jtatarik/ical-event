;;; gnus-cal2org-sync.el --- 

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

(require 'org)
(require 'org-capture)
(require 'ical-event)


(defgroup cal-event nil "Settings for Calendar Event gnus/org integration."
  :group 'calendar
  :prefix "cal")

(defcustom cal-capture-file nil
  "Target Org file for storing captured calendar events."
  :type '(file)
  :group 'cal-event)

(defcustom cal-capture-headline nil
  "Target outline in `cal-capture-file' for storing captured events."
  :type '(repeat string)
  :group 'cal-event)

(defcustom cal-org-template-name "used by cal-event-mode"
  "Org-mode template name."
  :type '(string)
  :group 'cal-event)

(defcustom cal-org-template-key "#"
  "Org-mode template hotkey."
  :type '(char)
  :group 'cal-event)

(defmethod ical->org-repeat ((event cal-event))
  "Builds `org-mode' repeater string for EVENT.
Returns nil for non-recurring EVENT."
  (when (recurring-p event)
    (let* ((freq-map '(("HOURLY" . "h")
                       ("DAILY" . "d")
                       ("WEEKLY" . "w")
                       ("MONTHLY" . "m")
                       ("YEARLY" . "y")))
           (org-freq (cdr (assoc (recurring-freq event) freq-map))))

      (when org-freq
        (format "+%s%s" (recurring-interval event) org-freq)))))

(defmethod ical->org-timestamp ((event cal-event))
  "Builds `org-mode' timestamp from EVENT start/end dates, and recurrence info."
  (let* ((start (start-time event))
         (end (end-time event))
         (start-date (format-time-string "%Y-%m-%d %a" start t))
         (start-time (format-time-string "%H:%M" start t))
         (end-date (format-time-string "%Y-%m-%d %a" end t))
         (end-time (format-time-string "%H:%M" end t))
         (repeat (or (ical->org-repeat event) "")))

    (if (equal start-date end-date)
        (format "<%s %s-%s %s>" start-date start-time end-time repeat)
      (format "<%s %s>--<%s %s>" start-date start-time end-date end-time))))

(defmethod ical->org-entry ((event cal-event))
  "Formats new entry from EVENT."
  (with-temp-buffer
    (org-mode)
    (with-slots (organizer summary description location
                           recur uid) event
      (let ((props `(("ICAL_EVENT" . "t")
                     ("ID" . ,uid)
                     ("DT" . ,(ical->org-timestamp event))
                     ("ORGANIZER" . ,(organizer event))
                     ("LOCATION" . ,(location event))
                     ("RRULE" . ,(recur event)))))

        (insert (format "* %s (%s)\n\n" summary location))
        (mapc (lambda (prop)
                (org-entry-put (point) (car prop) (cdr prop)))
              props))

      (save-restriction
        (narrow-to-region (point) (point))
        (insert description)
        (indent-region (point-min) (point-max) 2)
        (fill-region (point-min) (point-max)))

      (buffer-substring (point-min) (point-max)))))

(defun org-deactivate-timestamp (ts)
  (if (string-match "<\\(.*\\)>$" ts)
      (format "[%s]" (match-string 1 ts))
    ts))

(defun org-show-event (event org-file)
  (let (event-pos)
    (with-current-buffer (find-file-noselect org-file)
      (setq event-pos (org-find-entry-with-id (uid event))))
    (when event-pos
      (switch-to-buffer (find-file org-file))
      (goto-char event-pos)
      (org-show-entry))))

(defun org-update-event (event org-file)
  (with-current-buffer (find-file-noselect org-file)
    (let ((event-pos (org-find-entry-with-id (uid event))))
      (when event-pos
        (goto-char event-pos)
        (let ((entry-end (org-entry-end-position))
              (entry-outline-level (org-outline-level)))
          (forward-line)
          (re-search-forward "^ *[^: ]" entry-end)
          (delete-region (point) entry-end)
          (save-restriction
            (narrow-to-region (point) (point))
            (insert (description event))
            (indent-region (point-min) (point-max) (1+ entry-outline-level))
            (fill-region (point-min) (point-max)))
          (org-entry-put event-pos "DT" (ical->org-timestamp event))
          (org-entry-put event-pos "ORGANIZER" (organizer event))
          (org-entry-put event-pos "LOCATION" (location event))
          (org-entry-put event-pos "RRULE" (recur event))
          (save-buffer))))))

(defun org-cancel-event (id org-file)
  (with-current-buffer (find-file-noselect org-file)
    (let ((event-pos (org-find-entry-with-id id)))
      (when event-pos
        (let ((ts (org-entry-get event-pos "DT")))
          (when ts
            (org-entry-put event-pos "DT" (org-deactivate-timestamp ts))
            (save-buffer)))))))

(defun org-get-event-status (id org-file)
  "Return multiple values.
The first value tells whether given event ID exists in ORG-FILE.
The second value tells whether the event is active."
  (save-excursion
    (with-current-buffer (find-file-noselect org-file)
      (let ((event-pos (org-find-entry-with-id id)))
        (if event-pos
            (let ((props (org-entry-properties event-pos)))
              (if (string= (cdr (assoc "ICAL_EVENT" props)) "t")
                  (values t (not (null (string-match "^<.*>$" (cdr (assoc "DT" props))))))
                (values nil nil)))
          (values nil nil))))))


(defun cal-event-insinuate-org-templates ()
  (unless (cl-find-if (lambda (x) (string= (second x) cal-org-template-name))
                      org-capture-templates)
    (setq org-capture-templates
          (append `((,cal-org-template-key
                     ,cal-org-template-name
                     entry
                     (file+olp ,cal-capture-file ,@cal-capture-headline)
                     "%i"
                     :immediate-finish t))
                  org-capture-templates))))

(defun cal-event-save (ical)
  (with-temp-buffer
    (org-capture-string (ical->org-entry ical) cal-org-template-key)))

(defun cal-event-update (ical)
  (org-update-event ical cal-capture-file))

(defun cal-event-cancel (ical)
  (org-cancel-event (uid ical) cal-capture-file))

(defun cal-event-sync (ical)
  (let ((org-event-exists (first (org-get-event-status
                                  (uid ical) cal-capture-file))))

    ;; FIXME: separate classes for request/cancel events, make this (and
    ;;other) functions methods to simplify the if blocks
    (if org-event-exists
        (if (cancel-event-p ical)
            (cal-event-cancel ical)
          (cal-event-update ical))
      (unless (cancel-event-p ical)
        (cal-event-save ical)))))


(provide 'gnus-cal2org-sync)
;;; gnus-cal2org-sync.el ends here
