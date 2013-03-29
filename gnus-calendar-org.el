;;; gnus-calendar-org.el --- store iCalendar events as org-mode entries

;; Copyright (C) 2013  Jan Tatarik

;; Author: Jan Tatarik <Jan.Tatarik@gmail.com>
;; Keywords: mail, icalendar, org

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


(defgroup gnus-calendar-org nil
  "Settings for Calendar Event gnus/org integration."
  :group 'gnus-calendar
  :prefix "gnus-calendar-org-")

(defcustom gnus-calendar-org-capture-file nil
  "Target Org file for storing captured calendar events."
  :type '(file)
  :group 'gnus-calendar-org)

(defcustom gnus-calendar-org-capture-headline nil
  "Target outline in `gnus-calendar-org-capture-file' for storing captured events."
  :type '(repeat string)
  :group 'gnus-calendar-org)

(defcustom gnus-calendar-org-template-name "used by gnus-calendar-org"
  "Org-mode template name."
  :type '(string)
  :group 'gnus-calendar-org)

(defcustom gnus-calendar-org-template-key "#"
  "Org-mode template hotkey."
  :type '(string)
  :group 'gnus-calendar-org)

(defvar gnus-calendar-org-enabled-p nil)


(defmethod ical-event:org-repeat ((event ical-event))
  "Return `org-mode' timestamp repeater string for recurring EVENT.
Return nil for non-recurring EVENT."
  (when (ical-event:recurring-p event)
    (let* ((freq-map '(("HOURLY" . "h")
                       ("DAILY" . "d")
                       ("WEEKLY" . "w")
                       ("MONTHLY" . "m")
                       ("YEARLY" . "y")))
           (org-freq (cdr (assoc (ical-event:recurring-freq event) freq-map))))

      (when org-freq
        (format "+%s%s" (ical-event:recurring-interval event) org-freq)))))

(defmethod ical-event:org-timestamp ((event ical-event))
  "Build `org-mode' timestamp from EVENT start/end dates and recurrence info."
  (let* ((start (ical-event:start-time event))
         (end (ical-event:end-time event))
         (start-date (format-time-string "%Y-%m-%d %a" start t))
         (start-time (format-time-string "%H:%M" start t))
         (end-date (format-time-string "%Y-%m-%d %a" end t))
         (end-time (format-time-string "%H:%M" end t))
         (org-repeat (ical-event:org-repeat event))
         (repeat (if org-repeat (concat " " org-repeat) "")))

    (if (equal start-date end-date)
        (format "<%s %s-%s%s>" start-date start-time end-time repeat)
      (format "<%s %s>--<%s %s>" start-date start-time end-date end-time))))

;; TODO: make the template customizable
(defmethod ical-event->org-entry ((event ical-event))
  "Return string with new `org-mode' entry describing EVENT."
  (with-temp-buffer
    (org-mode)
    (with-slots (organizer summary description location
                           recur uid) event
      (let ((props `(("ICAL_EVENT" . "t")
                     ("ID" . ,uid)
                     ("DT" . ,(ical-event:org-timestamp event))
                     ("ORGANIZER" . ,(ical-event:organizer event))
                     ("LOCATION" . ,(ical-event:location event))
                     ("RRULE" . ,(ical-event:recur event)))))

        (insert (format "* %s (%s)\n\n" summary location))
        (mapc (lambda (prop)
                (org-entry-put (point) (car prop) (cdr prop)))
              props))

      (save-restriction
        (narrow-to-region (point) (point))
        (insert description)
        (indent-region (point-min) (point-max) 2)
        (fill-region (point-min) (point-max)))

      (buffer-string))))

(defun gnus-calendar--deactivate-org-timestamp (ts)
  (replace-regexp-in-string "[<>]"
                            (lambda (m) (pcase m ("<" "[") (">" "]")))
                            ts))

(defun gnus-calendar--show-org-event (event org-file)
  (let (event-pos)
    (with-current-buffer (find-file-noselect org-file)
      (setq event-pos (org-find-entry-with-id (ical-event:uid event))))
    (when event-pos
      (switch-to-buffer (find-file org-file))
      (goto-char event-pos)
      (org-show-entry))))

(defun gnus-calendar--update-org-event (event org-file)
  (with-current-buffer (find-file-noselect org-file)
    (with-slots (uid description organizer location recur) event
      (let ((event-pos (org-find-entry-with-id uid)))
        (when event-pos
          (goto-char event-pos)
          (let ((entry-end (org-entry-end-position))
                (entry-outline-level (org-outline-level)))
            (forward-line)
            (re-search-forward "^ *[^: ]" entry-end)
            (delete-region (point) entry-end)
            (save-restriction
              (narrow-to-region (point) (point))
              (insert description)
              (indent-region (point-min) (point-max) (1+ entry-outline-level))
              (fill-region (point-min) (point-max)))
            (org-entry-put event-pos "DT" (ical-event:org-timestamp event))
            (org-entry-put event-pos "ORGANIZER" organizer)
            (org-entry-put event-pos "LOCATION" location)
            (org-entry-put event-pos "RRULE" recur)
            (save-buffer)))))))

(defun gnus-calendar--cancel-org-event (event org-file)
  (with-current-buffer (find-file-noselect org-file)
    (let ((event-pos (org-find-entry-with-id (ical-event:uid event))))
      (when event-pos
        (let ((ts (org-entry-get event-pos "DT")))
          (when ts
            (org-entry-put event-pos "DT" (gnus-calendar--deactivate-org-timestamp ts))
            (save-buffer)))))))

(defun gnus-calendar-org-event-exists-p (id org-file)
  "Return t when given event ID exists in ORG-FILE."
  (save-excursion
    (with-current-buffer (find-file-noselect org-file)
      (let ((event-pos (org-find-entry-with-id id)))
        (when event-pos
          (string= (cdr (assoc "ICAL_EVENT" (org-entry-properties event-pos)))
                   "t"))))))


(defun gnus-calendar-insinuate-org-templates ()
  (unless (cl-find-if (lambda (x) (string= (second x) gnus-calendar-org-template-name))
                      org-capture-templates)
    (setq org-capture-templates
          (append `((,gnus-calendar-org-template-key
                     ,gnus-calendar-org-template-name
                     entry
                     (file+olp ,gnus-calendar-org-capture-file ,@gnus-calendar-org-capture-headline)
                     "%i"
                     :immediate-finish t))
                  org-capture-templates))

    ;; hide the template from interactive template selection list (org-capture)
    (when (boundp 'org-capture-templates-contexts)
      (push `(,gnus-calendar-org-template-key ((lambda () gnus-article-mode)))
            org-capture-templates-contexts))))

(defun gnus-calendar:org-event-save (event)
  (with-temp-buffer
    (org-capture-string (ical-event->org-entry event) gnus-calendar-org-template-key)))

(defun gnus-calendar:org-event-update (event)
  (gnus-calendar--update-org-event event gnus-calendar-org-capture-file))

(defun gnus-calendar:org-event-cancel (event)
  (gnus-calendar--cancel-org-event event gnus-calendar-org-capture-file))

(defun gnus-calendar-show-org-entry (event)
  (gnus-calendar--show-org-event event gnus-calendar-org-capture-file))

(defmethod cal-event:sync-to-org ((event ical-event-request))
  (if (gnus-calendar-org-event-exists-p (ical-event:uid event) gnus-calendar-org-capture-file)
      (gnus-calendar:org-event-update event)
    (gnus-calendar:org-event-save event)))

(defmethod cal-event:sync-to-org ((event ical-event-cancel))
  (when (gnus-calendar-org-event-exists-p
         (ical-event:uid event) gnus-calendar-org-capture-file)
    (gnus-calendar:org-event-cancel event)))

(defun gnus-calendar-org-setup ()
  (if (and gnus-calendar-org-capture-file gnus-calendar-org-capture-headline)
      (progn
        (gnus-calendar-insinuate-org-templates)
        (setq gnus-calendar-org-enabled-p t))
    (message "Cannot enable Calendar->Org: missing capture file, headline")))

(provide 'gnus-calendar-org)
;;; gnus-calendar-org.el ends here
