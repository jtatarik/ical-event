;;; gnus-calendar.el --- 

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
(require 'ical-gnus2org-sync)
(require 'mm-decode)

(add-to-list 'mm-inlined-types "text/calendar")
(add-to-list 'mm-automatic-display "text/calendar")
(add-to-list 'mm-inline-media-tests '("text/calendar" mm-inline-text-calendar identity))

(defun mm-inline-text-calendar (handle)
  (let ((ical (with-temp-buffer
               (mm-insert-part handle)
               (mm-decode-coding-region (point-min) (point-max) 'utf-8)
               (ical-from-buffer (current-buffer)))))

    (when ical
      (insert (ical->gnus-view ical)))))

(defun icalendar-save-part (handle)
  (when (equal (car (mm-handle-type handle)) "text/calendar")
    (let ((ical (with-temp-buffer
                 (mm-insert-part handle)
                 (mm-decode-coding-region (point-min) (point-max) 'utf-8)
                 (ical-from-buffer (current-buffer)))))

      ;; FIXME: save on new, sync on existing, cancel on cancel
      (when ical
        (org-capture-string (ical->org-entry ical) cal-org-template-key)))))


(defun icalendar-save-event ()
  "Save the Calendar event in the text/calendar part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (icalendar-save-part data))))

;; FIXME: should go to .emacs
(require 'gnus-art)
(add-to-list 'gnus-mime-action-alist
             (cons "save calendar event" 'icalendar-save-event)
             t)

(provide 'gnus-calendar)
;;; gnus-calendar.el ends here
