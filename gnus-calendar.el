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
(require 'gnus-cal2org-sync)
(require 'mm-decode)


;; TODO: make the template customizable
(defmethod ical->gnus-view ((event cal-event))
  "Format an overview of EVENT details."
  (with-slots (organizer summary description location recur uid method) event
    (format "Summary:   %s
Location:  %s
Time:      %s
Organizer: %s
Method:    %s

%s
" summary location (ical->org-timestamp event)
   organizer method description)))

(defun ical-from-handle (handle)
  (let ((charset (cdr (assoc 'charset (mm-handle-type handle)))))
    (with-temp-buffer
      (mm-insert-part handle)
      (when (string= charset "utf-8")
        (mm-decode-coding-region (point-min) (point-max) 'utf-8))
      (ical-from-buffer (current-buffer)))))

(defun gnus-icalendar-insert-button (text callback data)
  (let ((start (point)))
    (gnus-add-text-properties
     start
     (progn
       (insert "[ " text " ]")
       (point))
     `(gnus-callback
       ,callback
       keymap ,gnus-mime-button-map
       face ,gnus-article-button-face
       gnus-data ,data))
    (widget-convert-button 'link start (point)
                           :action 'gnus-widget-press-button
                           :button-keymap gnus-widget-button-keymap)))

(defun gnus-icalendar-accept (ed)
  (message "Not implemented."))

(defun gnus-icalendar-decline (ed)
  (message "Not implemented."))

(defun mm-inline-text-calendar (handle)
  (let ((ical (ical-from-handle handle)))

    (when ical
      (when (rsvp ical)
        (gnus-icalendar-insert-button "Accept" 'gnus-icalendar-accept ical)
        (insert "    ")
        (gnus-icalendar-insert-button "Decline" 'gnus-icalendar-decline ical)
        (insert "    "))
      ;; TODO: sync to org should be optional, too
      (gnus-icalendar-insert-button "Export to Org" 'cal-event-sync ical)
      (insert "\n\n")
      (insert (ical->gnus-view ical)))))

(defun icalendar-save-part (handle)
  (let (ical)
    (when (and (equal (car (mm-handle-type handle)) "text/calendar")
               (setq ical (ical-from-handle handle)))

      (cal-event-sync ical))))


(defun icalendar-save-event ()
  "Save the Calendar event in the text/calendar part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (icalendar-save-part data))))

;; FIXME: should go to .emacs
;; TODO: offer to show the org entry?
(defun gnus-calendar-setup ()
  (add-to-list 'mm-inlined-types "text/calendar")
  (add-to-list 'mm-automatic-display "text/calendar")
  (add-to-list 'mm-inline-media-tests '("text/calendar" mm-inline-text-calendar identity))

  (require 'gnus-art)
  (add-to-list 'gnus-mime-action-alist
               (cons "save calendar event" 'icalendar-save-event)
               t))

(provide 'gnus-calendar)
;;; gnus-calendar.el ends here
