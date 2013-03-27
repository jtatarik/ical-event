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
(require 'gnus-sum)

;; FIXME: better separate reply/sync functionalities
;; FIXME: configurable reply temp buffer name

;; (defcustom cal-bufname "*CAL*"
;;   "Buffer name for displaying calendar event details."
;;   :type '(string)
;;   :group 'cal-event)


(defvar gnus-calendar-identities
  (cl-mapcan (lambda (x) (if (listp x) x (list x)))
             (list user-full-name (regexp-quote user-mail-address)
                   ; NOTE: this one can be a list
                   gnus-ignored-from-addresses)))

;; TODO: make the template customizable
(defmethod ical->gnus-view ((event ical-event))
  "Format an overview of EVENT details."
  (with-slots (organizer summary description location recur uid method) event
    (let ((headers `(("Summary" ,summary)
                     ("Location" ,location)
                     ("Time" ,(ical->org-timestamp event))
                     ("Organizer" ,organizer)
                     ("Method" ,method))))

      (concat
       (apply #'concat (mapcar (lambda (x)
                                 (format "%-12s%s\n"
                                         (propertize (concat (car x) ":") 'face 'bold)
                                         (cadr x)))
                               headers))
       "\n"
       description))))

(defmacro with-buffer-from-handle (handle &rest body)
  (let ((charset (make-symbol "charset")))
    `(let ((,charset (cdr (assoc 'charset (mm-handle-type ,handle)))))
       (with-temp-buffer
         (mm-insert-part ,handle)
         (when (string= ,charset "utf-8")
           (mm-decode-coding-region (point-min) (point-max) 'utf-8))

         ,@body))))


(defun ical-from-handle (handle &optional attendee-name-or-email)
  (with-buffer-from-handle handle
      (ical-event-from-buffer (current-buffer) attendee-name-or-email)))

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

(defun send-buffer-by-mail (buffer-name recipient subject)
  (compose-mail)
  (message-goto-body)
  (mml-attach-buffer buffer-name "text/calendar" nil "attachment")
  (message-goto-to)
  (insert recipient)
  (message-goto-subject)
  (insert subject)
  ;(message-send-and-exit)
  )

(defun gnus-icalendar-reply (data)
  (let* ((handle (first data))
         (status (second data))
         (ical (third data))
         (reply (with-buffer-from-handle handle
                   (event-to-reply (current-buffer) status gnus-calendar-identities))))

    (when reply
      (let ((subject (concat (capitalize (symbol-name status))
                             ": " (summary ical)))
            (organizer (ical-event:organizer ical)))

        (message reply)
        (with-current-buffer (get-buffer-create "*CAL*")
          (delete-region (point-min) (point-max))
          (insert reply)
          (send-buffer-by-mail (buffer-name) organizer subject))))))


(defun mm-inline-text-calendar (handle)
  (let ((ical (ical-from-handle handle gnus-calendar-identities))
        buttons)

    (when ical
      (when (ical-event:rsvp ical)
        (setq buttons (append `(("Accept" gnus-icalendar-reply (,handle accepted ,ical))
                                ("Tentative" gnus-icalendar-reply (,handle tentative ,ical))
                                ("Decline" gnus-icalendar-reply (,handle declined ,ical)))
                              buttons)))

      ;; TODO: sync to org should be an optional feature, too
      (when t
        (setq buttons (append buttons
                              `(("Export to Org" cal-event-sync ,ical)
                                ("Show Org Entry" cal-event-show-org-entry ,ical)))))

      (when buttons
        (mapc (lambda (x)
                (apply 'gnus-icalendar-insert-button x)
                (insert "    "))
              buttons)
        (insert "\n\n"))

      (insert (ical->gnus-view ical)))))

(defun icalendar-save-part (handle)
  (let (ical)
    (when (and (equal (car (mm-handle-type handle)) "text/calendar")
               (setq ical (ical-from-handle handle gnus-calendar-identities)))

      (cal-event-sync ical))))


(defun icalendar-save-event ()
  "Save the Calendar event in the text/calendar part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (icalendar-save-part data))))

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
