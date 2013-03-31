;;; gnus-calendar.el --- inline iCalendar display in gnus articles

;; Copyright (C) 2013  Jan Tatarik

;; Author: Jan Tatarik <Jan.Tatarik@gmail.com>
;; Keywords: mail, icalendar

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
(require 'ical-event-reply)
(require 'gnus-calendar-org)
(require 'mm-decode)
(require 'gnus-sum)


(defgroup gnus-calendar nil
  "Settings for inline display of iCalendar events."
  :group 'gnus-article
  :prefix "gnus-calendar-")

(defcustom gnus-calendar-reply-bufname "*CAL*"
  "Buffer used for building iCalendar reply."
  :type '(string)
  :group 'gnus-calendar)


(defvar gnus-calendar-identities
  (cl-mapcan (lambda (x) (if (listp x) x (list x)))
             (list user-full-name (regexp-quote user-mail-address)
                   ; NOTE: this one can be a list
                   gnus-ignored-from-addresses)))

;; TODO: make the template customizable
(defmethod ical-event->gnus-calendar ((event ical-event))
  "Format an overview of EVENT details."
  (with-slots (organizer summary description location recur uid method) event
    (let ((headers `(("Summary" ,summary)
                     ("Location" ,location)
                     ("Time" ,(ical-event:org-timestamp event))
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

(defmacro with-decoded-handle (handle &rest body)
  "Execute BODY in buffer containing the decoded contents of HANDLE."
  (let ((charset (make-symbol "charset")))
    `(let ((,charset (cdr (assoc 'charset (mm-handle-type ,handle)))))
       (with-temp-buffer
         (mm-insert-part ,handle)
         (when (string= ,charset "utf-8")
           (mm-decode-coding-region (point-min) (point-max) 'utf-8))

         ,@body))))


(defun ical-event-from-handle (handle &optional attendee-name-or-email)
  (with-decoded-handle handle
      (ical-event-from-buffer (current-buffer) attendee-name-or-email)))

(defun gnus-calendar-insert-button (text callback data)
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

(defun gnus-calendar-send-buffer-by-mail (buffer-name recipient subject)
  (compose-mail)
  (message-goto-body)
  (mml-attach-buffer buffer-name "text/calendar; method=REPLY; charset=UTF-8" nil "inline")
  (message-goto-to)
  (insert recipient)
  (message-goto-subject)
  (insert subject)
  ;(message-send-and-exit)
  )

(defun gnus-calendar-reply (data)
  (let* ((handle (first data))
         (status (second data))
         (event (third data))
         (reply (with-decoded-handle handle
                   (ical-event-reply-from-buffer (current-buffer)
                                                 status gnus-calendar-identities))))

    (when reply
      (cl-flet ((fold-icalendar-buffer ()
                  (goto-char (point-min))
                  (while (re-search-forward "^\\(.\\{72\\}\\)\\(.+\\)$" nil t)
                    (replace-match "\\1\n \\2")
                    (goto-char (line-beginning-position)))))
        (let ((subject (concat (capitalize (symbol-name status))
                               ": " (summary event)))
              (organizer (ical-event:organizer event)))

          (with-current-buffer (get-buffer-create gnus-calendar-reply-bufname)
            (delete-region (point-min) (point-max))
            (insert reply)
            (fold-icalendar-buffer)
            (gnus-calendar-send-buffer-by-mail (buffer-name) organizer subject)))))))


(defun gnus-calendar-mm-inline (handle)
  (let ((event (ical-event-from-handle handle gnus-calendar-identities))
        buttons)

    (when event
      (when (ical-event:rsvp event)
        (setq buttons (append `(("Accept" gnus-calendar-reply (,handle accepted ,event))
                                ("Tentative" gnus-calendar-reply (,handle tentative ,event))
                                ("Decline" gnus-calendar-reply (,handle declined ,event)))
                              buttons)))

      (when gnus-calendar-org-enabled-p
        (setq buttons (append buttons
                              `(("Export to Org" cal-event:sync-to-org ,event)
                                ("Show Org Entry" gnus-calendar-show-org-entry ,event)))))

      (when buttons
        (mapc (lambda (x)
                (apply 'gnus-calendar-insert-button x)
                (insert "    "))
              buttons)
        (insert "\n\n"))

      (insert (ical-event->gnus-calendar event)))))

(defun gnus-calendar-save-part (handle)
  (let (event)
    (when (and (equal (car (mm-handle-type handle)) "text/calendar")
               (setq event (ical-event-from-handle handle gnus-calendar-identities)))

      (cal-event:sync-to-org event))))


(defun gnus-calendar-save-event ()
  "Save the Calendar event in the text/calendar part under point."
  (interactive)
  (gnus-article-check-buffer)
  (let ((data (get-text-property (point) 'gnus-data)))
    (when data
      (gnus-calendar-save-part data))))

(defun gnus-calendar-setup ()
  (add-to-list 'mm-inlined-types "text/calendar")
  (add-to-list 'mm-automatic-display "text/calendar")
  (add-to-list 'mm-inline-media-tests '("text/calendar" gnus-calendar-mm-inline identity))

  (require 'gnus-art)
  (add-to-list 'gnus-mime-action-alist
               (cons "save calendar event" 'gnus-calendar-save-event)
               t))

(provide 'gnus-calendar)
;;; gnus-calendar.el ends here
