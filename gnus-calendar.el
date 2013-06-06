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

(make-variable-buffer-local
 (defvar gnus-calendar-reply-status nil))

(make-variable-buffer-local
 (defvar gnus-calendar-event nil))

(make-variable-buffer-local
 (defvar gnus-calendar-handle nil))

(defvar gnus-calendar-identities
  (cl-mapcan (lambda (x) (if (listp x) x (list x)))
             (list user-full-name (regexp-quote user-mail-address)
                   ; NOTE: this one can be a list
                   gnus-ignored-from-addresses)))

;; TODO: make the template customizable
(defmethod ical-event->gnus-calendar ((event ical-event) &optional reply-status)
  "Format an overview of EVENT details."
  (with-slots (organizer summary description location recur uid method rsvp) event
    (let ((headers `(("Summary" ,summary)
                     ("Location" ,location)
                     ("Time" ,(ical-event:org-timestamp event))
                     ("Organizer" ,organizer)
                     ("Method" ,method))))

      (when (and (not (ical-event-reply-p event)) rsvp)
        (setq headers (append headers
                              `(("Status" ,(or reply-status "Not replied yet"))))))

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
  ;; FIXME: the gnus-mime-button-map keymap does not make sense for this kind
  ;; of button.
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

(defun gnus-calendar-send-buffer-by-mail (buffer-name subject)
  (let ((message-signature nil))
    (with-current-buffer gnus-summary-buffer
      (gnus-summary-reply)
      (message-goto-body)
      (mml-insert-multipart "alternative")
      (mml-insert-empty-tag 'part 'type "text/html")
      (mml-attach-buffer buffer-name "text/calendar; method=REPLY; charset=UTF-8")
      (message-goto-subject)
      (delete-region (line-beginning-position) (line-end-position))
      (insert "Subject: " subject)
      (message-send-and-exit))))

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
                               ": " (ical-event:summary event))))

          (with-current-buffer (get-buffer-create gnus-calendar-reply-bufname)
            (delete-region (point-min) (point-max))
            (insert reply)
            (fold-icalendar-buffer)
            (gnus-calendar-send-buffer-by-mail (buffer-name) subject))

          ;; Back in article buffer
          (setq-local gnus-calendar-reply-status status)
          (when gnus-calendar-org-enabled-p
            (gnus-calendar:org-event-update event status)
            ;; refresh article buffer to update the reply status
            (with-current-buffer gnus-summary-buffer
              (gnus-summary-show-article))))))))

(defun gnus-calendar-sync-event-to-org (event)
  (cal-event:sync-to-org event gnus-calendar-reply-status))

(defun gnus-calendar-mm-inline (handle)
  (let ((event (ical-event-from-handle handle gnus-calendar-identities))
        (reply-status "Not replied yet")
        reply-buttons
        org-buttons)

    (setq gnus-calendar-reply-status nil)

    (when event
      (when (and (not (ical-event-reply-p event))
                 (ical-event:rsvp event))
        (when gnus-calendar-org-enabled-p
          (setq reply-status (or (gnus-calendar:org-event-reply-status event)
                                 reply-status)))

        (setq reply-buttons
              `(("Accept" gnus-calendar-reply (,handle accepted ,event))
                ("Tentative" gnus-calendar-reply (,handle tentative ,event))
                ("Decline" gnus-calendar-reply (,handle declined ,event)))))

      (when gnus-calendar-org-enabled-p
        (let* ((org-entry-exists-p (gnus-calendar:org-entry-exists-p event))
               (export-button-text (if org-entry-exists-p "Update Org Entry" "Export to Org")))

          (setq org-buttons (append org-buttons
                                `(("Show Agenda" gnus-calendar-show-org-agenda ,event))))

          (when (ical-event-request-p event)
            (setq org-buttons (append org-buttons
                                  `((,export-button-text gnus-calendar-sync-event-to-org ,event)))))
          (when org-entry-exists-p
            (setq org-buttons (append org-buttons
                                  `(("Show Org Entry" gnus-calendar-show-org-entry ,event)))))))

      (cl-flet ((insert-button-group (buttons)
                  (when buttons
                    (mapc (lambda (x)
                            (apply 'gnus-calendar-insert-button x)
                            (insert "    "))
                          buttons)
                    (insert "\n\n"))))

        (insert-button-group reply-buttons)
        (insert-button-group org-buttons))

      (setq gnus-calendar-event event
            gnus-calendar-handle handle)
      (insert (ical-event->gnus-calendar event reply-status)))))

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

(defun gnus-calendar-reply-accept ()
  "Accept invitation in the current article."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-reply (list gnus-calendar-handle 'accepted gnus-calendar-event))
    (setq-local gnus-calendar-reply-status 'accepted)))

(defun gnus-calendar-reply-tentative ()
  "Send tentative response to invitation in the current article."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-reply (list gnus-calendar-handle 'tentative gnus-calendar-event))
    (setq-local gnus-calendar-reply-status 'tentative)))

(defun gnus-calendar-reply-decline ()
  "Decline invitation in the current article."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-reply (list gnus-calendar-handle 'declined gnus-calendar-event))
    (setq-local gnus-calendar-reply-status 'declined)))

(defun gnus-calendar-event-export ()
  "Export calendar event to `org-mode', or update existing agenda entry."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (gnus-calendar-sync-event-to-org gnus-calendar-event))
  ;; refresh article buffer in case the reply had been sent before initial org
  ;; export
  (with-current-buffer gnus-summary-buffer
    (gnus-summary-show-article)))

(defun gnus-calendar-event-show ()
  "Display `org-mode' agenda entry related to the calendar event."
  (interactive)
  (gnus-calendar-show-org-entry
   (with-current-buffer gnus-article-buffer
     gnus-calendar-event)))

(defun gnus-calendar-event-check-agenda ()
  "Display `org-mode' agenda for days between event start and end dates."
  (interactive)
  (gnus-calendar-show-org-agenda
   (with-current-buffer gnus-article-buffer gnus-calendar-event)))

(defun gnus-calendar-setup ()
  (add-to-list 'mm-inlined-types "text/calendar")
  (add-to-list 'mm-automatic-display "text/calendar")
  (add-to-list 'mm-inline-media-tests '("text/calendar" gnus-calendar-mm-inline identity))

  (gnus-define-keys (gnus-summary-calendar-map "i" gnus-summary-mode-map)
    "a" gnus-calendar-reply-accept
    "t" gnus-calendar-reply-tentative
    "d" gnus-calendar-reply-decline
    "c" gnus-calendar-event-check-agenda
    "e" gnus-calendar-event-export
    "s" gnus-calendar-event-show)

  (require 'gnus-art)
  (add-to-list 'gnus-mime-action-alist
               (cons "save calendar event" 'gnus-calendar-save-event)
               t))

(provide 'gnus-calendar)
;;; gnus-calendar.el ends here
