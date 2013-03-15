;;; ical-gnus2org-sync.el --- Sync calendar events from gnus to org

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
(require 'ical-event)

(defgroup cal-event nil "Settings for Calendar Event gnus/org integration."
  :group 'convenience
  :prefix "cal")

(defcustom cal-bufname "*CAL*"
  "Buffer name for displaying calendar event details."
  :type '(string)
  :group 'cal-event)

(defcustom cal-capture-file nil
  "Target Org file for storing captured calendar events."
  :type '(file)
  :group 'cal-event)

(defcustom cal-capture-headline nil
  "Target outline in `cal-capture-file' for storing captured events."
  :type '(repeat string)
  :group 'cal-event)

(defgroup cal-event-faces nil "Faces used in Calendar Event buffer."
  :group 'cal-event
  :prefix "cal")

(defcustom cal-title-face 'info-title-1
  "Title face for calendar event buffer."
  :type '(face)
  :group 'cal-event-faces)

(defcustom cal-header-name-face 'font-lock-comment-face
  ""
  :type '(face)
  :group 'cal-event-faces)

(defcustom cal-header-time-face 'font-lock-function-name-face
  ""
  :type '(face)
  :group 'cal-event-faces)

(defcustom cal-header-summary-face 'font-lock-variable-name-face
  ""
  :type '(face)
  :group 'cal-event-faces)

(defcustom cal-header-location-face 'font-lock-keyword-face
  ""
  :type '(face)
  :group 'cal-event-faces)

(defcustom cal-header-orgranizer-face 'font-lock-constant-face
  ""
  :type '(face)
  :group 'cal-event-faces)

(defcustom cal-header-method-face 'font-lock-doc-face
  ""
  :type '(face)
  :group 'cal-event-faces)


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




;; (defun cal-capture-target ()
;;   (append (list cal-capture-file) cal-capture-headline))

(defun gnus-kill-optional-layout-buffers ()
  (cl-flet ((kill-buf (buf)
                      (when (get-buffer buf)
                        (with-current-buffer buf
                          (set-buffer-modified-p nil)
                          (kill-buffer buf)))))
    (mapc #'kill-buf (list "*BBDB*" cal-bufname))))


(make-variable-buffer-local
 (defvar cal-event nil))

(defun gnus-calendar-buffer (ical)
  (multiple-value-bind (org-event-exists-p org-event-active-p)
      (org-get-event-status (uid ical) cal-capture-file)
    (with-current-buffer (get-buffer-create cal-bufname)
      (let (title buttons)
        (setq buffer-read-only nil)
        (delete-region (point-min) (point-max))

        ;; title & buttons
        (if (request-event-p ical)
            (if org-event-exists-p
                (progn
                  (setq title "Event Update")
                  (setq buttons '(("Update" . cal-event-buffer-update)))
                  (when org-event-active-p
                    (setq buttons
                          (append buttons '(("Cancel" . cal-event-buffer-cancel))))))
              (setq title "New Event")
              (setq buttons '(("Save" . cal-event-buffer-save))))
          (setq title "Event Canceled")
          (when org-event-active-p
            (setq buttons '(("Cancel" . cal-event-buffer-cancel)))))

        (insert (propertize title 'font-lock-face cal-title-face)
                "\n\n")

        (when buttons
          (mapc (lambda (b)
                  (insert-text-button (car b) 'action (cdr b))
                  (insert " "))
                buttons)
          (newline 2))

        (save-restriction
          (narrow-to-region (point) (point))
          (insert (ical->gnus-view ical))
          (goto-char (point-min))
          (when (re-search-forward "\n\n" nil t)
            (fill-region (point) (point-max))))
        (goto-char (point-min))
        (cal-event-mode)
        (setq-local cal-event ical)))))

;; gnus function to be called as gnus-article-mime-part-function
(defun import-icalendar-part (handle)
  "Offer to import text/calendar MIME HANDLE into org."
  (when (and (bufferp (car handle))
             (string= (mm-handle-media-type handle) "text/calendar"))

    (let ((ical (with-temp-buffer
                  (mm-display-inline handle)
                  (ical-from-buffer (current-buffer)))))

      (when ical
        (gnus-calendar-buffer ical)))))

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
        (let ((entry-end (org-entry-end-position)))
          (forward-line)
          (re-search-forward "^ *[^: ]" entry-end)
          (delete-region (point) entry-end)
          (insert (description event))
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

(defvar cal-event-keywords '("Summary" "Location" "Time" "Organizer" "Method"))

(defun cal-event-cancel ()
  (org-cancel-event (uid cal-event) cal-capture-file))

(defun cal-event-buffer-cancel (&optional ignore)
  (interactive)
  (cal-event-cancel)
  (gnus-revert-calendar-buffer))

(defun cal-event-update ()
  (org-update-event cal-event cal-capture-file))

(defun cal-event-buffer-update (&optional ignore)
  (interactive)
  (cal-event-update)
  (gnus-revert-calendar-buffer))

(defun cal-event-save ()
  (let ((ical cal-event))
    (with-temp-buffer
      (org-capture-string (ical->org-entry ical) cal-org-template-key))))

(defun cal-event-buffer-save (&optional ignore)
  (interactive)
  (cal-event-save)
  (gnus-revert-calendar-buffer))

(defun cal-event-buffer-sync ()
  (interactive)
  (let ((org-event-exists (first (org-get-event-status
                                  (uid cal-event) cal-capture-file))))

    (if org-event-exists
        (if (cancel-event-p cal-event)
            (cal-event-buffer-cancel)
          (cal-event-buffer-update))
      (unless (cancel-event-p cal-event)
        (cal-event-buffer-save)))))

(defun cal-event-buffer-show-org-entry ()
  (interactive)
  (org-show-event cal-event cal-capture-file))

(defun gnus-revert-calendar-buffer (&rest ignore)
  (gnus-calendar-buffer cal-event))

(define-derived-mode cal-event-mode special-mode "CalEvent"
  :group 'cal-event
  (setq font-lock-defaults
        `(((,(concat "^" (regexp-opt cal-event-keywords t) ":") . cal-header-name-face)
           ("^Time:\\(.*?\\)$" . (1 cal-header-time-face))
           ("^Summary:\\(.*?\\)$" . (1 cal-header-summary-face))
           ("^Location:\\(.*?\\)$" . (1 cal-header-location-face))
           ("^Organizer:\\(.*?\\)$" . (1 cal-header-orgranizer-face))
           ("^Method:\\(.*?\\)$" . (1 cal-header-method-face)))))

  (setq-local revert-buffer-function #'gnus-revert-calendar-buffer)

  (define-key cal-event-mode-map "s" 'cal-event-buffer-sync)
  (define-key cal-event-mode-map "c" 'cal-event-buffer-cancel)
  (define-key cal-event-mode-map "v" 'cal-event-buffer-show-org-entry))

(defcustom cal-org-template-name "used by cal-event-mode"
  "Org-mode template name."
  :type '(string)
  :group 'cal-event)

(defcustom cal-org-template-key "#"
  "Org-mode template hotkey."
  :type '(char)
  :group 'cal-event)

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

;; TODO: this should go to user's .emacs
(cal-event-insinuate-org-templates)


(provide 'ical-gnus2org-sync)
;;; ical-gnus2org-sync.el ends here
