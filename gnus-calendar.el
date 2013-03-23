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

(add-to-list 'mm-inlined-types "text/calendar")
(add-to-list 'mm-automatic-display "text/calendar")
(add-to-list 'mm-inline-media-tests '("text/calendar" mm-inline-text-calendar identity))

(defun mm-inline-text-calendar (handle)
  (let ((cal (with-temp-buffer
               (mm-insert-part handle)
               (mm-decode-coding-region (point-min) (point-max) 'utf-8)
               (ical-from-buffer (current-buffer)))))

    (insert (ical->gnus-view cal))))


(provide 'gnus-calendar)
;;; gnus-calendar.el ends here
