ical-event
==========

Accept/Decline iCalendar appointment invitations from Gnus.
Sync iCalendar event invitations from Gnus to Org agenda.

Status
======

This code works and so I'm trying to get it merged into Gnus proper. That
means I will not implement any new features here.

You're invited to look at
https://github.com/jtatarik/gnus/blob/icalendar/lisp/gnus-icalendar.el, where
I'm working to make the code acceptable for inclusion in Gnus.


Setup
=====

    (add-to-list 'load-path "/path/to/ical-event")
    (require 'gnus-calendar)
    (gnus-calendar-setup)
    
    ;; to enable optional iCalendar->Org sync functionality
    ;; NOTE: both the capture file and the headline(s) inside must already exist
    (setq gnus-calendar-org-capture-file "~/org/agenda.org")
    (setq gnus-calendar-org-capture-headline '("Calendar"))
    (gnus-calendar-org-setup)


