ical-event
==========

Sync iCal event invitations from Gnus to Org agenda.

Setup
=====

    (add-to-list 'load-path "/path/to/ical-event")
    (require 'gnus-calendar)
    (gnus-calendar-setup)
    ;; both the capture file and the headline(s) inside must already exist
    (setq cal-capture-file "~/org/agenda.org")
    (setq cal-capture-headline '("Calendar"))
    (cal-event-insinuate-org-templates)


