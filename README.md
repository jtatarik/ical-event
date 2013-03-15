ical-event
==========

Sync iCal event invitations from Gnus to Org agenda.

Setup
=====

    (add-to-list 'load-path "/path/to/ical-event")
    (require 'ical-gnus2org-sync)
    ;; both the capture file and the headline(s) inside must already exist
    (setq cal-capture-file "~/org/agenda.org")
    (setq cal-capture-headline '("Calendar"))
    (cal-event-insinuate-org-templates)

    (defun my:gnus-window-layout ()
      "Set up custom window layout for Gnus."
      ;; custom window layout for gnus (with optional bbdb window)
      (setq gnus-window-to-buffer
            (append (list '(bbdb . "*BBDB*") (cons 'cal cal-bufname))
                    gnus-window-to-buffer))

      (gnus-add-configuration
       '(article
         (horizontal 1.0
                     (vertical 43
                               (group 1.0)
                               ;; the height of the bbdb window is bogus here, it
                               ;; has to be customized from bbdb itself
                               (if (buffer-live-p (get-buffer "*BBDB*")) '(bbdb 10)))
                     (vertical 1.0
                               (summary 12 point)
                               (horizontal 1.0
                                           (article 1.0)
                                           (if (buffer-live-p (get-buffer cal-bufname))
                                               '(cal 0.5)))))))

      (gnus-add-configuration
       '(summary
         (horizontal 1.0
                     (vertical 43 (group 1.0))
                     (vertical 1.0 (summary 1.0 point))))))

    (add-hook 'gnus-started-hook #'my:gnus-window-layout)
    (add-hook 'gnus-mark-article-hook #'gnus-kill-optional-layout-buffers)
    (setq gnus-article-mime-part-function #'import-icalendar-part)
    (setq gnus-always-force-window-configuration t)

