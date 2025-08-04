(setq gnus-select-method '(nntp "news.gwene.org"))

(setq user-full-name "Oliver Leis")
(setq user-mail-address "oliver.leis@gmail.com")

(setq smtpmail-auth-credentials "~/.authinfo.epg")
(add-to-list 'gnus-secondary-select-methods
        '(nnimap "gmail"
           (nnimap-address "imap.gmail.com")
           (nnimap-server-port 993)
           (nnimap-stream ssl)
           (nnimap-authinfo-file "~/.authinfo.epg")
           )
        )


(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      smtpmail-debug-verb t
      gnus-agent nil
      gnus-message-archive-group nil
      gnus-use-demon t
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-group-line-format "%M%S%5y/%-5t: %uG %D\n")
(defun gnus-user-format-function-G (arg)
  (let ((mapped-name (assoc gnus-tmp-group group-name-map)))
    (if (null mapped-name)
        gnus-tmp-group
      (cdr mapped-name))))

(when (eq system-type 'gnu/linux)
  (defun gnus-notify-inbox ()
      (let* ((inbox "nnimap+gmail:INBOX")
             (unread (gnus-group-unread inbox)))
        (when (and unread (> unread 0))
          (call-process "notify-send" nil 0 nil
                        "Gnus"
			"-i" "mail"
                        (format "Du hast %d neue Nachricht(en) im Posteingang." unread)))))
(defun gnus-check-inbox-and-notify ()
  (gnus-group-get-new-news)
  (gnus-notify-inbox))
(add-hook 'gnus-startup-hook
  (lambda ()
    (setq gnus-demon-timestep 60)
    (gnus-demon-init)
    (gnus-demon-add-handler 'gnus-check-inbox-and-notify 5 t))))

(setq group-name-map '(("nnimap+gmail:INBOX" . "Posteingang")
                       ("nnimap+gmail:[Google Mail]/Gesendet" . "Gesendet")
                       ("nnimap+gmail:[Google Mail]/Wichtig" . "Wichtig")
                       ("nnimap+gmail:[Google Mail]/Papierkorb" . "Papierkorb")
                       ("nnimap+gmail:[Google Mail]/Alle Nachrichten" . "Alle Nachrichten")
                       ("nndraft:drafts" . "Entwürfe")
        	       ("nnimap+gmail:[Google Mail]/Spam" . "Spam")
        	       ("nnimap+gmail:archiviert/2020" . "2020")
        	       ("nnimap+gmail:archiviert/2021" . "2021")
        	       ("nnimap+gmail:archiviert/2022" . "2022")
        	       ("nnimap+gmail:archiviert/2023" . "2023")
        	       ("nnimap+gmail:archiviert/2024" . "2024")
        	       ("nnimap+gmail:archiviert/alt" . "alt")
        	       ("gwene.de.heise.developer" . "Heise Developer")
        	       ("gwene.de.heise.newsticker" . "Heise News")
		       ("gwene.de.golem.open-source" . "Golem Open Source")
		       ("gwene.net.eurogamer" . "Eurogamer")
		       ("gwene.de.linuxnews.de" . "Linuxnews")
		       ("gwene.de.tagesschau". "Tagesschau")
		       ("gwene.de.sportschau.fussball". "Sportschau Fußball")
		       ("gwene.gr.kathimerini.latestnews". "Kathimerini")
		       ("gmane.comp.xfce.announce". "Xfce Ankündigungen")))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
