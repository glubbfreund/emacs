;; Install and configure eradio
(use-package eradio
  :ensure t)
(global-set-key (kbd "C-c r p") 'eradio-play)
(global-set-key (kbd "C-c r s") 'eradio-stop)
(global-set-key (kbd "C-c r t") 'eradio-toggle)
(setq eradio-player '("mpv" "--no-video" "--no-terminal"))
(setq eradio-channels
    '(("Palmos"
       . "http://ice.streamcloud.mediacdn.com/palmos983")
      ("Ellinadiko"
       . "https://radio.streamings.gr/proxy/radioellinadiko?mp=/stream")
      ("Sfera"
       . "http://sfera.live24.gr/sfera4132")
      ("Bayern3"
       . "http://streams.br.de/bayern3_2.m3u")
      ("Bayern1"
       . "http://streams.br.de/bayern1_2.m3u")))

;; Get the name of the playing station, not the url
(defun eradio-get-name-from-url (url)
  "Find name in `eradio-channels`."
  (let ((channel (cl-find-if (lambda (pair)
                                (string= (cdr pair) url))
                              eradio-channels)))
    (if channel
        (car channel)
      "Station unknown")))

;; Show radio station in modeline if playing
(defun eradio-update-modeline ()
  (if eradio--process
    (let ((station-name (eradio-get-name-from-url eradio-current-channel)))
        (setq global-mode-string
              (add-to-list 'global-mode-string (format "   o/%s" station-name) 'APPEND)))
    (setq global-mode-string
          (delete (format "   o/%s" (eradio-get-name-from-url eradio-current-channel)) global-mode-string))))

;; Aktualisiere die Modeline alle paar Sekunden
(run-with-timer 0 5 'eradio-update-modeline)
