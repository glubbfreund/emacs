;; Use and autoinstall my favourite theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t)
  :ensure t)

;; Nov is a package for reading epubs
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :ensure t)

;; For general ChatGPT usage
(use-package gptel
  :init
  (setq auth-sources '("~/.authinfo")
	gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
  :bind
  ("C-c k o" . gptel)
  ("C-c k a" . gptel-send)
  :ensure t)

;; Install and configure eradio
(use-package eradio
  :init
  (setq eradio-player '("mpv" "--no-video" "--no-terminal")
	eradio-channels
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
  :bind
  ("C-c r p" . eradio-play)
  ("C-c r s" . eradio-stop)
  ("C-c r t" . eradio-toggle)
  :ensure t)
