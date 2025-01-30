;; Nov is a package for reading epubs
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :ensure t)

;; Copilot settings
(use-package copilot
  :bind
  ("C-c c <tab>" . copilot-accept-completion)
  ("C-c c TAB" . copilot-accept-completion)
  ("C-c c w" . copilot-accept-completion-by-word)
  ("C-c c n" . copilot-next-completion)
  ("C-c c p" . copilot-previous-completion)
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
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
