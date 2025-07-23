;; Use and autoinstall my favourite theme
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t)
  :ensure t)

;; Allow sudo commands
(use-package sudo-edit
  :ensure t)

;; EMMS setup
(use-package emms
  :config
  (require 'emms-setup)
  (cond
   ((eq system-type 'windows-nt)
    (require 'emms-player-mplayer)
    (setq emms-player-list '(emms-player-mplayer)))
   ((memq system-type '(gnu gnu/linux))
    (require 'emms-player-mpv)
    (setq emms-player-list '(emms-player-mpv))))
  (emms-all)
  :bind
  ("C-c e p" . emms-play-dired)
  ("C-c e s" . emms-stop)
  ("C-c e l" . emms-play-m3u-playlist)
  ("C-c e f" . emms-next)
  ("C-c e b" . emms-previous)
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
