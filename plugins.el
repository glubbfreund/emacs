;; Nov is a package for reading epubs
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Copilot settings
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(define-key copilot-completion-map (kbd "C-c c <tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-c c TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-c c w") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "C-c c n") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-c c p") 'copilot-previous-completion)
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(add-to-list 'copilot-indentation-alist '(org-mode 2))
(add-to-list 'copilot-indentation-alist '(text-mode 2))

;; For general ChatGPT usage
(require 'gptel)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
(global-set-key (kbd "C-c k o") 'gptel)
(global-set-key (kbd "C-c k a") 'gptel-send)

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
