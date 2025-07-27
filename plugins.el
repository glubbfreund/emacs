;; Use and autoinstall my favourite theme
(use-package gruber-darker-theme
  :ensure t)

;; Auto theme if using linux
(when (eq system-type 'gnu/linux)
  (defvar my/xfce-theme-check-interval 30
    "How often we check for theme change.")
  (defvar my/xfce-current-theme ""
    "Save current theme name.")
  (defun my/xfce-get-current-theme ()
    "Read current theme."
    (string-trim
     (shell-command-to-string
      "xfconf-query -c xsettings -p /Net/ThemeName 2>/dev/null")))
  (defun my/xfce-theme-sync ()
    "Check sys theme and set it."
    (let ((theme (my/xfce-get-current-theme)))
      (unless (string= theme my/xfce-current-theme)
        (setq my/xfce-current-theme theme)
        (message "Theme changed: %s" theme)
        (cond
         ((string= theme "Arc-Dark-Custom")
          (my/load-emacs-theme 'gruber-darker))
         ((string= theme "Arc-Lighter-Custom")
          (my/load-emacs-theme 'tsdh-light))
         (t
          (message "No theme found for: %s" theme))))))
  (defun my/load-emacs-theme (theme)
    "Load the current theme and disable others"
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))
  (run-at-time nil my/xfce-theme-check-interval #'my/xfce-theme-sync))

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

;; Need java for Minecraft modding
(use-package eglot-java
  :ensure t)
