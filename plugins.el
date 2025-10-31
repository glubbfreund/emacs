;; Use and autoinstall my favourite theme
(use-package gruber-darker-theme
  :ensure t)

;; vterm if using linux
(when (eq system-type 'gnu/linux)
  (use-package vterm
    :ensure t
    :bind (("C-c t" . vterm)
		   ("C-c r" . run-in-vterm)))
  (defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

  (defun run-in-vterm (command)
	"Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
	(interactive
	 (list
      (let* ((f (cond (buffer-file-name)
                      ((eq major-mode 'dired-mode)
                       (dired-get-filename nil t))))
			 (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
		(read-shell-command "Terminal command: "
							(cons filename 0)
							(cons 'shell-command-history 1)
							(list filename)))))
	(with-current-buffer (vterm (concat "*" command "*"))
      (set-process-sentinel vterm--process #'run-in-vterm-kill)
      (vterm-send-string command)
      (vterm-send-return))))

;; Auto theme if using linux
(when (eq system-type 'gnu/linux)
  (defconst my/xfce-theme-check-interval 30
    "How often we check for theme change (in seconds).")

  (defvar my/xfce-current-theme nil
    "Currently detected XFCE theme name.")

  (defun my/xfce-get-current-theme ()
    "Return the current XFCE theme name as a trimmed string, or nil if unavailable."
    (let ((theme (string-trim
                  (shell-command-to-string
                   "xfconf-query -c xsettings -p /Net/ThemeName 2>/dev/null"))))
      (if (string-empty-p theme) nil theme)))

  (defun my/load-emacs-theme (theme-symbol)
    "Load THEME-SYMBOL only if it's not already active."
    (unless (and custom-enabled-themes
                 (eq (car custom-enabled-themes) theme-symbol))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme-symbol t)
      (message "Emacs theme set to: %s" theme-symbol)))

  (defun my/xfce-theme-sync ()
    "Synchronize Emacs theme with XFCE theme, but only if changed."
    (let ((theme (my/xfce-get-current-theme)))
      (when (and theme (not (string= theme my/xfce-current-theme)))
        (setq my/xfce-current-theme theme)
        (cond
         ((string= theme "Arc-Dark")
          (my/load-emacs-theme 'gruber-darker))
         ((string= theme "Arc-Lighter")
          (my/load-emacs-theme 'tsdh-light))
         (t
          (message "No matching Emacs theme for XFCE theme: %s" theme))))))

  ;; Initial sync immediately
  (my/xfce-theme-sync)
  ;; Then check periodically
  (run-with-timer my/xfce-theme-check-interval
                  my/xfce-theme-check-interval
                  #'my/xfce-theme-sync))

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

;; Company Mode
(use-package company
   :ensure t
   :init
   (global-company-mode)
:config
(setq company-minimum-prefix-length 1
      company-idle-delay 4
      company-selection-wrap-around t
	eglot-autoshutdown t
      company-tooltip-align-annotations t))

;; Magit Git Client
(use-package magit
  :ensure t)
