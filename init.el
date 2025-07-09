;; Load custom file and let emacs spam there
(setq custom-file (concat user-emacs-directory "auto.el"))
(load custom-file)

;; Adding the package archives
(package-initialize)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

;; Frame size set
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remove not used features
(scroll-bar-mode -1)
(put 'dired-find-alternate-file 'disabled nil)
(menu-bar-mode -1)
(setq make-backup-files nil
      use-dialog-box nil
      inhibit-startup-message t
      initial-scratch-message nil
      org-startup-truncated nil
      auto-save-default nil
      server-client-instructions nil
      vc-suppress-confirm t
      inhibit-startup-buffer-menu t
      ring-bell-function 'ignore)

;; Activate some wanted features
(global-auto-revert-mode 1)
(save-place-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(savehist-mode 1)
(delete-selection-mode 1)
(ffap-bindings)
(prefer-coding-system 'utf-8)
(windmove-default-keybindings)
(setq indent-tabs-mode t
      tab-width 4
      gdb-many-windos 1
      history-length 500
      warning-minimum-level :error
      use-short-answers t
      pr-temp-dir "/tmp"
      auth-sources "~/.authinfo"
      ange-ftp-netrc-filename auth-sources
      ange-ftp-try-passive-mode t
      printer-name "HL3040CN"
      rmail-file-name (concat user-emacs-directory ".RMAIL")
      message-directory (concat user-emacs-directory "mail")
      gnus-directory (concat user-emacs-directory "news")
      nnfolder-directory (concat user-emacs-directory "mail/archive")
      gnus-startup-file (concat user-emacs-directory ".newsrc")
      doc-view-continuous t
      doc-view-resolution 400
      delete-by-moving-to-trash t
      delete-selection-mode t
      dired-dwim-target t
      ido-enable-flex-matching t
      gdb-many-windows t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point nil
      ido-ignore-buffers '("\\` " "^\*Completions\*" "^\*Messages\*" "^\*copilot events\*"
			   "^\*Quail Completions\*" "^.newsrc-dribble" "^\*EGLOT\*"
			   "^\*Warnings\*" "^\*vc-git\*" "^\*vc\*" "^\*vc-diff\*"
			   "^\*log-edit-files\*" "^\*changes to\*" "^\*undo-tree\*"
			   "^\*nov unzip\*" "^\*Async-native-compile-log\*"))

;; C Style
(defun c-style-settings()
    (c-set-style "ellemtel")
    (setq c-basic-offset 4))
(add-hook 'c-mode-common-hook #'c-style-settings)

;; I dont want to trash on remote directories
(defun cfg-dired-setup ()
  "Custom setup hook for `dired-mode'."
  (interactive)
  (cfg-dired-setup--avoid-remote-trash))
(defun cfg-dired-setup--avoid-remote-trash ()
  (when (and (boundp 'dired-directory)
             dired-directory
             (file-remote-p dired-directory))
    (setq-local delete-by-moving-to-trash nil)))

(add-hook 'dired-mode-hook #'cfg-dired-setup)

;; Kill buffer without asking which one first
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Custom keybinds
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c t") 'term)
(global-set-key (kbd "C-c g") 'gdb)

;; Clean instruction messages
(defun display-startup-echo-area-message () (message ""))

;; Display file numbers only in programming mode
(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;; Automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; Get rid of trailing whitespaces
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Don't show ANSI escape sequences in compile buffer (Windows issue)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Eglot releated settings
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'java-mode-hook 'eglot-java-mode)

;; Loading all the oder modules
(load (concat user-emacs-directory "modeline.el"))
(load (concat user-emacs-directory "plugins.el"))
(load (concat user-emacs-directory "org.el"))

;; Win specific settings
(when (eq system-type 'windows-nt)
  (load (concat user-emacs-directory "windows.el")))
