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

;; Frame size set client friendly
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 160))

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
(which-key-mode t)
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
      printer-name "HL3040CN"
      rmail-file-name "~/.emacs.d/.RMAIL"
      message-directory "~/.emacs.d/mail/"
      gnus-directory "~/.emacs.d/news/"
      nnfolder-directory "~/.emacs.d/mail/archive"
      gnus-startup-file "~/.emacs.d/.newsrc"
      doc-view-continuous t
      doc-view-resolution 400
      delete-by-moving-to-trash t
      delete-selection-mode t
      dired-dwim-target t
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point nil
      ido-ignore-buffers '("\\` " "^\*Completions\*" "^\*Messages\*" "^\*copilot events\*"
			   "^\*Quail Completions\*" "^.newsrc-dribble" "^\*EGLOT\*"
			   "^\*Warnings\*" "^\*vc-git\*" "^\*vc\*" "^\*vc-diff\*"
			   "^\*log-edit-files\*" "^\*changes to\*" "^\*undo-tree\*"
			   "^\*nov unzip\*" "^\*Async-native-compile-log\*"))

;; I dont want ton trash on remote directories
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

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

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

;; Evil mode, org/writing and plugins
(load "~/.emacs.d/modeline.el")
(load "~/.emacs.d/plugins.el")
(load "~/.emacs.d/org.el")

;; Win specific settings
(when (eq system-type 'windows-nt)
  (load "~/.emacs.d/windows.el"))
