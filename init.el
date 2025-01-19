;; load custom file and let emacs spam there
(setq custom-file (concat user-emacs-directory "auto.el"))
(load custom-file)

;; adding the package archives
(package-initialize)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(package-install-selected-packages)

;; remove not used features
(scroll-bar-mode -1)
(put 'dired-find-alternate-file 'disabled nil)
(menu-bar-mode -1)
(setq make-backup-files nil
      use-dialog-box nil
      inhibit-startup-message t
      initial-scratch-message nil
      auto-save-default nil
      vc-suppress-confirm t
      inhibit-startup-buffer-menu t
      ring-bell-function 'ignore)

;; activate some wanted features
(global-auto-revert-mode 1)
(save-place-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(savehist-mode 1)
(ffap-bindings)
(prefer-coding-system 'utf-8)
(setq indent-tabs-mode t
      tab-width 4
      gdb-many-windows 1
      history-length 500
      warning-minimum-level :error
      use-short-answers t
      auth-sources "~/.authinfo"
      ange-ftp-netrc-filename auth-sources
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point nil
      ido-ignore-buffers '("\\` " "^\*Completions\*" "^\*Messages\*" "^\*copilot events\*"
			   "^\*Quail Completions\*" "^.newsrc-dribble" "^\*EGLOT\*" "^\*Warnings\*"
			   "^\*vc-git\*" "^\*vc\*" "^\*vc-diff\*" "^\*log-edit-files\*"
			   "^\*changes to\*" "^\*undo-tree\*" "^\*nov unzip\*"))

;; OS specific settings
(when (eq system-type 'windows-nt)
  (setq find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""))
(if (eq system-type 'gnu/linux)
    (setq pr-temp-dir "/tmp")
  (setq pr-temp-dir "~/AppData/Local/Temp"))

;; Configure authentication
(setq auth-sources "~/.secrets/authinfo.gpg")
(setq ange-ftp-netrc-filename auth-sources)

;; clean instruction messages
(defun display-startup-echo-area-message () (message ""))
(setq server-client-instructions nil)

;; display file numbers only in programming mode
(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;; automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; Get rid of trailing whitespaces
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; don't show ANSI escape sequences in compile buffer (Windows issue)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Eglot releated settings
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'java-mode-hook 'eglot-java-mode)

;; Evil mode, org/writing and plugins
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/plugins.el")
