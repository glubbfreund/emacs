;; load custom file and let emacs spam there
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; adding the package archives
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)

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
(setq pr-temp-dir "~/AppData/Local/Temp"
      find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\""
	  indent-tabs-mode t
	  tab-width 4
	  gdb-many-windows 1
	  history-length 250
	  warning-minimum-level :error
	  use-short-answers t
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point nil
      ido-ignore-buffers '("\\` " "^\*Completions\*" "^\*Messages\*" "^\*copilot events\*" "^\*Quail Completions\*"
						   "^\*EGLOT\*" "^\*Warnings\*" "^\*vc-git\*" "^\*vc\*" "^\*vc-diff\*"
						   "^\*log-edit-files\*" "^\*changes to\*" "^\*undo-tree\*"))

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

;; Org and writing related settings
(load "~/.emacs.d/org.el")

;; Load plugins
(load "~/.emacs.d/plugins.el")
