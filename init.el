;; Load custom file and let emacs spam there
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Add Melpa repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; List of visible packages from melpa-unstable
(defvar melpa-include-packages '(kv))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(package-install-selected-packages)

;; remove not used features
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq make-backup-files nil
      use-dialog-box nil
	  inhibit-startup-message t
	  initial-scratch-message nil
	  auto-save-default nil
      ring-bell-function 'ignore)

;; activate some wanted features
(global-auto-revert-mode 1)
(save-place-mode 1)
(ido-mode 1)
(savehist-mode 1)
(setq gdb-many-windows 1
      history-length 25
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-everywhere 1)

;; Set tab-width
(setq-default tab-width 4)

;; Ask for y/n instead of yes/no
(setopt use-short-answers t)

;; clean instruction messages
(defun display-startup-echo-area-message () (message ""))
(setq server-client-instructions nil)

;; Fixed encoding issues on some Windows systems
(setq-default buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read 'utf-8)
(prefer-coding-system 'utf-8)

;; don't show ANSI escape sequences in compile buffer (Windows issue)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; set eglot events buffer to 0 to prevent performance issues
(setq eglot-events-buffer-size 0)

;; Autorun eglot
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

;; My custom functions (advices, hooks, etc)
(load "~/.emacs.d/myfunctions.el")
;; Small Plugin configurations that doesnt need separate el file
(load "~/.emacs.d/plugins.el")
;; Config of margit git client
(load "~/.emacs.d/magit.el")
;; Eradio client modifications
(load "~/.emacs.d/eradio.el")
