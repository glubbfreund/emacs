;; Load custom file and let emacs spam there
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; add Melpa stable
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
      ring-bell-function 'ignore)

;; activate some wanted features
(global-auto-revert-mode 1)
(save-place-mode 1)
(ido-mode 1)
(savehist-mode 1)
(ffap-bindings)
(setq pr-temp-dir "~/AppData/Local/Temp"
      find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\"")
(setq gdb-many-windows 1
      history-length 25)

;; display file numbers only in programming mode
(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;; set to use tabs and 4 spaces
(setq indent-tabs-mode t
      tab-width 4)

;; Enable Evil
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(require 'evil-collection)
(evil-collection-init)
(define-key evil-insert-state-map "jj" 'evil-normal-state)
(evil-ex-define-cmd "wq" 'save-kill-and-delete-window)
(evil-ex-define-cmd "q" 'kill-and-delete-window)
(evil-ex-define-cmd "qq" 'delete-frame)
(defun kill-and-delete-window()(interactive)(kill-current-buffer)(delete-window))
(defun save-kill-and-delete-window()(interactive)(save-buffer)(kill-current-buffer)(delete-window))

;; Ask for y/n instead of yes/no
(setopt use-short-answers t)

;; clean instruction messages
(defun display-startup-echo-area-message () (message ""))
(setq server-client-instructions nil)

;; don't show ANSI escape sequences in compile buffer (Windows issue)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Eglot releated settings
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'java-mode-hook 'eglot-java-mode)
(setq eglot-events-buffer-config '(:size 0 :format full))
(setq eglot-events-buffer-size 0)

;; Undo tree settings - global mode and dont spam my fs
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; enable nov.el for epub format
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Copilot settings
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "C-n") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-p") 'copilot-previous-completion)
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(add-to-list 'copilot-indentation-alist '(org-mode 2))
(add-to-list 'copilot-indentation-alist '(text-mode 2))
(add-to-list 'copilot-indentation-alist '(closure-mode 2))
(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

;; For general ChatGPT usage
(global-set-key (kbd "C-c k o") 'gptel)
(global-set-key (kbd "C-c k a") 'gptel-send)
(require 'gptel)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))

;; automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; Get rid of trailing whitespaces
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; Get rid of trailing whitespaces
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
