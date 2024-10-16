(cond
 ;; Font settings for Windows
 ((eq system-type 'windows-nt)
  (custom-set-faces
   '(default ((t (:family "Iosevka NFM" :foundry "outline" :slant normal :weight regular :height 120 :width normal))))
   '(fixed-pitch ((t nil)))
   '(markdown-language-keyword-face ((t (:family "Iosevka NFM"))))
   '(markdown-pre-face ((t (:family "Iosevka NFM"))))))
 ;; Font settings for Debian/Linux
 ((eq system-type 'gnu/linux)
  (custom-set-faces
   '(default ((t (:family "Iosevka Nerd Font Mono" :foundry "outline" :slant normal :weight regular :height 150 :width normal))))
   '(fixed-pitch ((t nil)))
   '(markdown-language-keyword-face ((t (:family "Iosevka Nerd Font Mono"))))
   '(markdown-pre-face ((t (:family "Iosevka Nerd Font Mono")))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" default))
 '(display-battery-mode t)
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(undo-tree pdf-tools typescript-mode markdown-mode go-mode gruber-darker-theme magit))
 '(require 'gruber-darker-theme)
 '(tool-bar-mode nil))

;; Fixed encoding issues on some Windows systems
(setq-default buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read 'utf-8)
(prefer-coding-system 'utf-8)

;; don't show ANSI escape sequences in compile buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Add Melpa repository
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(package-install-selected-packages)

;; Ask for y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)  

;; remove not used features
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq make-backup-files nil
      use-dialog-box nil
	  inhibit-startup-message t
	  initial-scratch-message nil
	  auto-save-default nil
      ring-bell-function 'ignore)

;; custom command to open term with zsh without asking (and eshell on windows)
(defun ter ()
  (interactive)
  (if (eq system-type 'gnu/linux)(term "/usr/bin/zsh"))
  (if (eq system-type 'windows-nt)(eshell)))

;; automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; Resizing the window to my needs
(if (window-system)
(setq default-frame-alist
        (append '((width . 150) (height . 30) (top . 120) (left . 185))
                default-frame-alist)))

;; SAVE what is entered into minibuffer prompts
;; use M-n for next and M-p for previous
(setq history-length 25)
(savehist-mode 1)

;; Save last file position
(save-place-mode 1)

;; Enable Ido mode
(ido-mode 1)
(ido-everywhere 1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Set tab-width 
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Autorun eglot
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

;; show battery state in status line and prevent startup message
(defun display-startup-echo-area-message () (message ""))

;; show the big gdb layout 
(setq gdb-many-windows 1)

;; set eglot events buffer to 0 to prevent performance issues
(setq eglot-events-buffer-size 0)

;; Gives me git changes in the status line with theme-dependent highlighting and hides zero changes
(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
    (concat ad-return-value
            (let ((plus-minus (vc-git--run-command-string
                               file "diff" "--numstat" "--")))
              (and plus-minus
                   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                   (let ((added (match-string 1 plus-minus))
                         (removed (match-string 2 plus-minus))
                         (result ":"))
                     ;; Nur anzeigen, wenn hinzugefügt wurde
                     (when (not (string= added "0"))
                       (setq result (concat result
                                            (propertize (format "+%s" added)
                                                        'face 'diff-added))))
                     ;; Nur anzeigen, wenn entfernt wurde
                     (when (not (string= removed "0"))
                       (setq result (concat result 
                                            (propertize (format "-%s" removed)
                                                        'face 'diff-removed))))
                     result))))))

;; Dont ask for saving magit, just do it
(require 'magit)
(magit-save-repository-buffers 'dontask)

;; Adding magit hook to refresh the vc state in statusbar
(defun my-update-vc-state ()
  "Update VC state in all relevant buffers after Magit actions."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (vc-backend (buffer-file-name))  ;; check if buffer is affectend of vc-control
        (vc-refresh-state)))))
(add-hook 'magit-post-refresh-hook 'my-update-vc-state)

;; Install pdf-loader for fast startup while beeing able to load pdfs
(require 'pdf-tools)
(pdf-loader-install)

;; Helper function for disabling line numbers in some modes
(defun my-turn-off-line-numbers ()
  "Disable line numbering in the current buffer."
  (display-line-numbers-mode -1))

;; Adding hooks for modes where I dont want to have line numbers
(add-hook 'doc-view-mode #'my-turn-off-line-numbers)
(add-hook 'eshell-mode-hook #'my-turn-off-line-numbers)
(add-hook 'term-mode-hook #'my-turn-off-line-numbers)
(add-hook 'undo-tree-visualizer-mode-hook #'my-turn-off-line-numbers)

;; Undo tree settings - global mode and dont spam my fs 
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
