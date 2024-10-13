(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka NFM" :foundry "outline" :slant normal :weight regular :height 120 :width normal))))
 '(fixed-pitch ((t nil)))
 '(markdown-language-keyword-face ((t (:family "Iosevka NFM"))))
 '(markdown-pre-face ((t (:family "Iosevka NFM")))))
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
   '(undo-tree pdf-tools typescript-mode markdown-mode gruber-darker-theme magit go-mode gruber-darker))
 '(tool-bar-mode nil))

;; Ask for y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)  

;; remove not used features
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq make-backup-files nil
      use-dialog-box nil
	  inhibit-startup-message 1
	  initial-scratch-message nil
	  auto-save-default nil
      ring-bell-function 'ignore)

;; Add Melpa repository
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; custom command to open term with zsh without asking (and eshell on windows)
(defun ter ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
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

;; Enable Evil
(require 'evil)
(evil-mode 0)

;; show battery state in status line and prevent startup message
(defun display-startup-echo-area-message () (message ""))

;; show the big gdb layout 
(setq gdb-many-windows 1)

;; set the default coding system 
(set-default-coding-systems 'utf-8)

;; set eglot events buffer to 0 to prevent performance issues
(setq eglot-events-buffer-size 0)

;; Gives me git changes in the status line
(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
    (concat ad-return-value
            (let ((plus-minus (vc-git--run-command-string
                               file "diff" "--numstat" "--")))
              (and plus-minus
                   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                   (format " +%s-%s" (match-string 1 plus-minus) (match-string 2 plus-minus)))))))

;; Install pdf-loader for fast startup while beeing able to load pdfs
(pdf-loader-install)

(require 'undo-tree)
(global-undo-tree-mode)
