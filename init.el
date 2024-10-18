;; Load custom file and let emacs spam there
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Fixed encoding issues on some Windows systems
(setq-default buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read 'utf-8)
(prefer-coding-system 'utf-8)

;; don't show ANSI escape sequences in compile buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

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

;; automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; SAVE what is entered into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Save last file position
(save-place-mode 1)

;; Enable Ido mode
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-everywhere 1)
(ido-mode 1)

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

;; clean instruction messages
(defun display-startup-echo-area-message () (message ""))
(setq server-client-instructions nil)

;; show the big gdb layout 
(setq gdb-many-windows 1)

;; set eglot events buffer to 0 to prevent performance issues
(setq eglot-events-buffer-size 0)

;; enable org-modern
(with-eval-after-load 'org (global-org-modern-mode))

;; enable nov.el for epub format
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; use msys64 for unzip in windows
(if (eq system-type 'windows-nt)
    (setq nov-unzip-program (executable-find "C:/msys64/usr/bin/unzip")))

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
                     (when (not (string= added "0"))
                       (setq result (concat result
                                            (propertize (format "+%s" added)
                                                        'face 'diff-added))))
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
      (when (vc-backend (buffer-file-name))  
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
(add-hook 'xref-buffer-mode-hook #'my-turn-off-line-numbers)

;; Undo tree settings - global mode and dont spam my fs 
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Install and configure eradio
(use-package eradio
  :ensure t)
(global-set-key (kbd "C-c r p") 'eradio-play)
(global-set-key (kbd "C-c r s") 'eradio-stop)
(global-set-key (kbd "C-c r t") 'eradio-toggle)
(setq eradio-player '("mpv" "--no-video" "--no-terminal"))
(setq eradio-channels
    '(("Παλμός Κεφαλλονιάς 98.3"
       . "http://ice.streamcloud.mediacdn.com/palmos983")
      ("Radio Ellinadiko"
       . "https://radio.streamings.gr/proxy/radioellinadiko?mp=/stream")
      ("Sfera 102.2"
       . "http://sfera.live24.gr/sfera4132")
      ("Ράδιο Θεσσαλονίκη 94.5"
       . "http://eu7.fastcast4u.com:6156/stream?type=http&nocache=136596")
      ("Bayern 3"
       . "http://streams.br.de/bayern3_2.m3u")
      ("Bayern 1"
       . "http://streams.br.de/bayern1_2.m3u")))

;; Get the name of the playing station, not the url
(defun eradio-get-name-from-url (url)
  "Find name in `eradio-channels`."
  (let ((channel (cl-find-if (lambda (pair)
                                (string= (cdr pair) url))
                              eradio-channels)))
    (if channel
        (car channel) 
      "Station unknown")))

;; Show radio station in modeline if playing
(defun eradio-update-modeline ()
  (if eradio--process
    (let ((station-name (eradio-get-name-from-url eradio-current-channel)))
        (setq global-mode-string
              (add-to-list 'global-mode-string (propertize (format "  ☊ %s" station-name) 'face 'mode-line-inactive) 'APPEND)))
    (setq global-mode-string
          (delete (propertize (format "  ☊ %s" (eradio-get-name-from-url eradio-current-channel)) 'face 'mode-line-inactive) global-mode-string))))

;; Aktualisiere die Modeline alle paar Sekunden
(run-with-timer 0 5 'eradio-update-modeline)
