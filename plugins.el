;; enable nov.el for epub format
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; config noa to use msys64 path for unzip in windows
(if (eq system-type 'windows-nt)
    (setq nov-unzip-program (executable-find "C:/msys64/usr/bin/unzip")))

;; Install pdf-loader for fast startup while beeing able to load pdfs
(require 'pdf-tools)
(pdf-loader-install)

;; Undo tree settings - global mode and dont spam my fs
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Evil mode


;; EVERYBODYu needs AI now
(global-set-key (kbd "C-c k o") 'gptel)
(global-set-key (kbd "C-c k a") 'gptel-send)
(require 'gptel)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))

;; Eglot Java
(require 'eglot-java)
(add-hook 'java-mode-hook 'eglot-java-mode)
(with-eval-after-load 'eglot-java
  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh))
