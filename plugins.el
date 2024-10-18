;; enable org-modern
(with-eval-after-load 'org (global-org-modern-mode))

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

;; Settings for go-translate package
(setq gt-langs '(de el))
(setq gt-default-translator
      (gt-translator
       :taker   (gt-taker :text 'buffer :pick 'paragraph) 
       :engines (gt-google-engine)                  
       :render  (gt-buffer-render)))
