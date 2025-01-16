;; enable nov.el for epub format
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; config noa to use msys64 path for unzip in windows
(if (eq system-type 'windows-nt)
    (setq nov-unzip-program (executable-find "C:/msys64/usr/bin/unzip")))

;; Install pdf-loader for fast startup while beeing able to load pdfs
;;(require 'pdf-tools)
;;(pdf-loader-install)
;; Compilation broken for now, see
;; https://github.com/vedang/pdf-tools/issues/282 and https://github.com/vedang/pdf-tools/pull/295

;; Undo tree settings - global mode and dont spam my fs
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; I want company mode to have better autocomplete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Everybody needs AI now
(global-set-key (kbd "C-c k o") 'gptel)
(global-set-key (kbd "C-c k a") 'gptel-send)
(require 'gptel)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))

;; Copilot
(require 'copilot)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(add-hook 'prog-mode-hook 'copilot-mode)

;; I want company mode to have better autocomplete
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; Everybody needs AI now
(global-set-key (kbd "C-c k o") 'gptel)
(global-set-key (kbd "C-c k a") 'gptel-send)
(require 'gptel)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))

;; Copilot
(require 'copilot)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-to-list 'copilot-indentation-alist '(prog-mode 4))
