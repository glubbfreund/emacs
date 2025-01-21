;; Enable modded modeline
(require 'mood-line)
(mood-line-mode)

;; For the beginning I need whichkey
(require 'which-key)
(which-key-mode)

;; enable nov.el for epub format
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; to use emacs as a pdf reader
(require 'pdf-tools)
(pdf-loader-install)

;; Undo tree settings - global mode and dont spam my fs
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

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

;; For general ChatGPT usage
(global-set-key (kbd "C-c k o") 'gptel)
(global-set-key (kbd "C-c k a") 'gptel-send)
(require 'gptel)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
