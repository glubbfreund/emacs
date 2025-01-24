;; For the beginning I need whichkey
(require 'which-key)
(which-key-mode)

;; Nov is a package for reading epubs
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Copilot settings
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(define-key copilot-completion-map (kbd "C-c c <tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-c c TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-c c w") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "C-c c n") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-c c p") 'copilot-previous-completion)
(add-to-list 'copilot-indentation-alist '(prog-mode 2))
(add-to-list 'copilot-indentation-alist '(org-mode 2))
(add-to-list 'copilot-indentation-alist '(text-mode 2))

;; For general ChatGPT usage
(require 'gptel)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com"))
(global-set-key (kbd "C-c k o") 'gptel)
(global-set-key (kbd "C-c k a") 'gptel-send)
