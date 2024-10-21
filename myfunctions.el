;; automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

(defun my-highlight-input-method-in-modeline ()
  "Highlight the input method indicator in the mode line if an input method is active."
  (setq mode-line-mule-info
        '(:eval (if current-input-method
                    (propertize (concat "⚑" current-input-method-title "  ")
                                'face 'warning)
                  "⚐DE "))))
(add-hook 'post-command-hook #'my-highlight-input-method-in-modeline)

;; Get rid of trailing whitespaces
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Spell checking functions
(global-set-key (kbd "C-c s d") 'flyspell-german)
(defun flyspell-german ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "de")
  (flyspell-buffer))
(global-set-key (kbd "C-c s e") 'flyspell-greek)
(defun flyspell-greek ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "el")
  (flyspell-buffer))
(global-set-key (kbd "C-c s i") 'flyspell-english)
(defun flyspell-english ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "en")
  (flyspell-buffer))

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
(add-hook 'image-mode-hook #'my-turn-off-line-numbers)
