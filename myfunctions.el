;; automatically kill term buffer if process exits
(defun my-term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
  (kill-buffer (current-buffer)))
(advice-add 'term-handle-exit :after 'my-term-handle-exit)

;; Get rid of trailing whitespaces
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Spell checking functions
(defun flyspell-german ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "de")
  (flyspell-buffer))

(defun flyspell-greek ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "el")
  (flyspell-buffer))

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
