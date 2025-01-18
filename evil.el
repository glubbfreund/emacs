;; Should be set before loading evil
(setq evil-want-keybinding nil)

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Enable all Evil keybindings
(require 'evil-collection)
(evil-collection-init)

;; Rewire some keybindings
(evil-ex-define-cmd "wq" 'save-kill-and-delete-window)
(evil-ex-define-cmd "q" 'kill-and-delete-window)
(evil-ex-define-cmd "qq" 'delete-frame)

;; And the functions for the rewritten keybindings
(defun kill-and-delete-window()
  (interactive)
  (kill-current-buffer)
  (when (> (length (window-list)) 1)
   (delete-window)))

(defun save-kill-and-delete-window()
  (interactive)
  (save-buffer)
  (kill-current-buffer)
  (when (> (length (window-list)) 1)
   (delete-window)))
