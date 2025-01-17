;; Enable greek input method
(setq default-input-method "greek")
(global-set-key (kbd "C-c i g") 'toggle-input-method)

;; Spell checking functions
(global-set-key (kbd "C-c s d") 'flyspell-german)
(defun flyspell-german ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "de")
  (flyspell-buffer))
(global-set-key (kbd "C-c s g") 'flyspell-greek)
(defun flyspell-greek ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "el")
  (flyspell-buffer))
(global-set-key (kbd "C-c s e") 'flyspell-english)
(defun flyspell-english ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "en")
  (flyspell-buffer))

;; org-mode settings
(setq org-hide-emphasis-markers t
	  org-modern-star 'replace)

;; Org Export Settings
(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%d %B %Y"))

;; enable org-modern
(with-eval-after-load 'org (global-org-modern-mode))
