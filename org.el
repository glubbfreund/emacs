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

;; Org Export Settings
(use-package org
  :init
  (setq org-hide-emphasis-markers t)
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%d %B %Y"))

;; Nicer and more modern look in org
(use-package org-modern
  :init
  (global-org-modern-mode)
  :config
  (setq org-modern-star 'replace)
  :ensure t)
