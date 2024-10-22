;; org-mode settings
(setq org-hide-emphasis-markers t)

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
