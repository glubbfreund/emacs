;; Gives me git changes in the status line with theme-dependent highlighting and hides zero changes
(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
    (concat ad-return-value
            (let ((plus-minus (vc-git--run-command-string
                               file "diff" "--numstat" "--")))
              (and plus-minus
                   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                   (let ((added (match-string 1 plus-minus))
                         (removed (match-string 2 plus-minus))
                         (result ":"))
                     (when (not (string= added "0"))
                       (setq result (concat result
                                            (propertize (format "+%s" added)
                                                        'face 'diff-added))))
                     (when (not (string= removed "0"))
                       (setq result (concat result 
                                            (propertize (format "-%s" removed)
                                                        'face 'diff-removed))))
                     result))))))

;; Dont ask for saving magit, just do it
(require 'magit)
(magit-save-repository-buffers 'dontask)

;; Adding magit hook to refresh the vc state in statusbar
(defun my-update-vc-state ()
  "Update VC state in all relevant buffers after Magit actions."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (vc-backend (buffer-file-name))  
        (vc-refresh-state)))))
(add-hook 'magit-post-refresh-hook 'my-update-vc-state)
