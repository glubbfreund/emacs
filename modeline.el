;; My rewritten Vanilla modeline, pretty much default
;; but cleaned up a bit

(defun clean-misc-modeline (str)
  "Escape all '%' in STR and remove '[' and ']' to ensure correct formatting."
  (let ((escaped-str (replace-regexp-in-string "%" "%%" str)))
    (let ((trimmed-escaped-str (replace-regexp-in-string "   +" "  " escaped-str)))
      (replace-regexp-in-string "\\[\\|\\]" "" trimmed-escaped-str))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space ;; mode-line-client removed
		mode-line-mule-info
                mode-line-modified
                mode-line-remote
		" "
                (:eval (propertize (format-mode-line '("%e" mode-line-buffer-identification)) 'face 'bold))
		" "
		mode-line-modes
		(:eval
                 (let* ((mode (format-mode-line '("%e"
						  "  "
						  (vc-mode vc-mode)
						  mode-line-end-spaces)))
			(misc (clean-misc-modeline (format-mode-line '("%e"
						  mode-line-misc-info
						  mode-line-position))))
                        (space `((space :align-to (- (+ right right-fringe right-margin)
                                                     ,(+ (string-width mode) (string-width misc)))))))
                   (concat (propertize " " 'display space)
			   (propertize misc 'face 'shadow)
			   mode)))))
