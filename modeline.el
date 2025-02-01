;; My rewritten Vanilla modeline, pretty much default
;; but cleaned up a bit

(defun trim-misc-modeline (str)
  "Remove all whitespace from STR."
  (replace-regexp-in-string " +" " " str))

(defun clean-misc-modeline (str)
  "Escape all '%' in STR and remove '[' and ']' to ensure correct formatting."
  (let ((escaped-str (replace-regexp-in-string "%" "%%" str)))
    (let ((trimmed-escaped-str (trim-misc-modeline escaped-str)))
      (replace-regexp-in-string "\\[\\|\\]" "" trimmed-escaped-str))))

(setq-default mode-line-format
              '("%e"
		mode-line-front-space
		(:eval (propertize (format-mode-line '("%e"
						       mode-line-mule-info
						       mode-line-modified
						       mode-line-remote))
				   'face 'shadow))
		" "
                (:eval (string-trim (propertize (format-mode-line '("%e" mode-line-buffer-identification)) 'face 'warning)))
		" "
		(:eval (clean-misc-modeline (format-mode-line '("%e" mode-line-position))))
		(:eval (string-trim (format-mode-line '("%e" (vc-mode vc-mode)))))
		(:eval
                 (let* ((mode (string-trim (format-mode-line '("%e"
							       mode-line-modes))))
			(misc (clean-misc-modeline (format-mode-line '("%e"
								       " "
								       mode-line-misc-info
								       " "))))
                        (space `((space :align-to (- (+ right right-fringe right-margin)
                                                     ,(+ (string-width mode) (string-width misc)))))))
                   (concat (propertize " " 'display space)
			   mode
			   (propertize misc 'face 'shadow))))))
