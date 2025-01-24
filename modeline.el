;; My rewritten Vanilla modeline, pretty much default
;; but cleaned up a bit

(defun do-percentage-esc(str)
  "Escape all '%' in STR to ensure correct formatting."
  (replace-regexp-in-string "%" "%%" str))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space ;;mode-line-mule-info & mode-line-client removed
                mode-line-modified
                mode-line-remote
		" "
                (:eval (propertize (format-mode-line '("%e" mode-line-buffer-identification)) 'face 'bold))
		mode-line-modes
		(:eval
                 (let* ((mode (format-mode-line '("%e"
						  "  "
						  (vc-mode vc-mode))))
			(misc (do-percentage-esc (format-mode-line '("%e"
						  mode-line-misc-info
						  mode-line-position))))
                        (space `((space :align-to (- (+ right right-fringe right-margin)
                                                     ,(+ (string-width mode) (string-width misc)))))))
                   (concat (propertize " " 'display space)
			   (propertize misc 'face 'shadow)
			   mode)))))
