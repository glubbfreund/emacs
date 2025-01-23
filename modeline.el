;; My rewritten Vanilla modeline, pretty much default
;; but with the vc-mode on the right side

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
		" "
                mode-line-buffer-identification
		mode-line-position
		"   "
		mode-line-modes
		(:eval
                 (let* ((mode (format-mode-line '("%e"
						  "   "
						  (vc-mode vc-mode)
						  " "
						  )))
			(misc (format-mode-line '("%e"
						   mode-line-misc-info
						   )))
                        (space `((space :align-to (- (+ right right-fringe right-margin)
                                                     ,(+ (string-width mode) (string-width misc)))))))
                   (concat (propertize " " 'display space)(propertize misc 'face 'shadow) mode)))
		))
