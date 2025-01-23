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
		mode-line-misc-info
		(:eval
                 (let* ((mode (format-mode-line '("%e"
						  mode-line-misc-info
						  "   "
						  (vc-mode vc-mode)
						  " "
						  )))
                        (space `((space :align-to (- (+ right right-fringe right-margin)
                                                     ,(string-width mode))))))
                   (concat (propertize " " 'display space) mode)))
		))
