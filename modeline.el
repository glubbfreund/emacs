;; My rewritten Vanilla modeline, pretty much default
;; but cleaned up a bit

(defun clean-modeline-str (str)
  "Trim whitespace and remove brackets in STR for modeline."
  (let ((fixed-str (replace-regexp-in-string "%" "%%" str)))
    (let ((cleaned (replace-regexp-in-string "\\[\\|\\]" "" fixed-str)))
      (replace-regexp-in-string " +" " " (string-trim cleaned)))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
		mode-line-mule-info
                mode-line-modified
                mode-line-remote
		" "
                (:eval (propertize (string-trim (format-mode-line mode-line-buffer-identification))
                                   'face 'warning))
                " "
                (:eval (clean-modeline-str (format-mode-line mode-line-position)))
		" "
                (:eval (string-trim (format-mode-line vc-mode)))
                ;; Right side: Modes + Misc
                (:eval
                 (let* ((mode (clean-modeline-str (format-mode-line mode-line-modes)))
                        (misc (clean-modeline-str (format-mode-line mode-line-misc-info)))
                        (pad `((space :align-to (- (+ right right-fringe right-margin)
                                                   ,(+ (string-width mode)
                                                       (string-width misc)
						       2 ))))))
                   (concat (propertize " " 'display pad)
                           mode
			   " "
                           (propertize misc 'face 'shadow)
			   " ")))))
