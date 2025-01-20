;; Win specific variables
(setq pr-temp-dir "~/AppData/Local/Temp"
      find-program "\"C:\\Program Files\\Git\\usr\\bin\\find.exe\"")

;; Win specific functions
(defun w32-winprint-show-waiting(msg sec)
  "Show user Emacs is waiting"
  (let ((lsec sec)(lmsg msg))
    (while (> lsec 0)
(message lmsg)
(sleep-for 1)
(setq lmsg (concat lmsg "."))
(setq lsec (- lsec 1)))
    (message "")))

(defcustom w32-winprint-shell-execute-wait 5
  "Default number of seconds to show waiting message after calling
ShellExecute."
  :group 'w32-winprint)

(defun w32-winprint-shell-execute(verb file-name
				 &optional parameters show-flag)
  "Wrapper around w32-shell-execute that gives a message on error
instead of signaling the error. It also gives a waiting message in the
bottom of the screen."
  (condition-case err
(progn
  (let ((msg (format "Asked Explorer to %s %s " verb file-name)))
    (message (concat msg "(locked? popup minimized?)"))
    (w32-shell-execute verb file-name parameters (if show-flag show-flag 1))
    (w32-winprint-show-waiting msg w32-winprint-shell-execute-wait)))
    (error (let ((msg (car (cdr err))))
       ;;(message "orig err: %s" msg)
       (setq msg (replace-regexp-in-string "ShellExecute failed: " "" msg))
       (setq msg (replace-regexp-in-string "[\r\n ]*$" "" msg))
       (setq msg (replace-regexp-in-string "this operation"
					   (concat "'" verb "'") msg))
       (message msg)))))

  (defvar w32-winprint-kill-buffers t
    "For debbugging purpose. If false keeps the temporary buffers used
for printing.")

(defun w32-winprint-gettemp-print-name(html)
  "Get a temp file name for printing"
  (expand-file-name (concat "~/temp-print." (if html "html" "tmp"))))


(defun w32-winprint-print-file-notepad(file-name)
  "Print file with notepad"
  (message "w32-winprint-shell-execute open notepad.exe (concat /p %s" file-name)
  (w32-winprint-shell-execute "open" "notepad.exe" (concat "/p " file-name)
			))

(defun w32-winprint-print-part-notepad(start-point end-point)
  "Print current buffer part with notepad"
  (setq file (w32-winprint-gettemp-print-name nil))
  (save-excursion
    (let (deactivate-mark)
(copy-to-buffer "*print-temp*" start-point end-point)
(set-buffer "*print-temp*")
(write-file file nil)
(w32-winprint-print-buffer-notepad))))


(defun w32-winprint-print-region-notepad()
  "Print current region with notepad"
  (interactive)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
  (setq start (prog1 end (setq end start))))
    (w32-winprint-print-part-notepad start end)))


(defun w32-winprint-print-buffer-notepad()
  "Print whole current buffer with notepad"
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (if (not file)
  (progn
    (save-excursion
      (widen)
      (w32-winprint-print-part-notepad (point-min) (point-max))))
(if (buffer-modified-p)
    (let ((use-dialog-box nil))
      (if (y-or-n-p "File is modified. Save before printing? ")
	  (save-buffer))))
(w32-winprint-print-file-notepad file))))

;; We override the print functions because they are not working on windows
(defun print-buffer()
  "Print the buffer (windows compatible)"
  (interactive)
  (w32-winprint-print-buffer-notepad))

(defun print-region()
  "Print the region (windows compatible)"
  (interactive)
  (w32-winprint-print-region-notepad))
