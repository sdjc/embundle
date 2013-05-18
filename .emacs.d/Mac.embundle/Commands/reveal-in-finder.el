(defun reveal-in-finder ()
  (interactive)
  ;; open default-directory
  (shell-command (format "open '%s'" default-directory)))
