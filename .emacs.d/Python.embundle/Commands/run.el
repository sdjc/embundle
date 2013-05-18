(defun run (program)
  (interactive (list buffer-file-name))
  (let ((buffer (get-buffer-create "python-run")))
    (start-process-shell-command "xxx" buffer
                                 (format "python %s" program))))
