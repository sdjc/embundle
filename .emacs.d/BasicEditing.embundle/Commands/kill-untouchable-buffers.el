(defun kill-untouchable-buffers ()
  "关闭不可及的buffer，比如文件被删除、或者远程连接断开"
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (not (file-exists-p buffer-file-name)))
        (kill-buffer buffer)))))
