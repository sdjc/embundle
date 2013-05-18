(defun file-buffer-list ()
  "所有包含文件名称(buffer-file-name)的buffer"
  (delq nil
        (mapcar (lambda (buffer)
                  (with-current-buffer buffer
                    (when buffer-file-name
                      buffer)))
                (buffer-list))))

(defun generate-list ()
  (let ((scratch (get-buffer "*scratch*")))
    (append (file-buffer-list)
            (if scratch
                (list scratch)
              '()))))


(setq tabbar-buffer-list-function 'generate-list)
