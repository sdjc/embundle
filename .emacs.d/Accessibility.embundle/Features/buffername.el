;; buffername.el
;; yjzeng, 2011-8-14
;; 处理同名称的file-buffer-name


(defvar generate-file-buffer-name-functions nil)


(defun my-create-file-buffer (filename)
  "修改原来的create-file-buffer逻辑，把文件名的全路径传递给
my-generate-new-buffer-name"
  ;; genreate-new-buffer中会调用到generate-new-buffer-name创建一个唯一
  ;; 的buffer名称
  (generate-new-buffer (generate-file-buffer-name filename)))


(defun generate-file-buffer-name (file-name &optional ignore)
  ;; 如果starting-name存在同名的buffer
  (let* ((file-dir (file-name-directory-1 file-name))
         (dir-parts (reverse (split-string file-dir "/")))
         (basename (file-name-nondirectory file-name))
         (name basename))
    (or
     (catch 'got
       ;; TODO: 对于文件名至少包含上一层目录，这个做法好吗?
       (dolist (part dir-parts)
         (setq name (concat part "/" name))
         (unless (get-buffer name)
           (run-hook-with-args generate-file-buffer-name-functions name)
           (throw 'got name))))
     (generate-new-buffer-name file-name))))


(defun file-name-directory-1 (file-name)
  "取文件全路径中的目录名称，去掉最后的/"
  (let ((file-dir-1 (file-name-directory file-name)))
    (substring file-dir-1 0 (1- (length file-dir-1)))))


(fset 'create-file-buffer 'my-create-file-buffer)


(provide 'buffername)
