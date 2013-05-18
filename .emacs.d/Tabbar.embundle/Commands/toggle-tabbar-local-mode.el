(defun toggle-tabbar-local-mode ()
  "决定是否需要关闭本buffer中tabbar的显示"
  (interactive)
  (if (not buffer-file-name)
      (tabbar-local-mode 1)
    (tabbar-local-mode -1)))
