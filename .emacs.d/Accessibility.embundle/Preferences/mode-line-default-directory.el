(defvar mode-line-default-directory
    '(:eval (propertize ;; (directory-file-name
                         (expand-file-name default-directory))))
;; )
