;; 如果已经有注册了rst-mode，那么同样是.txt的文件，就会现匹配到
;; rst-mode，所以要先进行cmake-mode的匹配
(when (fboundp 'rst-mode)
  (add-to-list 'auto-mode-alist '("\\.txt$" . rst-mode)))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))



(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))
