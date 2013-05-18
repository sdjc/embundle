;; sourceheaderswitch.el
;; Created by yjzeng, 2011-3-1
;;
;; 在当前buffer的头文件和源文件之间切换，比如当前buffer中是a.h，那么就
;; 查找相应的a.cpp。如果Pymacs的速度可以提高的话，这个功能应该使用
;; Python来实现


;; 定义customize-group


(defvar header-extensions '("hpp" "h")
  "Extensions of header files.")


(defvar source-extensions '("mm" "m" "cxx" "cc" "cpp" "c")
  "Extensions of source files.")


;;


(defun find-header (source-file-name)
  "查找source-file-name对应的头文件"
  ;; 得到对应的源文件
    ;; 把source-file-name映射成为header文件名列表
  ;; 在当前目录中查找文件
  ;; 如果没有找到
  ;; 在对应的include/src目录中进行查找
  )

(defun find-source (header-file-name)
  "查找header-file-name对应的源文件"
  )

;; 编写切换header和source的脚本；很简单的；绑定到M-s-<up>上
;; 如果是.h的文件
(defun switch-header-source (buffer)
  "根据当前buffer切换源文件或者是头文件"
  (interactive
   (list (current-buffer)))
  (with-current-buffer buffer
      ;; 得到文件是源文件还是头文件
    (if (headerp buffer-file-name)
        (find-source buffer-file-name)
      (find-header buffer-file-name))))


 ;; 得到文件的ext
 ;; (when (> (length buffer-file-name) 0)
 ;;      (let ((base-name (file-name-sans-extension buffer-file-name))
 ;;            (ext (downcase (file-name-extension buffer-file-name)))
 ;;            (final nil)
 ;;            (select-exts nil)
 ;;            (base-name-other nil)
 ;;            (find-list nil))
 ;;        (when (not ext)
 ;;          (error "File has not extension."))
 ;;        (cond ((string-in-list ext header-extensions)
 ;;               ;; 根据模式调整查找的优先级
 ;;               (setq select-exts source-extensions)
 ;;               ;; 如果文件路径中有include，换成src
 ;;               (when (strstr buffer-file-name "include")
 ;;                 (setq base-name-other (file-name-sans-extension
 ;;                                        (string-replace buffer-file-name
 ;;                                                        "include"
 ;;                                                        "src")))))
 ;;              ((string-in-list ext source-extensions)
 ;;               (setq select-exts header-extensions)
 ;;               ;; 如果文件路径中有include，换成src
 ;;               (when (strstr buffer-file-name "src")
 ;;                 (setq base-name-other (file-name-sans-extension
 ;;                                        (string-replace buffer-file-name
 ;;                                                        "src"
 ;;                                                        "include")))))
 ;;              (t ;; otherwise
 ;;               (error "This is neight a source, nor a header.")))
 ;;        (setq find-list (append (add-ext base-name select-exts)
 ;;                                (add-ext base-name-other select-exts)))
 ;;        ;; 遍历find-list中的每个元素
 ;;        (when (not (find-exists-file-in-list find-list))
 ;;          (message "Can't find header or source of '%s'."
 ;;                   (file-name-nondirectory buffer-file-name)))))))


;; private


;; 打开find-list中存在的文件
(defun find-exists-file-in-list (find-list)
  (let ((final nil))
    ;; 遍历find-list中的每个元素
    (dolist (fff find-list final)
      (when (file-exists-p fff)
        (find-file fff)
        (setq final t)))
    final))


;; 判断一个字符串是否在列表中
(defun string-in-list (str list)
  (let ((final nil))
    (dolist (s list final)
      (when (string= str s)
        (setq final t)))))


;; 给一个字符串前缀都加上扩展名
(defun add-ext (basename extlist)
  (let ((r '()))
    (dolist (ext extlist)
      (add-to-list 'r
                   (concat basename "." ext)))
    r))


;; 实现C语言中的strstr
(defun string-index-of (str substr)
  (let ((c 0)
        (substr-length (length substr))
        (str-length (length str)))
    ;; 从第一个字符比较substr
    (catch 'got
      (while (<= (+ c substr-length) str-length)
        (when (string= (substring str c (+ c substr-length))
                       substr)
          (throw 'got c))
        (setq c (+ 1 c))))))


(defun strstr (str substr)
  "在str中查找substr，返回查找到的位置到结尾的字符串"
  (let ((r (string-index-of str substr)))
    (substring str r)))


(defun string-replace (str sub with)
  (let ((f (string-index-of str sub))
        (first))
    (setq first (substring str 0 f))
    (setq first (concat first with))
    (setq first (concat first (substring str (+ f (length sub)))))
    first))
