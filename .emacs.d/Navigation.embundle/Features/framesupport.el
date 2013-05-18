;; framesupport.el
;; 2011-5-12
;;
;; 改进Emacs切换窗口的逻辑；这个东西还有一个目的，就是让Emacs工作得更像
;; Mac程序

"
基本概念
-----------

1. 固定有个用来浏览的窗口；
2. 有个主窗口；
3. 有个辅助窗口。

浏览的窗口是不能关闭掉的，而且在需要时总是出现在相同的位置上(左边)。
主窗口和辅窗口可以随时互换。

这里要强制几个buffer是关不掉的。

可以让辅窗口变为主窗口，但无法让主窗口变为辅窗口，除非kill掉。

~~~~~~~~~~~~~

一些新的功能，在不计效率的情况下，先使用Pymacs来实现了，这个用来实
现零散功能的模块就叫em。

这里要明确em是什么:

  1. em实现了一些在日常的文字工作中常用的操作；
  2. em用于改善和试验改善日常文字工作；
"


(provide 'framesupport)


(defun switch-to-next-frame ()
  (interactive)
  (cycle-switch 1))


(defun switch-to-previous-frame ()
  (interactive)
  (cycle-switch -1))


;; private


(defun tabsupport-user-buffer-list ()
  (delq nil
        (mapcar
         (lambda (buffer)
           (when (tabsupport-user-buffer-p (buffer-name buffer))
             buffer))
         (buffer-list))))


(defun tabsupport-buffer-list ()
  (let ((list (tabsupport-user-buffer-list)))
    (add-to-list 'list (current-buffer))))


(defun tabsupport-frame-list ()
  (delq nil
        (mapcar
         (lambda (frame)
           ;; 用frame是否可见取代对名称的判断
           (when (frame-parameter frame 'visibility)
             frame))
         (frame-list))))


(defun cycle-switch (amount)
  "切换到下一个frame，切换的顺序、次数由amount决定"
  (let ((frm-list (tabsupport-frame-list))
        (sel (selected-frame)))

    ;; 我明白了，在daemon方式下启动，会出现一个bug: 因为minibuf在
    ;; daemon下也被算作是一个frame，所以就会出现一个什么值都没有的
    ;; frame；一个比较合适的做法就是把这个frame去掉；两个不相同的frame
    ;; 对象，得到的buffer-list结果居然是一模一样的?! 为什么?
    (when (and frm-list
               (> (length frm-list) 1))
      ;; todo: sort
      (setq frm-list (sort frm-list 'left<))
      ;; todo: index
      (setq idx (index sel frm-list))
      ;; (assert idx) ;;;;
      (setq r (+ idx amount))
      (if (>= r (length frm-list))
          (setq r 0))
      (if (< r 0)
          (setq r (- (length frm-list) 1)))
      (setq new-frame (nth r frm-list))
      (select-frame-set-input-focus new-frame))))


(defun left< (frm1 frm2)
  (< (frame-parameter frm1 'left)
     (frame-parameter frm2 'left)))


(defun index (element list)
  "在list中查找element，并返回序号，序号从0开始"
  (let ((idx 0))
    (catch 'done
      (dolist (e list)
        (when (eq element e)
          (throw 'done idx))
        (setq idx (1+ idx))))))
