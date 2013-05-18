;; multishell.el
;; Created by yjzeng, 2011-3-11
;; 把多开shell的逻辑独立出来


(provide 'multishell)
(require 'shell)


(defvar shell-id 0)
(defvar current-command nil)
(defvar cmd-timer nil)


(defun shellsupport-init ()
  (when (equal major-mode 'shell-mode)
    ;; 编号
    (setq shell-id (+ 1 shell-id))
    (make-local-variable 'shell-id)
    (make-local-variable 'current-command)
    (set-buffer-process-coding-system 'utf-8 'utf-8)
    (shellsupport-update)))


(defun shellsupport-update ()
  ;; 取得编号
  (when (local-variable-if-set-p 'shell-id)
    ;; 重命名
    (rename-buffer (shellsupport-make-buffer-name-unique
                    (shellsupport-buffer-orignal-name (current-buffer))
                    (current-buffer)))
    (set-process-query-on-exit-flag
     (get-buffer-process (current-buffer))
     current-command)))


(defun shellsupport-comint-output (text)
  (when (string-match "\\$ $" text)
    (setq current-command nil)
    ;; 要记得cancel掉这个用来更新标题的timer
    (when cmd-timer
      (cancel-timer cmd-timer)))
  (shellsupport-update))


(defun shellsupport-comint-input (text)
  (when (and (not current-command)
             (string-match "\n$" text))
    (setq text (substring text 0 -1))
    ;; 过0.5秒再更新buffer标题，因为ls等等命令的执行时间非常短，所以没必
    ;; 要看因为执行ls，shell-buffer的标题闪一下
    (setq cmd-timer
          (run-at-time .5
                       nil
                       (lambda (text)
                         (setq current-command text)
                         (shellsupport-update))
                       text))))


(defun shellsupport-buffer-orignal-name (buffer)
  "获得shell-buffer原来应该使用的名字，就是没有避免重名的那部分"
  (let ((command (buffer-local-value 'current-command buffer))
        (ddirectory (buffer-local-value 'default-directory buffer)))
    (if command
        (format "proc - %s" command)
      ;; else
      (format "shell - %s" (last-parts-of-file-name ddirectory)))))


(defun last-parts-of-file-name (file-name)
  "取文件名file-name的最后几个部分"
  (let ((parts (reverse (split-string file-name "/")))
        (r)
        (f 0)
        (i 0))
    (catch 'result
      (dolist (part parts r)
        (setq i (1+ i))
        (when (> (+ f (length part)) 12)
          (when (< i (length parts))
            (setq r (cons ".." r)))
          (throw 'result t))
        (setq f (+ f (length part)))
        ;; 原来add-to-list还会自动取出重复的对象
        (setq r (cons part r))))
    (join-list r "/")))


(defun join-list (list sep)
  (let ((r))
    (dolist (elm list r)
      (setq r (concat r (when r sep) elm)))))


(defun shellsupport-make-buffer-name-unique (new-name self-buffer)
  (when (and (get-buffer new-name)
              (not (eq self-buffer (get-buffer new-name))))
    (setq new-name (format "%s - %d"
                           new-name
                           (buffer-local-value 'shell-id self-buffer))))
  new-name)


(defun shellsupport-last-part-of-file-name (filename)
  "获得文件名的最后一段"

  ;; 把文件名按照/打断
  (let* ((parts1 (split-string filename "/"))
         (parts (reverse parts1)))
    (catch 'except-name
      ;; 返回最后一段
      (dolist (name parts)
        (when (> (length name) 0)
          (throw 'except-name name))))))


;; Subscribe events


(defadvice shell (after emshell-after-shell activate)
  (shellsupport-init))


(add-hook 'comint-input-filter-functions 'shellsupport-comint-input)


(add-hook 'comint-output-filter-functions 'shellsupport-comint-output)
