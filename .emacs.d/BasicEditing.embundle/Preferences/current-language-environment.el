(if (eq system-type 'windows-nt)
(custom-set-variables '(current-language-environment "Chinese-GB")))

(setq default-buffer-file-coding-system 'utf-8-unix)            ;缓存文件编码
(setq default-file-name-coding-system 'gb2312)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8-unix)               ;键盘输入编码
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)) ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8-unix)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8-unix)               ;终端编码