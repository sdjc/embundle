(if (eq system-type 'windows-nt)
(custom-set-variables '(current-language-environment "Chinese-GB")))

(setq default-buffer-file-coding-system 'utf-8-unix)            ;�����ļ�����
(setq default-file-name-coding-system 'gb2312)              ;�ļ�������
(setq default-keyboard-coding-system 'utf-8-unix)               ;�����������
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)) ;��������������
(setq default-sendmail-coding-system 'utf-8-unix)               ;�����ʼ�����
(setq default-terminal-coding-system 'utf-8-unix)               ;�ն˱���