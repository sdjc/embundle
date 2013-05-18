(defvar my-compile-command nil)
(defvar my-compile-directory nil)


(defun compile-at (compile-command compile-directory)
  "在directory执行编译操作"
  (interactive (list
                (read-compile-command)
                (read-compile-directory)))
  (cd compile-directory)
  (compile compile-command))


(defun read-compile-command ()
  "读取编译命令"
  (unless my-compile-command
    (setq my-compile-command compile-command))
  (setq my-compile-command
        (read-shell-command "Command: " my-compile-command)))


(defun read-compile-directory ()
  "读取编译目录"
  (unless my-compile-directory
    (setq my-compile-directory default-directory))
  (setq my-compile-directory
        (read-directory-name  "Compile at: " my-compile-directory)))
