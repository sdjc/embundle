;; embundle.el
;; 2011-9-4


(provide 'embundle)


(defun load-all-bundles (directory)
  (dolist (bundle-name (directory-files directory))
    ;; 其实也不需要什么忽略名称的规则了，不是那个后缀的文件夹或者文件直
    ;; 接忽略
    (when (string-match "\\.embundle$" bundle-name)
      (load-bundle (format "%s/%s" directory bundle-name)))))


;; 我觉得需要下面这么多种的目录吗？综合分析起来只有两种调用需求：
;; require和load-library

;; *.embundle
;; Features 特征
;; Modes
;; Commands
;; AutoLoads
;; Hook
;; Support 支持
;; Preferences
;; KeyBindings
;; Snippets
;; PyModules


(defun load-bundle (directory)
  ;; 想想看，老改变bundle的目录名称也不是办法，还不如加一个文件，告诉不
  ;; 要加载了，这样，如果目录中有一个叫做disabled的文件，就什么都不继续
  ;; 加载
  (unless (file-exists-p (format "%s/disabled" directory))
    ;; add-to-load-path
    ;; Support
    ;; Modes
    (do-when-directories-exists 'add-to-load-path
                                (format "%s/Support" directory)
                                (format "%s/Modes" directory))
    ;; eval-all-scripts
    ;; AutoLoads
    (do-when-directories-exists 'eval-all-scripts
                                ;; (format "%s/Includes" directory)
                                (format "%s/AutoLoads" directory))
    ;; add-to-load-path-and-require
    ;; Features
    (do-when-directories-exists 'add-to-load-path-and-require
                                (format "%s/Features" directory)
                                (format "%s/Models" directory))

    ;; eval-all-scripts
    ;; Preferences
    (do-when-directories-exists 'eval-all-scripts
                                (format "%s/Preferences" directory))

    ;; load-all-pymodules
    ;; PyModules
    (do-when-directories-exists 'load-all-pymodules
                                (format "%s/PyModules" directory))

    ;; eval-all-scripts
    ;; Commands
    ;; Hook
    ;; KeyBindings
    (do-when-directories-exists 'eval-all-scripts
                                (format "%s/Commands" directory)
                                (format "%s/Hooks" directory)
                                (format "%s/KeyBindings" directory))
    ;; TODO: rsync to Support/snippets
    ;; Snippets
    ))


(defun do-when-directories-exists (procedure &rest directories)
  "对directories的目录，如果存在则各个执行procedure"
  (dolist (directory directories)
    (when (file-directory-p directory)
      (funcall procedure directory))))


(defun add-to-load-path (directory)
  (add-to-list 'load-path directory))


(defun eval-all-scripts (directory)
  (dolist (script (directory-files directory))
    (when (string-match "\\.el$" script)
      (load (format "%s/%s" directory script)))))


(defun add-to-load-path-and-require (directory)
  (add-to-load-path directory)
  (eval-all-scripts directory))


(defun python-module-p (python-module-path)
  (or (string-match "\\.py$" python-module-path)
      (and (file-directory-p python-module-path)
           (file-exists-p (format "%s/__init__.py"
                                  python-module-path)))))


(defun load-all-pymodules (directory)
  "不需要再去改动pymacs-load-path了，所有的pymacs的模块都会被自动
  链接到Support/pymodules中"
  (dolist (module-file (directory-files directory))
    (when (python-module-p (format "%s/%s" directory module-file))
      ;; 我们假设这个包已经创建了到pymacs-load-path中的链接，所以直接载
      ;; 入就可以了
      ;; TODO: 插入自动创建链接的逻辑
      (pymacs-load (file-name-sans-extension module-file)))))
