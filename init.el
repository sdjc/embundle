;; .emacs
;; 2011-4-2
(unless (fboundp 'cl-flet)
  (defalias 'cl-flet 'flet))
(setq configuration-directory
      (directory-file-name (file-name-directory load-file-name)))


;; 载入bundle支持文件
(load-library (format "%s/.emacs.d/Support/embundle"
                      configuration-directory))


;; 载入所有和语言无关的包
(dolist (bundle '("BasicEditing" "Shell" "ColorTheme" "php"))
  (load-bundle (format "%s/.emacs.d/%s.embundle"
                       configuration-directory
                       bundle)))


(defun load-extra-bundle ()
  "载入其他的扩展Emacs的功能，通常可以认为带某种语言特性的包是附加
的"
  (interactive)
  (load-all-bundles (format "%s/.emacs.d/Support" configuration-directory))
  (load-all-bundles (format "%s/.emacs.d" configuration-directory)))
(load-extra-bundle)