;; 一些特殊的buffer如果kill掉将会造成其相应的process也被kill，比如
;; *Pymac*、*Python Run*等等


;; (defun kill-buffer-query ()
;;   (not (string-match "\*.+\*" (buffer-name))))
;; (add-hook 'kill-buffer-query-functions 'kill-buffer-query)
