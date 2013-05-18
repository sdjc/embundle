(defvar hs-flag nil)


(defun hs-toggle-hiding-all ()
  "隐藏/显示所有的"
  (interactive)
  (when (local-variable-if-set-p 'hs-flag)
   (make-variable-buffer-local 'hs-flag))
  (print hs-flag) ;;;;
  (if hs-flag
      (hs-show-all)
    (hs-hide-all))
  (setq hs-flag (not hs-flag)))
