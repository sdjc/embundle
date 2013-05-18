;; Spotlight.el
;; 2011-8-31


(defun locate-symbol-in-cursor (symbol)
  (interactive (list (thing-at-point 'word)))
  (locate symbol))


(defun locate-selection (beg end)
  (interactive (list (region-beginning) (region-end)))
  (let ((word (buffer-substring beg end)))
    (locate word)))


(defun locate-region ()
  (interactive)
  (call-interactively
   (if (use-region-p)
       'locate-selection
     'locate-symbol-in-cursor)))
