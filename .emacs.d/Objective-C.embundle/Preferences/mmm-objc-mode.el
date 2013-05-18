(require 'mmm-mode)

(mmm-add-group 'mmm-objc-mode
               '((interface
                  :submode objc-mode
                  :face mmm-delimiter-face
                  :front "@interface"
                  :back "@end"
                  :include-front t
                  :include-back t)
                 (implenemtation
                  :submode objc-mode
                  :face mmm-delimiter-face
                  :front "@implementation"
                  :back "@end"
                  :include-front t
                  :include-back t)))
