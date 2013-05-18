(mmm-add-group 'mmm-html-mode
               '((js
                  :submode js-mode
                  :face mmm-delimiter-face
                  :front "<script>"
                  :back "</script>")
                 (css
                  :submode css-mode
                  :face mmm-delimiter-face
                  :front "<style>"
                  :back "</style>")
                 (php
                  :submode php-mode
                  :face mmm-delimiter-face
                  :front "<%php"
                  :back "%>")))
