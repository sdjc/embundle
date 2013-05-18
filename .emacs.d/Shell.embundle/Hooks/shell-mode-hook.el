(add-hook 'shell-mode-hook
          (lambda ()
            (setq mode-line-format `("%e" ,mode-line-default-directory))))