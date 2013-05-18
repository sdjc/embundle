(add-hook 'python-mode-hook
          (lambda ()
            (flymake-mode t)
            (hs-minor-mode t)))
