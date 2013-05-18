;; 2011-5-10


(provide 'pwhash)


(defvar last-host "localhost" "save the last inputed host name.")


(defun read-domain ()
  (let ((prompt nil)
        (host nil))
    (if last-host
      (setq prompt (format "Host(default %s): " last-host))
      (setq prompt "Host: "))
    (setq host (read-from-minibuffer prompt))
    (if (= 0 (length host)) last-host host)))


(defun pwhash-generate (pw host)
  (interactive (list (read-passwd "Password: ") (read-domain)))
  (setq last-host host)
  (pwhash-pymacs-generate pw host))


(defun pwhash-generate-g1 (pw host)
  (interactive (list (read-passwd "Password: ") (read-domain)))
  (setq last-host host)
  (pwhash-pymacs-generate-g1 pw host))


(defun pwhash-generate-g3 (pw host)
  (interactive (list (read-passwd "Password: ") (read-domain)))
  (setq last-host host)
  (pwhash-pymacs-generate-g3 pw host))
