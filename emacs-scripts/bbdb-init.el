(require 'package)
(package-initialize)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" .  "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'bbdb-autoloads)
(require 'bbdb)
(require 'bbdb-com)
(bbdb-initialize 'gnus 'message)

(defun dump-buffer (buffer-name)
  "Dump contents of buffer to standard out in batch mode"
  (let ((buf (get-buffer buffer-name)))
    (if buf
        (save-current-buffer
          (set-buffer buf)
          (print (buffer-string))))))
