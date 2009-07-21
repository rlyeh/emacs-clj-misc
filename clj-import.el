(defun clj-import (re)
  (interactive "sClass regex?: ")
  (slime-eval-with-transcript
   `(swank:interactive-eval ,(format "(user/find-classes #\"%s\")" re))
   nil t
   `(lambda (s)
      (with-current-buffer ,(current-buffer)
        (let ((classes (car (read-from-string s))))
          (insert (clj-format-import classes)))))))



(defun clj-format-import (classes)
  (let ((packages (make-hash-table :test 'equal)))
    (mapc (lambda (class)
            (let ((pkg (file-name-directory class))
                  (name (file-name-nondirectory class)))
              (puthash pkg (cons name (gethash pkg packages '()))
                       packages )))
          classes)
    (let ((imports '()))
      (maphash (lambda (pkg names)
                 (push (format "'(%s %s)"
                               (replace-regexp-in-string
                                "/" "."
                                (replace-regexp-in-string "/$" "" pkg))
                               (mapconcat 'identity names " "))
                       imports))
               packages)
      (format "(import %s)\n"
              (mapconcat 'identity imports "\n        ")))))
