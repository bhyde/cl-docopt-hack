(in-package #:cl-docopt)


(defun snarf-file-contents (pathname)
  (esrap-peg::file-contents pathname))

;;;; parsing utilities

(defun flatten-into-string (parse-tree)
  (with-output-to-string (s)
    (labels ((recure (x)
               (typecase x
                 (string
                  (princ x s))
                 (character
                  (princ x s))
                 (cons
                  (recure (car x))
                  (recure (cdr x))))))
      (recure parse-tree))))

