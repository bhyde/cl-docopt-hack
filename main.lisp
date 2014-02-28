(in-package #:cl-docopt)

;;;; Convert the docopt parse into pseudo lisp code

;;; This pseudo code is intended to be suitable for trival
;;; transformation into real code in assorted langauges, for example
;;; bash.

(defun parse2code (docopt-parse)
  (let ((all-vars ()))
    (labels ((note-var (var-name init)
               (push (cons var-name init) all-vars)
               var-name)
             (nl (&optional continue?)
               (when continue?
                 (format t " \\"))
               (format t "~&~A" indent))
             (recure* (x)
               (mapcar #'recure x))
             (recure (x)

              (ematch x
                 (`(:usage :templates ,usage-patterns :option-descriptions ,_)
                   `(with-boiler-plate ()
                      (or ,@(recure* usage-patterns)
                          (error-and-usage "Bad Arguments"))))
                 (`(:usage-pattern ,command-name ,@arg-patterns)
                   (declare (ignore command-name))
                   `(and 
                     ,@(recure* arg-patterns)
                     (success)))
                 (`(:argument-sequence ,@args)
                   `(and ,@(recure* args)))

                 (`(:command-option ,name)
                   `(when (match ,name) (setf ,name 1) (advance)))
                 (`(:short-option ,name)
                   `(when (match ,name) (setf ,(subseq name 1) 1) (advance)))
                 (`(:long-option ,name)
                   `(when (match ,name) (setf ,(subseq name 2) 1) (advance)))
                 (`(:long-option-set ,name ,var)
                   `(when (match-prefix ,name) (setf ,var (get-suffix ,name)) (advance)))
                 (`(:positional-argument ,name)
                   `(progn (setf ,name 1) (advance)))
                 (:options-wildcard
                  `(:options-wildcard))
                 (`(:optional ,@options)
                   `(or ,@(recure* options) (success)))
                 (`(:one-of ,@alternatives)
                   `(or ,@(recure* alternatives) (fail)))

                 )))
      (recure docopt-parse))))


(defun code2bash (code)
  (let ((*print-pretty* t)
        (*print-right-margin* 78)
        (*print-miser-width* 5))
    (labels
        ((f (&rest x) (apply #'format t x))
         (recure* (x join)
           (cond
             ((rest x)
              (pprint-logical-block (*standard-output* nil :prefix "{ " :suffix "; }")
                (loop 
                 initially (pprint-newline :linear) (recure (first x))
                 for i in (rest x)
                 do (f " ~A " join) (pprint-newline :linear) (recure i))))
             (t
              (recure (first x)))))
         (current-arg () 
           (f "\"${A[i]}\""))
         (recure (x)
           (ematch x
             (`(with-boiler-plate nil ,body)
               (f "function usage()~&{~&  cat <<EOF~&~A~&EOF~&}" *last-docopt*)
               (f "~2&function error_and_usage() { echo \"ERROR: $@\"; usage ; exit 1 ; }")
               (f "~2&function parse_arguments(){~&")
               (f "  local i=0 A=($@)~&  ")
               (recure body)
               (f "~&}~&"))
             (`(success)        (f "let 1"))
             (`(fail)           (f "let 0"))
             (`(advance)        (f "let ++i"))
             (`(progn ,@stms)    (recure* stms    ";"))
             (`(and ,@clauses)  (recure* clauses "&&"))
             (`(or ,@clauses)   (recure* clauses "||"))
             (`(when ,pred ,@body)
               (recure `(and (test ,pred) (progn ,@body (success)))))
             (`(test ,pred)
               (f "[[ ") (recure pred) (f " ]] "))
             (`(setf ,name ,value)
               (f "~A=" name)
               (recure value))
             (`(match ,var-name)
               (f "~S = " var-name) (current-arg))
             (`(match-prefix ,prefix)
               (current-arg)
               (f " =~~ ^~A=.*$" prefix))
             (`(get-suffix ,prefix)
               (f "\"${A[i]/#~A=/}\"" prefix))
             (`(error-and-usage ,msg)
               (f "error_and_usage ~S" msg))
             ((type integer) (f "~S" x))
             ((type string)  (f "~S" x)))))
      (recure code))))

;;;; Code Emitter

#+nil
(defun emit-bash-from-parse (docopt-parse)
  (let ((all-vars ())
        (indent (make-array 50
                            :initial-element #\space
                            :element-type 'character
                            :fill-pointer 0)))
    (macrolet ((with-block ((&optional single-line?) &body body)
                 (cond
                   (single-line?
                    `(progn
                       (f " { ")
                       ,@body
                       (f " ; } ")))
                   (t
                    `(progn (f "~&~A{ " indent)
                            (incf (fill-pointer indent) 2)
                            ,@body
                            (decf (fill-pointer indent) 2)
                            (f "~&~A}" indent))))))
      (labels ((note-var (var-name init)
                 (push (cons var-name init) all-vars)
                 var-name)
               (nl (&optional continue?)
                 (when continue?
                   (format t " \\"))
                 (format t "~&~A" indent))
               (f (&rest x) (apply #'format t x))
               (f+ (&rest x)
                 (nl t)
                 (apply #'format t x))
               (f* (&rest x)
                 (loop
                    with length = (length x)
                    for i below 30
                    for start = 0 then (1+ end)
                    while (< start length)
                    for end = (or (position :! x
                                            :start (1+ start))
                                  length)
                    do 
                      (nl)
                      (apply #'format t (subseq x start end))))
               (&& () (f " && "))
               (next-arg () (f " ; let ++i "))
               (recure* (join x)
                 (loop
                      for first = t then nil
                      for arg in x
                      unless first do (nl t) (format t "~A \\" join)
                      do (nl) (recure arg)))
               (recure (x)

                 (ematch x
                   (`(:usage :templates ,usage-patterns :option-descriptions ,_)
                     (f "#!/bin/bash")
                     (f "~&function usage()")
                     (with-block ()
                       (nl) (f "cat <<EOF~&~A" *last-docopt*)
                       (f "~&EOF"))
                     (nl) 
                     (nl)
                     (f "~&function error_and_usage()")
                     (with-block ()
                       (nl) (f "echo \"ERROR: $*\"")
                       (nl) (f "usage")
                       (nl) (f "exit 1"))
                     (nl)
                     (nl)
                     (let ((body (with-output-to-string (*standard-output*)
                                   (recure* "||" usage-patterns))))
                       (f "function parse_arguments()")
                       (with-block ()
                         (nl) (f "local i=0 A=(\"$@\")~&")
                         (loop 
                            for (var . init) in all-vars
                            do (nl) (f "~A=~S" var init)
                            finally (nl))
                         (format t "~A" body)
                         )))

                   (`(:usage-pattern ,command-name ,@arg-patterns)
                     (declare (ignore command-name))
                     (recure* "&&" arg-patterns)
                     (format t " ||")
                     (nl t)
                     (f "error_and_usage 'Bad Args'"))

                   (`(:command-option ,name)
                     (with-block (t)
                       (f "[[ ~s = \"${A[i]}\" ]]" name)
                       (&&)
                       (with-block ()
                         (f "~A=1" (note-var name 0)) 
                         (next-arg))))
                   (`(:short-option ,name)
                     (with-block (t)
                       (f "[[ ~s = \"${A[i]}\" ]]" name)
                       (&&)
                       (with-block (t)
                         (f "~A=1" (note-var (subseq name 1) 0)) 
                         (next-arg))))
                   (`(:long-option ,name)
                     (with-block (t)
                       (f "[[ ~s = \"${A[i]}\" ]]" name)
                       (&&)
                       (with-block (t)
                         (f "~A=1" (note-var (subseq name 2) 0)) 
                         (next-arg))))
                   (`(:positional-argument ,name)
                     (let ((name (if (char= #\< (char name 0))
                                      (subseq name 1 (- (length name) 1))
                                      name)))
                       (with-block (t)
                         (f "~A=\"${A[i]}\"" (note-var name 0)) (next-arg))))
                   (:options-wildcard
                    (with-block (t)
                      (f "{ REST=\"$@\"")
                      (next-arg)))
                   (`(:optional ,@options)
                     (recure* "||" options)
                     (f " || :"))
                   (`(:one-of ,@alternatives)
                     (loop for alt in alternatives
                        do (with-block ()
                             (recure* "||" alt))))

                   )))
        (recure docopt-parse)))))

;;;; Entry point

(defvar *last-code*)

(defun docopt2bash (&optional 
                      (docopt-pathname-namestring-or-text "~/w/cl-docopt/example.txt"))
  (parse-docopt docopt-pathname-namestring-or-text)
  (setf *last-code* (parse2code *last-parse*))
  (let ((script (with-output-to-string (*standard-output*)
                  (code2bash *last-code*))))
    (format t "~&~A" script)
    (with-open-file (s "/tmp/foo.sh" :direction :output :if-exists :rename-and-delete)
      (format s "~A" script)
      (format s "~2&parse_arguments \"$@\"")
      (format s "~&echo \"o: $o\""))))

