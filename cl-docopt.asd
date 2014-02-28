; -*- common-lisp -*-

(defsystem cl-docopt
  :depends-on ("esrap-peg" "optima.ppcre" "fare-quasiquote-optima")
  :components ((:file "packages")
               (:file "utilities")
               (:file "parse")
               (:file "main")))
