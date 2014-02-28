(in-package #:cl-docopt)


(defvar *last-docopt*)
(defvar *last-parse*)

(defun parse-docopt (pathname-or-text)
  (setf *last-docopt*
        (typecase pathname-or-text
          (pathname (snarf-file-contents pathname-or-text))
          (string
           (cond
             ((and 
               (not (search "sage:" pathname-or-text))
               (< (length pathname-or-text) 200)
               (probe-file pathname-or-text))
              (snarf-file-contents (probe-file pathname-or-text)))
             (T
              pathname-or-text)))))
  (setf *last-parse*
        (parse '<command-usage> *last-docopt*)))


(defun not-usage (x)
  (not (string= x "Usage:")))

(defun short-text (txt)
  (< (length txt) #.(length "Usage:")))

;;;; Grammar for docopt


(defrule <command-usage>
    (and  (? <info-header>)
         <usage-patterns>
         (? (and <eol> <eol> <option-descriptions>))
         (* (or <ws> <eol>)))
  (:destructure (a b c d)
                (declare (ignore d))
                `(:usage-patterns :one-line-doc? ,a
                                  :templates ,b
                                  :option-descriptions ,(third c))))

(defrule <info-header> (and (or (and (not-usage (string 6)) (? <text>))
                                (short-text <text>))
                            <eol> <ws?> <eol>)
  (:lambda (x) (flatten-into-string (first x))))

(defrule <usage-patterns> (or <multiline-usage-patterns>
                              <single-line-usage-pattern>))

(defrule <single-line-usage-pattern> (and "Usage:" <a-command-template>)
  (:lambda (x) (list x)))

(defrule <multiline-usage-patterns> (and "Usage:" (+ (and <eol> <a-command-template>)))
  (:lambda (x) (mapcar #'second (second x))))

(defrule <a-command-template> 
    (and <ws?> <command-name> (? <arguments>))
  (:destructure (a b c) (declare (ignore a)) `(:usage-pattern ,b ,c)))

(defrule <arguments> (and <ws?> <argument> (* (and <ws> <argument>)))
  (:lambda (x)
    (let ((a (second x))
          (b (mapcar #'second (third x))))
      (if b
          `(:argument-sequence ,a ,@b)
          a))))

(defrule <argument> (or <options-wildcard>
                        <end-of-options-marker>
                        <use-std-in>
                        <one-or-more>
                        <one-prime>))

(defrule <one-prime> (or <simple-alteration>
                         <one>))

(defrule <one> (or <positional-argument>
                   <command-argument>
                   <short-option>
                   <long-option-with-arg>
                   <long-option>
                   <optional-argument>
                   <required-argument>))

(defrule <simple-alteration> (and <one> <ws?> #\| <ws?> <one>)
  (:destructure (a x y z b)
                (declare (ignore x y z))
                `(:one-of ,a ,b)))

(defrule <one-or-more> (and <argument> "...")
  (:lambda (x) `(one-or-more ,(first x))))

(defrule <arg-alternatives> (and <arguments> (* (and <ws?> #\| <arguments>)))
  (:destructure (a b) `(,a ,@(loop for (nil nil x) in b collect x))))

(defrule <optional-argument> (and #\[ <arg-alternatives> <ws?> #\])
  (:destructure (a b c d) 
                (declare (ignore a c d))
                `(:optional ,@b)))

(defrule <required-argument> (and #\( <arg-alternatives> <ws?> #\))
  (:destructure (a b c d) 
                (declare (ignore a c d))
                `(one-of ,@b)))

(defrule <options-wildcard> "[options]" (:constant :options-wildcard))

(defrule <end-of-options-marker> "[--]" (:constant :end-of-options))
(defrule <use-std-in> "[-]" (:constant :use-std-in))
  


(defrule <one-option-description> (and <ws> <first-option-description-line> )
  (:destructure (a b) (declare (ignore a)) b))

(defrule <first-option-description-line> 
    (and <ws?> #\- #\- )
  (:destructure (a b c) (declare (ignore a)) `(,b ,@c)))

;;; misc. lexical elements

(defrule <ws> (+ " ") (:constant :ws))
(defrule <ws?> (? <ws>))
(defrule <eol> (or (and #\return #\linefeed) #\linefeed #\return) (:constant :eol))

(defrule <text> (+ (character-ranges (#\space #\~)))
  (:lambda (x) (flatten-into-string x)))
(defrule <all-name-chars> (character-ranges (#\A #\Z) (#\a #\z) (#\0 #\9) #\_ #\-))
(defrule <blank-line> (and <ws?> <eol>)
  (:constant :blank-line))

(defrule <command-name> (+ (character-ranges (#\A #\Z) (#\a #\z) #\_ #\-))
  (:function flatten-into-string))

(defrule <positional-argument>
    (or (and #\<  (+ (character-ranges (#\a #\z) (#\A #\Z) #\_ #\-)) #\>)
        (and (character-ranges (#\A #\Z)) (* (character-ranges (#\A #\Z) #\_ #\-))))
  (:lambda (x) `(:positional-argument ,(flatten-into-string x))))
(defrule <command-argument>
    (and (character-ranges (#\a #\z)) (* (character-ranges (#\a #\z) #\_ #\-)))
  (:lambda (x) `(:command-option ,(flatten-into-string x))))
(defrule <short-option>
    (and #\- (+ (character-ranges (#\a #\z) (#\A #\Z))))
  (:lambda (x) `(:short-option ,(flatten-into-string x))))
(defrule <long-option>
    (and #\- #\- (+ (character-ranges (#\a #\z) (#\A #\Z))))
  (:lambda (x) `(:long-option ,(flatten-into-string x))))
(defrule <long-option-with-arg>
    (and (and #\- #\- (+ (character-ranges (#\a #\z) (#\A #\Z))))  #\= <positional-argument>)
  (:destructure (a b c)
                (declare (ignore b))
                `(:long-option-set ,(flatten-into-string a) ,(flatten-into-string c))))



(defrule <option-descriptions> (or <multiline-option-descriptions>
                                   <single-line-option-descriptions>))

(defrule <single-line-option-descriptions> (and "Options:"  <info-for-an-option>)
  (:lambda (x) `(,(second x))))

(defrule <multiline-option-descriptions> (and "Options:"  (+ (and <eol> <info-for-an-option>)))
  (:lambda (x) (mapcar #'second (second x))))

(defrule <info-for-an-option> (and <option-list> 
                                   (? (and #\space <ws> <text>)) 
                                   (* (and <eol> <more-option-doc>)))
  (:destructure (a b c)
                (declare (ignore x y))
                (let* ((b (or (third b) ""))
                       (doc (format nil "~a~{~^~&~a~}" b (mapcar #'second c)))
                       (default (match doc
                                  ((ppcre "\\[default: (.*)]" d) d)
                                  (_ :no-default-given))))
                  `(:option-info :forms ,a
                                 :default ,default
                                 :doc ,doc))))

(defrule <option-list> (and <ws> <option-id> (* (and (? #\,) #\space <option-id>)))
  (:lambda (x) `(,(second x) ,@(mapcar #'third (third x)))))

(defrule <option-id> (or <option-id-a>
                         <option-id-b>
                         <option-id-c>))

(defrule <option-id-a> (and <option-id-c> #\space <positional-argument>)
  (:destructure (a b c) (declare (ignore b)) `(:long-set-option ,(second a) ,(second c))))

(defrule <option-id-b> (and <option-id-c> #\= <positional-argument>)
  (:destructure (a b c) (declare (ignore b)) `(:long-set-option= ,(second a) ,(second c))))

(defrule <option-id-c> (and #\- (+ <all-name-chars>))
  (:lambda (x) `(:simple-option ,(flatten-into-string x))))

(defrule <more-option-doc> (and #\space <ws> (not #\-) <text>)
  (:destructure (a b c)
                (declare (ignore a))
                (format nil "~a~a" b c)))

;;;; Testcases

(defvar *testcases* nil)

(defun get-test-cases ()
  (length 
   (setf *testcases*
         (loop 
            with pathname = (asdf:system-relative-pathname "cl-docopt" "testcases.docopt")
            with text = (snarf-file-contents pathname)
            with scan-for-open = (cl-ppcre:create-scanner "r\"\"\"" :single-line-mode t)
            with scan-for-close = (cl-ppcre:create-scanner "\"\"\"" :single-line-mode t)
            for start = 0 then (+ 3 end)
            for first = (+ 4
                           (or (cl-ppcre:scan scan-for-open text :start start)
                               (return result)))
            for end = (cl-ppcre:scan scan-for-close text :start first)
            collect (subseq text first end) into result))))

(defun do-parse-test (n)
  (unless *testcases* (get-test-cases))
  (parse-docopt (nth n *testcases*)))
