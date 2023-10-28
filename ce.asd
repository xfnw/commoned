; -*- lisp -*-

; override file extension
; defaults to .lisp rather than .cl
(defclass clfile (cl-source-file)
  ((type :initform "cl")))

(defsystem :ce
  :depends-on (#:asdf)
  :components
  ((:clfile "ce" :depends-on ("pregexp/pregexp"))
   (:file "pregexp/pregexp")))

