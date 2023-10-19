
; override file extension
; defaults to .lisp rather than .cl
(defclass file (cl-source-file)
  ((type :initform "cl")))

(defsystem :ce
  :depends-on (#:asdf)
  :components ((:file "ce")))

