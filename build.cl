; -*- lisp -*-
(require :asdf)
(require :cmp)

(setf c::*compile-in-constants* t)

; borrowed from ecl example
(push (make-pathname :name nil :type nil :version nil
		     :defaults *load-truename*)
      asdf:*central-registry*)

(asdf:make-build :ce :type :fasl :move-here "./")

(load "ce.fasb")

(asdf:make-build :ce
		 :type :program
		 :epilogue-code '(ce-main)
		 :move-here "./")

(format t "done building~%")

