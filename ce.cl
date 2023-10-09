; -*- lisp -*-
(require :asdf)

(defvar ce-commands-alist '(
 (#\: . ce-command-eval)
 (#\q . quit)
 ))

(defun concat (&rest args)
  "concatenate strings together but with format"
  (format nil "~{~a~}" args))

(defun ce-repl ()
  "parse commands from stdin"
  (loop
   (let ((cmd (cdr (assoc (read-char) ce-commands-alist))))
    (if cmd
     (funcall cmd)
     (progn (read-line) (format t "?~%"))))))

(defun ce-command-eval ()
  "evaluate a lisp expression"
  (format t "~a~%" (eval (read))))

