; -*- lisp -*-
(require :asdf)

(defvar ce-commands-alist '((#\: . '(eval (read)))))

(defun concat (&rest args)
  "concatenate strings together but with format"
  (format nil "~{~a~}" args))

(defun ce-command-loop ()
  "parse commands from stdin"
  (let ((cmd (cdr (assoc (read-char) ce-commands-alist))))
   (if cmd
    (print cmd)
    (progn (read-line) (format t "?~%")))))

