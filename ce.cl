; -*- lisp -*-
(require :asdf)

(defvar ce-commands-alist '((#\q . quit)
 (#\Newline . ce-command-enter)
 (#\: . ce-command-eval)
 (#\, . ce-command-swappoint)
 (#\/ . ce-command-search)
 (#\? . ce-command-search-backwards)
 (#\a . ce-command-add)
 (#\B . ce-command-open)
 (#\c . ce-command-line-replace)
 (#\h . ce-command-help)
 (#\i . ce-command-insert)
 (#\I . ce-command-insert-beg)
 (#\o . ce-command-newline)
 (#\O . ce-command-newline-before)
 (#\p . ce-command-print)
 (#\s . ce-command-reg-replace)
 (#\w . ce-command-write)
 (#\0 . ce-command-number)
 (#\1 . ce-command-number)
 (#\2 . ce-command-number)
 (#\3 . ce-command-number)
 (#\4 . ce-command-number)
 (#\5 . ce-command-number)
 (#\6 . ce-command-number)
 (#\7 . ce-command-number)
 (#\8 . ce-command-number)
 (#\9 . ce-command-number)
 ))

; newpoint values:
; 0 - reusing previous point
; 1 - outpoint set, discard inpoint
; 2 - inpoint set
(defvar newpoint 0)
(defvar inpoint 0)
(defvar outpoint -1)
(defvar inline-inpoint 0)
(defvar inline-outpoint -1)

(defun concat (&rest args)
  "concatenate strings together but with format"
  (format nil "~{~a~}" args))

(defun ce-reset-point ()
  "fix point to allow inputting new numbers,
  should be called at the end of most commands"
  (setq newpoint 0))

(defun ce-repl ()
  "parse commands from stdin"
  (loop
   (let ((input (read-char)))
    (let ((cmd (cdr (assoc input ce-commands-alist))))
     (if cmd
      (funcall cmd input)
      (progn (read-line) (format t "?~%")))))))

(defun ce-command-eval (c)
  "evaluate a lisp expression"
  (format t "~a~%" (eval (read)))
  (ce-reset-point))

