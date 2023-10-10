; -*- lisp -*-
(require :asdf)

(defvar ce-commands-alist '((#\q . quit)
 (#\Newline . ce-command-enter)
 (#\: . ce-command-eval)
 (#\; . ce-command-eval-region)
 (#\, . ce-command-swap-point)
 (#\/ . ce-command-search)
 (#\? . ce-command-search-backwards)
 (#\= . ce-command-get-point)
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
; 1 - outpoint set, inpoint is outpoint
; 2 - inpoint set, outpoint is eof
; 3 - both set
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
  should be called at the beginning of most commands"
  (if (= 1 newpoint)
   (setq inpoint outpoint))
  (setq newpoint 0))

(defun ce-repl ()
  "parse commands from stdin"
  (loop
   (let ((input (read-char)))
    (let ((cmd (cdr (assoc input ce-commands-alist))))
     (if cmd
      (funcall cmd input)
      (progn (read-line) (setq newpoint 0) (format t "?~%")))))))

(defun ce-command-eval (c)
  "evaluate a lisp expression"
  (ce-reset-point)
  (format t "~a~%" (eval (read))))

(defun ce-command-swap-point (c)
  "set the inpoint"
  (if (= 0 newpoint)
   (setq inpoint 0)
   (setq inpoint outpoint))
  (setq outpoint -1)
  (setq newpoint 2))

(defun ce-command-get-point (c)
  "print the point"
  (ce-reset-point)
  (read-line)
  (if (not (= inpoint outpoint))
   (format t "~a," inpoint))
  (format t "~a~%" outpoint))

(defun ce-command-number (c)
  "input a number"
  (if (or (= 0 newpoint) (= 2 newpoint))
   (progn (setq newpoint (+ 1 newpoint)) (setq outpoint 0)))
  (setq outpoint (+ (* 10 outpoint) (digit-char-p c))))

