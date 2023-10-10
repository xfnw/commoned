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

(defun not-num-new-p (c)
  "return nil if numeric or newline"
  (not (or (digit-char-p c) (char= #\Newline c))))

(defun ce-reset-input ()
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
  (ce-reset-input)
  (format t "~a~%" (eval (read))))

(defun ce-command-swap-point (c)
  "set the inpoint to recent outpoint or beginning, outpoint to eof

  for example:
  3,5 selects lines 3 through 5 (inclusive)
  ,5  selects from the beginning of the document through line 5
  4,  selects from line 4 to the end of the document"
  (if (= 0 newpoint)
   (setq inpoint 0)
   (setq inpoint outpoint))
  (setq outpoint -1)
  (setq newpoint 2))

(defun ce-command-get-point (c)
  "print the point"
  (ce-reset-input)
  (read-line)
  (if (not (= inpoint outpoint))
   (format t "~a," inpoint))
  (format t "~a~%" outpoint))

(defun ce-command-number (c)
  "input a number"
  (if (or (= 0 newpoint) (= 2 newpoint))
   (progn (setq newpoint (+ 1 newpoint)) (setq outpoint 0)))
  (setq outpoint (+ (* 10 outpoint) (digit-char-p c))))

(defun ce-command-help (c)
  "get help for commoned commands"
  (ce-reset-input)
  (let ((key (read-char)))
   (if (char= #\Newline key)
    (format t "Welcome to commoned. try h<letter> to get help for a
specific command. the recognized commands are as follows:
~{~a~^ ~}" (remove-if-not 'not-num-new-p (mapcar 'car ce-commands-alist)))
    (progn (read-line) (help (cdr (assoc key ce-commands-alist))))))
  (format t "~%"))

