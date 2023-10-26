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
 (#\A . ce-command-add-before)
 (#\c . ce-command-line-replace)
 (#\d . ce-command-delete)
 (#\e . ce-command-open)
 (#\h . ce-command-help)
 (#\i . ce-command-insert)
 (#\I . ce-command-insert-beg)
 (#\m . ce-command-move)
 (#\p . ce-command-print)
 (#\s . ce-command-reg-replace)
 (#\t . ce-command-copy)
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

(defvar buffer nil)
(defvar filename nil)

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

(defun ce-mod (num div)
  "modulus but handle zero"
  (if (= 0 div) 0 (mod num div)))

(defun ce-push-line (index line)
  "push a line into the buffer at index"
  (if (= 0 index) ; index 0 is special as buffer is a singly linked list
   (setq buffer (cons line buffer))
   (push line (cdr (nthcdr (1- index) buffer)))))

(defun ce-push-lines (index lines)
  "push lines into the buffer at index"
  (if lines
   (progn
    (ce-push-line index (car lines))
    (ce-push-lines (1+ index) (cdr lines)))))

(defun ce-delete (in out)
  "delete a range of lines"
  (setq buffer (nconc
   (subseq buffer 0 in)
   (nthcdr (1+ out) buffer))))

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

(defun ce-main ()
  "initalize commoned from bin"
  (let ((args (cdr (ext:command-args))))
   (case (list-length args)
    (0 ())
    (1 (ce-open (car args)))
    (otherwise (format t "?~%"))))
  (ce-repl)
  (ext:quit 0))

(defun ce-command-enter (&optional c)
  "process newlines if not eaten by another command"
  (if (= 0 newpoint)
   (if (>= (1+ outpoint) (list-length buffer))
    (progn
     (format t "?~%")
     (return-from ce-command-enter))
    (progn
     (setq outpoint (1+ outpoint))
     (setq inpoint outpoint)))
   (ce-reset-input))
  (let ((out (ce-mod outpoint (list-length buffer))))
   (format t "~a~%" (car (nthcdr out buffer)))))

(defun ce-command-eval (&optional c)
  "evaluate a lisp expression"
  (ce-reset-input)
  (format t "~a~%" (eval (read))))

(defun ce-command-eval-region (&optional c)
  "evaluate a region"
  (ce-reset-input)
  (read-line)
  (let ((mlen (list-length buffer)))
   (let ((in (ce-mod inpoint mlen)) (out (1+ (ce-mod outpoint mlen))))
    (format t "~a~%" (eval (read-from-string
     (format nil "~{~a~%~}" (subseq buffer in out))))))))

(defun ce-command-swap-point (&optional c)
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

(defun ce-command-get-point (&optional c)
  "print the point"
  (ce-reset-input)
  (read-line)
  (if (not (= inpoint outpoint))
   (format t "~a," inpoint))
  (format t "~a ~a~%" outpoint filename))

(defun ce-add-til-dot (index lines)
  "read input until dot, add to buffer"
  (let ((line (read-line)))
   (if (string= "." line)
    (progn
     (setq inpoint index)
     (setq outpoint (+ index (1- (list-length lines))))
     (ce-push-lines index (reverse lines)))
    (ce-add-til-dot index (cons line lines)))))

(defun ce-common-add (index)
  "common parts of ce-command-add and ce-command-add-before"
  (let ((line (read-line)))
   (if (string= "" line)
    (ce-add-til-dot index nil)
    (progn
     (setq inpoint index)
     (setq outpoint index)
     (ce-push-line index line)))))

(defun ce-command-add (&optional c)
  "add lines after point"
  (ce-reset-input)
  (if buffer
   (ce-common-add (1+ (ce-mod outpoint (list-length buffer))))
   (ce-common-add 0)))

(defun ce-command-add-before (&optional c)
  "add lines before point"
  (ce-reset-input)
  (ce-common-add (ce-mod inpoint (list-length buffer))))

(defun ce-command-delete (&optional c)
  "delete the region"
  (ce-reset-input)
  (read-line)
  (let ((mlen (list-length buffer)))
   (ce-delete (ce-mod inpoint mlen) (ce-mod outpoint mlen))
   (setq outpoint inpoint)))

(defun ce-command-line-replace (&optional c)
  "replace the region"
  (ce-reset-input)
  (let ((mlen (list-length buffer)))
   (let ((in (ce-mod inpoint mlen)) (out (ce-mod outpoint mlen)))
    (ce-delete in out)
    (ce-common-add in))))

; TODO: needs error handling, and to be able to "open" nonexistant files
(defun ce-open (name)
  "function to open a file for editing"
  (setq filename name)
  (setq buffer (uiop:read-file-lines filename)))

(defun ce-command-open (&optional c)
  "open a file for editing"
  (ce-reset-input)
  (let ((name (read-line)))
   (if (string= "" name)
    (format t "?~%")
    (ce-open name))))

(defun ce-command-help (&optional c)
  "get help for commoned commands"
  (ce-reset-input)
  (let ((key (read-char)))
   (if (char= #\Newline key)
    (format t "Welcome to commoned. try h<letter> to get help for a
specific command. the recognized commands are as follows:
~{~a~^ ~}" (remove-if-not 'not-num-new-p (mapcar 'car ce-commands-alist)))
    (progn (read-line) (help (cdr (assoc key ce-commands-alist))))))
  (format t "~%"))

(defun ce-command-print (&optional c)
  "print a region"
  (ce-reset-input)
  (read-line)
  (let ((mlen (list-length buffer)))
   (if (not (= 0 mlen))
    (let ((in (ce-mod inpoint mlen)) (out (1+ (ce-mod outpoint mlen))))
     (format t "~{~a~%~}" (subseq buffer in out)))
    (format t "?~%"))))

; TODO: needs error handling
(defun ce-command-write (&optional c)
  "write a file to disk"
  (ce-reset-input)
  (let ((name (read-line)))
   (with-open-file (out (if (string= "" name) filename name)
			:direction :output
			:if-exists :overwrite
			:if-does-not-exist :create)
    (format out "~{~a~%~}" buffer))))

(defun ce-command-number (c)
  "input a number"
  (if (or (= 0 newpoint) (= 2 newpoint))
   (progn (setq newpoint (1+ newpoint)) (setq outpoint 0)))
  (setq outpoint (+ (* 10 outpoint) (digit-char-p c))))

