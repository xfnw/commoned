; -*- lisp -*-

(require :asdf)
; pregexp seems to not have a provide, so we cannot require
; instead try loading it if not already bundled in via asdf
(when (not (boundp '*pregexp-version*))
 (load "pregexp/pregexp.lisp"))

(defvar ce-commands-alist '((#\q . quit)
 (#\Newline . ce-command-enter)
 (#\: . ce-command-eval)
 (#\; . ce-command-eval-region)
 (#\{ . ce-command-expand-before)
 (#\} . ce-command-expand)
 (#\= . ce-command-get-point)
 (#\, . ce-command-swap-point)
 (#\/ . ce-command-search)
 (#\? . ce-command-search-before)
 (#\a . ce-command-add)
 (#\A . ce-command-add-before)
 (#\c . ce-command-line-replace)
 (#\d . ce-command-delete)
 (#\e . ce-command-open)
 (#\f . ce-command-fold)
 (#\h . ce-command-help)
 (#\i . ce-command-insert)
 (#\I . ce-command-insert-beg)
 (#\j . ce-command-join)
 (#\m . ce-command-copy)
 (#\n . ce-command-num-print)
 (#\p . ce-command-print)
 (#\s . ce-command-reg-replace)
 (#\t . ce-command-copy)
 (#\w . ce-command-write)
 (#\x . ce-command-chop)
 (#\X . ce-command-chop-beg)
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
(defvar query nil)
(defvar ins nil)
(defvar sfl nil)
(defvar err nil)
(defvar errf "?~%")

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

(defun string-or (a b)
  "or but first being empty is equivalent to nil"
  (or (if (string= "" a) nil a) b))

(defun ce-mod (num div)
  "modulus but handle zero"
  (if (= 0 div) 0 (mod num div)))

(defun ce-apply-region (fun)
  "apply fun to each line of region"
  (ce-pipe-region (lambda (x) (mapcar fun x))))

(defun ce-pipe-region (fun)
  "pipe region through function"
  (let ((mlen (list-length buffer)))
   (let ((in (ce-mod inpoint mlen)) (out (1+ (ce-mod outpoint mlen))))
    (let ((new (funcall fun (subseq buffer in out))))
     (setq outpoint (+ in (1- (list-length new))))
     (ce-replace-lines in out new)))))

(defun ce-push-line (index line)
  "push a line into the buffer at index"
  (if (= 0 index) ; index 0 is special as buffer is a singly linked list
   (setq buffer (cons line buffer))
   (push line (cdr (nthcdr (1- index) buffer)))))

(defun ce-push-lines (index lines)
  "push lines into the buffer at index"
  (setq buffer (nconc
   (subseq buffer 0 index)
   (nconc lines (nthcdr index buffer)))))

(defun ce-replace-lines (in out lines)
  "replace a range of lines"
  (setq buffer (nconc
   (subseq buffer 0 in)
   (nconc lines (nthcdr out buffer)))))

(defun ce-delete (in out)
  "delete a range of lines"
  (setq buffer (nconc
   (subseq buffer 0 in)
   (nthcdr (1+ out) buffer))))

(defun ce-tokens (str tok &optional (len (length str)) (sta 0) (cur 0)
		  (bs nil) (out nil) (rep (pregexp (pregexp-quote
		   (format nil "\\~c" tok)))) (ins (format nil "~c" tok)))
  "tokenize of str at character tok. respects backslashes for escaping."
  (if (< cur len)
   (let ((c (char str cur)))
    (if bs
     ; last char was a backslash, ignore this one
     (ce-tokens str tok len sta (1+ cur) nil out rep ins)
     (if (char= tok c)
      (ce-tokens str tok len (1+ cur) (1+ cur) nil (cons
       (pregexp-replace* rep (subseq str sta cur) ins) out) rep ins)
      (ce-tokens str tok len sta (1+ cur) (char= #\\ c) out rep ins))))
   (reverse (if (= sta cur)
	     out ; there was a trailing delimiter, ignore it
	     (cons (pregexp-replace* rep (subseq str sta cur) ins) out)))))

; TODO: possibly flatten the region here instead of nearly
; every command needing ce-mod to get proper numbers?
; commands that change number of lines would change
; the region anyways
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
      (handler-case (funcall cmd input)
       (error (e) (setq err e) (format t errf)))
      (progn (read-line) (setq newpoint 0) (format t errf)))))))

(defun ce-main ()
  "initalize commoned from bin"
  (let ((args (cdr (ext:command-args))))
   (case (list-length args)
    (0 ())
    (1 (ce-open (car args)))
    (otherwise (format t errf))))
  (ce-repl)
  (ext:quit 0))

(defun ce-command-enter (&optional c)
  "process newlines if not eaten by another command"
  (declare (ignore c))
  (if (= 0 newpoint)
   (if (>= (1+ outpoint) (list-length buffer))
    (progn
     (format t errf)
     (return-from ce-command-enter))
    (progn
     (setq outpoint (1+ outpoint))
     (setq inpoint outpoint)))
   (ce-reset-input))
  (let ((out (ce-mod outpoint (list-length buffer))))
   (format t "~a~%" (car (nthcdr out buffer)))))

(defun ce-command-eval (&optional c)
  "evaluate a lisp expression"
  (declare (ignore c))
  (ce-reset-input)
  (format t "~a~%" (eval (read))))

(defun ce-command-eval-region (&optional c)
  "evaluate first expression in region"
  (declare (ignore c))
  (ce-reset-input)
  (read-line)
  (let ((mlen (list-length buffer)))
   (let ((in (ce-mod inpoint mlen)) (out (1+ (ce-mod outpoint mlen))))
    (format t "~a~%" (eval (read-from-string
     (format nil "~{~a~%~}" (subseq buffer in out))))))))

(defun ce-walk-match (dir match n stop &optional (offset 0))
  "increment n in dir direction until line matches match
  or n reaches stop"
  (if (or (= n stop) (= (+ n offset) stop))
   (progn (format t errf) stop)
   (let ((nn (+ n dir)))
    ; using nth like this is a bit silly and inefficent
    ; when walking forwards, but we can then reuse the
    ; function when walking backwards
    (if (pregexp-match-positions match (nth (+ nn offset) buffer))
     nn
     (ce-walk-match dir match nn stop offset)))))

(defun ce-command-expand-before (&optional c)
  "decrement inpoint until line matches argument"
  (declare (ignore c))
  (ce-reset-input)
  (let ((inp (read-line)) (len (list-length buffer)))
   ; TODO: checking the same condition twice is silly,
   ; find a better way
   (let ((off (if (string= "" inp) -1 0))
	 (match (pregexp (if (string= "" inp) "^[[:space:]]*$" inp))))
    (setq inpoint
     (ce-walk-match -1 match (ce-mod inpoint len) 0 off)))))

(defun ce-command-expand (&optional c)
  "increment outpoint until line matches argument"
  (declare (ignore c))
  (ce-reset-input)
  (let ((inp (read-line)) (len (list-length buffer)))
   (let ((match (pregexp (if (string= "" inp) "^[[:space:]]*$" inp))))
    (setq outpoint
     (ce-walk-match 1 match (ce-mod outpoint len) (1- len))))))

(defun ce-command-get-point (&optional c)
  "print the point"
  (declare (ignore c))
  (ce-reset-input)
  (read-line)
  (let ((len (list-length buffer)))
   (if (not (= inpoint outpoint))
    (format t "~a," (ce-mod inpoint len)))
   (format t "~a ~a~%" (ce-mod outpoint len) filename)))

(defun ce-command-swap-point (&optional c)
  "set the inpoint to recent outpoint or beginning, outpoint to eof

  for example:
  3,5 selects lines 3 through 5 (inclusive)
  ,5  selects from the beginning of the document through line 5
  4,  selects from line 4 to the end of the document"
  (declare (ignore c))
  (if (= 0 newpoint)
   (setq inpoint 0)
   (setq inpoint outpoint))
  (setq outpoint -1)
  (setq newpoint 2))

(defun ce-command-search (&optional c)
  "set point at next line to match argument"
  (declare (ignore c))
  (ce-reset-input)
  (let ((inp (read-line)) (len (list-length buffer)))
   (let ((match
	  (pregexp (if (string= "" inp) query (setq query inp)))))
    (setq
     outpoint
     (ce-walk-match 1 match (ce-mod inpoint len) (1- len)))))
  (setq inpoint outpoint)
  (format t "~a~%" (car (nthcdr outpoint buffer))))

(defun ce-command-search-before (&optional c)
  "set point at previous line to match argument"
  (declare (ignore c))
  (ce-reset-input)
  (let ((inp (read-line)) (len (list-length buffer)))
   (let ((match
	  (pregexp (if (string= "" inp) query (setq query inp)))))
    (setq
     inpoint
     (ce-walk-match -1 match (ce-mod outpoint len) 0))))
  (setq outpoint inpoint)
  (format t "~a~%" (car (nthcdr outpoint buffer))))

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
  (declare (ignore c))
  (ce-reset-input)
  (if buffer
   (ce-common-add (1+ (ce-mod outpoint (list-length buffer))))
   (ce-common-add 0)))

(defun ce-command-add-before (&optional c)
  "add lines before point"
  (declare (ignore c))
  (ce-reset-input)
  (ce-common-add (ce-mod inpoint (list-length buffer))))

(defun ce-command-delete (&optional c)
  "delete the region"
  (declare (ignore c))
  (ce-reset-input)
  (read-line)
  (let ((mlen (list-length buffer)))
   (ce-delete (ce-mod inpoint mlen) (ce-mod outpoint mlen))
   (setq outpoint inpoint)))

(defun ce-command-line-replace (&optional c)
  "replace the region"
  (declare (ignore c))
  (ce-reset-input)
  (let ((mlen (list-length buffer)))
   (let ((in (ce-mod inpoint mlen)) (out (ce-mod outpoint mlen)))
    (ce-delete in out)
    (ce-common-add in))))

; TODO: needs error handling
(defun ce-open (name)
  "function to open a file for editing"
  (setq filename name)
  (if (uiop:file-exists-p name)
   (handler-case (setq buffer (uiop:read-file-lines filename))
    (error (e) (setq err e) (format t errf)))
   (format t errf)))

(defun ce-command-open (&optional c)
  "open a file for editing"
  (declare (ignore c))
  (ce-reset-input)
  (let ((name (read-line)))
   (ce-open (if (string= "" name)
	     filename name))))

; FIXME: this is stupidly inefficent
(defun ce-fold (col lins)
  "fold lines"
  (let ((o (list "")))
   (loop for lin in lins do
    (loop for w in (uiop:split-string lin) do
     (if (< (+ (length w) (length (car o))) col)
      (setf (car o) (if (string= "" (car o)) w
       (format nil "~a ~a" (car o) w)))
      (setq o (cons w o))))
    (setq o (cons "" o)))
   (reverse (cdr o))))

(defun ce-command-fold (&optional c)
  "fold lines in region to argument columns"
  (declare (ignore c))
  (ce-reset-input)
  (let ((sw (read-line)))
   (let ((w (if (string= "" sw) 70 (parse-integer sw))))
    (ce-pipe-region (lambda (x) (ce-fold w x))))))

(defun ce-command-help (&optional c)
  "get help for commoned commands"
  (declare (ignore c))
  (ce-reset-input)
  (let ((key (read-char)))
   (if (char= #\Newline key)
    (format t "Welcome to commoned. try h<letter> to get help for a
specific command. the recognized commands are as follows:
~{~a~^ ~}" (remove-if-not 'not-num-new-p (mapcar 'car ce-commands-alist)))
    (progn (read-line) (help (cdr (assoc key ce-commands-alist))))))
  (format t "~%"))

(defmacro ce-build-insert (a b)
  "helper for insert and insert-beg"
  `(progn
    (ce-reset-input)
    (let ((inp (read-line)))
     (ce-apply-region (lambda (x) (concat ,a ,b))))))

(defun ce-command-insert (&optional c)
  "add to the end of each line in region"
  (declare (ignore c))
  (ce-build-insert x inp))

(defun ce-command-insert-beg (&optional c)
  "add to the start of each line in region"
  (declare (ignore c))
  (ce-build-insert inp x))

(defun ce-command-join (&optional c)
  "join lines together, replacing newlines with spaces"
  (declare (ignore c))
  (ce-reset-input)
  (read-line)
  (ce-pipe-region
   (lambda (x) (list (format nil "~{~a~^ ~}" x)))))

(defun ce-command-num-print (&optional c)
  "print a region with line numbers"
  (declare (ignore c))
  (ce-reset-input)
  (read-line)
  (let ((mlen (list-length buffer)))
   (let ((in (ce-mod inpoint mlen)) (out (1+ (ce-mod outpoint mlen))))
    (loop for lin in (nthcdr in buffer) and i from in repeat (- out in) do
     (format t "~3d ~a~%" i lin)))))

(defun ce-command-print (&optional c)
  "print a region"
  (declare (ignore c))
  (ce-reset-input)
  (read-line)
  (let ((mlen (list-length buffer)))
   (if (not (= 0 mlen))
    (let ((in (ce-mod inpoint mlen)) (out (1+ (ce-mod outpoint mlen))))
     (format t "~{~a~%~}" (subseq buffer in out)))
    (format t errf))))

(defun ce-command-reg-replace (&optional c)
  "do a sed-like replacement"
  (declare (ignore c))
  (ce-reset-input)
  (let ((sep (read-char)))
   (if (char= #\Newline sep)
    (when (or (not query) (not ins) (not sfl))
     (format t errf)
     (return-from ce-command-reg-replace))
    (let ((pat (ce-tokens (read-line) sep)))
     (setq query (car pat))
     (setq ins (or (nth 1 pat) ""))
     (setq sfl (or (nth 2 pat) "")))))
  (let ((mlen (list-length buffer)))
   (let ((in (ce-mod inpoint mlen)) (out (1+ (ce-mod outpoint mlen))))
    (if (find #\g sfl)
     (loop for lin in (nthcdr in buffer) and i from in repeat (- out in) do
      (let ((res (pregexp-replace* query lin ins)))
       (ce-delete i i)
       (ce-push-line i res)))
     (loop for lin in (nthcdr in buffer) and i from in repeat (- out in) do
      (let ((res (pregexp-replace query lin ins)))
       (when (not (string= lin res))
	(ce-delete i i)
	(ce-push-line i res)
	(return))))))))

(defun ce-command-copy (c)
  "copy or move the region to argument line"
  (ce-reset-input)
  (let ((mlen (list-length buffer)))
   (let ((inp (1+ (ce-mod (parse-integer (read-line)) mlen)))
	 (in (ce-mod inpoint mlen))
	 (out (1+ (ce-mod outpoint mlen))))
    (let ((copy (subseq buffer in out))
	  (diff (1- (- out in))))
     (let ((ninp (if (char= c #\m)
		  (progn
		   (ce-delete in (1- out))
		   (if (< in inp) (1- (- inp diff)) inp))
		  inp)))
      (ce-push-lines ninp copy)
      (setq inpoint ninp)
      (setq outpoint (+ ninp diff)))))))

; TODO: needs error handling
(defun ce-command-write (&optional c)
  "write a file to disk"
  (declare (ignore c))
  (ce-reset-input)
  (let ((name (read-line)))
   (with-open-file (out (if (string= "" name) filename name)
			:direction :output
			:if-exists :new-version
			:if-does-not-exist :create)
    (format out "~{~a~%~}" buffer))))

(defmacro ce-build-chop (fin)
  "helper for chop and chop-beg"
  `(progn
    (ce-reset-input)
    (let ((inp (parse-integer (string-or (read-line) "1"))))
     (ce-apply-region (lambda (x)
		       (let ((llen (length x)))
			(if (< llen inp)
			 x ,fin)))))))

(defun ce-command-chop (&optional c)
  "chop off argument characters from end of each line in region"
  (declare (ignore c))
  (ce-build-chop (subseq x 0 (- llen inp))))

(defun ce-command-chop-beg (&optional c)
  "chop off argument characters from start of each line in region"
  (declare (ignore c))
  (ce-build-chop (subseq x inp)))

(defun ce-command-number (c)
  "input a number"
  (if (or (= 0 newpoint) (= 2 newpoint))
   (progn (setq newpoint (1+ newpoint)) (setq outpoint 0)))
  (setq outpoint (+ (* 10 outpoint) (digit-char-p c))))

