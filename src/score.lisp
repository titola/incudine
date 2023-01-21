;;; Copyright (c) 2013-2023 Tito Latini
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :incudine)

;;; A score file can contain time-tagged lisp functions, lisp statements,
;;; arbitrary score statements and lisp tags.
;;;
;;; The syntax of a time-tagged lisp function is:
;;;
;;;     start-time-in-beats [time-increment]* function-name [arg1] [arg2] ...
;;;
;;; The optional numbers between start-time-in-beats and function-name
;;; increment the start time. For example:
;;;
;;;     0.8          foo 220 .2
;;;     2.5 .15      foo 440 .5
;;;     3.2 .25 -.11 foo 432 .2
;;;
;;; is equivalent to
;;;
;;;     0.8              foo 220 .2
;;;     (+ 2.5 .15)      foo 440 .5
;;;     (+ 3.2 .25 -.11) foo 432 .2

(declaim (special
           ;; Stack used to check recursive inclusions of rego files.
           *include-rego-stack*
           *score-function-name*
           *score-local-function-name*
           *score-macros*
           *score-package*
           *score-start-time*
           *score-realtime*)
         (type list *include-rego-stack* *score-macros*)
         (type package *score-package*))

(defvar *score-radix* 10)
(declaim (type (integer 2 36) *score-radix*))

(defvar *score-float-format* incudine.config:*sample-type*)
(declaim (type (member single-float double-float short-float long-float rational)
               *score-float-format*))

(defvar *score-statements* (make-hash-table :test #'equal))
(declaim (type hash-table *score-statements*))

;;; Ignore a vertical line followed by a white space or a closed parenthesis.
;;; Note: also "#+TBLFM:" is ignored by default; it means that we can edit a
;;; spreadsheet in a rego file with Emacs in org-mode (or orgtbl-mode).
(defun vertical-line-reader (stream char)
  (declare (ignore char))
  (let ((c (peek-char nil stream t nil nil)))
    (if (member c '(#\Space #\Tab #\)))
        (values)
        (let (chars)
          (do ((c (read-char stream) (read-char stream)))
              ((char= c #\|)
               (intern (coerce (nreverse chars) 'string)))
            (push c chars))))))

(defvar *score-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (add-sharp-square-bracket-syntax)
    (set-macro-character #\| #'vertical-line-reader)
    *readtable*)
  "Readtable to read a rego file.")

(defun normalize-score-statement-name (name)
  (if (stringp name)
      (if (or (and (char= #\# (char name 0))
                   (char= #\+ (char name 1)))
              (char= #\: (char name 0) (char name (1- (length name)))))
          ;; Org syntax.
          (string-upcase name)
          name)
      (let ((name (symbol-name name)))
        (if (every (lambda (c)
                     (or (not (alpha-char-p c))
                         (upper-case-p c)))
                   name)
            (string-downcase name)
            name))))

(defun ignore-score-statements (name-list)
  "Ignore the score statements in NAME-LIST. The name of a statement is
a symbol, a string or a list of two score statements which delimit a
score block to ignore."
  (flet ((string-or-symbol-p (x)
           (or (stringp x) (symbolp x))))
    (assert (every (lambda (x)
                     (or (string-or-symbol-p x)
                         (and (consp x)
                              (= (length x) 2)
                              (string-or-symbol-p (first x))
                              (string-or-symbol-p (second x)))))
                   name-list))
    (dolist (i name-list name-list)
      (let ((name (typecase i
                    (symbol (symbol-name i))
                    (cons (normalize-score-statement-name (car i)))
                    (otherwise i))))
        (setf (gethash name *score-statements*)
              (if (consp i)
                  (cons :ignore (second i))
                  :ignore))))))

(defun score-statement-to-ignore-p (name)
  (declare (type string name))
  (let ((res (gethash (if (string-lessp "#+BEGIN_" name)
                          (string-upcase name)
                          name)
                      *score-statements*)))
    (unless (null res)
      (if (consp res)
          (values (eq (car res) :ignore) (cdr res))
          (values (eq res :ignore) nil)))))

(when (zerop (hash-table-count *score-statements*))
  ;; Score statements ignored by default, useful to edit a regofile
  ;; with Emacs in org-mode.
  ;;
  ;; Note: if a line starts with #+... and the word after sharp-plus
  ;; is not a feature in *FEATURES*, the score statement is ignored if
  ;; it is undefined. That rule is useful to ignore all the #+KEYWORDS
  ;; used in org-mode.
  (ignore-score-statements
    (append '("*" "**" "***" "****" "*****" "+" "-" "/" "!" "^" "$" "$$"
              "#" ":" "DEADLINE:" "SCHEDULED:" ("#+BEGIN:" "#+END:"))
            (mapcar (lambda (name)
                      (list (format nil "#+BEGIN_~A" name)
                            (format nil "#+END_~A" name)))
                    '("VERSE" "QUOTE" "CENTER" "COMMENT" "EXAMPLE" "SRC"
                      "LATEX" "ABSTRACT" "PROOF" "ASCII" "BEAMER" "HTML"
                      "VIDEO" "ODT" "TEXINFO")))))

(defmacro defscore-statement (name args &rest body)
  "Define an arbitrary score statement named NAME, formed by the
elements of the returned list.

The name of the score statement is a symbol or a string.
The symbol SomeName is equivalent to the string \"somename\".
The symbol |SomeName| is equivalent to the string \"SomeName\".

Example:

    (defscore-statement i1 (time dur freq amp)
      `(,time my-func (dur ,dur) ,freq ,amp))

where the Csound score statement

    i1 3.4 1.75 440 .3

will be expanded in a time tagged lisp function

    3.4 my-func (dur 1.75) 440 0.3"
  (multiple-value-bind (decl rest)
      (incudine.util::separate-declaration body)
    `(progn
       (setf (gethash (normalize-score-statement-name ',name) *score-statements*)
             (lambda ,args ,@decl
               (let* ((*print-pretty* nil)
                      (str (format nil "~S" (progn ,@rest)))
                      (len (length str)))
                 (if (< len 2)
                     ""
                     (subseq str 1 (1- len))))))
       ',name)))

(defun delete-score-statement (name)
  "Delete the score statement defined by DEFSCORE-STATEMENT
or IGNORE-SCORE-STATEMENTS."
  (remhash (normalize-score-statement-name name) *score-statements*))

;;; The score statement `:score-tempo:' is an alternative to the local
;;; macro TEMPO:
;;;
;;;     :score-tempo: bpm
;;;     :score-tempo: bpms beats &key curve loop-node [...]
;;;
;;; Note: the name is surrounded by colons, so it is also a valid
;;; property in Org markup language.
(defscore-statement ":score-tempo:" (&rest args)
  `((,(find-symbol "TEMPO") ,@args)))

;;; The score statement `:score-time:' sets the time offset in beats.
;;; For example:
;;;
;;;     :PROPERTIES:
;;;     :score-time: 8
;;;     :END:
;;;
;;; is equivalent to
;;;
;;;     (setf time 8)
;;;
(defscore-statement ":score-time:" (value)
  `((setf time ,value)))

;;; The score statement `:score-realtime-offset:' sets the absolute
;;; time offset in samples for the scheduled score events.
(defscore-statement ":score-realtime-offset:" (value-form)
  `((unless (incudine::nrt-edf-heap-p)
      (setf incudine::score-realtime-offset (lambda () ,value-form)))))

(declaim (inline next-blank-position))
(defun next-blank-position (string)
  (position-if #'blank-char-p string))

(defun score-statement-name (str)
  (let ((name-endpos (next-blank-position str)))
    (values (subseq str 0 name-endpos) name-endpos)))

(declaim (inline score-statement-args))
(defun score-statement-args (str name-endpos)
  (read-from-string
    (concatenate 'string "(" (subseq str name-endpos) ")")))

(declaim (inline score-statement-args-string))
(defun score-statement-args-string (str name-endpos)
  (string-left-trim '(#\Space #\Tab) (subseq str name-endpos)))

(defun skip-org-table-char (str)
  (multiple-value-bind (name name-endpos)
      (score-statement-name str)
    (if (and name-endpos
             (member name '("#" "*") :test #'string=))
        ;; Skip special marking character.
        (score-statement-args-string str name-endpos)
        str)))

(defun expand-score-statement (str)
  (declare (type string str))
  (multiple-value-bind (name name-endpos)
      (score-statement-name str)
    (when name
      (if (string= name "|")
          (when name-endpos
            (expand-score-statement
              (skip-org-table-char
                (score-statement-args-string str name-endpos))))
          (let ((fn (gethash (normalize-score-statement-name name)
                             *score-statements*)))
            (declare (type (or function symbol cons) fn))
            (when (functionp fn)
              (apply fn (when name-endpos
                          (let ((*readtable* *score-readtable*))
                            (score-statement-args str name-endpos))))))))))

(declaim (inline blank-char-p))
(defun blank-char-p (c)
  (member c '(#\Space #\Tab) :test #'char=))

(declaim (inline string-trim-blank))
(defun string-trim-blank (string)
  (string-trim '(#\Space #\Tab #\Return) string))

(defun line-parse-skip-string (str index end)
  (declare (type string str) (type non-negative-fixnum index end))
  (labels ((skip-p (i)
             (if (>= i end)
                 i
                 (case (char str i)
                   (#\" i)
                   (#\\ (skip-p (+ i 2)))
                   (otherwise (skip-p (1+ i)))))))
    (skip-p (1+ index))))

(defun %time-tagged-function-p (s)
  (declare (type string s))
  (let* ((slen (length s))
         (last (1- slen)))
    (declare (type non-negative-fixnum slen last))
    (or (char/= (char s last) #\))
        (labels ((stmt-p (i unmatched-parens)
                   (when (< i slen)
                     (if (zerop unmatched-parens)
                         (find-if-not #'blank-char-p s :start i)
                         (case (char s i)
                           (#\" (stmt-p (line-parse-skip-string s i slen)
                                        unmatched-parens))
                           (#\) (stmt-p (1+ i) (1- unmatched-parens)))
                           (#\( (stmt-p (1+ i) (1+ unmatched-parens)))
                           (otherwise (stmt-p (1+ i) unmatched-parens)))))))
          (stmt-p 1 1)))))

(defun time-tagged-function-p (string)
  (declare (type string string))
  (if (char= (char string 0) #\()
      (%time-tagged-function-p string)
      (let ((space-pos (next-blank-position string)))
        (declare (type (or non-negative-fixnum null) space-pos))
        (when space-pos
          (find-if-not #'blank-char-p string :start space-pos)))))

(defun sharp-plus-to-skip (line)
  (and (char= (char line 0) #\#)
       (> (length line) 2)
       (char= (char line 1) #\+)
       (char/= (char line 2) #\()
       (not (score-macro-block-p line))
       (let ((name (string-upcase
                     (subseq line 2 (next-blank-position line)))))
         (and (null (gethash name *score-statements*))
              (null (find name *features* :key #'symbol-name
                          :test #'string=))))))

(defun ignore-score-statement-with-colon-p (name &optional (ignore-bindings-p t))
  (declare (type string name))
  (let ((colon-position
          ;; A keyword is not ignored because it could be a label.
          (position #\: name :start 1)))
    (and colon-position
         (not (symbol-name-p name colon-position))
         (or ignore-bindings-p
             (string-not-equal name ":score-bindings:"))
         (or (and (string-equal name ":score-realtime-offset:")
                  (or (included-regofile-p)
                      (not (score-realtime-p))))
             (not (or (gethash (string-upcase name) *score-statements*)
                      (score-property-statement-p name)))))))

(defun symbol-name-p (statement-name colon-position)
  (when (find-package (string-upcase (subseq statement-name 0 colon-position)))
    (let* ((start (1+ colon-position))
           (n (count #\: (subseq statement-name start))))
      (or (= n 0)
          (and (= n 1)
               (char= #\: (char statement-name start)))))))

(defun score-property-statement-p (name)
  (let ((prefix-test (string-greaterp name ":score-")))
    (when prefix-test
      (let ((prefix-length #.(length ":score-")))
        (and (= prefix-test prefix-length)
             (member (subseq name prefix-length)
                     '("package:" "float-format:" "radix:" "start-time:"
                       "function-name:" "local-function-name:")
                     :test #'string-equal)
             t)))))

(declaim (inline org-table-line-p))
(defun org-table-line-p (string)
  (char= (char string 0) #\|))

(defun org-table-left-trim (line)
  (string-left-trim '(#\| #\Space #\Tab) line))

(defun org-table-item-position (line)
  (or (position-if-not
        (lambda (x) (member x '(#\| #\Space #\Tab) :test #'char=))
        line)
      0))

(defun org-table-mark (line)
  (let ((pos (position #\| (subseq line 1))))
    (when pos
      (let ((str (string-trim-blank (subseq line 1 pos))))
        (and (= (length str) 1) (char str 0))))))

(defun org-table-line-to-skip-p (line)
  (when (org-table-line-p line)
    (or ;; Horizontal separator.
        (every (lambda (c) (member c '(#\+ #\- #\|))) line)
        ;; Empty.
        (every (lambda (c) (member c '(#\| #\Space #\Tab))) line)
        ;; Marking characters to ignore.
        (member (org-table-mark line) '(#\! #\^ #\_ #\$ #\/)))))

(defun org-table-filter (line)
  (if (org-table-line-p line)
      (let ((c (car (member (org-table-mark line) '(#\# #\*)))))
        (if c
            (subseq line (1+ (position c line)))  ; Ignore org-table-mark
            line))
      line))

(defun ignore-score-statement (name stream &optional (ignore-bindings-p t))
  (declare (type string name) (type stream stream))
  (or (multiple-value-bind (ignore-p block-end)
          (score-statement-to-ignore-p name)
        (when ignore-p
          (when block-end
            (do ((line (read-line stream) (read-line stream)))
                ((string-equal
                   (score-statement-name (string-trim-blank line))
                   block-end))))
          t))
      (ignore-score-statement-with-colon-p name ignore-bindings-p)))

(defun score-skip-line-p (line stream &optional (ignore-bindings-p t))
  (declare (type string line) (type stream stream))
  (let ((str (string-trim-blank line)))
    (or (zerop (length str))
        (let ((c (char str 0)))
          (and (char/= c #\()
               (or (char= c #\;)
                   (ignore-score-statement (score-statement-name str) stream
                                           ignore-bindings-p)
                   (org-table-line-to-skip-p str)
                   (sharp-plus-to-skip str)))))))

(defun shebang-line-p (line)
  (and (> (length line) 2)
       (char= #\# (char line 0))
       (char= #\! (char line 1))))

(defmacro %at-sample (at-fname beats &optional func-symbol &rest args)
  `(,at-fname ,beats ,func-symbol ,@args))

(defun %score-statement-p (line name &optional (string-test #'string=) (start 0))
  (let ((len1 (- (length line) start))
        (len2 (length name)))
    (cond ((> len1 len2)
           (let ((end (+ start len2)))
             (and (blank-char-p (char line end))
                  (funcall string-test line name :start1 start :end1 end))))
          ((= len1 len2)
           (funcall string-test line name :start1 start :end1 (+ start len2))))))

(defun score-realtime-p () *score-realtime*)

;;; Score statement used to include the content of another rego file:
;;;
;;;     include "regofile" [time]
;;;
(defun include-regofile-p (line)
  (%score-statement-p line "include" #'string=
    (if (org-table-line-p line) (org-table-item-position line) 0)))

(defun include-rego-args (line)
  (declare (type string line))
  (let ((*package* *score-package*)
        (*read-base* *score-radix*)
        (*read-default-float-format* *score-float-format*)
        (*readtable* *score-readtable*))
    (destructuring-bind (file &optional time &rest args)
        (rest (read-from-string (concatenate 'string "(" line ")") nil))
      (values file time args))))

(defun included-regofile-p ()
  (and (cdr *include-rego-stack*) t))

;;; Score macro block:
;;;
;;;     #+begin_macro name
;;;     ...
;;;     #+end_macro
;;;
(defun score-macro-block-p (line)
  (%score-statement-p line "#+begin_macro" #'string-equal))

(defun score-end-macro-block-p (line)
  (%score-statement-p line "#+end_macro" #'string-equal))

(defun read-score-macro-block (stream line)
  (let ((name (read-from-string (subseq line (next-blank-position line)))))
    (when (and name (symbolp name))
      (when (assoc name *score-macros*)
        (incudine-error "A score macro named ~S already exists" name))
      (push (cons name
                  (with-output-to-string (str)
                    (loop for line = (read-line stream)
                          with count = 0 do
                            (cond ((score-end-macro-block-p line)
                                   (if (= count 0)
                                       (return)
                                       (decf count)))
                                  ((score-macro-block-p line)
                                   (incf count)))
                            (write-line line str))))
            *score-macros*)
      name)))

(defun score-macro-statement-p (line)
  (when *score-macros*
    (let ((start (if (org-table-line-p line) (org-table-item-position line) 0)))
      (some (lambda (x)
              (let* ((name (symbol-name (car x)))
                     (len (string-not-lessp line name :start1 start)))
                (and len
                     (= (- len start) (length name))
                     (or (= (length line) (length name))
                         (blank-char-p (char line len)))
                     t)))
            *score-macros*))))

(defun score-macro-name-p (obj)
  (and (assoc obj *score-macros* :test #'string-equal) t))

(defun score-macro-string (name)
  (cdr (assoc name *score-macros*)))

(defmacro include-regofile (name write-args)
  (with-gensyms (score args)
    `(flet ((inc (,score ,args)
              (let ((*score-package* *score-package*)
                    (*score-radix* *score-radix*)
                    (*score-float-format* *score-float-format*))
                (apply #'%write-regofile ,score ,args))))
       (let ((,args ,write-args))
         (if (symbolp ,name)
             (with-input-from-string (,score (score-macro-string ,name))
               (inc ,score ,args))
             (with-open-file (,score ,name) (inc ,score ,args)))))))

;;; The score statement `call' pushes the return position on the stack
;;; and transfers program control to the point labeled by a tag.
;;; The score statement `return' transfers control to the return position
;;; located on the top of the stack.
;;;
;;; Syntax for call statement (a tag between [[ ]] is a facility for Org mode):
;;;
;;;     call tag
;;;     call tag time
;;;     call [[tag]] time                ; the target label is <<tag>>
;;;     call [[tag][description]] time   ; the target label is <<tag>>
;;;
;;; Example:
;;;
;;;     call p1 0
;;;     call [[p2][pattern two]] 1
;;;     call p3 1.5
;;;     call p1 2
;;;     return              ; end of score
;;;
;;;     p1
;;;     0 write-line "pattern 1" // force-output
;;;     call p3 .1
;;;     call p3 .25
;;;     return
;;;
;;;     <<p2>>
;;;     0 write-line "pattern 2" // force-output
;;;     return
;;;
;;;     p3
;;;     0 write-line "pattern 3" // force-output
;;;     return
;;;
(defun score-call-statement-p (line)
  (%score-statement-p line "call" #'string=
    (if (org-table-line-p line) (org-table-item-position line) 0)))

(defun score-call-org-internal-link (line)
  (let* ((l (string-trim "[]" line))
         (end (position #\] l))
         (time-start-pos (position #\] line :from-end t)))
    (format nil "<<~A>>~@[ ~A~]"
            (if end (subseq l 0 end) l)
            (and time-start-pos
                 (subseq line (1+ time-start-pos))))))

(defun score-call-arguments (line)
  (let* ((label-start (org-table-left-trim
                        (subseq line (+ (position #\l line) 2))))
         (*package* *score-package*)
         (*read-base* *score-radix*)
         (*readtable* *score-readtable*)
         (args (read-from-string
                 (format nil "(~A)"
                         (if (char= (char label-start 0) #\[)
                             (score-call-org-internal-link label-start)
                             label-start)))))
    (if (<= 1 (length args) 2)
        (values-list args)
        (msg error "malformed call statement: ~S" line))))

(defmacro score-jump (label ret-label time)
  (let ((time (if (and (numberp time) (= time 0)) nil time)))
    `(progn
       (push ,(if time
                  `(let ((t0 time))
                     (lambda () (setf time t0) (go ,ret-label)))
                  `(lambda () (go ,ret-label)))
             __stack__)
       ,@(and time `((incf time ,time)))
       (go ,label))))

(defun expand-score-call-statement (line)
  (multiple-value-bind (label time) (score-call-arguments line)
    (when label
      (let ((ret-label (gensym "CONTINUE")))
        `((score-jump ,label ,ret-label ,time) ,ret-label)))))

(defun score-return-statement-p (line)
  (let ((pos (string<= "return" line)))
    (and pos (= 6 pos) (= 6 (length (string-trim-blank line))))))

;;; The score statement `:score-package:' sets the name of the package
;;; used to read the rest of the score lines.
(defun score-package-statement-p (line)
  (%score-statement-p line ":score-package:"))

(defun set-score-package (line)
  (let* ((name (read-from-string (subseq line (next-blank-position line))))
         (pkg (find-package name)))
    (if pkg
        (setf *score-package* pkg)
        (error 'package-error :package name))))

;;; The score statement `:score-float-format:' sets the variable
;;; *READ-DEFAULT-FLOAT-FORMAT* to read the rest of the score lines.
;;; The default is double-float (the sample type) if there is not a
;;; parent rego file.
(defun score-float-format-statement-p (line)
  (%score-statement-p line ":score-float-format:"))

(defun set-score-float-format (line)
  (setf *score-float-format*
        (read-from-string (subseq line (next-blank-position line)))))

;;; The score statement `:score-radix:' sets the variable *READ-BASE*
;;; to read the rest of the score lines.
;;; The default is 10 if there is not a parent rego file.
(defun score-radix-statement-p (line)
  (%score-statement-p line ":score-radix:"))

(defun set-score-radix (line)
  (setf *score-radix*
        (read-from-string (subseq line (next-blank-position line)))))

;;; The score statement `:score-start-time:' sets the start time in beats.
;;; The default is zero.
(defun score-start-time-statement-p (line)
  (%score-statement-p line ":score-start-time:"))

(defun set-score-start-time (line)
  (unless (included-regofile-p)
    (let ((time (read-from-string (subseq line (next-blank-position line)))))
      (when (and (realp time) (> time 0.0))
        (setf *score-start-time* time)))))

;;; The score statement `:score-function-name:' sets the name of the
;;; function defined with REGOFILE->FUNCTION.
;;; The score statement `:score-local-function-name:' sets the name
;;; of a recursive local function.
(defun score-function-name-statement-p (line)
  (or (%score-statement-p line ":score-function-name:")
      (%score-statement-p line ":score-local-function-name:")))

(defun set-score-function-name (line)
  (unless (included-regofile-p)
    (let ((name (read-from-string (subseq line (next-blank-position line)))))
      (when (symbolp name)
        (if (%score-statement-p line ":score-local-function-name:")
            (setf *score-local-function-name* name)
            (setf *score-function-name* name))))))

;;; If we use the symbol // to separate the functions with the same
;;; time-tag, we get a polyphonic vertical sequencer in text files.
;;; A quoted function name is ignored; useful to mute an instrument.
;;;
;;; For example:
;;;
;;;     2.5 foo 440 .08 // bar 550 .1 // 'baz 660 .05 // sev 770 .1
;;;     3.2                           //  baz 330 .03
;;;     4.5 foo 220 .02                               // sev 772 .07
;;;
;;; is equivalent to
;;;
;;;     2.5 foo 440 .08
;;;     2.5 bar 550 .1
;;;     2.5 sev 770 .1
;;;     3.2 baz 330 .03
;;;     4.5 foo 220 .02
;;;     4.5 sev 772 .07
;;;
;;; Example with delay-time:
;;;
;;;     0 .11 i1 1 2 3 // .25 i2 1 2 3 // .05 i3 1 2 3
;;;     1     i1 1 2 3 // .05 i2 1 2 3 // .36 i3 1 2 3
;;;
;;; is equivalent to
;;;
;;;     0.05 i3 1 2 3
;;;     0.11 i1 1 2 3
;;;     0.25 i2 1 2 3
;;;     1.00 i1 1 2 3
;;;     1.05 i2 1 2 3
;;;     1.36 i3 1 2 3
(defun score-expand-parallel-functions (form args)
  (labels ((ignore-func-p (fname)
             (or (not fname)
                 (eq fname 'quote)
                 (and (consp fname) (eq (car fname) 'quote))))
           (function-position (list i delimiter-position)
             (if list
                 (let ((delim (if (eq (car list) '//) i)))
                   (if (and delimiter-position (not delim))
                       (values i delimiter-position)
                       (function-position (cdr list) (1+ i)
                                          (or delimiter-position delim))))
                 (values nil delimiter-position)))
           (next (at-fname time form)
             (multiple-value-bind (func-pos delim-pos)
                 (function-position form 0 nil)
               (let ((next (when func-pos
                             (next at-fname time (subseq form func-pos)))))
                 (if (ignore-func-p (find-if-not #'numberp form))
                     next
                     (cons (maybe-expand-score-statement-form
                             (normalize-score-statement-form
                               (list* at-fname time
                                      (if delim-pos
                                          (subseq form 0 delim-pos)
                                          form)))
                             args)
                           next))))))
    (let ((pos (position '// form)))
      (cond ((null pos)
             (if (ignore-func-p (third form))
                 ''nil
                 (maybe-expand-score-statement-form
                   (normalize-score-statement-form form) args)))
            ((= pos 2)
             (let ((func-pos (function-position form 0 nil)))
               (if func-pos
                   (score-expand-parallel-functions
                     (list* (first form) (second form) (subseq form func-pos))
                     args)
                   ''nil)))
            (t
             (with-gensyms (time)
               `(let ((,time ,(cadr form)))
                  ,@(next (car form) time (cddr form)))))))))

(defun normalize-score-statement-form (form)
  (if (numberp (third form))
      ;; start-time delay [time-increment]* function-name [arg1] [arg2] ...
      (let ((func-pos (+ 2 (or (position-if-not #'numberp (cddr form)) 1))))
        (list* (first form)
               `(+ ,@(subseq form 1 func-pos))
               (subseq form func-pos)))
      form))

(defun maybe-expand-score-statement-form (form args)
  (let ((name (third form)))
    (if (or (stringp name) (score-macro-name-p name))
        ;; Time-tagged score macro or included regofile.
        (destructuring-bind (at time name &rest rest) form
          (score-line->sexp (list name time rest) at args))
        form)))

(defun score-line->sexp (line at-fname args)
  (declare (type (or string cons) line) (type list args))
  (if (or (consp line) (include-regofile-p line))
      (multiple-value-bind (name time include-args)
          (if (consp line)
              (values-list line)
              (include-rego-args line))
        (let* ((score-macro-p (symbolp name))
               (name (if score-macro-p
                         name
                         (incudine.util::truename*
                           (merge-pathnames name
                             (or (car args) *default-pathname-defaults*))))))
          (cond ((find name *include-rego-stack* :test #'equal)
                 (msg error "recursive inclusion of ~S~%  => ~A" name
                      (reverse *include-rego-stack*)))
                (t (push name *include-rego-stack*)
                   (include-regofile name
                     `(,at-fname ,@(cdr args) t ,include-args ,time))))))
      (let ((line (or (expand-score-statement line) (org-table-filter line)))
            (*package* *score-package*)
            (*readtable* *score-readtable*)
            (*read-base* *score-radix*)
            (*read-default-float-format* *score-float-format*))
        (declare (type string line))
        (cond ((score-return-statement-p line)
               '(funcall (pop __stack__)))
              ((score-macro-statement-p line)
               (score-line->sexp
                 (concatenate 'string "include " line) at-fname args))
              ((time-tagged-function-p line)
               (score-expand-parallel-functions
                 (macroexpand-1
                   (read-from-string
                     (format nil "(INCUDINE::%AT-SAMPLE ~S ~A)" at-fname line)))
                 args))
              (t
               ;; Tag or lisp statement.
               (let ((form (read-from-string
                            (string-left-trim '(#\Space #\Tab #\Return) line))))
                 (if (numberp form)
                     ;; A number is not a lisp tag otherwise a time-tagged
                     ;; function gets confused.
                     ;;
                     ;; PROGN + time value for debug.
                     `(progn ,form)
                     form)))))))

(defun read-time-score-statement-p (line)
  (or (score-call-statement-p line)
      (score-macro-block-p line)
      (and (char= #\: (char line 0))
           (or (score-start-time-statement-p line)
               (score-package-statement-p line)
               (score-function-name-statement-p line)
               (score-radix-statement-p line)
               (score-float-format-statement-p line)))))

(defun expand-read-time-score-statement (stream line)
  (cond ((score-call-statement-p line)
         (expand-score-call-statement line))
        ((score-start-time-statement-p line)
         (set-score-start-time line)
         nil)
        ((score-package-statement-p line)
         (set-score-package line)
         nil)
        ((score-function-name-statement-p line)
         (set-score-function-name line)
         nil)
        ((score-macro-block-p line)
         (read-score-macro-block stream line)
         nil)
        ((score-radix-statement-p line)
         (set-score-radix line)
         nil)
        ((score-float-format-statement-p line)
         (set-score-float-format line)
         nil)))

(defun score-lines->sexp (stream at-fname args)
  (declare (type stream stream) (type list args))
  (loop for line of-type (or string null)
                 = (read-score-line stream)
        until (end-of-score-p line)
        unless (score-skip-line-p line stream)
        if (read-time-score-statement-p line)
          append (expand-read-time-score-statement stream line)
        else collect (score-line->sexp line at-fname args)))

(defun end-of-score-p (line)
  (or (null line)
      (and (string/= line "")
           (char= (char line 0) #\Page))))

(defun find-score-local-bindings (stream at args)
  (declare (type stream stream) (type symbol at))
  (labels ((score-bindings-p (line)
             (or (%score-statement-p line "with" #'string-equal)
                 ;; The score statement `:score-bindings:' is an alias of WITH.
                 (%score-statement-p line ":score-bindings:" #'string-equal)))
           (get-bindings (line)
             (mapcar (lambda (x) (if (consp x) x (list x nil)))
               (let ((*package* *score-package*))
                 (read-from-string
                   (concatenate 'string "("
                     (subseq line (if (char= #\: (char line 0))
                                      #.(1+ (length ":score-bindings:"))
                                      #.(1+ (length "with"))))
                     ")")))))
           (first-score-stmt (line bindings declarations nl)
             (declare (type (or string null) line)
                      (type list bindings declarations))
             (when line
               (cond ((or (and (= nl 1) (shebang-line-p line))
                          (score-skip-line-p line stream nil)
                          (score-package-statement-p line))
                      (when (score-package-statement-p line)
                        (set-score-package line))
                      (first-score-stmt
                        (read-score-line stream) bindings declarations (1+ nl)))
                     ((score-bindings-p line)
                      (first-score-stmt
                        (read-score-line stream)
                        ;; Local bindings at the beginning of the score.
                        (get-bindings line)
                        nil (1+ nl)))
                     ((read-time-score-statement-p line)
                      (values bindings (nreverse declarations)
                              (expand-read-time-score-statement stream line)))
                     (t
                      (let ((form (score-line->sexp line at args)))
                        (if (and (consp form)
                                 (eq (car form) 'declare))
                            (first-score-stmt
                              (read-score-line stream)
                              bindings (cons form declarations) (1+ nl))
                            (values bindings (nreverse declarations)
                                    (list form)))))))))
    (first-score-stmt (read-score-line stream) nil nil 1)))

(defun read-score-line (stream)
  (declare (type stream stream))
  (flet ((remove-comment (str)
           ;; A comment starts with `;'
           (subseq str 0 (position #\; str)))
         (line-break-p (str strlen)
           ;; Line continuation with `\' at the end
           (and (> strlen 1)
                (char=  (char str (- strlen 1)) #\\)
                (char/= (char str (- strlen 2)) #\\))))
    (let ((line (read-line stream nil nil)))
      (declare (type (or string null) line))
      (when line
        (let* ((s (remove-comment (string-trim-blank line)))
               (slen (length s)))
          (declare (type non-negative-fixnum slen))
          (if (line-break-p s slen)
              (concatenate 'string
                           (subseq s 0 (- slen 2))
                           " "
                           (read-score-line stream))
              s))))))

;;; Extend the last time if there is a pending event.
;;; Note: the duration of an event is known only if it uses the local
;;; macro DUR (see REGOFILE->SEXP).
(defmacro maybe-extend-time (now max-time tempo-env)
  (with-gensyms (to-free)
    ``(progn
        ;; Check if there is a pending event after the last event.
        (setf ,,now (* (+ (now) (incudine.edf:last-time)) *sample-duration*))
        (at (+ (* ,,now  *sample-rate*) 0.5)
          (lambda (,',to-free)
            (flet ((end-of-rego (&optional arg)
                     (declare (ignorable arg))
                     (free ,',to-free)
                     (nrt-msg info "end of rego")))
              (cond
                ((and (plusp ,,max-time)
                      (plusp (setf ,,max-time
                                   (- (beats->seconds ,,tempo-env ,,max-time)
                                      (beats->seconds ,,tempo-env ,,now)))))
                 ;; End after the pending event.
                 (at (+ (now) (* ,,max-time *sample-rate*))
                     #'end-of-rego))
                ((rt-thread-p) (nrt-funcall #'end-of-rego))
                (t (end-of-rego)))))
          incudine::*to-free*))))

(defun advance-score-start-time (start-time)
  (do ((i incudine.edf:+root-node+))
      ((>= i (incudine.edf::heap-next-node incudine.edf:*heap*)))
    (declare (type positive-fixnum i))
    (let ((node (incudine.edf::heap-node i)))
      (cond ((>= (incudine.edf::node-time node) start-time)
             (decf (incudine.edf::node-time node) start-time)
             (incf i))
            ((incudine.edf::force-scheduled-event-p node)
             (setf (incudine.edf::node-time node) (sample 0))
             (incf i))
            (t
             (incudine.edf::delete-event i)
             ;; Restart from the beginning of the current tree-level.
             (setf i (1- (integer-length (logior i 2)))))))))

(defun maybe-advance-score-start-time (tempo-env max-time)
  (when (> *score-start-time* 0.0)
    (with-gensyms (start-time)
      `(let ((,start-time (* *sample-rate*
                             (beats->seconds ,tempo-env ,*score-start-time*))))
         (decf ,max-time ,start-time)
         (advance-score-start-time ,start-time)))))

(defun get-regolist (r-events tempo-env beats)
  (let ((events (sort (nreverse r-events) '< :key 'first)))
    (if beats
        (let ((start-time (beats->seconds tempo-env beats)))
          (loop for (time func) in events
             if (>= time start-time)
               collect (let ((new-time (- time start-time)))
                         (cons new-time (funcall func new-time)))))
        (mapcar (lambda (l)
                  (cons (first l) (funcall (second l) (first l))))
                events))))

(defun default-tempo-envelope ()
  (make-tempo-envelope (list *default-bpm* *default-bpm*) '(0)))

;;; Symbols with complex names used in the code generated by a rego file.
;;; *PRINT-GENSYM* is NIL in REGOFILE->LISPFILE
(defmacro with-complex-gensyms (names &body forms)
  `(let ,(mapcar (lambda (name)
                   `(,name (gensym ,(format nil "__~A__" name))))
                 names)
     ,@forms))

(defun ensure-complex-gensym (name)
  (ensure-symbol (symbol-name (gensym (format nil "__~A__" (string name))))))

(defun interpret-score (score-args local-fname body)
  (multiple-value-bind (local-function local-macro)
      (when local-fname
        (values `((,local-fname *score-local-function*))
                `((,local-fname (&rest args)
                     `(funcall ,,local-fname ,@args)))))
    `((let ((*score-args* ,score-args)
            (*score-local-function* ,(if local-fname `(function ,local-fname))))
        ;; `(lambda ... (eval form))' because the function
        ;; `(eval (quote (lambda ...)))' is compilable.
        (declare (special *score-args* *score-local-function*))
        (incudine.util::cudo-eval
          (quote (let ((,score-args *score-args*)
                       ,@local-function)
                   (macrolet (,@local-macro) ,@body))))))))

(defmacro with-rego-function ((fname stmt-fname local-fname compile-rego-p)
                              &body body)
  (let* ((score-args (ensure-symbol "SCORE-ARGS"))
         (body (cons `(declare (ignorable ,score-args)) body)))
    (multiple-value-bind (maybe-compile expr)
        (if compile-rego-p
            (values `(compile nil) body)
            (values '(progn)
                    (interpret-score score-args local-fname body)))
      (let ((args-body `((&rest ,score-args) ,@expr)))
        (flet ((set-function-if (name &optional other-name)
                 (if name
                     `(setf (symbol-function ',(or other-name name)))
                     '(progn))))
          `(,@maybe-compile
            ,(cond
               (local-fname
                `(labels ((,local-fname ,@args-body))
                   (,@(set-function-if fname)
                      (,@(set-function-if stmt-fname)
                         (function ,local-fname)))))
               ((or fname stmt-fname)
                `(progn (defun ,(or fname stmt-fname) ,@args-body)
                        (,@(set-function-if (and fname stmt-fname) stmt-fname)
                           (function ,(or fname stmt-fname)))))
               (t `(lambda ,@args-body)))))))))

;;; Foreign memory to reduce consing.
(defmacro with-rego-samples ((wrapper smptime-var smptime0-var time-var
                              sched-var last-time-var last-dur-var max-time-var)
                             &body body)
  (with-complex-gensyms (names)
    (let ((var-names (list smptime-var smptime0-var time-var sched-var
                           last-time-var last-dur-var max-time-var)))
      `(let* ((incudine::*to-free* nil)
              (,wrapper
                 (make-foreign-array ,(length var-names) 'sample :zero-p t))
              (,names (foreign-array-data ,wrapper)))
         (symbol-macrolet ,(loop for var in var-names for i from 0
                                 collect `(,var (smp-ref ,names ,i)))
           (setf ,smptime-var
                 (if (incudine::nrt-edf-heap-p) (now) +sample-zero+))
           (setf ,smptime0-var ,smptime-var)
           ,@body)))))

(defun rego-local-tempo (vars declarations body time time-offset
                         time-offset-var tempo-env)
  (let ((stack-bind '(__stack__ nil))
        (time-bind `((__parent-time__ ,time)
                     (__parent-smptime0__ __smptime0__)
                     (__parent-smptime__ __smptime__)))
        (time-os-bind `(,time-offset-var ,(or time-offset +sample-zero+)))
        (tenv-bind `(,tempo-env ,tempo-env))
        (local-tempo `(progn
                        ,@(when time-offset
                            `((incf __smptime__
                                    (* *sample-rate*
                                       (%beats->seconds
                                         ,tempo-env ,time-offset-var)))))
                        (setf __smptime0__ __smptime__)
                        (setf __smptime1__ +sample-zero+)
                        (setf ,tempo-env (copy-tempo-envelope ,tempo-env))))
        (decl `((declare (ignorable ,time-offset-var)) ,@declarations))
        (init-stack '(push (lambda () (go __end_of_score__)) __stack__)))
    `((,stack-bind ,@time-bind ,time-os-bind ,tenv-bind ,@vars)
      ,@decl ,init-stack ,local-tempo ,@body)))

(defun %write-regofile (score at-fname time-var dur-var max-time tenv
                        &optional included-p include-args time-offset
                        (extend-time-p t))
  `(prog*
     ,@(progn
         (unless included-p
           (push (if (file-name score)
                     (incudine.util::truename* score)
                     score)
                 *include-rego-stack*))
         (with-ensure-symbols (time tempo-env)
           (let ((write-args (list (and (file-name score)
                                        (directory-namestring score))
                                   time-var dur-var max-time tenv)))
             (append
              (multiple-value-bind (vars decl body)
                  (find-score-local-bindings score at-fname write-args)
                (let ((stack-bind '(__stack__ nil))
                      (init-stack
                        '(push (lambda () (go __end_of_score__)) __stack__)))
                  (if included-p
                      (with-gensyms (time-offset-var)
                        (rego-local-tempo
                          (cons 'score-realtime-offset
                                (included-regofile-bindings vars include-args))
                          (cons '(declare (ignore score-realtime-offset)) decl)
                          body time time-offset time-offset-var tempo-env))
                      `(,(cons stack-bind vars) ,@decl ,init-stack ,@body))))
              (score-lines->sexp score at-fname write-args)
              '(__end_of_score__)
              (cond (included-p
                     ;; End of the included regofile or score macro expansion.
                     (pop *include-rego-stack*)
                     `((setf __time__ __parent-time__)
                       (setf __smptime0__ __parent-smptime0__)
                       (setf __smptime__ __parent-smptime__)))
                    (extend-time-p
                     (list
                      (maybe-advance-score-start-time tenv max-time)
                      (maybe-extend-time at-fname max-time tenv))))))))))

(defun included-regofile-bindings (bindings args)
  (let ((args (loop for x on args
                    for y on bindings
                    if (or (atom (car x))
                           (cddar x)
                           (eq (caar x) 'quote))
                      collect (list (caar y) (car x))
                    else append x and do (setf x nil)
                    unless (cdr y) append (cdr x)))
        (acc nil))
    ;; The new bindings precede the score-bindings of the included
    ;; file, therefore a score binding can optionally refer to some
    ;; arguments in ARGS.
    (dolist (x args)
      (unless (assoc (car x) bindings) (push x acc)))
    (dolist (x bindings (nreverse acc))
      (push (or (assoc (car x) args) x) acc))))

(define-constant +rego-time-index+ 0)
(define-constant +rego-time-offset-index+ 1)
(define-constant +rego-beats-index+ 2)

(declaim (inline rego-time))
(defun rego-time (time-ptr tempo-env)
  (declare (ignore tempo-env))
  (smp-ref time-ptr +rego-beats-index+))

(defun set-rego-time (time-ptr tempo-env value)
  (declare (type foreign-pointer time-ptr) (type tempo-envelope tempo-env)
           (type real value))
  (let ((value (max 0 value)))
    (setf (smp-ref time-ptr +rego-time-index+)
          (+ (smp-ref time-ptr +rego-time-offset-index+)
             (* *sample-rate* (%beats->seconds tempo-env value))))
    (setf (smp-ref time-ptr +rego-beats-index+) (sample value))
    value))

(defsetf rego-time set-rego-time)

;;; Local variables usable inside the rego file:
;;;
;;;     TIME          time offset in beats
;;;     TEMPO-ENV     temporal envelope of the events
;;;
;;; It is possible to define other local variables by inserting
;;; the bindings after WITH, at the beginning of the score.
;;; For example:
;;;
;;;     ;;; test.rego
;;;     with (id 1) (last 4)
;;;
;;;     ;; simple oscillators
;;;     0          simple 440 .2 :id id
;;;     1          simple 448 .2 :id (+ id 1)
;;;     (1- last)  simple 661 .2 :id (+ id 2)
;;;     last       free 0
;;;
;;; We can also add a DECLARE expression after the bindings.
;;;
;;; DUR is a local macro to convert the duration from
;;; beats to seconds with respect to TEMPO-ENV.
;;;
;;; TEMPO is a local macro to change the tempo of the score.
;;; The syntax is
;;;
;;;     (tempo bpm)
;;;     (tempo bpms beats &key curve loop-node release-node
;;;                            restart-level real-time-p)
;;;
;;; The syntax to include the content of an external regofile is:
;;;
;;;     include "regofile" [time]
;;;
;;; where `time' is an optional time offset in beats.
;;; TIME and TEMPO-ENV are a parent's copy within an included regofile,
;;; so we can locally change the temporal envelope and/or the time offset
;;; without side effects. Moreover, all the local bindings and the labels
;;; contained in a regofile continue to have lexical scope and dynamic
;;; extent, therefore it is possible to include the same regofile multiple
;;; times without name collisions.
;;; There is not a specific limit on the depth of included rego files.
;;;
;;; Note: we can use TEMPO-ENV within an event only if the event terminates
;;; before the end of the regofile.  A regofile ends after the last event
;;; or after a long pending event if the duration is known (defined with
;;; the local macro DUR). For example:
;;;
;;;     0    ...
;;;     1.5  ...
;;;     3    ...
;;;
;;; ends after 3 beats but
;;;
;;;     0    ...
;;;     1.5  ... (dur 5) ...
;;;     3    ...
;;;
;;; ends after 6.5 beats.
;;;
(defun rego-stream->sexp (stream &optional function-name compile-rego-p)
  (declare (type stream stream))
  (with-ensure-symbols (time dur score-realtime-p tempo tempo-env)
    (with-complex-gensyms
        (time-beats beats last-time last-dur max-time update-max-time wrapper)
      (let* ((%sched (ensure-complex-gensym "AT"))
             (sched (ensure-complex-gensym "AT"))
             (*include-rego-stack* nil)
             (*score-macros* nil)
             (*score-function-name* nil)
             (*score-local-function-name* nil)
             (*score-package* *package*)
             (*score-realtime* '#:maybe)
             (*score-start-time* 0.0)
             (*score-radix* 10)
             (*score-float-format* '#.incudine.config:*sample-type*)
             (score-body (%write-regofile
                           stream sched last-time last-dur max-time tempo-env)))
        (prog1
          `(with-rego-function (,function-name ,*score-function-name*
                                ,*score-local-function-name* ,compile-rego-p)
             (with-schedule (:start-time 0 score-realtime-offset)
               (with-rego-samples (,wrapper __smptime__ __smptime0__ ,time-beats
                                   ,sched ,last-time ,last-dur ,max-time)
                 (let ((,tempo-env (default-tempo-envelope))
                       (,score-realtime-p (not (nrt-edf-heap-p))))
                   (flet ((,update-max-time (,dur)
                            (setf ,last-time ,sched)
                            (setf ,last-dur (sample ,dur))
                            (setf ,max-time
                                  (max ,max-time (+ ,last-time ,last-dur)))
                            nil)
                          (,%sched (at-beat ,tempo-env fn)
                            (incudine.edf:schedule-at
                             (+ __smptime__
                                (* *sample-rate*
                                   (%beats->seconds ,tempo-env
                                                    (setf ,sched (sample at-beat)))))
                             fn (list at-beat))))
                     (declare (ignorable (function ,update-max-time)))
                     (macrolet ((,tempo (&rest args)
                                  `(set-tempo-envelope ,',tempo-env
                                     ,@(if (cdr args)
                                           args
                                           ;; Constant tempo
                                           `((list ,(car args) ,(car args)) '(0)))))
                                (,dur (,beats)
                                  (with-gensyms (x)
                                    `(let ((,x ,,beats))
                                       (,',update-max-time ,x)
                                       (beats->seconds ,',tempo-env ,x ,',sched))))
                                (,sched (at-beat fn &rest args)
                                  (with-gensyms (x)
                                    `(,',%sched ,at-beat ,',tempo-env
                                                (lambda (,x)
                                                  (setf ,',sched (sample ,x))
                                                  (,fn ,@args))))))
                       (symbol-macrolet
                           ((,time (rego-time
                                     (foreign-array-data ,wrapper) ,tempo-env)))
                         ,score-body)))))))
          (mapc #'unintern (list %sched sched)))))))

(defun regofile->sexp (path &optional function-name compile-rego-p)
  "From a rego file PATH, return the corresponding lisp form inside the
definition of a function optionally named FUNCTION-NAME.

If COMPILE-REGO-P is NIL (default), use an interpreter to evaluate the
event list at runtime when this function is called."
  (with-open-file (score (incudine.util::truename* path))
    (rego-stream->sexp score function-name compile-rego-p)))

(defun regostring->sexp (string &optional function-name compile-rego-p)
  (with-input-from-string (score string)
    (rego-stream->sexp score function-name compile-rego-p)))

(defun regofile->function (path &optional function-name compile-rego-p)
  "From a rego file PATH, define a function optionally named
FUNCTION-NAME to evaluate the corresponding lisp form.

If COMPILE-REGO-P is NIL (default), use an interpreter to evaluate the
event list at runtime.

The score statement :SCORE-FUNCTION-NAME: or :SCORE-LOCAL-FUNCTION-NAME:
is an alternative method to set the function name."
  (eval (regofile->sexp path function-name compile-rego-p)))

(defun regostring->function (string &optional function-name compile-rego-p)
  "From a string containing a score in rego file format, define a
function optionally named FUNCTION-NAME to evaluate the corresponding
lisp form.

If COMPILE-REGO-P is NIL (default), use an interpreter to evaluate the
event list at runtime.

The score statement :SCORE-FUNCTION-NAME: or :SCORE-LOCAL-FUNCTION-NAME:
is an alternative method to set the function name."
  (eval (regostring->sexp string function-name compile-rego-p)))

(defun regofile->lispfile (path &optional function-name lisp-file compile-rego-p)
  "Convert from a rego file to a lisp file.

The lisp file contains the definition of a function optionally named
FUNCTION-NAME.

If LISP-FILE is NIL, the name of the lisp file is the name of the rego
file PATH with file extension \"cudo\".

If COMPILE-REGO-P is NIL (default), use an interpreter to evaluate the
event list at runtime when the function is called."
  (declare (type (or pathname string null) path lisp-file))
  (let ((lisp-file (or (and lisp-file
                            (incudine.util::%parse-filepath lisp-file))
                       (make-pathname
                         :defaults (incudine.util::%parse-filepath path)
                         :type "cudo"))))
    (with-open-file (lfile lisp-file :direction :output :if-exists :supersede)
      (let ((form (regofile->sexp path function-name compile-rego-p)))
        (write (if compile-rego-p
                   `(eval-when (:compile-toplevel :load-toplevel :execute) ,form)
                   form)
               :stream lfile :gensym nil))
      (terpri lfile)
      (msg debug "convert ~A -> ~A" path lisp-file)
      lisp-file)))

;;; Similar to REGO-STREAM->SEXP
(defun %stream->regolist (stream &optional args)
  (let ((%sched (ensure-complex-gensym "AT"))
        (sched (ensure-complex-gensym "AT"))
        (incudine::*include-rego-stack* nil)
        (*score-macros* nil)
        (*score-package* *package*)
        (*score-realtime* nil)
        (*score-start-time* 0.0)
        (*score-radix* 10)
        (*score-float-format* '#.incudine.config:*sample-type*))
    (with-ensure-symbols (time dur score-args score-realtime-p tempo tempo-env)
      (with-gensyms (wrapper time-beats beats last-time last-dur max-time
                     update-max-time flist)
        `(with-cleanup
           (with-rego-samples (,wrapper __smptime__ __smptime0__ ,time-beats
                               ,sched ,last-time ,last-dur ,max-time)
             (let ((,tempo-env (default-tempo-envelope))
                   (,score-realtime-p nil)
                   (,score-args ',args)
                   (,flist nil)
                   (score-realtime-offset 0))
               (declare (ignorable score-realtime-offset ,score-args))
               (flet ((,update-max-time (,dur)
                        (setf ,last-time ,sched)
                        (setf ,last-dur (sample ,dur))
                        (setf ,max-time
                              (max ,max-time (+ ,last-time ,last-dur)))
                        nil)
                      (,%sched (at-beat ,tempo-env fn)
                        (push (list (+ (* __smptime__ *sample-duration*)
                                       (%beats->seconds ,tempo-env
                                         (setf ,sched (sample at-beat))))
                                    fn)
                              ,flist)))
                 (declare (ignorable (function ,update-max-time)))
                 (macrolet ((,tempo (&rest args)
                              `(set-tempo-envelope ,',tempo-env
                                 ,@(if (cdr args)
                                       args
                                       `((list ,(car args) ,(car args)) '(0)))))
                            (,dur (,beats)
                              (with-gensyms (x)
                                `(let ((,x ,,beats))
                                   (,',update-max-time ,x)
                                   (beats->seconds ,',tempo-env ,x ,',sched))))
                            (,sched (at-beat fn &rest args)
                              (with-gensyms (x)
                                `(,',%sched ,at-beat ,',tempo-env
                                   (lambda (,x)
                                     (setf ,',sched (sample ,x))
                                     (list ',fn ,@(mapcar #'quote-var-special
                                                          args)))))))
                   (symbol-macrolet
                       ((,time (rego-time (foreign-array-data ,wrapper)
                                          ,tempo-env)))
                     ,(incudine::%write-regofile
                        stream sched last-time last-dur max-time tempo-env
                        nil nil nil nil)
                     (incudine::get-regolist ,flist ,tempo-env
                                             ,(if (> *score-start-time* 0.0)
                                                  *score-start-time*))))))))))))

(defun quote-var-special (x)
  (cond ((and (symbolp x)
              (not (fboundp x))
              (incudine.util::var-globally-special-p x))
         (list 'quote x))
        ((consp x)
         (mapcar #'quote-var-special x))
        (t x)))

(defun stream->regolist (stream &optional score-args)
  (incudine.util::cudo-eval (%stream->regolist stream score-args)))

(defun add-line-continuation (string stream)
  (with-input-from-string (s string)
    (let ((*print-pretty* t))
      (loop for l0 = (read-line s nil nil nil) then l1
            for l1 = (read-line s nil nil nil)
            while l1 do
              (write-line (format nil "~A \\" l0) stream)
            finally (write-line l0 stream)))))

(defun write-regolist (list stream)
  (dolist (l list list)
    (add-line-continuation (format nil "~{~S~^ ~}~%" l) stream)))

(defun regofile->list (path &key score-args)
  "From a rego file PATH, return the corresponding event list as a
list of lists (time function-name &rest arguments)."
  (with-open-file (f (incudine.util::truename* path))
    (stream->regolist f score-args)))

(defun regolist->file (list path)
  "Write a rego file PATH with the event list obtained from a list of
lists (time function-name &rest arguments)."
  (with-open-file (f (incudine.util::%parse-filepath path)
                   :direction :output :if-exists :supersede)
    (write-regolist list f)
    path))

(defun regostring->list (string &key score-args)
  "From a string containing a score in rego file format, return the
corresponding event list as a list of lists
(time function-name &rest arguments)."
  (with-input-from-string (score string)
    (stream->regolist score score-args)))
