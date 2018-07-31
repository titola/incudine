;;; Copyright (c) 2013-2018 Tito Latini
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
;;;     start-time-in-beats   function-name   [arg1]   [arg2]   ...
;;;

(declaim (special
           ;; Stack used to check recursive inclusions of rego files.
           *include-rego-stack*)
         (type list *include-rego-stack*))

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

(defun ignore-score-statements (name-list)
  "Ignore the score statements in NAME-LIST. The name of a statement is
a symbol, a string or a list of two score statements which delimit a
score block to ignore."
  (flet ((string-or-symbol-p (x)
           (or (stringp x) (symbolp x)))
         (statement-name (x)
           (if (stringp x) x (symbol-name x))))
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
                    (cons (statement-name (car i)))
                    (otherwise i))))
        (setf (gethash name *score-statements*)
              (if (consp i)
                  (cons :ignore (second i))
                  :ignore))))))

(defun score-statement-to-ignore-p (name)
  (declare (type string name))
  (let ((res (gethash name *score-statements*)))
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
       (setf (gethash ,(if (stringp name) name `(symbol-name ',name))
                      *score-statements*)
             (lambda ,args ,@decl
               (let* ((*print-pretty* nil)
                      (str (format nil "~S" (progn ,@rest)))
                      (len (length str)))
                 (if (< len 2)
                     ""
                     (subseq str 1 (1- len))))))
       ',name)))

(declaim (inline delete-score-statement))
(defun delete-score-statement (name)
  "Delete the score statement defined by DEFSCORE-STATEMENT
or IGNORE-SCORE-STATEMENTS."
  (remhash (symbol-name name) *score-statements*))

(declaim (inline next-blank-position))
(defun next-blank-position (string)
  (position-if #'blank-char-p string))

(declaim (inline score-statement-name))
(defun score-statement-name (str)
  (let ((name-endpos (next-blank-position str)))
    (values (string-upcase (subseq str 0 name-endpos))
            name-endpos)))

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
          (let ((fn (gethash name *score-statements*)))
            (declare (type (or function symbol cons) fn))
            (when (functionp fn)
              (apply fn (when name-endpos
                          (let ((*readtable* *score-readtable*))
                            (score-statement-args str name-endpos))))))))))

(declaim (inline blank-char-p))
(defun blank-char-p (c)
  (member c '(#\Space #\Tab)))

(declaim (inline string-trim-blank))
(defun string-trim-blank (string)
  (string-trim '(#\Space #\Tab) string))

(declaim (inline string-trim-blank-and-quotation))
(defun string-trim-blank-and-quotation (string)
  (string-trim '(#\Space #\Tab #\") string))

(declaim (inline line-parse-skip-string))
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

(declaim (inline time-tagged-function-p))
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
       (let ((name (string-upcase
                     (subseq line 2 (next-blank-position line)))))
         (and (null (gethash name  *score-statements*))
              (null (find name *features* :key #'symbol-name
                          :test #'string=))))))

(declaim (inline ignore-score-statement-with-colon-p))
(defun ignore-score-statement-with-colon-p (name)
  (declare (type string name))
  ;; A keyword is not ignored because it could be a label.
  (find #\: name :start 1))

(declaim (inline org-table-line-p))
(defun org-table-line-p (string)
  (char= (char string 0) #\|))

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

(defun ignore-score-statement (name stream)
  (declare (type string name) (type stream stream))
  (or (multiple-value-bind (ignore-p block-end)
          (score-statement-to-ignore-p name)
        (when ignore-p
          (when block-end
            (do ((line (read-line stream) (read-line stream)))
                ((string= (score-statement-name (string-trim-blank line))
                          block-end))))
          t))
      (ignore-score-statement-with-colon-p name)))

(defun score-skip-line-p (line stream)
  (declare (type string line) (type stream stream))
  (let ((str (string-trim-blank line)))
    (or (zerop (length str))
        (let ((c (char str 0)))
          (and (char/= c #\()
               (or (char= c #\;)
                   (ignore-score-statement (score-statement-name str) stream)
                   (org-table-line-to-skip-p str)
                   (sharp-plus-to-skip str)))))))

(defmacro %at-sample (at-fname beats func-symbol &rest args)
  `(,at-fname ,beats ,func-symbol ,@args))

(define-constant +include-strlen+ (length "include"))

;;; Score statement used to include the content of another rego file:
;;;
;;;     include "regofile" [time]
;;;
(declaim (inline include-regofile-p))
(defun include-regofile-p (line)
  (and (>= (length line) #.(length "include \"x\""))
       (string= line "include" :end1 7)
       (blank-char-p (char line 7))
       t))

(defun include-rego-path-and-time (line)
  (declare (type string line))
  (let ((time-pos (position #\" line :from-end t)))
    (values (string-trim-blank-and-quotation (subseq line (1+ +include-strlen+)
                                                     time-pos))
            (read-from-string (string-trim-blank-and-quotation
                                (subseq line time-pos))
                              nil))))

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
(defun score-expand-parallel-functions (form)
  (labels ((ignore-func-p (fname)
             (or (eq fname 'quote)
                 (and (consp fname) (eq (car fname) 'quote))))
           (next (at-fname time form)
             (let ((end (position '// form)))
               (if end
                   (let ((next (next at-fname time (subseq form (1+ end)))))
                     (if (ignore-func-p (car form))
                         next
                         (cons `(,at-fname ,time ,@(subseq form 0 end)) next)))
                   (unless (ignore-func-p (car form))
                     `((,at-fname ,time ,@form)))))))
    (let ((pos (position '// form)))
      (cond ((null pos)
             (unless (ignore-func-p (third form))
               form))
            ((= pos 2)
             (score-expand-parallel-functions (remove '// form :count 1)))
            (t
             (with-gensyms (time)
               `(let ((,time ,(cadr form)))
                  ,@(next (car form) time (cddr form)))))))))

(defun score-line->sexp (line at-fname &optional args)
  (declare (type string line) (type list args))
  (if (include-regofile-p line)
      (multiple-value-bind (path time)
          (include-rego-path-and-time line)
        (let ((incfile (truename (merge-pathnames path (car args)))))
          (cond ((find incfile *include-rego-stack* :test #'equal)
                 (msg error "recursive inclusion of ~S~%  => ~A" incfile
                      (reverse *include-rego-stack*)))
                (t (push incfile *include-rego-stack*)
                   (with-open-file (score incfile)
                     (apply #'%write-regofile score
                            `(,at-fname ,@(cdr args) t ,time)))))))
      (let ((line (or (expand-score-statement line) (org-table-filter line)))
            (*readtable* *score-readtable*)
            (*read-default-float-format* incudine.config:*sample-type*))
        (declare (type string line))
        (if (time-tagged-function-p line)
            (score-expand-parallel-functions
              (macroexpand-1
                (read-from-string
                  (format nil "(INCUDINE::%AT-SAMPLE ~A ~A)" at-fname line))))
            ;; Tag or lisp statement.
            (read-from-string (string-left-trim '(#\Space #\Tab) line))))))

(defun score-lines->sexp (stream at-fname args)
  (declare (type stream stream) (type list args))
  (loop for line of-type (or string null)
                 = (read-score-line stream)
        while line
        unless (score-skip-line-p line stream)
        collect (score-line->sexp line at-fname
                                  (and (include-regofile-p line) args))))

(defun find-score-local-bindings (stream at args)
  (declare (type stream stream) (type symbol at))
  (labels ((score-bindings-p (line)
             (string-equal (subseq line 0 (min 5 (length line))) "with "))
           (format-bindings (line)
             (concatenate 'string "(" (subseq line 5) ")"))
           (first-score-stmt (line)
             (declare (type (or string null) line))
             (when line
               (cond ((score-skip-line-p line stream)
                      (first-score-stmt (read-score-line stream)))
                     ((score-bindings-p line)
                      ;; Local bindings at the beginning of the score
                      (read-from-string (format-bindings line)))
                     (t ;; There aren't local bindings
                      (list nil (score-line->sexp line at args)))))))
    (first-score-stmt (read-score-line stream))))

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
;;; function DUR (see REGOFILE->SEXP).
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
              (cond ((and (plusp ,,max-time)
                          (plusp (setf ,,max-time
                                       (- (time-at ,,tempo-env ,,max-time)
                                          (time-at ,,tempo-env ,,now)))))
                     ;; End after the pending event.
                     (at (+ (now) (* ,,max-time *sample-rate*))
                         #'end-of-rego))
                    ((rt-thread-p) (nrt-funcall #'end-of-rego))
                    (t (end-of-rego)))))
          incudine::*to-free*))))

(declaim (inline default-tempo-envelope))
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

(defmacro with-rego-function ((fname compile-rego-p) &body body)
  (let ((maybe-compile (if compile-rego-p
                           `(compile ,@(unless fname '(nil)))
                           '(progn)))
        (expr `(progn ,@body)))
    (unless compile-rego-p
      (setf expr `(incudine.util::cudo-eval (quote ,expr))))
    `(,@maybe-compile
       (,@(if fname `(defun ,fname) '(lambda)) () ,expr))))

;;; Foreign memory to reduce consing.
(defmacro with-rego-samples ((foreign-array-name t0-var t1-var time-var
                              sched-var last-time-var last-dur-var max-time-var)
                             &body body)
  (with-complex-gensyms (c-array)
    (let ((var-names (list t0-var t1-var time-var sched-var last-time-var
                           last-dur-var max-time-var)))
      `(let* ((incudine::*to-free* nil)
              (,foreign-array-name (make-foreign-array ,(length var-names)
                                                       'sample :zero-p t))
              (,c-array (foreign-array-data ,foreign-array-name)))
         (symbol-macrolet ,(loop for var in var-names for i from 0
                                 collect `(,var (smp-ref ,c-array ,i)))
           (setf ,t0-var (if (incudine::nrt-edf-heap-p)
                             (now)
                             +sample-zero+))
           (setf ,t1-var ,t0-var)
           (setf ,time-var ,t0-var)
           ,@body)))))

;;; An included regofile doesn't change the temporal envelope and/or
;;; the time of the parent regofile.
(defun rego-local-tempo (local-bindings time parent-time time-offset
                         time-offset-var tempo-env parent-tempo-env)
  (let ((time-bind `(,parent-time ,time))
        ;; TIME-OFFSET should be altered if it is defined with a
        ;; parent's variable shadowed in the included rego file,
        ;; therefore it is safe to create a new variable binding.
        (time-os-bind `(,time-offset-var ,(or time-offset +sample-zero+)))
        (tenv-bind `(,parent-tempo-env ,tempo-env))
        (local-tempo `(progn
                        ,@(and time-offset `((incf ,time ,time-offset-var)))
                        (setf ,tempo-env (copy-tempo-envelope ,tempo-env))))
        (decl `(declare (ignorable ,time-offset-var))))
    (if (car local-bindings)
        ;; Update the local bindings.
        `((,time-bind ,time-os-bind ,tenv-bind ,@local-bindings)
          ,decl ,local-tempo)
        ;; Set the local bindings.
        `((,time-bind ,time-os-bind ,tenv-bind) ,decl ,local-tempo
          ,@(cdr local-bindings)))))

(defun %write-regofile (score at-fname time-var dur-var max-time tenv
                        &optional included-p time-offset (extend-time-p t))
  `(prog*
     ,@(progn
         (unless included-p
           (push (if (file-name score) (truename score) score)
                 *include-rego-stack*))
         (with-ensure-symbols (time tempo-env)
           (with-gensyms (parent-time parent-tempo-env)
             (let ((write-args (list (and (file-name score)
                                          (directory-namestring score))
                                     time-var
                                     dur-var max-time tenv)))
               (append
                 (let ((vars (find-score-local-bindings score at-fname
                                                        write-args)))
                   (cond (included-p
                          (with-gensyms (time-offset-var)
                            (rego-local-tempo vars time parent-time time-offset
                                              time-offset-var tempo-env
                                              parent-tempo-env)))
                         ((car vars) (list vars))
                         ;; No local bindings.
                         (t vars)))
                 (score-lines->sexp score at-fname write-args)
                 (cond (included-p
                        ;; End of the included regofile.
                        (pop *include-rego-stack*)
                        `((setf ,time ,parent-time ,tempo-env ,parent-tempo-env)))
                       (extend-time-p
                        (list (maybe-extend-time at-fname max-time tenv)))))))))))

(define-constant +rego-time0-index+ 0)
(define-constant +rego-time1-index+ 1)
(define-constant +rego-time-index+ 2)

(declaim (inline rego-time))
(defun rego-time (time-ptr tempo-env)
  (declare (ignore tempo-env))
  ;; Time offset in beats.
  (* (- (smp-ref time-ptr +rego-time1-index+)
        (smp-ref time-ptr +rego-time0-index+))
     *sample-duration*))

(defun set-rego-time (time-ptr tempo-env value)
  (declare (type foreign-pointer time-ptr) (type tempo-envelope tempo-env)
           (type real value))
  (let ((value (max 0 value)))
    ;; Time offset used in REGOFILE->SEXP. The value is converted in
    ;; seconds before the update.
    (setf (smp-ref time-ptr +rego-time-index+)
          (+ (smp-ref time-ptr +rego-time0-index+)
             (* *sample-rate* (%time-at tempo-env value))))
    ;; Update without conversion from beats to seconds.
    (setf (smp-ref time-ptr +rego-time1-index+)
          (+ (smp-ref time-ptr +rego-time0-index+)
             (* *sample-rate* value)))))

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
;;; DUR is a local function to convert the duration from
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
;;; the local function DUR). For example:
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
  (with-ensure-symbols (time dur tempo tempo-env)
    (let ((%sched (ensure-complex-gensym "AT"))
          (sched (ensure-complex-gensym "AT"))
          (*include-rego-stack* nil))
      (with-complex-gensyms (smptime0 smptime1 smptime beats last-time
                             last-dur max-time c-array-wrap)
        (prog1
         `(with-rego-function (,function-name ,compile-rego-p)
           (with-schedule
             (with-rego-samples (,c-array-wrap ,smptime0 ,smptime1 ,smptime
                                 ,sched ,last-time ,last-dur ,max-time)
               (let ((,tempo-env (default-tempo-envelope)))
                 (flet ((,dur (,beats)
                          (setf ,last-time ,sched)
                          (setf ,last-dur (sample ,beats))
                          (setf ,max-time
                                (max ,max-time (+ ,last-time ,last-dur)))
                          (time-at ,tempo-env ,beats ,sched))
                        (,%sched (at-beat fn)
                          (incudine.edf:schedule-at
                            (+ ,smptime
                               (* *sample-rate*
                                  (%time-at ,tempo-env
                                            (setf ,sched (sample at-beat)))))
                            fn (list at-beat))))
                   (declare (ignorable (function ,dur)))
                   (macrolet ((,tempo (&rest args)
                                `(set-tempo-envelope ,',tempo-env
                                   ,@(if (cdr args)
                                         args
                                         ;; Constant tempo
                                         `((list ,(car args) ,(car args)) '(0)))))
                              (,sched (at-beat fn &rest args)
                                (with-gensyms (x)
                                  `(,',%sched ,at-beat
                                              (lambda (,x)
                                                (setf ,',sched (sample ,x))
                                                (,fn ,@args))))))
                     (symbol-macrolet
                         ((,time (rego-time (foreign-array-data ,c-array-wrap)
                                            ,tempo-env)))
                       ,(%write-regofile stream sched last-time last-dur max-time
                                         tempo-env))))))))
        (mapc #'unintern (list %sched sched)))))))

(defun regofile->sexp (path &optional function-name compile-rego-p)
  "From a rego file PATH, return the corresponding lisp form inside the
definition of a function optionally named FUNCTION-NAME.

If COMPILE-REGO-P is NIL (default), use an interpreter to evaluate the
event list at runtime when this function is called."
  (with-open-file (score path)
    (rego-stream->sexp score function-name compile-rego-p)))

(declaim (inline regofile->function))
(defun regofile->function (path &optional function-name compile-rego-p)
  "From a rego file PATH, define a function optionally named
FUNCTION-NAME to evaluate the corresponding lisp form.

If COMPILE-REGO-P is NIL (default), use an interpreter to evaluate the
event list at runtime."
  (eval (regofile->sexp path function-name compile-rego-p)))

(defun regofile->lispfile (path &optional function-name lisp-file compile-rego-p)
  "Convert from a rego file to a lisp file.

The lisp file contains the definition of a function optionally named
FUNCTION-NAME.

If LISP-FILE is NIL, the name of the lisp file is the name of the rego
file PATH with file extension \"cudo\".

If COMPILE-REGO-P is NIL (default), use an interpreter to evaluate the
event list at runtime when the function is called."
  (declare (type (or pathname string null) path lisp-file))
  (let ((lisp-file (or lisp-file (make-pathname :defaults path
                                                :type "cudo"))))
    (with-open-file (lfile lisp-file :direction :output :if-exists :supersede)
      (write (regofile->sexp path function-name compile-rego-p)
             :stream lfile :gensym nil)
      (terpri lfile)
      (msg debug "convert ~A -> ~A" path lisp-file)
      lisp-file)))

;;; Similar to REGO-STREAM->SEXP
(defun %stream->regolist (stream)
  (let ((%sched (ensure-complex-gensym "AT"))
        (sched (ensure-complex-gensym "AT"))
        (incudine::*include-rego-stack* nil))
    (with-ensure-symbols (time dur tempo tempo-env)
      (with-gensyms (c-array-wrap smptime0 smptime1 smptime beats last-time
                     last-dur max-time flist)
        `(with-cleanup
           (with-rego-samples (,c-array-wrap ,smptime0 ,smptime1 ,smptime
                               ,sched ,last-time ,last-dur ,max-time)
             (let ((,tempo-env (default-tempo-envelope))
                   (,flist nil))
               (flet ((,dur (,beats)
                        (setf ,last-time ,sched)
                        (setf ,last-dur (sample ,beats))
                        (setf ,max-time
                              (max ,max-time (+ ,last-time ,last-dur)))
                        (time-at ,tempo-env ,beats ,sched))
                      (,%sched (at-beat fn)
                        (push (list (+ (* ,smptime *sample-duration*)
                                       (%time-at ,tempo-env
                                                 (setf ,sched (sample at-beat))))
                                    fn)
                              ,flist)))
                 (declare (ignorable (function ,dur)))
                 (macrolet ((,tempo (&rest args)
                              `(set-tempo-envelope ,',tempo-env
                                 ,@(if (cdr args)
                                       args
                                       `((list ,(car args) ,(car args)) '(0)))))
                            (,sched (at-beat fn &rest args)
                              (with-gensyms (x)
                                `(,',%sched ,at-beat
                                   (lambda (,x)
                                     (setf ,',sched (sample ,x))
                                     (list ',fn ,@(mapcar #'quote-var-special
                                                          args)))))))
                   (symbol-macrolet
                       ((,time (rego-time (foreign-array-data ,c-array-wrap)
                                          ,tempo-env)))
                     ,(incudine::%write-regofile stream sched last-time last-dur
                                                 max-time tempo-env nil nil nil)
                     (mapcar (lambda (l)
                               (cons (first l)
                                     (funcall (second l) (first l))))
                             (sort (nreverse ,flist) '< :key 'first))))))))))))

(defun quote-var-special (x)
  (cond ((and (symbolp x)
              (not (fboundp x))
              (incudine.util::var-globally-special-p x))
         (list 'quote x))
        ((consp x)
         (mapcar #'quote-var-special x))
        (t x)))

(defun stream->regolist (stream)
  (incudine.util::cudo-eval (%stream->regolist stream)))

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

(defun regofile->list (path)
  "From a rego file PATH, return the corresponding event list as a
list of lists (time function-name &rest arguments)."
  (with-open-file (f path) (stream->regolist f)))

(defun regolist->file (list path)
  "Write a rego file PATH with the event list obtained from a list of
lists (time function-name &rest arguments)."
  (with-open-file (f path :direction :output :if-exists :supersede)
    (write-regolist list f)
    path))
