;;; Copyright (c) 2013 Tito Latini
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

;;; A score file can contain time-tagged lisp functions, lisp statements
;;; and lisp tags.

(declaim (inline blank-char-p))
(defun blank-char-p (c)
  (member c '(#\space #\tab)))

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

;;; The syntax of a time-tagged lisp function is:
;;;
;;;     start-time-in-beats   function-name   [arg1]   [arg2]   ...
;;;
(declaim (inline time-tagged-function-p))
(defun time-tagged-function-p (string)
  (declare (type string string))
  (if (char= (char string 0) #\()
      (%time-tagged-function-p string)
      (let ((space-pos (position-if #'blank-char-p string)))
        (declare (type (or non-negative-fixnum null))) 
        (when space-pos
          (find-if-not #'blank-char-p string :start space-pos)))))

(declaim (inline score-skip-line-p))
(defun score-skip-line-p (line)
  (declare (type string line))
  (let ((non-blank (find-if-not #'blank-char-p line)))
    (or (null non-blank)
        (char= non-blank #\;))))

(defmacro %at-sample (at-var time-at-var offset env beats func-symbol &rest args)
  `(,at-var (+ ,offset (* *sample-rate*
                          (,time-at-var ,env (setf ,at-var (sample ,beats)))))
       #',func-symbol ,@args))

(declaim (inline score-line->sexp))
(defun score-line->sexp (line at-var time-at-var)
  (declare (type string line))
  (if (time-tagged-function-p line)
      (macroexpand-1
       (read-from-string
        (format nil "(INCUDINE::%AT-SAMPLE ~A ~A TIME TEMPO-ENV ~A)"
                at-var time-at-var line)))
      ;; Tag or lisp statement
      (read-from-string (string-left-trim '(#\space #\tab) line))))

(defun find-score-local-bindings (stream at time-at)
  (declare (type stream stream) (type symbol at))
  (labels ((score-bindings-p (line)
             (string-equal (subseq line 0 (min 5 (length line))) "with "))
           (format-bindings (line)
             (concatenate 'string "((" (subseq line 5) "))"))
           (first-score-stmt (line)
             (declare (type (or string null) line))
             (when line
               (cond ((score-skip-line-p line)
                      (first-score-stmt (read-score-line stream)))
                     ((score-bindings-p line)
                      ;; Local bindings at the beginning of the score
                      (read-from-string (format-bindings line)))
                     (t ;; There aren't local bindings
                      (list nil (score-line->sexp line at time-at)))))))
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
        (let* ((s (remove-comment (string-trim '(#\space #\tab) line)))
               (slen (length s)))
          (declare (type non-negative-fixnum slen))
          (if (line-break-p s slen)
              (concatenate 'string
                           (subseq s 0 (- slen 2))
                           " "
                           (read-score-line stream))
              s))))))

;;; Local variables usable inside the rego file:
;;;
;;;     TIME          initial time in samples
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
(defun regofile->sexp (path &optional fname)
  (declare (type (or pathname string)))
  (with-ensure-symbol (time dur tempo tempo-env)
    (let ((at (ensure-symbol (symbol-name (gensym "%AT%"))))
          (time-at (ensure-symbol (symbol-name (gensym "%TIME-AT%"))))
          ;; *PRINT-GENSYM* is NIL in REGOFILE->LISPFILE
          (beats (gensym "%%%BEATS%%%")))
      `(,@(if fname `(defun ,fname) '(lambda)) ()
         (let ((,tempo-env (make-tempo-envelope '(60 60) '(0))))
           (with-samples ((,time (now))
                          (,at +sample-zero+))
             (flet ((,dur (,beats)
                      (time-at ,tempo-env ,beats ,at))
                    (,time-at (tenv beats)
                      (%time-at tenv beats))
                    (,at (time fn &rest args)
                      (incudine.edf::%%at time fn args)))
               (declare (ignorable (function ,dur)))
               (macrolet ((,tempo (&rest args)
                            `(set-tempo-envelope ,',tempo-env
                               ,@(if (cdr args)
                                     args
                                     ;; Constant tempo
                                     `((list ,(car args) ,(car args)) '(0))))))
                 (prog*
                   ,@(with-open-file (score path)
                       (append (find-score-local-bindings score at time-at)
                               (loop for line of-type (or string null)
                                              = (read-score-line score)
                                     while line
                                     unless (score-skip-line-p line)
                                     collect (score-line->sexp line at time-at)))))))))))))

(declaim (inline regofile->function))
(defun regofile->function (path &optional fname)
  (eval (regofile->sexp path fname)))

(defun regofile->lispfile (rego-file &optional fname lisp-file)
  (declare (type (or pathname string null) rego-file lisp-file))
  (let ((lisp-file (or lisp-file
                       (make-pathname :defaults rego-file
                                      :type "cudo"))))
    (with-open-file (lfile lisp-file :direction :output
                     :if-exists :supersede)
      (write (regofile->sexp rego-file fname) :stream lfile :gensym nil)
      (terpri lfile)
      (msg debug "convert ~A -> ~A" rego-file lisp-file)
      lisp-file)))
