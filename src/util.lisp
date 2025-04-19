;;; Copyright (c) 2013-2024 Tito Latini
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

(in-package :incudine.util)

;;; TYPES

(define-constant maximum-channel-number 1023)

(define-constant maximum-bus-number 16383)

;; NON-NEGATIVE-FIXNUM64 used to get a better optimization
;; on 64bit machines. MOST-POSITIVE-FIXNUM 2^59-1 is good
;; at least with SBCL and CCL.
(define-constant most-positive-fixnum64 (1- (ash 1 59)))

(deftype non-negative-fixnum64 ()
  "Non negative FIXNUM on 64-bit platforms."
  '(integer 0 #.(1- (ash 1 59))))

(deftype limited-sample ()
  "A LIMITED-SAMPLE is a SAMPLE between -4e18 and 4e18.

SIN, COS and TAN are optimized on x86 if the argument type is
LIMITED-SAMPLE."
  (let ((high (sample 4.0d18)))
    `(,incudine.config:*sample-type* ,(- high) ,high)))

(deftype maybe-limited-sample ()
  "Correspond to LIMITED-SAMPLE on SBCL x86."
  #+(and sbcl x86) 'limited-sample
  #-(and sbcl x86) 'sample)

(deftype channel-number () `(integer 0 ,(1+ maximum-channel-number)))

(deftype bus-number () `(integer 0 ,maximum-bus-number))

;;; MISC

(defvar *sf-metadata-keywords*
  '(:title :copyright :software :artist :comment
    :date :album :license :tracknumber :genre))

(defun incudine-version ()
  "Return a string that identifies the Incudine version."
  #.(format nil "~D.~D.~D"
            incudine.config::+incudine-major+
            incudine.config::+incudine-minor+
            incudine.config::+incudine-patch+))

(defun incudine-version->= (&rest subversions)
  "Whether the current Incudine version is equal to or greater than
the version specified in the arguments.

Example:

    (incudine-version->= 0 9 25)"
  (destructuring-bind (&optional (major 0) (minor 0) (patch 0)) subversions
    (and (>= incudine.config::+incudine-major+ major)
         (>= incudine.config::+incudine-minor+ minor)
         (>= incudine.config::+incudine-patch+ patch))))

(defmacro incudine-optimize (&body body)
  `(locally (declare ,*standard-optimize-settings*) ,@body))

(defmacro with-ensure-symbols (names &body forms)
  `(let ,(mapcar (lambda (name) `(,name (ensure-symbol ,(symbol-name name))))
                 names)
     ,@forms))

(defun apply-sample-coerce (form)
  (if (atom form)
      (cond ((and (numberp form) (floatp form))
             (force-sample-format form))
            ((eq form 'pi) '(sample pi))
            (t form))
      (cons (apply-sample-coerce (car form))
            (apply-sample-coerce (cdr form)))))

(defun alloc-multi-channel-data (channels size &key real-time-p)
  (declare (type channel-number channels) (type positive-fixnum size)
           (type boolean real-time-p))
  (let ((ptr (funcall (if real-time-p #'foreign-rt-alloc #'foreign-alloc)
                      :pointer :count channels)))
    (dotimes (ch channels ptr)
      (declare (type channel-number ch))
      (setf (cffi:mem-aref ptr :pointer ch)
            (if real-time-p
                (foreign-rt-alloc-sample size)
                (foreign-alloc-sample size))))))

(defun free-multi-channel-data (data channels
                                &optional (free-function #'foreign-free))
  (declare (type foreign-pointer data) (type channel-number channels)
           (type function free-function))
  (dotimes (ch channels)
    (let ((frame (cffi:mem-aref data :pointer ch)))
      (unless (cffi:null-pointer-p frame)
        (funcall free-function frame))))
  (funcall free-function data)
  (values))

(defmacro dochannels ((var channels &optional (result nil))
                      &body body)
  "Iterate over the CHANNELS with VAR bound to each number of channel
and execute the body once for each channel, then RESULT form is evaluated."
  `(do ((,var 0 (1+ ,var)))
       ((>= ,var ,channels) ,result)
     (declare (type channel-number ,var))
     ,@body))

(declaim (inline hz->radians))
(defun hz->radians (value)
  "Convert the VALUE from Hz to radians."
  (* value *twopi-div-sr*))

(declaim (inline radians->hz))
(defun radians->hz (value)
  "Convert the VALUE from radians to Hz."
  (* value *sr-div-twopi*))

(declaim (inline linear->db))
(defun linear->db (value)
  "Convert the VALUE from linear to dB."
  (let ((in (if (zerop value) least-positive-sample value)))
    (* (log in) #.(/ (sample 20) (log (sample 10))))))

(declaim (inline db->linear))
(defun db->linear (value)
  "Convert the VALUE from dB to linear."
  (expt (sample 10) (* value (sample 5d-2))))

(declaim (inline linear-interp))
(defun linear-interp (in y0 y1)
  "Linear interpolation between Y0 and Y1 by IN."
  (+ y0 (* in (- y1 y0))))

(declaim (inline cos-interp))
(defun cos-interp (in y0 y1)
  "Sinusoidal interpolation between Y0 and Y1 by IN."
  (linear-interp (* (- 1 (cos (the limited-sample (* in (sample pi)))))
                    0.5) y0 y1))

;;; Catmull-Rom spline
(declaim (inline cubic-interp))
(defun cubic-interp (in y0 y1 y2 y3)
  "Four-point interpolation."
  (let ((a0 (+ (* -0.5 y0) (* 1.5 y1) (* -1.5 y2) (* 0.5 y3)))
        (a1 (+ y0 (* -2.5 y1) (* 2.0 y2) (* -0.5 y3)))
        (a2 (+ (* -0.5 y0) (* 0.5 y2))))
    (+ (* in (+ (* in (+ (* a0 in) a1)) a2)) y1)))

(declaim (inline t60->pole))
(defun t60->pole (time)
  "Return a real pole for a 60dB exponential decay in TIME seconds."
  (if (plusp time)
      ;; tau = time / log(0.001) = time / 6.9077
      (incudine.external::%exp (/ *log001-div-sr* time))
      +sample-zero+))

(declaim (inline cheb))
(defun cheb (order x)
  "Return the ORDER Chebyshev polynomial calculated at the point x."
  (declare (type fixnum order) (type sample x))
  (if (<= -1 x 1)
      (cos (the limited-sample
             (* order (the limited-sample (acos x)))))
      (cosh (the limited-sample
              (* order (the limited-sample (acosh x)))))))

(defun set-sample-rate (value)
  "Set the sample rate to VALUE and run the hook *SAMPLE-RATE-HOOK*.

The following variables are updated: *SAMPLE-RATE*, *SAMPLE-DURATION*,
*TWOPI-DIV-SR*, *SR-DIV-TWOPI*, *PI-DIV-SR*, *MINUS-PI-DIV-SR* and *CPS2INC*."
  (setf *sample-rate* (sample value)
        *sample-duration* (/ 1.0 *sample-rate*))
  (incudine::call-hooks "set-sample-rate" *sample-rate-hook*)
  *sample-rate*)

(defun set-sample-duration (value)
  "Set the sample duration to VALUE and run the hook *SAMPLE-RATE-HOOK*.

The following variables are updated: *SAMPLE-RATE*, *SAMPLE-DURATION*,
*TWOPI-DIV-SR*, *SR-DIV-TWOPI*, *PI-DIV-SR*, *MINUS-PI-DIV-SR* and *CPS2INC*."
  (setf *sample-duration* (sample value)
        *sample-rate* (/ 1.0 *sample-duration*))
  (incudine::call-hooks "set-sample-duration" *sample-rate-hook*)
  *sample-duration*)

(defun set-sound-velocity (value)
  "Set the the velocity of the sound in m/s at 22 degrees Celsius, 1
atmosfera and run the hook *SOUND-VELOCITY-HOOK*."
  (setf *sound-velocity* (sample value)
        *r-sound-velocity* (/ 1.0 *sound-velocity*))
  (incudine::call-hooks "set-sound-velocity" *sound-velocity-hook*)
  *sound-velocity*)

(declaim (inline sample->fixnum))
(defun sample->fixnum (value &key roundp)
  "Coerce VALUE from type SAMPLE to type FIXNUM.

If ROUNDP is T, round VALUE to the nearest integer.

VALUE has to be between MOST-NEGATIVE-FIXNUM and MOST-POSITIVE-FIXNUM."
  (declare (type (sample
                  #.(coerce (ash most-negative-fixnum -1) 'sample)
                  #.(coerce (ash most-positive-fixnum -1) 'sample)) value)
           (type boolean roundp))
  (multiple-value-bind (result rem) (if roundp (round value) (floor value))
    (declare (ignore rem))
    result))

(declaim (inline sample->int))
(defun sample->int (value)
  "Coerce VALUE from type SAMPLE to type INTEGER."
  (declare (type sample value))
  (multiple-value-bind (result rem) (floor value)
    (declare (ignore rem))
    result))

(declaim (inline float->fixnum))
(defun float->fixnum (value &key roundp)
  "Coerce VALUE from type SINGLE-FLOAT or DOUBLE-FLOAT to type FIXNUM.

If ROUNDP is T, round VALUE to the nearest integer.

VALUE has to be between MOST-NEGATIVE-FIXNUM and MOST-POSITIVE-FIXNUM."
  (declare (type (or (single-float
                      #.(coerce (ash most-negative-fixnum -1) 'single-float)
                      #.(coerce (ash most-positive-fixnum -1) 'single-float))
                     (double-float
                      #.(coerce (ash most-negative-fixnum -1) 'double-float)
                      #.(coerce (ash most-positive-fixnum -1) 'double-float)))
                 value)
           (type boolean roundp))
  (multiple-value-bind (result rem) (if roundp (round value) (floor value))
    (declare (ignore rem))
    result))

(declaim (inline sort-samples))
(defun sort-samples (pointer size)
  "Sort a foreign array of SIZE samples pointed to by POINTER."
  (incudine.external::qsort pointer size +foreign-sample-size+
                            (cffi:callback incudine.external::sample-cmp)))

(defun rationalize* (x &optional (significand-error 5.f-6))
  "Convert reals to rationals and try to minimize the ratios by
introducing an error in the significand of the floating point number.
The error is 0.0005% by default."
  (declare (type real x) (type (float 0.0 1.0) significand-error))
  (if (zerop significand-error)
      (rationalize x)
      (let ((x (coerce x 'single-float)))
        (multiple-value-bind (m e s) (integer-decode-float x)
          (let ((e (expt 2f0 e)))
            (labels ((rat (i r)
                       (declare (type fixnum i) (type rational r))
                       (if (>= i (floor (* m (+ 1 significand-error))))
                           r
                           (rat (1+ i)
                                (let ((n (rationalize (* i e s))))
                                  (if (< (numerator n) (numerator r)) n r))))))
              (rat (floor (* m (- 1 significand-error)))
                   (rationalize x))))))))

(defun parse-float (string &key (start 0) end (type 'single-float))
  "Parse a floating point number from the substring of STRING
delimited by START and END.

The first value returned is either the floating point number that was
parsed or NIL.

The second value is either the index into the string of the delimiter
that terminated the parse, or the upper bounding index of the substring.

The TYPE of the returned value is SINGLE-FLOAT by default."
  (declare (type simple-string string)
           (type (member single-float double-float) type))
  (multiple-value-bind (int pos)
      (parse-integer string :start start :end end :junk-allowed t)
    (if int
        (let ((frac-start (1+ pos)))
          (if (and (< frac-start (or end (length string)))
                   (char= (char string pos) #\.)
                   (digit-char-p (char string frac-start)))
              (multiple-value-bind (frac len)
                  (parse-integer string :start frac-start :end end
                                 :junk-allowed t)
                (values
                  (+ int (* (/ frac (expt (if (eq type 'double-float) 10d0 10f0)
                                          (the (unsigned-byte 8)
                                               (- len frac-start))))
                            (if (minusp int) -1 1)))
                  len))
              (values (coerce int type) pos)))
        (values nil 0))))

(declaim (inline declare-form-p))
(defun declare-form-p (lst)
  (eq (car lst) 'declare))

(defun separate-declaration (form)
  (let (acc)
    (do ((l form (cdr l)))
        ((null (and (consp (car l)) (declare-form-p (car l))))
         (values (nreverse acc) l))
      (push (car l) acc))))

(declaim (inline rt-thread-p))
(defun rt-thread-p ()
  "Return T if the current thread is the real-time thread."
  (eq (bt:current-thread) *rt-thread*))

(declaim (inline allow-rt-memory-p))
(defun allow-rt-memory-p ()
  "Return T if the current thread is the real-time thread and the
related foreign memory pool is usable."
  (and (rt-thread-p) *allow-rt-memory-pool-p*))

(macrolet
  ((define-*-ref (getter setter type)
     (let ((type-size (cffi:foreign-type-size type)))
       `(progn
          (defun ,getter (ptr &optional (index 0))
            ,(format nil "Access the foreign array element of type ~S ~
                          specified by INDEX. Setfable." type)
            (mem-ref ptr ',type
                     (the non-negative-fixnum (* index ,type-size))))
          (define-compiler-macro ,getter (ptr &optional (index 0))
            (if (constantp index)
                `(mem-ref ,ptr ',',type ,(* (eval index) ,type-size))
                `(mem-ref ,ptr ',',type
                          (the non-negative-fixnum (* ,index ,,type-size)))))
          (defun ,setter (ptr index value)
            (setf (mem-ref ptr ',type (the non-negative-fixnum
                                        (* index ,type-size)))
                  value))
          (define-compiler-macro ,setter (ptr index value)
            (if (constantp index)
                `(setf (mem-ref ,ptr ',',type ,(* (eval index) ,type-size))
                       ,value)
                `(setf (mem-ref ,ptr ',',type (the non-negative-fixnum
                                                (* ,index ,,type-size)))
                       ,value)))
          (defsetf ,getter (ptr &optional (index 0)) (value)
            `(,',setter ,ptr ,index ,value))))))
  (define-*-ref smp-ref smp-set sample)
  (define-*-ref i8-ref i8-set :int8)
  (define-*-ref u8-ref u8-set :uint8)
  (define-*-ref i16-ref i16-set :int16)
  (define-*-ref u16-ref u16-set :uint16)
  (define-*-ref i32-ref i32-set :int32)
  (define-*-ref u32-ref u32-set :uint32)
  (define-*-ref i64-ref i64-set :int64)
  (define-*-ref u64-ref u64-set :uint64)
  (define-*-ref f32-ref f32-set :float)
  (define-*-ref f64-ref f64-set :double)
  (define-*-ref ptr-ref ptr-set :pointer))

(defmacro with-doc-string ((doc-var form-var) &body body)
  (with-gensyms (decl-var)
    `(multiple-value-bind (,decl-var ,form-var) (separate-declaration ,form-var)
       (let* ((,doc-var (when (and (stringp (car ,form-var)) (cdr ,form-var))
                          (list (car ,form-var))))
              (,form-var (append ,decl-var
                                 (if ,doc-var (cdr ,form-var) ,form-var))))
         ,@body))))

(defun lambda*-arguments-without-dot (arguments)
  (do ((acc nil (cons (car arg) acc))
       (arg arguments (cdr arg)))
      ((atom (cdr arg))
       (if (cdr arg)
           ;; Replace dot with &rest
           (nreverse (list* (cdr arg) '&rest (car arg) acc))
           arguments))))

(defun lambda*-arguments (keys vals)
  (cond ((or (null vals)
             (and (keywordp (car vals))
                  (member (car vals) keys)))
         vals)
        ((null keys) nil)
        (t
         (multiple-value-bind (k v next-keys)
             (let ((k0 (car keys)))
               (if (eq k0 '&rest)
                   (values (cadr keys) vals (cddr keys))
                   (values k0 (car vals) (cdr keys))))
           (list* k v (lambda*-arguments next-keys (cdr vals)))))))

(defun lambda*-list-keywords (arguments)
  (mapcar (lambda (x)
            (if (eq x '&rest)
                x
                (make-keyword (if (listp x) (car x) x))))
          arguments))

(defun defun*-body (args kargs body)
  (with-gensyms (kname)
    `(flet ((,kname (&key ,@kargs) ,@body))
       (apply #',kname (lambda*-arguments ',(lambda*-list-keywords args)
                                          optional-keywords)))))

(defmacro with-lambda*-arguments ((args-var kargs-var aux-var arguments)
                                  &body body)
  `(let* ((,args-var (lambda*-arguments-without-dot ,arguments))
          (,kargs-var (remove '&rest ,arguments))
          (,aux-var (make-symbol "LAMBDA-LIST")))
     ,@body))

;;; DEFUN*, LAMBDA* and DEFMACRO* are inspired by the extensions
;;; define*, lambda* and define-macro* in Bill Schottstaedt's Scheme
;;; implementation s7.
;;;
;;; Note: the text of the doc-string in DEFUN* is copied/edited from
;;; the s7.html file provided with the source code:
;;;
;;;       ftp://ccrma-ftp.stanford.edu/pub/Lisp/s7.tar.gz
;;;
;;; Some examples from s7.html translated to CL:
;;;
;;;   (defun* hi (a (b 32) (c "hi")) (list a b c))
;;;
;;;   (hi 1)             ; => (1 32 "hi")
;;;   (hi :b 2 :a 3)     ; => (3 2 "hi")
;;;   (hi 3 2 1)         ; => (3 2 1)
;;;
;;;   (defun* foo ((a 0) (b (+ a 4)) (c (+ a 7))) (list a b c))
;;;
;;;   (foo :b 2 :a 60)   ; => (60 2 67)
;;;
;;;   (defun* foo (&rest a &rest b) (mapcar #'+ a b))
;;;
;;;   (foo 1 2 3 4 5)    ; => (3 5 7 9)
;;;
;;;   (defun* foo ((b 3) &rest x (c 1)) (list b c x))
;;;
;;;   (foo 32)           ; => (32 1 NIL)
;;;   (foo 1 2 3 4 5)    ; => (1 3 (2 3 4 5))
;;;
;;;   (funcall (lambda* ((b 3) &rest x (c 1) . d) (list b c x d)) 1 2 3 4 5)
;;;   ; => (1 3 (2 3 4 5) (4 5))
;;;
;;;   (defmacro* add-2 (a (b 2)) `(+ ,a ,b))
;;;
;;;   (add-2 1 3)        ; => 4
;;;   (add-2 1)          ; => 3
;;;   (add-2 :b 3 :a 1)  ; => 4
;;;
(defmacro defun* (name (&rest args) &body body)
  "Every argument in ARGS has a default value and is automatically
available as a keyword argument. The default value is either NIL if
unspecified, or given in a list whose first member is the argument name.
The last argument can be preceded by &rest or a dot to indicate that
all other trailing arguments should be packaged as a list under that
argument's name. A trailing or rest argument's default value is NIL.
You can have more than one &rest parameter."
  (with-doc-string (doc-string body)
    (with-lambda*-arguments (args kargs aux-var args)
      `(defun ,name (&rest optional-keywords &aux (,aux-var ',args))
         ,@doc-string
         (declare (ignore ,aux-var))
         ,(defun*-body args kargs body)))))

(defmacro lambda* ((&rest args) &body body)
  "See DEFUN*."
  (with-doc-string (doc-string body)
    (with-lambda*-arguments (args kargs aux-var args)
      `(lambda (&rest optional-keywords &aux (,aux-var ',args))
         ,@doc-string
         (declare (ignore ,aux-var))
         ,(defun*-body args kargs body)))))

(defun macro*-arguments (lambda-list
                         &optional type local-args lambda-list-keyword-p)
  (flet ((optional-keys-p (x)
           (and (consp x)
                (symbolp (car x))
                (string= (symbol-name (car x)) "&OPTIONAL-KEY"))))
    (let ((fst (car lambda-list)))
      (if (optional-keys-p fst)
          (let ((lambda-list-keyword (and (eq type 'documentation)
                                          (not (optional-keys-p (cadr fst)))
                                          '(&any))))
            (cons (append lambda-list-keyword
                          (macro*-arguments (cdr fst) type (car local-args)))
                  (macro*-arguments
                    (cdr lambda-list) type (cdr local-args) t)))
          (let ((args (lambda*-arguments-without-dot lambda-list)))
            (if (eq type 'call)
                ;; Arguments passed to the local macro.
                (lambda*-arguments (lambda*-list-keywords args) local-args)
                (if (eq type 'lambda-list)
                    ;; Lambda list for the local macro.
                    (let ((args (remove '&rest args)))
                      (and args `(&key ,@args)))
                    ;; Lambda list for the documentation.
                    (if (and lambda-list-keyword-p args)
                        (cons '&any args)
                        args))))))))

;;; A simplified version for default values in VUGLET.
(defun simple-defmacro*-body (optkey-var args kargs body)
  `(with-gensyms (kname)
     `(macrolet ((,kname (,(cons '&key ',kargs)) ,',@body))
        (,kname ,(lambda*-arguments
                   ',(lambda*-list-keywords args) ,optkey-var)))))

(defun defmacro*-body (optkey-var args kargs body)
  `(with-gensyms (kname)
     `(macrolet ((,kname (,',kargs) ,',@body))
        (,kname ,(macro*-arguments ',args 'call ,optkey-var)))))

(defmacro defmacro* (name (&rest arguments) &body body)
  "See DEFUN* for details.

A nested lambda list with optional keywords begins with the special keyword
&optional-key, and precedes the optional keywords of the (possibly nested)
lambda list. For example:

    (defmacro* optkey-test-1 ((a 1) (b 2) (c 3))
      `(list ,a ,b ,c))

    (defmacro* optkey-test-2 ((&optional-key (a 1) (b 2)) &rest rest)
      `(list ,a ,b ,@rest))

    (defmacro* optkey-test-3 ((&optional-key (a 1) (b 2))
                              (&optional-key
                               (&optional-key (c 3) (d 4)) (e 5))
                              (f 6) g . h)
      `(list ,a ,b ,c ,d ,e ,f ,g ,@h))

    (optkey-test-1)                   ; => (1 2 3)
    (optkey-test-1 :b 123)            ; => (1 123 3)
    (optkey-test-2)                   ; => (1 2)
    (optkey-test-2 (:b 3) 4 5 6)      ; => (1 3 4 5 6)
    (optkey-test-3)                   ; => (1 2 3 4 5 6 NIL)
    (optkey-test-3 () ((:d 123)))     ; => (1 2 3 123 5 6 NIL)
    (optkey-test-3 () () :h (1 2 3))  ; => (1 2 3 4 5 6 NIL 1 2 3)

    (optkey-test-3 (10) ((20) 30) :g 40 :f 50 :h (60))
    ;; => (10 2 20 4 30 50 40 60)"
  (with-doc-string (doc-string body)
    (let* ((args (macro*-arguments arguments 'documentation))
           (kargs (macro*-arguments arguments 'lambda-list))
           (aux-var (make-symbol "LAMBDA-LIST"))
           (lambda-list `(&rest optional-keywords &aux (,aux-var ',args))))
        `(progn
           (defmacro ,name ,lambda-list ,@doc-string
             (declare (ignore ,aux-var))
             ,(defmacro*-body 'optional-keywords arguments kargs body))
           ;; The compiler could remove the lambda list keywords &AUX
           ;; but we use them to read the optional-key arguments.
           #+sbcl (force-macro-lambda-list ',name ',lambda-list)
           #-sbcl ',name))))

(defun aux-lambda-list-p (arg)
  (when (consp arg)
    (let ((fst (first arg)))
      (and (symbolp fst)
           (null (symbol-package fst))
           (string= "LAMBDA-LIST" (symbol-name fst))))))

(defun lambda-list-to-star-list (arglist)
  "Return the optional-key arguments of ARGLIST."
  (let ((aux-pos (and ;; Ignore the lists ({arguments} . rest)
                      (null (cdr (last arglist)))
                      (position '&aux arglist))))
    (when aux-pos
      (let ((arglist (find-if #'aux-lambda-list-p
                              (cdr (subseq arglist aux-pos)))))
        (when arglist
          (labels ((optional-keywords-p (x)
                     (if (consp x)
                         (optional-keywords-p (car x))
                         (and (symbolp x) (string= (symbol-name x) "&ANY")))))
            (let ((args (cadadr arglist)))
              (if (optional-keywords-p (car args))
                  args
                  (cons '&any args)))))))))

(in-package :incudine)

(defvar *init-p* t)
(declaim (type boolean *init-p*))

;;; Borrowed from sbcl/src/code/late-extensions.lisp
(defun call-hooks (kind hooks &key (on-error :error))
  (dolist (hook hooks)
    (handler-case
        (funcall hook)
      (error (c)
        (if (eq :warn on-error)
            (warn "Problem running ~A hook ~S:~%  ~A" kind hook c)
            (with-simple-restart (continue "Skip this ~A hook." kind)
              (incudine-error "Problem running ~A hook ~S:~%  ~A"
                              kind hook c)))))))

(defun init (&optional force-p)
  "Incudine initialization, optionally forced if FORCE-P is T."
  (when (or force-p *init-p*)
    (call-hooks "Incudine initialization" *initialize-hook*)
    (setf *init-p* nil)
    t))

(defvar *sample-counter* (foreign-alloc 'sample :initial-element +sample-zero+))
(declaim (type foreign-pointer *sample-counter*))

(defvar *rt-sample-counter* *sample-counter*)
(declaim (type foreign-pointer *rt-sample-counter*))

(defglobal *null-counter*
    (cffi:foreign-alloc 'sample :initial-element +sample-zero+))
(declaim (type cffi:foreign-pointer *null-counter*))

(defmacro with-null-counter (&body body)
  `(let ((*sample-counter* *null-counter*)
         ;; The counter is read-only.
         (*rt-sample-counter* *null-counter*))
     ,@body))

(declaim (inline now))
(defun now ()
  "Return the current time in samples."
  (mem-ref *sample-counter* 'sample))

(defun set-time (value)
  (declare (type real value))
  (unless (eq *sample-counter* *rt-sample-counter*)
    (setf (mem-ref *sample-counter* 'sample) (sample value)))
  value)

(defsetf now set-time)

(defmacro with-local-time ((&key (start 0)) &body body)
  "Use a local counter during BODY.

START is the initial time in samples and defaults to zero.

The function NOW is setfable to change the time, for example:

    (with-local-time
      (loop repeat 8 collect (incf (now))))
    ;; => (1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0 7.0d0 8.0d0)"
  `(cffi:with-foreign-object (*sample-counter* 'sample)
     (setf (mem-ref *sample-counter* 'sample) (sample ,start))
     ,@body))

(defvar *portmidi-time* (foreign-alloc 'sample :initial-element +sample-zero+))
(declaim (type foreign-pointer *portmidi-time*))

(declaim (inline inc-portmidi-time))
(defun inc-portmidi-time (delta)
  (incf (smp-ref *portmidi-time* 0) delta))

(declaim (inline portmidi-time))
(defun portmidi-time ()
  (the (integer 0 #.(min most-positive-fixnum #x7fffffff))
    ;; 6 days: (log (* 6 24 3600 1000) 2) => 28.94949
    ;; If you want more days on a 32 bit arch, restart the timer of portmidi.
    (values
      (truncate (the (sample #.+sample-zero+ #.(sample (min most-positive-fixnum #x7fffffff)))
                     (smp-ref *portmidi-time* 0))))))

(defmacro dsp-seq (&rest function-call-forms)
  "Define a sequence of DSP's by using the STOP-HOOK of the functions
defined by DSP!. The last of the FUNCTION-CALL-FORMS is arbitrary
and useful to define recursive sequences.

Example:

    (defun seq-test (rep freq amp)
      (when (plusp rep)
        (dsp-seq (dsp-test freq amp env)
                 (dsp-test (* freq 7/4) amp env)
                 (dsp-test (* freq 2) amp env)
                 (seq-test (1- rep) freq amp))))"
  (with-gensyms (n)
    (labels ((%dsp-seq (functions)
               (if (null functions)
                   nil
                   (if (cdr functions)
                       (append (car functions)
                               (list :stop-hook
                                     `(list (lambda (,n)
                                              (declare (ignore ,n))
                                              ,(%dsp-seq (cdr functions))))))
                       (car functions)))))
      (%dsp-seq function-call-forms))))

(defgeneric circular-shift (obj n &key)
  (:documentation "Perform a circular shift of length N."))

(defgeneric free-p (obj)
  (:documentation "Return T if the object obj is deallocated."))

(defgeneric free (obj)
  (:documentation "Deallocate the object OBJ."))

(defmethod free ((obj t)) (values))

(defmethod free ((obj list))
  (dolist (x obj) (free x)))

(defmethod free ((obj function))
  (funcall obj)
  (values))

(defvar *to-free*)

(defmacro with-cleanup (&body body)
  "All the objects with finalizations arranged by INCUDINE-FINALIZE
and method INCUDINE:FREE instantiated within BODY are invalid beyond
the dynamic extent of BODY."
  `(let ((*to-free* nil))
     (unwind-protect
          (progn ,@body)
       (when *to-free*
         (free (the list *to-free*))
         (if (allow-rt-memory-p)
             (incudine.util:rt-global-pool-push-list *to-free*)
             (incudine.util:nrt-global-pool-push-list *to-free*))))))

(defmacro without-cleanup (&body body)
  "Used inside the body of WITH-CLEANUP to ignore the dynamic
finalizers during BODY."
  `(let ((*to-free* nil)) ,@body))

(declaim (inline dynamic-incudine-finalizer-p))
(defun dynamic-incudine-finalizer-p ()
  "Return T within the dynamic extend of WITH-CLEANUP."
  (boundp '*to-free*))

(defun incudine-finalize (obj function &optional (dynamic-p t))
  "Arrange for the designated FUNCTION to be called when there are no
more references to OBJ, including references in FUNCTION itself.

If DYNAMIC-P is NIL, the object is valid outside WITH-CLEANUP."
  (incudine.util::finalize obj function)
  (when (and dynamic-p (dynamic-incudine-finalizer-p))
    (let ((cons (if (allow-rt-memory-p)
                    (incudine.util:rt-global-pool-pop-cons)
                    (incudine.util:nrt-global-pool-pop-cons))))
      (rplaca cons obj)
      (setf *to-free* (rplacd cons *to-free*))))
  obj)

(declaim (inline incudine-cancel-finalization))
(defun incudine-cancel-finalization (obj)
  "Cancel all finalizations arranged for OBJ."
  (incudine.util::cancel-finalization obj))

(defmacro maybe-unwind-protect (protected &body cleanup)
  (with-gensyms (func)
    `(flet ((,func () ,protected))
       (if (dynamic-incudine-finalizer-p)
           (,func)
           (unwind-protect (,func) ,@cleanup)))))

(defgeneric quantize (obj from &key)
  (:documentation "Quantize OBJ with respect to a real number, a
vector, a BUFFER or TUNING structure in sorted order.

If OBJ is a vector, a BUFFER or TUNING structure, the keywords START and
END are the bounding index designators, and the keyword FILTER-FUNCTION is
usable to apply a function to the quantized value. The arguments of that
function are the vector index and the quantized value."))

(defun foreign-circular-shift (ptr type size n)
  (declare (type positive-fixnum size) (type fixnum n))
  (let ((m (abs n)))
    (declare (type fixnum m))
    (when (> m size)
      (setf n (mod n size) m (abs n)))
    (unless (zerop m)
      (when (> m (ash size -1))
        ;; Change direction to reduce the space for the temporary buffer.
        (if (plusp n)
            (setf n (- n size) m (- n))
            (setf n (+ n size) m n)))
      (let* ((tsize (cffi:foreign-type-size type))
             (bytes (* m tsize))
             (len (* size tsize)))
        (declare (type fixnum tsize bytes len))
        (flet ((circshift (ptr tmp bytes len n)
                 (if (> n 0)
                     (cffi:foreign-funcall "circular_rshift" :pointer ptr
                       :pointer tmp :unsigned-int bytes :unsigned-int len :void)
                     (cffi:foreign-funcall "circular_lshift" :pointer ptr
                       :pointer tmp :unsigned-int bytes :unsigned-int len :void))))
          (if (rt-thread-p)
              (let ((tmp (incudine.external:foreign-alloc-ex bytes
                           incudine.util::*foreign-rt-memory-pool*)))
                (unwind-protect (circshift ptr tmp bytes len n)
                  (foreign-rt-free tmp)))
              (cffi:with-foreign-pointer (tmp bytes)
                (circshift ptr tmp bytes len n))))))))

(defmacro quantize-vector (vec from start end filter-function fget flen data
                           &optional type)
  (with-gensyms (len i value maybe-coerce)
    `(let ((,len (,flen ,vec))
           (,end (or ,end (,flen ,vec))))
       (if (or (>= ,start ,len) (> ,end ,len))
           (incudine-error "Cannot quantize from ~D to ~D because data size is ~D"
                           ,start ,end ,len)
           (do ((,i start (1+ ,i)))
               ((>= ,i ,end) ,vec)
             (declare (fixnum ,i))
             (let ((,value (quantize (,fget ,data ,i) ,from)))
               (flet ((,maybe-coerce (,value)
                        ,(if type `(coerce ,value ',type) value)))
                 (setf (,fget ,data ,i)
                       (,maybe-coerce
                         (if ,filter-function
                             (funcall ,filter-function ,i ,value)
                             ,value))))))))))

;; Quantization with respect to a vector in sorted order.
(defmacro quantize-from-vector (value vec fget flen data)
  (with-gensyms (start end i prev curr next %curr-delta curr-delta)
    `(labels ((rec (,start ,end)
                (declare (fixnum ,start ,end))
                (let ((,i (+ ,start (ash (- ,end ,start) -1))))
                  (declare (fixnum ,i))
                  (cond ((= ,i ,start) ,i)
                        ((= ,i (1- ,end))
                         (if (< (abs (- ,value (,fget ,data (1- ,i))))
                                (abs (- ,value (,fget ,data ,i))))
                             (1- ,i)
                             ,i))
                        (t
                         (let* ((,prev (,fget ,data (1- ,i)))
                                (,curr (,fget ,data ,i))
                                (,next (,fget ,data (1+ ,i)))
                                (,%curr-delta (- ,value ,curr))
                                (,curr-delta (abs ,%curr-delta)))
                           (if (and (< ,prev ,value) (/= ,prev ,curr) (<= ,value ,next))
                               (cond ((< (- ,value ,prev) ,curr-delta) (1- ,i))
                                     ((< (- ,next ,value) ,curr-delta) (1+ ,i))
                                     (t ,i))
                               (if (>= ,curr ,value)
                                   (rec ,start ,i)
                                   (rec ,i ,end)))))))))
       (,fget ,data (rec 0 (,flen ,vec))))))

(defmethod quantize ((obj real) (from real) &key)
  (* from (round (/ obj from))))

(defmethod quantize ((obj real) (from simple-vector) &key)
  (quantize-from-vector obj from svref length from))

(defmethod quantize ((obj real) (from simple-array) &key)
  (quantize-from-vector obj from aref length from))

(defmethod quantize ((obj simple-vector) (from real)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function svref length obj))

(defmethod quantize ((obj simple-vector) (from simple-vector)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function svref length obj))

(defmethod quantize ((obj simple-vector) (from simple-array)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function svref length obj))

(defmethod quantize ((obj simple-array) (from real)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function aref length obj))

(defmethod quantize ((obj simple-array) (from simple-vector)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function aref length obj))

(defmethod quantize ((obj simple-array) (from simple-array)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function aref length obj))

;;; If a item in SLOT-NAMES list is a list, the format of that item is
;;;
;;;     (copy-func slot-name [optional-args-for-copy-func])
;;;
;;; where copy-func is the function name used to create a copy of the
;;; content referred to SLOT-NAME.  For example:
;;;
;;;    (copy-struct-slots name (x y (copy-seq list-of-things) z) src dest)
;;;
;;; after macroexpansion:
;;;
;;;    (progn
;;;      (setf (name-x dest) (name-x src))
;;;      (setf (name-y dest) (name-y src))
;;;      (setf (name-list-of-things dest) (copy-seq (name-list-of-things src)))
;;;      (setf (name-z dest) (name-z src)))
;;;
(defmacro copy-struct-slots (struct-name slot-names from to)
  `(progn
     ,@(flet ((format-name (name)
                (format-symbol *package* "~A-~A" struct-name name)))
         (mapcar (lambda (slot-name)
                   (multiple-value-bind (name src)
                       (if (listp slot-name)
                           (let ((name (format-name (cadr slot-name))))
                             (values name `(,(car slot-name) (,name ,from)
                                            ,@(cddr slot-name))))
                           (let ((name (format-name slot-name)))
                             (values name `(,name ,from))))
                     `(setf (,name ,to) ,src)))
                 slot-names))))

(defun |#t-reader| (stream subchar arg)
  (declare (type stream stream) (ignore subchar))
  (let* ((arg-index (or (car (read-delimited-list #\( stream t))
                        0))
         (form (read-delimited-list #\) stream t))
         (beg (subseq form 0 (+ arg-index 1)))
         (end (subseq form (+ arg-index 2))))
    (labels ((rec (i)
               (if (= i 1)
                   form
                   (append beg (list* (rec (1- i)) end)))))
      (rec (or arg 1)))))

(defun set-sharp-t-syntax ()
  (set-dispatch-macro-character #\# #\t #'|#t-reader|))

(defun add-sharp-t-syntax ()
  (setf *readtable* (copy-readtable *readtable*))
  (set-sharp-t-syntax))

(pushnew #'add-sharp-t-syntax *initialize-hook*)

(defmacro enable-sharp-t-syntax ()
  "Enable sharp-t reader syntax, useful to apply a filter multiple times.

Example: apply a pole filter four times (4t)

    #4t(pole in coef)

is equivalent to

    (pole (pole (pole (pole in coef) coef) coef) coef)

Often the input of a filter is the first argument, but if it is not
the case, an integer after sharp-t specifies the position of the input
in the argument list starting from zero. Example:

    #4t1(fname x in y)

is equivalent to

    (fname x (fname x (fname x (fname x in y) y) y) y)"
  `(eval-when (:compile-toplevel :execute)
     (add-sharp-t-syntax)))
