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

(in-package :incudine.util)

;;; TYPES

;; NON-NEGATIVE-FIXNUM64 used to get a better optimization
;; on 64bit machines. MOST-POSITIVE-FIXNUM 2^59-1 is good
;; at least with SBCL and CCL.
(define-constant most-positive-fixnum64 (1- (ash 1 59)))

(deftype non-negative-fixnum64 () '(integer 0 #.(1- (ash 1 59))))

;; FLOAT with arbitrary range between -2^63 and 2^63.
;; Used in SBCL on X86 with SIN, COS and TAN.
(deftype limited-sample () (let ((high (force-sample-format 4.0e18)))
                             `(,*sample-type* ,(- high) ,high)))

(deftype maybe-limited-sample () #+(and sbcl x86) 'limited-sample
                                 #-(and sbcl x86) 'sample)

(deftype channel-number () '(integer 0 1023))

(deftype bus-number () '(integer 0 16383))

;;; MISC

(defvar *dummy-function-without-args* (lambda ()))
(declaim (type function *dummy-function-without-args*))

(defvar *sf-metadata-keywords*
  '(:title :copyright :software :artist :comment
    :date :album :license :tracknumber :genre))

(declaim (inline incudine-version))
(defun incudine-version ()
  #.(format nil "~D.~D.~D"
            incudine.config::+incudine-major+
            incudine.config::+incudine-minor+
            incudine.config::+incudine-patch+))

(defmacro with-ensure-symbols (names &body forms)
  `(let ,(mapcar (lambda (name) `(,name (ensure-symbol ,(symbol-name name))))
                 names)
     ,@forms))

;;; Defined as macro to reduce the inlined functions inside the
;;; definition of a DSP
(defmacro sample (number)
  `(coerce ,number 'sample))

(defun apply-sample-coerce (form)
  (if (atom form)
      (cond ((and (numberp form) (floatp form))
             (force-sample-format form))
            ((eq form 'pi) '(sample pi))
            (t form))
      (cons (apply-sample-coerce (car form))
            (apply-sample-coerce (cdr form)))))

(defun alloc-multi-channel-data (channels size)
  (declare (type channel-number channels) (type positive-fixnum size))
  (let ((ptr (cffi:foreign-alloc :pointer :count channels)))
    (dotimes (ch channels ptr)
      (declare (type channel-number ch))
      (setf (cffi:mem-aref ptr :pointer ch)
            (foreign-alloc-sample size)))))

(defun free-multi-channel-data (data channels)
  (dotimes (ch channels)
    (let ((frame (cffi:mem-aref data :pointer ch)))
      (unless (cffi:null-pointer-p frame)
        (foreign-free frame))))
  (foreign-free data)
  (values))

(defmacro dochannels ((var channels &optional (result nil))
                      &body body)
  `(do ((,var 0 (1+ ,var)))
       ((>= ,var ,channels) ,result)
     (declare (type channel-number ,var))
     ,@body))

(declaim (inline lin->db))
(defun lin->db (x)
  (let ((in (if (zerop x) least-positive-sample x)))
    (* (log in) #.(/ (sample 20) (log (sample 10))))))

(declaim (inline db->lin))
(defun db->lin (x)
  (expt (sample 10) (* x (sample 0.05))))

(declaim (inline linear-interp))
(defun linear-interp (in y0 y1)
  (+ y0 (* in (- y1 y0))))

(declaim (inline cos-interp))
(defun cos-interp (in y0 y1)
  (linear-interp (* (- 1 (cos (the limited-sample (* in (sample pi)))))
                    0.5) y0 y1))

;;; Catmull-Rom spline
(declaim (inline cubic-interp))
(defun cubic-interp (in y0 y1 y2 y3)
  (let ((a0 (+ (* -0.5 y0) (* 1.5 y1) (* -1.5 y2) (* 0.5 y3)))
        (a1 (+ y0 (* -2.5 y1) (* 2.0 y2) (* -0.5 y3)))
        (a2 (+ (* -0.5 y0) (* 0.5 y2))))
    (+ (* in (+ (* in (+ (* a0 in) a1)) a2)) y1)))

(declaim (inline t60->pole))
(defun t60->pole (time)
  "Return a real pole for a 60dB exponential decay in TIME seconds."
  (if (plusp time)
      ;; tau = time / log(0.001) = time / 6.9077
      (exp (* *sample-duration* (/ +log001+ time)))
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

(declaim (inline set-sample-rate))
(defun set-sample-rate (value)
  (setf *sample-rate* (sample value)
        *sample-duration* (/ 1.0 *sample-rate*))
  (mapc #'funcall sample-rate-hook)
  (mapc #'funcall sample-duration-hook)
  *sample-rate*)

(declaim (inline set-sample-duration))
(defun set-sample-duration (value)
  (setf *sample-duration* (sample value)
        *sample-rate* (/ 1.0 *sample-duration*))
  (mapc #'funcall sample-rate-hook)
  (mapc #'funcall sample-duration-hook)
  *sample-duration*)

(declaim (inline set-sound-velocity))
(defun set-sound-velocity (value)
  (setf *sound-velocity*   (sample value)
        *r-sound-velocity* (/ 1.0 *sound-velocity*))
  (mapc #'funcall sound-velocity-hook)
  *sound-velocity*)

(declaim (inline sample->fixnum))
(defun sample->fixnum (x &key roundp)
  (declare (type (sample
                  #.(coerce (ash most-negative-fixnum -1) 'sample)
                  #.(coerce (ash most-positive-fixnum -1) 'sample)) x)
           (type boolean roundp))
  (multiple-value-bind (result rem) (if roundp (round x) (floor x))
    (declare (ignore rem))
    result))

(declaim (inline sample->int))
(defun sample->int (x)
  (declare (type sample x))
  (multiple-value-bind (result rem) (floor x)
    (declare (ignore rem))
    result))

(declaim (inline float->fixnum))
(defun float->fixnum (x &key roundp)
  (declare (type (or (single-float
                      #.(coerce (ash most-negative-fixnum -1) 'single-float)
                      #.(coerce (ash most-positive-fixnum -1) 'single-float))
                     (double-float
                      #.(coerce (ash most-negative-fixnum -1) 'double-float)
                      #.(coerce (ash most-positive-fixnum -1) 'double-float)))
                 x)
           (type boolean roundp))
  (multiple-value-bind (result rem) (if roundp (round x) (floor x))
    (declare (ignore rem))
    result))

(declaim (inline calc-lobits))
(defun calc-lobits (size)
  (declare (type non-negative-fixnum size))
  (if (>= size +table-maxlen+)
      0
      (do ((i size (ash i 1))
           (lobits 0 (1+ lobits)))
          ((plusp (logand i +table-maxlen+)) lobits)
        (declare (type non-negative-fixnum i lobits)))))

(declaim (inline declare-form-p))
(defun declare-form-p (lst)
  (eq (car lst) 'declare))

(declaim (inline separate-declaration))
(defun separate-declaration (form)
  (let (acc)
    (do ((l form (cdr l)))
        ((null (and (consp (car l)) (declare-form-p (car l))))
         (values (nreverse acc) l))
      (push (car l) acc))))

(declaim (inline rt-thread-p))
(defun rt-thread-p ()
  (eq (bt:current-thread) *rt-thread*))

(declaim (inline allow-rt-memory-p))
(defun allow-rt-memory-p ()
  (and (rt-thread-p) *allow-rt-memory-pool-p*))

(defmacro smp-ref (samples index)
  `(mem-ref ,samples 'sample (the non-negative-fixnum
                                  (* ,index +foreign-sample-size+))))

(macrolet ((define-*-ref (name type)
             `(defmacro ,name (ptr index)
                `(mem-ref ,ptr ,',type
                          (the non-negative-fixnum
                               (* ,index ,(cffi:foreign-type-size ,type)))))))
  (define-*-ref i8-ref :int8)
  (define-*-ref u8-ref :uint8)
  (define-*-ref i16-ref :int16)
  (define-*-ref u16-ref :uint16)
  (define-*-ref i32-ref :int32)
  (define-*-ref u32-ref :uint32)
  (define-*-ref i64-ref :int64)
  (define-*-ref u64-ref :uint64)
  (define-*-ref f32-ref :float)
  (define-*-ref f64-ref :double)
  (define-*-ref ptr-ref :pointer))

(defmacro with-complex (real-and-imag-vars pointer &body body)
  `(symbol-macrolet ((,(car real-and-imag-vars) (smp-ref ,pointer 0))
                     (,(cadr real-and-imag-vars) (smp-ref ,pointer 1)))
     ,@body))

(defmacro do-complex ((realpart-var imagpart-var pointer size) &body body)
  (with-gensyms (i addr)
    `(do ((,i 0 (1+ ,i))
          (,addr (pointer-address ,pointer)
                 (pointer-address (inc-pointer (make-pointer ,addr)
                                               +foreign-complex-size+))))
         ((>= ,i ,size))
       (declare (type unsigned-byte ,i))
       (symbol-macrolet ((,realpart-var (foreign-slot-value (make-pointer ,addr)
                                          '(:struct sample-complex) 'realpart))
                         (,imagpart-var (foreign-slot-value (make-pointer ,addr)
                                          '(:struct sample-complex) 'imagpart)))
         ,@body))))

(defmacro with-doc-string ((var form) &body body)
  `(multiple-value-bind (,var ,form)
       (if (stringp (car ,form))
           (values (list (car ,form)) (cdr ,form))
           (values nil ,form))
     ,@body))

(defun lambda*-arguments-without-dot (arguments)
  (do ((acc nil (cons (car arg) acc))
       (arg arguments (cdr arg)))
      ((atom (cdr arg))
       (if (cdr arg)
           ;; Replace dot with &rest
           (nreverse (list* (cdr arg) '&rest (car arg) acc))
           arguments))))

(defun lambda*-arguments (keys vals)
  (cond ((or (null vals) (keywordp (car vals))) vals)
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
  (with-doc-string (doc-string body)
    (with-lambda*-arguments (args kargs aux-var args)
      `(lambda (&rest optional-keywords &aux (,aux-var ',args))
         ,@doc-string
         (declare (ignore ,aux-var))
         ,(defun*-body args kargs body)))))

;;; DEFMACRO*-BODY is also used in local VUG with :DEFAULTS spec.
(defun defmacro*-body (optkey-var args kargs body)
  `(with-gensyms (kname)
     `(macrolet ((,kname (,(cons '&key ',kargs)) ,',@body))
        (,kname ,(lambda*-arguments ',(lambda*-list-keywords args)
                                    ,optkey-var)))))

(defmacro defmacro* (name (&rest args) &body body)
  (with-doc-string (doc-string body)
    (with-lambda*-arguments (args kargs aux-var args)
      `(defmacro ,name (&rest optional-keywords &aux (,aux-var ',args))
         ,@doc-string
         (declare (ignore ,aux-var))
         ,(defmacro*-body 'optional-keywords args kargs body)))))

(defun lambda-list-to-star-list (arglist)
  "Return the optional-key arguments of ARGLIST."
  (let ((aux-pos (position '&aux arglist)))
    (when aux-pos
      (let ((arglist (assoc "LAMBDA-LIST" (cdr (subseq arglist aux-pos))
                            :key #'symbol-name :test #'string=)))
        (when arglist
          (cons '&any (cadadr arglist)))))))

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
  (when (or force-p *init-p*)
    (call-hooks "Incudine initialization" *initialize-hook*)
    (setf *init-p* nil)
    t))

(defvar *sample-counter* (foreign-alloc 'sample :initial-element +sample-zero+))
(declaim (type foreign-pointer *sample-counter*))

(declaim (inline now))
(defun now ()
  (mem-ref *sample-counter* 'sample))

(defvar *portmidi-time* (foreign-alloc 'sample :initial-element +sample-zero+))
(declaim (type foreign-pointer *portmidi-time*))

(declaim (inline inc-portmidi-time))
(defun inc-portmidi-time (delta)
  (incf (smp-ref *portmidi-time* 0) delta))

(declaim (inline portmidi-time))
(defun portmidi-time ()
  (the non-negative-fixnum
    ;; 6 days: (log (* 6 24 3600 1000) 2) => 28.94949
    ;; If you want more days on a 32 bit arch, restart the timer of portmidi.
    (floor (the (sample #.+sample-zero+ #.(sample most-positive-fixnum))
             (smp-ref *portmidi-time* 0)))))

;;; FUNCTIONS are generated by the function defined with DSP!.
;;; The last function is an arbitrary function without arguments;
;;; it is useful to define recursive sequences.
(defmacro dsp-seq (&rest functions)
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
      (%dsp-seq functions))))

(defgeneric circular-shift (obj n)
  (:documentation "Perform a circular shift of length N."))

(defmethod free ((obj t)) (values))

(defgeneric free-p (obj)
  (:documentation "Return T if the object obj is deallocated."))

(defmethod free ((obj list))
  (dolist (x obj) (free x)))

(defmethod free ((obj function))
  (funcall obj)
  (values))

(defgeneric free-p (obj)
  (:documentation "Return T if the object obj is deallocated."))

(defvar *to-free*)

(defmacro with-cleanup (&body body)
  "All the objects with finalizer INCUDINE-FINALIZE and method INCUDINE:FREE
instantiated within BODY are invalid beyond the dynamic extent of BODY."
  `(let ((*to-free* nil))
     (unwind-protect
          (progn ,@body)
       (when *to-free*
         (free (the list *to-free*))
         (if (allow-rt-memory-p)
             (incudine.util:rt-global-pool-push-list *to-free*)
             (incudine.util:nrt-global-pool-push-list *to-free*))))))

(declaim (inline dynamic-incudine-finalizer-p))
(defun dynamic-incudine-finalizer-p ()
  (boundp '*to-free*))

(defun incudine-finalize (obj function &optional (dynamic-p t))
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
  (incudine.util::cancel-finalization obj))

(defmacro maybe-unwind-protect (protected &body cleanup)
  (with-gensyms (func)
    `(flet ((,func () ,protected))
       (if (dynamic-incudine-finalizer-p)
           (,func)
           (unwind-protect (,func) ,@cleanup)))))

(defgeneric quantize (obj from &key)
  (:documentation "Quantize OBJ with respect to a real number, a vector
or a BUFFER-BASE structure (i.e. BUFFER or TUNING) in sorted order.
If OBJ is a vector or a BUFFER-BASE structure, the keywords START and END
are the bounding index designators, and the keyword FILTER-FUNCTION is
usable to apply a function to the quantized value.  The arguments of that
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
              (let ((tmp (incudine.external:foreign-rt-alloc-ex bytes
                           incudine.util::*foreign-rt-memory-pool*)))
                (unwind-protect (circshift ptr tmp bytes len n)
                  (foreign-rt-free tmp)))
              (cffi:with-foreign-pointer (tmp bytes)
                (circshift ptr tmp bytes len n))))))))

(declaim (inline sort-samples))
(defun sort-samples (pointer size)
  (incudine.external:qsort pointer size +foreign-sample-size+
                           (cffi:callback incudine.external::sample-cmp)))

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

;;; The read macro #T is useful to apply a filter multiple times.
;;; Example: apply a pole filter four times (4t)
;;;
;;;     #4t(pole in coef)
;;;
;;; is equivalent to
;;;
;;;     (pole (pole (pole (pole in coef) coef) coef) coef)
;;;
;;; Often the input of a filter is the first argument, but if it is
;;; not the case, a number after sharp-t is the position of the input
;;; in the list of the arguments starting from zero. Example:
;;;
;;;     #4t1(fname x in y)
;;;
;;; is equivalent to
;;;
;;;     (fname x (fname x (fname x (fname x in y) y) y) y)
;;;
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
  `(eval-when (:compile-toplevel :execute)
     (add-sharp-t-syntax)))
