;;; Incudine version of CLM
;;; Copyright (c) 2017 Tito Latini
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

(in-package :cudere-clm.sys)

(define-condition cudere-clm-error (incudine-simple-error) ())

(defstruct (clm-ugen-instance (:include ugen-instance) (:copier nil)))

(defmacro define-clm-ugen (name return-type lambda-list &body body)
  (let ((struct-name (format-symbol *package* "~A-INSTANCE" name))
        (predicate (format-symbol *package* "~A?" name))
        (ugen-name (alexandria:format-symbol *clm-ugens-package* "~A" name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,struct-name (:include clm-ugen-instance) (:copier nil)
                                  (:predicate ,predicate)))
         (define-ugen ,ugen-name ,return-type ,lambda-list ,@body))
       ',name)))

(defmacro increment-and-wrap-phase (phase-var inc &optional (max-phase +twopi+)
                                    value-var (trig-value (sample 1))
                                    no-trig-value)
  (multiple-value-bind (set-trig-on t-test)
      (when value-var
        (values `((setf ,value-var ,trig-value))
                (when no-trig-value
                  `((t (setf ,value-var ,no-trig-value))))))
    `(progn
       (incf ,phase-var ,inc)
       ;; First pass.
       (cond ((>= ,phase-var ,max-phase) (decf ,phase-var ,max-phase)
              ,@set-trig-on)
             ((minusp ,phase-var) (incf ,phase-var ,max-phase)
              ,@set-trig-on)
             ,@t-test)
       ;; Recheck and division.
       (when (or (minusp ,phase-var) (> ,phase-var ,max-phase))
         (setf phase (mod phase ,max-phase))))))

(defmacro ugen-tick (gen &optional (return-type 'sample))
  `(progn
     (funcall (ugen-perf-function ,gen))
     ,@(when (vug::foreign-type-p return-type)
         `((cffi:mem-ref (ugen-return-pointer ,gen) ',return-type)))))

(defmacro silence-compiler-if-no-args (fname lambda-list &body body)
  (with-gensyms (opt-args form)
    `(define-compiler-macro ,fname (&whole ,form ,@lambda-list &rest ,opt-args)
       (if ,opt-args ,form ,@body))))

(defmacro mus-frequency-method-from-radians (name)
  (let ((instance-type (format-symbol *package* "~A-INSTANCE" name))
        (increment-getter (format-symbol *package* "~A-INC" name))
        (increment-setter (format-symbol *package* "SET-~A-INC" name)))
    `(progn
       (defmethod mus-frequency ((gen ,instance-type))
         (radians->hz (,increment-getter gen)))
       (defmethod (setf mus-frequency) (hz (gen ,instance-type))
         (,increment-setter gen (hz->radians hz))
         hz))))

(macrolet ((defgenerics (&rest args)
             `(progn ,@(mapcar (lambda (x) `(defgeneric ,@x)) args))))
  (defgenerics
    (mus-channel (gen))
    (mus-channels (gen))
    (mus-close (obj))
    (mus-data (gen)) ((setf mus-data) (value gen))
    (mus-file-name (gen))
    (mus-feedback (gen)) ((setf mus-feedback) (value gen))
    (mus-feedforward (gen)) ((setf mus-feedforward) (value gen))
    (mus-frequency (gen)) ((setf mus-frequency) (hz gen))
    (mus-hop (gen)) ((setf mus-hop) (value gen))
    (mus-increment (gen)) ((setf mus-increment) (radians-per-sample gen))
    (mus-input? (obj))
    (mus-interp-type (gen))
    (mus-length (gen)) ((setf mus-length) (value gen))
    (mus-location (gen)) ((setf mus-location) (pos gen))
    (mus-offset (gen))
    (mus-order (gen))
    (mus-output? (obj))
    (mus-phase (gen)) ((setf mus-phase) (rad gen))
    (mus-ramp (gen)) ((setf mus-ramp) (value gen))
    (mus-reset (gen))
    (mus-safety (gen)) ((setf mus-safety) (value gen))
    (mus-scaler (gen)) ((setf mus-scaler) (value gen))
    (mus-xcoeff (gen loc)) ((setf mus-xcoeff) (value gen loc))
    (mus-xcoeffs (gen))
    (mus-ycoeff (gen loc)) ((setf mus-ycoeff) (value gen loc))
    (mus-ycoeffs (gen))
    (mus-width (gen)) ((setf mus-width) (value gen))))

(defmethod mus-channels ((gen soundfile:stream))
  (soundfile:channels gen))

(defmethod mus-channels ((gen string))
  (soundfile:channels gen))

(defmethod mus-close ((obj soundfile:stream))
  (soundfile:close obj))

(defmethod mus-file-name ((gen soundfile:stream))
  (namestring (soundfile:path gen)))

(defmethod mus-file-name ((gen pathname)) gen)

(defmethod mus-file-name ((gen string)) gen)

(defmethod mus-increment ((gen soundfile:stream)) 1)
(defmethod (setf mus-increment) (value (gen soundfile:stream)) value)

(defmethod mus-input? ((obj soundfile:input-stream)) t)
(defmethod mus-input? ((obj t)) nil)

(defmethod mus-length ((gen soundfile:stream)) (soundfile:frames gen))
(defmethod mus-length ((gen ugen-instance)) 1)

(defmethod mus-location ((gen soundfile:input-stream))
  (soundfile:current-frame gen))

(defmethod (setf mus-location) (pos (gen soundfile:input-stream))
  (setf (soundfile:position gen) pos))

(defmethod mus-output? ((obj soundfile:output-stream)) t)
(defmethod mus-output? ((obj t)) nil)

(defmethod mus-safety ((gen soundfile:output-stream)) t)
(defmethod (setf mus-safety) (value (gen soundfile:output-stream)) value)

(defmacro def-optkey-fun (name (&rest args) &body body)
  `(defun* ,name ,args ,@body))

(defun declare-optimize-p (decl-list)
  (some (lambda (x) (find 'optimize (cdr x) :key #'car)) decl-list))

(defmacro %definstrument (name lambda-list def &body body)
  (multiple-value-bind (doc-string body)
      (if (stringp (car body))
          (values (list (car body)) (cdr body))
          (values nil body))
    (multiple-value-bind (decl body)
        (incudine.util::separate-declaration body)
      `(progn
         ,(if *definstrument-hook*
              (funcall *definstrument-hook* name lambda-list)
              `(eval-when (:load-toplevel)
                 (when *definstrument-hook*
                   (eval (funcall *definstrument-hook* ',name ',lambda-list)))))
         (,def ,name ,lambda-list
           ,@doc-string
           ,@decl
           (locally ,@(when (and *clm-optimize-settings*
                                 (not (declare-optimize-p decl)))
                        `((declare ,*clm-optimize-settings*)))
             (with-cleanup ,@body))
           (values (compile ',name)))))))

(defmacro definstrument (name lambda-list &body body)
  `(%definstrument ,name ,lambda-list defun ,@body))

(defmacro definstrument* (name lambda-list &body body)
  `(%definstrument ,name ,lambda-list defun* ,@body))

(declaim (inline double-float))
(defun double-float (x) (coerce x 'double-float))

(declaim (inline double))
(defun double (x) (coerce x 'double-float))

(defun coerce-clm-struct-fields (fields)
  (loop for f in fields
            collect (if (and (consp f) (typep (second f) 'single-float))
                        (list (first f) (double (second f)))
                        f)))

(defmacro def-clm-struct (name &rest fields)
  `(defstruct ,name ,@(coerce-clm-struct-fields fields)))

;;; We try to fix the declarations in (loop ... do (declare ...))
;;; used in the original CLM for the C code.
(defun fix-run-loop-do-declare (form)
  (let ((sample-vars nil))
    (labels ((parsing (form)
               (if (consp form)
                   (case (car form)
                     (quote form)
                     (loop (fix-loop form))
                     (otherwise (cons (parsing (car form))
                                      (parsing (cdr form)))))
                   form))
             (merge-declarations (lst)
               (do ((acc (list (cdr (car lst))) (cons (cdar next) acc))
                    (next (cdr lst) (cdr next)))
                   ((not (eq (caar next) 'declare))
                    (values (apply #'append '(declare) (nreverse acc)) next))))
             (fix-loop (form)
               (let ((decl nil))
                 (loop for l on (cdr form) by #'cddr
                       for (k v) = l do
                         (when (and (eq k 'do) (consp v) (eq (car v) 'declare))
                           (multiple-value-bind (d r)
                               (merge-declarations (cdr l))
                             (setf decl d)
                             (rplacd l (cons '(values) r)))
                           (return)))
                 (if decl
                     (list 'locally (filter-types decl)
                           (if sample-vars
                               `(with-samples
                                  ,(mapcar (lambda (x) `(,x ,x)) sample-vars)
                                  ,form)
                               form))
                     form)))
             (filter-types (decl)
               (cons 'declare
                     (loop for x in (cdr decl)
                           if (and (eq (car x) 'type)
                                   (member (cadr x) '(fixnum :boolean)))
                             collect (if (eq (cadr x) :boolean)
                                         (list* 'type 'boolean (cddr x))
                                         x)
                           else if (and (eq (car x) 'type)
                                        (member (cadr x)
                                                '(float :float :double :real)))
                             do (setf sample-vars (nconc sample-vars (cddr x))))))
             (coerce-float-numbers (form)
               (typecase form
                 (cons (cons (coerce-float-numbers (car form))
                             (coerce-float-numbers (cdr form))))
                 (single-float (coerce form 'double-float))
                 (otherwise form))))
      (coerce-float-numbers (parsing form)))))

;;; RUN macro is unnecessary in cudere-clm but it is useful to compile
;;; the original CLM instruments.
(defmacro run (run-baby-run)
  `(progn ,(fix-run-loop-do-declare run-baby-run)))

(defmacro run* (vars body)
  (declare (ignore vars))
  `(progn ,(fix-run-loop-do-declare body)))

(declaim (inline hz->radians))
(defun hz->radians (value)
  (* value +twopi+ (/ *clm-srate*)))

(declaim (inline radians->hz))
(defun radians->hz (value)
  (* +rtwopi+ value *clm-srate*))

;;; Copied from clm-5/mus.lisp
(defun seconds->samples (&rest args)
  (if (= (length args) 1)
      (round (* (first args) *clm-srate*))
      (mapcar (lambda (x) (round (* x *clm-srate*))) args)))

;;; Copied from clm-5/mus.lisp
(defun samples->seconds (&rest args)
  (if (= (length args) 1)
      (/ (first args) *clm-srate*)
      (mapcar (lambda (x) (/ x *clm-srate*)) args)))

;;; Copied from clm-5/mus.lisp
(defun times->samples (beg dur)
  (values (seconds->samples beg)
          (seconds->samples (+ beg dur))))

(define-constant +random-distribution-table-size+ 512)
(define-constant +random-distribution-envelope-size+ 50)

(declaim (inline mus-rand-seed))
(defun mus-rand-seed ()
  (incudine.util::seed-from-random-state *random-state*))

(declaim (inline mus-set-rand-seed))
(defun mus-set-rand-seed (seed)
  (seed-random-state seed))

(defsetf mus-rand-seed mus-set-rand-seed)

(declaim (inline clm-random))
(defun clm-random (amp)
  (random (coerce amp 'double-float)))

(declaim (inline centered-random))
(defun centered-random (amp)
  (* amp (- (random 2d0) 1d0)))

(declaim (inline mus-random))
(defun mus-random (amp)
  (centered-random amp))

(defun randomantic (amp distribution)
  (declare (type (or null (simple-array double-float (*))) distribution))
  (if distribution
      (* amp (aref distribution (random (length distribution))))
      (centered-random amp)))

(declaim (inline sqr))
(defun sqr (x) (* x x))

(declaim (inline ensure-double-float-contents))
(defun ensure-double-float-contents (seq)
  (if (every (lambda (x) (typep x 'double-float)) seq)
      seq
      (map 'vector #'double seq)))

(defun make-double-float-array (lim &key initial-contents initial-element)
  (macrolet ((new-array (&rest args)
               `(make-array lim :element-type 'double-float ,@args)))
    (if initial-contents
        (new-array :initial-contents (ensure-double-float-contents
                                       initial-contents))
        (new-array :initial-element (double (or initial-element 0d0))))))

(defmacro make-double-array (lim &key initial-contents initial-element)
  `(make-double-float-array ,lim :initial-contents ,initial-contents
                            :initial-element ,initial-element))

(defun make-integer-array (len &key initial-contents initial-element)
  (macrolet ((new-array (&rest args)
               `(make-array len :element-type '(signed-byte 32) ,@args)))
    (if initial-contents
        (new-array :initial-contents initial-contents)
        (new-array :initial-element (or initial-element 0)))))

(declaim (inline clear-floats))
(defun clear-floats (double-float-array len)
  (dotimes (i len)
    (setf (aref double-float-array i) 0d0)))

(declaim (inline clear-array))
(defun clear-array (double-float-array)
  (clear-floats double-float-array (length double-float-array)))

(declaim (inline all-pass-interp))
(defun all-pass-interp (in y0 y1 yn1)
  (+ (* in y0) (* (- 1 in) (- y1 yn1))))

(declaim (inline lagrange-interp))
(defun lagrange-interp (in y0 y1 y2)
  (let ((in2 (* in in)))
    (+ (* 0.5 y0 (- in2 in)) (* (- 1 in2) y1) (* 0.5 y2 (+ in in2)))))

(declaim (inline bezier-interp))
(defun bezier-interp (in y0 y1 y2 y3)
  (let* ((p (* (1+ in) 1/3))
         (cy (* 3 (- y1 y0)))
         (by (- (* 3 (- y2 y1)) cy))
         (ay (- y3 y0 cy by)))
    (+ y0 (* p (+ cy (* p (+ by (* p ay))))))))

(defmacro with-interp-value ((int frac size value) &body body)
  `(multiple-value-bind (,int ,frac) (floor ,value)
     (declare (type fixnum ,int))
     (when (or (minusp ,int) (>= ,int ,size))
       (setf ,int (mod ,int ,size)))
     ,@body))

(defmacro %no-interp (buf x size ref)
  `(,ref ,buf (mod (floor ,x) ,size)))

(defmacro two-points-interp (func buf x size ref)
  (with-gensyms (i j frac)
    `(with-interp-value (,i ,frac ,size ,x)
       (let ((,j (1+ ,i)))
         (declare (type non-negative-fixnum ,j))
         (,func ,frac (,ref ,buf ,i) (,ref ,buf (if (= ,j ,size) 0 ,j)))))))

(defmacro three-points-interp (func buf x size ref)
  (with-gensyms (i j frac)
    `(with-interp-value (,i ,frac ,size ,x)
       (let ((,j (1+ ,i)))
         (declare (type non-negative-fixnum ,j))
         (,func ,frac (,ref ,buf (1- (if (= ,i 0) ,size ,i)))
                (,ref ,buf ,i) (,ref ,buf (if (= ,j ,size) 0 ,j)))))))

(defmacro four-points-interp (func buf x size ref)
  (with-gensyms (i j k frac last)
    `(with-interp-value (,i ,frac ,size ,x)
       (let* ((,last (1- ,size))
              (,j (if (= ,i ,last) 0 (1+ ,i)))
              (,k (1+ ,j)))
         (declare (type non-negative-fixnum ,last ,j ,k))
         (,func ,frac (,ref ,buf (if (= ,i 0) ,last (1- ,i)))
                (,ref ,buf ,i) (,ref ,buf ,j)
                (,ref ,buf (if (= ,k ,size) 0 ,k)))))))

(defmacro %all-pass-interp (buf x size yn1 ref)
  (with-gensyms (i j frac)
    `(with-interp-value (,i ,frac ,size ,x)
       (let ((,j (1+ ,i)))
         (declare (type non-negative-fixnum ,j))
         (all-pass-interp ,frac (,ref ,buf ,i)
                          (,ref ,buf (if (= ,j ,size) 0 ,j))
                          ,yn1)))))

(declaim (inline frame-no-interp))
(defun frame-no-interp (frame x size)
  (declare (type foreign-pointer frame) (type real x)
           (type positive-fixnum size))
  (%no-interp frame x size smp-ref))

(declaim (inline frame-linear-interp))
(defun frame-linear-interp (frame x size)
  (declare (type foreign-pointer frame) (type real x)
           (type positive-fixnum size))
  (two-points-interp linear-interp frame x size smp-ref))

(declaim (inline frame-cos-interp))
(defun frame-cos-interp (frame x size)
  (declare (type foreign-pointer frame) (type real x)
           (type positive-fixnum size))
  (two-points-interp cos-interp frame x size smp-ref))

(declaim (inline frame-all-pass-interp))
(defun frame-all-pass-interp (frame x size yn1)
  (declare (type foreign-pointer frame) (type real x)
           (type positive-fixnum size))
  (%all-pass-interp frame x size yn1 smp-ref))

(declaim (inline frame-lagrange-interp))
(defun frame-lagrange-interp (frame x size)
  (declare (type foreign-pointer frame) (type real x)
           (type positive-fixnum size))
  (three-points-interp lagrange-interp frame x size smp-ref))

(declaim (inline frame-bezier-interp))
(defun frame-bezier-interp (frame x size)
  (declare (type foreign-pointer frame) (type real x)
           (type positive-fixnum size))
  (four-points-interp bezier-interp frame x size smp-ref))

(declaim (inline frame-hermite-interp))
(defun frame-hermite-interp (frame x size)
  (declare (type foreign-pointer frame) (type real x)
           (type positive-fixnum size))
  (four-points-interp cubic-interp frame x size smp-ref))

(defvar *frame-interp-functions*
  (vector #'frame-no-interp #'frame-linear-interp #'frame-cos-interp
          #'frame-all-pass-interp #'frame-lagrange-interp
          #'frame-bezier-interp #'frame-hermite-interp))
(declaim (type (simple-vector 7) *frame-interp-functions*))

(declaim (inline array-no-interp))
(defun array-no-interp (array x size)
  (declare (type real x) (type positive-fixnum size))
  (%no-interp array x size aref))

(declaim (inline array-linear-interp))
(defun array-linear-interp (array x size)
  (declare (type real x) (type positive-fixnum size))
  (two-points-interp linear-interp array x size aref))

(declaim (inline array-cos-interp))
(defun array-cos-interp (array x size)
  (declare (type real x) (type positive-fixnum size))
  (two-points-interp cos-interp array x size aref))

(declaim (inline array-all-pass-interp))
(defun array-all-pass-interp (array x size yn1)
  (declare (type real x) (type positive-fixnum size))
  (%all-pass-interp array x size yn1 aref))

(declaim (inline array-lagrange-interp))
(defun array-lagrange-interp (array x size)
  (declare (type real x) (type positive-fixnum size))
  (three-points-interp lagrange-interp array x size aref))

(declaim (inline array-bezier-interp))
(defun array-bezier-interp (array x size)
  (declare (type real x) (type positive-fixnum size))
  (four-points-interp bezier-interp array x size aref))

(declaim (inline array-hermite-interp))
(defun array-hermite-interp (array x size)
  (declare (type real x) (type positive-fixnum size))
  (four-points-interp cubic-interp array x size aref))

(defvar *array-interp-functions*
  (vector #'array-no-interp #'array-linear-interp #'array-cos-interp
          #'array-all-pass-interp #'array-lagrange-interp
          #'array-bezier-interp #'array-hermite-interp))
(declaim (type (simple-vector 7) *array-interp-functions*))

(defvar *array-interp-function-names*
  (make-array 7
    :initial-contents '(array-no-interp array-linear-interp array-cos-interp
                        array-all-pass-interp array-lagrange-interp
                        array-bezier-interp array-hermite-interp)))

(declaim (inline array-interp))
(defun array-interp (array x &optional size)
  (array-linear-interp array x (or size (length array))))

(defun mus-interpolate (type x v &optional size y1)
  (declare (type mus-interp type))
  (let ((size (or size (length v))))
    (cond ((= type mus-interp-linear) (array-linear-interp v x size))
          ((= type mus-interp-all-pass) (array-all-pass-interp v x size y1))
          (t (funcall (svref *array-interp-functions* type) v x size)))))

(define-compiler-macro mus-interpolate (&whole form &environment env
                                        type x v &rest args)
  (if (constantp type env)
      (let ((type (if (numberp type)
                      type
                      (case type
                        (mus-interp-none 0)
                        (mus-interp-linear 1)
                        (mus-interp-sinusoidal 2)
                        (mus-interp-all-pass 3)
                        (mus-interp-lagrange 4)
                        (mus-interp-bezier 5)
                        (mus-interp-hermite 6)))))
        (if (numberp type)
            (let ((size (or (first args) `(length ,v))))
              `(,(svref *array-interp-function-names* type) ,v ,x ,size
                                                            ,@(cdr args)))
            form))
      form))

(defun %partials->wave (pl table norm)
  (let* ((table (or table (make-double-array *clm-table-size*)))
         (size (length table)))
    (with-buffer (b size :fill-function (gen:partials pl :normalize-p norm))
      (dotimes (i size)
        (setf (aref table i) (buffer-value b i))))
    table))

(defun partials->wave (synth-data &optional table (norm t))
  (%partials->wave (loop for (i a) on synth-data by #'cddr
                         collect (list i a))
                   table norm))

(defun phase-partials->wave (synth-data &optional table (norm t))
  (%partials->wave (loop for (i a p) on synth-data by #'cdddr
                         collect (list i a (* p +rtwopi+)))
                   table norm))

;;; Edited from clm-5/mus.lisp
(defun normalize-partials (partials)
  (let ((sum 0.0))
    (loop for i in (cdr partials) by #'cddr do (incf sum (abs i)))
    (when (zerop sum)
      (warn "all partials have 0.0 amplitude: ~A" partials))
    (setf sum (/ 1.0 sum))
    (loop for (p v) on partials by #'cddr
            collect p collect (* v sum))))

;;; Edited from clm-5/mus.lisp
(defun partials->polynomial (partials &optional (kind mus-chebyshev-first-kind))
  (let* ((partials (coerce partials 'list))
         (top (floor (loop for p in partials by #'cddr maximize p)))
	 (size (+ top 1))
	 (T0 (make-array size :element-type 'integer :initial-element 0))
	 (T1 (make-array size :element-type 'integer :initial-element 0))
	 (Tn (make-array size :element-type 'integer :initial-element 0))
	 (Cc1 (make-array size :element-type 'float :initial-element 0.0))
	 (amp 0.0)
         (first-kind-p (= kind mus-chebyshev-first-kind)))
    (setf (aref T0 0) (if first-kind-p 1 0))
    (setf (aref T1 1) 1)
    (loop for i from 1 to top do
            (setf amp (or (getf partials i) 0.0))
            (unless (zerop amp)
              (loop for k from (if first-kind-p 0 1) to i do
                   (incf (aref Cc1 (if first-kind-p k (1- k)))
                         (* amp (aref T1 k)))))
            (unless (= i top)
              (loop for k from (+ i 1) downto 1 do
                      (setf (aref Tn k)
                            (- (* 2 (aref T1 (- k 1))) (aref T0 k))))
              (setf (aref Tn 0) (- (aref T0 0)))
              (loop for k from (+ i 1) downto 0 do
                      (setf (aref T0 k) (aref T1 k))
                      (setf (aref T1 k) (aref Tn k)))))
    (let ((cc (make-double-array size)))
      (loop for i from 0 below size do
              (setf (aref cc i) (double (aref Cc1 i))))
      cc)))

;;; Edited from clm-5/mus.lisp
(declaim (inline polynomial))
(defun polynomial (coeffs x)
  (declare (type (simple-array double-float (*)) coeffs) (type real x))
  (let* ((top (1- (length coeffs)))
         (sum (aref coeffs top)))
    (declare (type double-float sum))
    (loop for i from (1- top) downto 0 do
            (setf sum (+ (* x sum) (aref coeffs i))))
    sum))

(defun file->array (file channel beg dur array)
  (declare (type (or string pathname) file)
           (type non-negative-fixnum channel)
           (type non-negative-fixnum64 beg dur))
  (with-open-soundfile (sf file)
    (when (> channel (1- (soundfile:channels sf)))
      (error 'cudere-clm-error
             :format-control "Can't read ~A channel ~D (file has ~D chans)"
             :format-arguments (list file channel (soundfile:channels sf))))
    (unless (>= beg (soundfile:frames sf))
      (let ((dur (min dur (- (soundfile:frames sf) beg) (length array))))
        (setf (soundfile:position sf) beg)
        (dotimes (i dur dur)
          (setf (aref array i) (soundfile:read-next sf channel))))))
  array)

(defun array->file (file data len srate channels)
  (let ((frames (min len (floor (/ (length data) channels))))
        (header-type (mus-to-sf-header-type *clm-header-type*))
        (data-format (mus-to-sf-data-format *clm-data-format*)))
    (with-open-soundfile (sf file :direction :output :if-exists :supersede
                          :sample-rate srate :channels channels
                          :header-type header-type
                          :data-format data-format)
      (let* ((len (* frames channels))
             (bufsize (min len *clm-file-buffer-size*))
             (start 0))
        (declare (type non-negative-fixnum bufsize)
                 (type non-negative-fixnum64 len start))
        (cffi:with-foreign-object (buf :double bufsize)
          (loop while (< start len) do
                  (loop for i below bufsize
                        for j from start below len do
                          (setf (cffi:mem-aref buf :double i) (aref data j))
                        finally
                          (incf start (soundfile:foreign-write sf buf i)))))))
    file))

;;; Edited from clm-5/mus.lisp
(defun frample->frample (m f res)
  (let* ((mx-chans (floor (sqrt (array-total-size m))))
	 (in-chans (min (array-total-size f) mx-chans))
	 (out-chans (min in-chans (array-total-size res))))
    (dotimes (i out-chans res)
      (setf (aref res i) 0d0)
      (dotimes (j in-chans)
	(incf (aref res i)
              (double (* (aref f j) (aref m (+ (* j mx-chans) i)))))))))

(defmacro delay-line-interp (line loc offset size type yn1)
  (with-gensyms (x)
    `(let ((,x (- ,loc ,offset)))
       (cond
         ((= ,type mus-interp-none)
          (aref ,line (mod (floor ,x) ,size)))
         ((= ,type mus-interp-linear)
          (array-linear-interp ,line ,x ,size))
         ((= ,type mus-interp-all-pass)
          (setf ,yn1 (array-all-pass-interp ,line ,x ,size ,yn1)))
         (t
          (funcall (aref *array-interp-functions* ,type) ,line ,x ,size))))))

(defun delay-fix-size (size max-size interp-type)
  (declare (type fixnum size max-size interp-type))
  (cond ((> size max-size) max-size)
        ((or (and (= interp-type mus-interp-none) (> size 0))
             (> size 3))
         size)
        (t
         (max size
              (cond ((= interp-type mus-interp-none) 1)
                    ((member interp-type
                             (list mus-interp-linear
                                   mus-interp-sinusoidal
                                   mus-interp-all-pass))
                     2)
                    ((= interp-type mus-interp-lagrange) 3)
                    (t 4))))))

(defun sort-breakpoint-list (lst)
  (loop for (x y) in (sort (loop for i on lst by #'cddr collect i)
                           #'< :key #'car)
        collect x collect y))

(defun* envelope->coeffs (order envelope)
  (with-cleanup
    (buffer->list (make-buffer order
                    :fill-function (gen:fir envelope :sample-rate 1.0)))))

(defun envelope-interp (x fn &optional (base 1))
  (labels ((rec (l x x0 y0 base)
             (if (or (null l) (<= x x0))
                 y0
                 (let ((x1 (first l))
                       (y1 (second l)))
                   (if (> x1 x)
                       (if (or (= y1 y0) (= base 0))
                           y0
                           (+ y0
                              (if (= base 1)
                                  (* (- x x0) (/ (- y1 y0) (- x1 x0)))
                                  (* (/ (- y1 y0) (- base 1.0))
                                     (- (expt base (/ (- x x0) (- x1 x0)))
                                        1.0)))))
                       (rec (cddr l) x x1 y1 base))))))
    (if (null fn)
        0.0
        (rec fn x (first fn) (second fn) base))))

(declaim (inline envelope-last-x))
(defun envelope-last-x (env)
  (if env (first (last env 2)) 0.0))

;;; Edited from clm-5/env.lisp
(defun x-norm (env xmax)
  (let ((scl (/ xmax (first (last env 2)))))
    (loop for (x y) on env by #'cddr
          collect (* x scl) collect y)))

;;; Edited from clm-5/env.lisp
(defun add-or-edit-breakpoint (fn x y)
  (if (null fn)
      (list x y)
      (let ((lim (length fn)))
        (loop for px in fn by #'cddr
              for i by 2
              when (>= px x) do
                (return-from add-or-edit-breakpoint
                  (cond ((= px x)
                         (if (= i 0)
                             (append (list x y) (subseq fn 2 lim))
                             (append (subseq fn 0 i) (list x y)
                                     (unless (>= (+ i 2) lim)
                                       (subseq fn (+ i 2) lim)))))
                        ((= i 0)
                         (append (list x y) fn))
                        (t
                         (append (subseq fn 0 i) (list x y) (subseq fn i lim))))))
        (append fn (list x y)))))

;;; Edited from clm-5/env.lisp
(defun reduce-amplitude-quantization-noise (e dur amp &optional (ramp-dur .5)
                                            (low-amp .005))
  (let ((new-e nil))
    (when (and e
	       (> (length e) 2)
	       (> dur ramp-dur)
	       (plusp amp))
      (let* ((x0 (first e))
	     (y0 (second e))
             (last-x (envelope-last-x e))
	     (x-dur (* ramp-dur (/ (- last-x x0) dur)))
	     (y-val (/ low-amp amp)))
	(loop for x1 in (cddr e) by #'cddr
              for y1 in (cdddr e) by #'cddr do
                (when (and (> (- x1 x0) x-dur) (/= y0 y1)
                           (or (< y0 y-val) (< y1 y-val)))
                  (flet ((interp (x) (max y-val (envelope-interp x e))))
                    (multiple-value-bind (x y)
                        (if (< y0 y-val)
                            (let ((x (if (< y0 y1) (+ x0 x-dur) (- x1 x-dur))))
                              (values x (min (max y0 y1) (interp x))))
                            (let ((x (- x1 x-dur)))
                              (values x (interp x))))
                      (setf new-e (add-or-edit-breakpoint
                                    (or new-e (copy-seq e)) x y)))))
                (setf x0 x1 y0 y1))))
    (or new-e e)))

(declaim (inline ensure-double-float-random-distribution))
(defun ensure-double-float-random-distribution (distribution)
  (when distribution
    (if (typep distribution '(simple-array double-float (*)))
        distribution
        (make-double-array (length distribution)
                           :initial-contents distribution))))

;;; Edited from clm-5/env.lisp
(defun inverse-integrate (dist &optional
                                 (data-size #.+random-distribution-table-size+)
                                 (e-size #.+random-distribution-envelope-size+))
  (declare (type cons dist) (type positive-fixnum data-size e-size))
  (let* ((e nil)
	 (sum (second dist))
	 (first-sum sum)
	 (data (make-double-array data-size))
	 (x0 (first dist))
	 (x1 (first (last dist 2))))
    (loop for i to e-size
          for x from x0 by (/ (- x1 x0) e-size) do
            (setf e (list* x sum e))
            (incf sum (envelope-interp x dist)))
    (let ((incr (/ (- (second e) first-sum) (- data-size 1))))
      (setf e (nreverse e))
      (loop for i below data-size
            for x from first-sum by incr do
              (setf (aref data i) (double (envelope-interp x e))))
      data)))

;;; Edited from clm-5/clm.c
(defun nsin-scaler (n)
  (labels ((find-nsin-scaler (n lo hi)
             (let ((mid (* (+ lo hi) .5))
                   (ylo (nsin-ns lo n))
                   (yhi (nsin-ns hi n)))
               (cond ((< (abs (- ylo yhi)) 1e-12)
                      (nsin-ns mid n))
                     ((> ylo yhi)
                      (find-nsin-scaler n lo mid))
                     (t
                      (find-nsin-scaler n mid hi)))))
           (nsin-ns (x n)
             (let* ((a2 (* x .5))
                    (den (sin a2)))
               (if (zerop den)
                   0d0
                   (/ (* (sin (* n a2))
                         (sin (* (1+ n) a2)))
                      den)))))
    (/ (find-nsin-scaler n 0d0 (/ pi (+ n .5))))))

(declaim (inline sound-chans))
(defun sound-chans (name) (soundfile:channels name))

(declaim (inline sound-duration))
(defun sound-duration (name) (soundfile:duration name))

(declaim (inline sound-data-format))
(defun sound-data-format (name)
  (sf-to-mus-data-format (soundfile:data-format name)))

(declaim (inline sound-header-type))
(defun sound-header-type (name)
  (sf-to-mus-header-type (soundfile:header-type name)))

(declaim (inline sound-length))
(defun sound-length (name)
  (soundfile:frames name))

(declaim (inline sound-samples))
(defun sound-samples (name)
  (multiple-value-bind (sr frames chans) (soundfile:read-header name)
    (declare (ignore sr))
    (* frames chans)))

(declaim (inline sound-frames))
(defun sound-frames (name)
  (soundfile:frames name))

(declaim (inline sound-framples))
(defun sound-framples (name)
  (soundfile:frames name))

(declaim (inline sound-srate))
(defun sound-srate (name)
  (soundfile:sample-rate name))

(declaim (inline sound-comment))
(defun sound-comment (name)
  (soundfile:metadata name 'comment))

(declaim (inline sound-datum-size))
(defun sound-datum-size (name)
  (bytes-per-sample (sound-data-format name)))

(declaim (inline sound-data-location))
(defun sound-data-location (file)
  (soundfile:data-location file))

(defun sound-maxamp (file chans maxamps times)
  (multiple-value-bind (max-arr pos-arr) (soundfile:maxamp file t)
    (dotimes (i (min chans (length max-arr)))
      (setf (aref maxamps i) (aref max-arr i))
      (setf (aref times i) (aref pos-arr i)))
    (sound-framples file)))

(declaim (inline foreign-memmove))
(cffi:defcfun ("memmove" foreign-memmove) :void
  (dest :pointer)
  (src :pointer)
  (bytes :unsigned-int))

(declaim (inline align-foreign-buffer-with-location))
(defun align-foreign-buffer-with-location (data loc size)
  (let ((good-samps (- size loc)))
    (foreign-memmove data (cffi:inc-pointer data (* loc +foreign-sample-size+))
                     (* good-samps +foreign-sample-size+))
    good-samps))

;;; Edited from clm-5/initmus.lisp
;;; Like cerror, except provides a default continuation value, and if
;;; continue-test, prompts for new value.
(defun clm-cerror (continue-control continue-default-value continue-test
                   error-control &rest args)
  (apply #'cerror continue-control error-control args)
  (if continue-test
      (loop
         (princ (format nil "new value (return=~A):" continue-default-value))
         (multiple-value-bind (new-str eof) (read-line)
           (if (or eof (zerop (length new-str)))
               (return-from clm-cerror continue-default-value)
               (let ((new-val (read-from-string new-str)))
                 (if (funcall continue-test new-val)
                     (return-from clm-cerror new-val)
                     (print (format nil "~A is not a valid value in this context"
                                    new-val)))))))
      continue-default-value))

;;; Copied from clm-5/initmus.lisp
(defun clm-print (fstr &rest args)
  (if (stringp fstr)
      (princ (apply #'format nil fstr args))
      (apply #'format fstr (car args) (cdr args))))

(defun princ! (s) (princ s) (force-output))

(defun timestring ()
  (multiple-value-bind (s mi h d m y) (get-decoded-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y m d h mi s)))

(defun make-banner (&optional stream)
  (format stream "~&;; <~A>" (timestring)))

(defun play-without-wait (command)
  (when (find-package "UIOP/RUN-PROGRAM")
    (let ((run (find-symbol "%RUN-PROGRAM" "UIOP/RUN-PROGRAM")))
      (when run
        (setf *dac-pid* (getf (funcall run command :wait nil) :process))))))

(defun* clm:play (file (start 0) end (wait *clm-dac-wait-default*))
  (let ((filename (if file
                      (namestring (merge-pathnames file (or *clm-file-name* "")))
                      last-dac-filename)))
    (stop-playing)
    (cond ((not filename)
           (warn "no previous file to play"))
          ((functionp *clm-player*)
           (funcall *clm-player* filename))
          ((stringp *clm-player*)
           (let ((cmd (format nil "~A ~S"
                              (format nil *clm-player* start end)
                              filename)))
             (if wait
                 (uiop:run-program cmd)
                 (play-without-wait cmd)))))
    (if (and filename (string/= filename last-dac-filename))
        (setf last-dac-filename filename)
        last-dac-filename)))

(defun stop-playing ()
  #+sbcl
  (when *dac-pid*
    (when (sb-ext:process-alive-p *dac-pid*)
      (sb-ext:process-kill *dac-pid* SB-UNIX:SIGTERM)
      (sb-ext:process-wait *dac-pid*))
    (setf *dac-pid* nil)))

(setf (symbol-function 'dac) (symbol-function 'clm:play))
(setf (symbol-function 'stop-dac) (symbol-function 'stop-playing))

(defun print-statistics (stats out-chans
                         &optional (stream *clm-logger-stream*) scaled)
  (when stats
    (let ((total-time (float (/ (- (get-internal-real-time) clm-start-time)
                                internal-time-units-per-second)))
          (clm-last-end-time (sound-framples clm-outfile-name))
          (rev-chans (if (and clm-revfile-name (not *clm-delete-reverb*))
                         (sound-chans clm-revfile-name)
                         0)))
      (multiple-value-bind (ovals otimes) (soundfile:maxamp clm-outfile-name t)
        (multiple-value-bind (rvals rtimes)
            (and (> rev-chans 0) (soundfile:maxamp clm-revfile-name))
          (labels ((convert-samples-to-seconds (samp)
                     (if samp (float (/ samp *srate*)) 0.0))
                   (float-string-length (arr fmt)
                     (reduce #'max arr :key (lambda (n)
                                              (length (format nil fmt n)))))
                   (chan-stats (tag chans vals times)
                     (let* ((vals-db
                              (map 'vector
                                   (lambda (x) (and (> x 1e-15) (lin->db x)))
                                   vals))
                            (lin-control-string
                              (format nil "~~~D,3F"
                                      (float-string-length vals "~,3F")))
                            (db-strlen (float-string-length vals-db "~,2F"))
                            (db-control-string
                              (format nil "~~:[~~~DT-inf~~;~~~D,2F~~]"
                                      (- db-strlen 4) db-strlen))
                            (pos-control-string
                              (format nil "~~~DD"
                                      (length (write-to-string
                                               (reduce #'max times))))))
                       (format stream
                         "~:{~%  ~A~A max amp~:[~; (before scaling)~]: ~
                             ~A (~A dB), pos: ~A~@[ (near ~,3F sec~:P)~]~}"
                         (loop for ch below chans
                               for val = (aref vals ch)
                               for db = (aref vals-db ch)
                               for pos = (aref times ch)
                               collect
                                 (list tag (1+ ch) scaled
                                       (format nil lin-control-string val)
                                       (format nil db-control-string db db)
                                       (format nil pos-control-string pos)
                                       (and (plusp pos)
                                            (convert-samples-to-seconds pos))))))))
            (format stream
              "~&~A: ~%  Duration: ~,4F~@[~A~], Last begin time: ~,4F~@[~A~]~%"
              (namestring clm-outfile-name)
              (convert-samples-to-seconds clm-last-end-time)
              (and (< clm-last-end-time 1000)
                   (format nil " (~D sample~:P)" clm-last-end-time))
              (convert-samples-to-seconds clm-last-begin-time)
              (and (< 0 clm-last-begin-time 1000)
                   (format nil " (sample ~D)" clm-last-begin-time)))
            (format stream "  Compute time: ~,3F, Compute ratio: ~,2F"
              total-time
              (if (zerop clm-last-end-time)
                  0.0
                  (/ total-time (convert-samples-to-seconds clm-last-end-time))))
            (when (> total-time 3600)
              (let* ((days (floor total-time (* 24 60 60)))
                     (notdays (- total-time (* days 24 60 60)))
                     (hours (floor notdays (* 60 60)))
                     (nothours (- notdays (* hours 60 60)))
                     (minutes (floor nothours 60))
                     (seconds (- nothours (* minutes 60))))
                (format stream
                  "~%~4T(~@[~A~]~@[~A~]~@[~A~]~,3F second~:P, finished~@[~A~] on ~A)"
                  (and (plusp days) (format nil "~D day~:P, " days))
                  (and (plusp hours) (format nil "~D hour~:P, " hours))
                  (and (plusp minutes) (format nil "~D minute~:P, " minutes))
                  seconds (and (plusp days) " (Laus Deo)") (timestring))))
            (chan-stats "Out" out-chans ovals otimes)
            (when clm-revfile-name
              (chan-stats "Rev" rev-chans rvals rtimes))
            (terpri stream)
            (force-output stream)))))))
