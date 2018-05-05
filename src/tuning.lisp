;;; Copyright (c) 2015-2018 Tito Latini
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

(defstruct (tuning (:include buffer-base)
                   (:constructor %make-tuning)
                   (:copier nil))
  "Tuning type."
  (description "" :type string)
  ;; Scale degrees specified as cents.
  (%cents (make-array 13 :element-type 'single-float)
          :type (simple-array single-float (*)))
  ;; Scale degrees specified as ratios.
  (%ratios (make-array 13 :element-type 'positive-rational :initial-element 1)
           :type (simple-array positive-rational (*)))
  ;; Memory reserved for 128 ratios of SAMPLE type plus the reciprocal
  ;; of the first ratio.
  (sample-ratios (cffi:null-pointer) :type foreign-pointer)
  ;; Auxiliary data:
  ;;
  ;;     KEYNUM-BASE     1 byte
  ;;     DEGREE-INDEX    1 byte
  ;;     FREQ-BASE       SAMPLE type
  ;;     TEMP-VALUE-1    SAMPLE type
  ;;     TEMP-VALUE-2    SAMPLE type
  ;;
  (aux-data (null-pointer) :type foreign-pointer))

(setf
  (documentation 'tuning-description 'function)
  "Return the description of the tuning.")

(define-constant +last-sample-ratio-reciprocal-index+ 128)

(define-constant +tuning-number-of-aux-data+ 5)

(define-constant +tuning-freq-base-index+    1)
(define-constant +tuning-temp-value-1-index+ 2)
(define-constant +tuning-temp-value-2-index+ 3)

(define-constant +tuning-pool-initial-size+ 20)

(defvar *tuning-pool*
  (make-incudine-object-pool +tuning-pool-initial-size+ #'%make-tuning nil))
(declaim (type incudine-object-pool *tuning-pool*))

(defvar *rt-tuning-pool*
  (make-incudine-object-pool +tuning-pool-initial-size+ #'%make-tuning t))
(declaim (type incudine-object-pool *rt-tuning-pool*))

(defvar *default-tuning-notes*
  (loop for i from 100 to 1200 by 100 collect (float i)))

(defvar *default-tuning-description* "12-tone equal temperament")

(declaim (inline tuning-data))
(defun tuning-data (obj)
  "Return the foreign pointer to the tuning data."
  (buffer-base-data-ptr obj))

(defmacro with-tuning-cents-and-ratios ((cents-var ratios-var length)
                                        &body body)
  (with-gensyms (len)
    `(let* ((,len (1+ ,length))  ; One slot for the unison.
            (,cents-var (make-array ,len :element-type 'single-float))
            (,ratios-var (make-array ,len
                                     :element-type 'positive-rational
                                     :initial-element 1)))
       ,@body)))

(defun %%make-tuning (notes length keynum-base freq-base degree-index
                      descr real-time-p)
  (declare (type list notes) (type fixnum length)
           (type (integer 0 127) keynum-base degree-index)
           (type (real 0 20000) freq-base) (type string descr)
           (type boolean real-time-p))
  (multiple-value-bind (cents ratios)
      (with-tuning-cents-and-ratios (cents ratios length)
        (scl-notes-to-cents-and-ratios notes cents ratios))
    (let* ((%size 128)
           (data-bytes (* %size +foreign-sample-size+))
           ;; DATA: size
           ;; SUM-RATIOS plus last-sample-ratio-reciprocal: (1+ size)
           ;; AUX-DATA: (1- +tuning-number-of-aux-data+)
           (foreign-size (+ (* %size 2) +tuning-number-of-aux-data+))
           (rt-p (and real-time-p *allow-rt-memory-pool-p*)))
      (declare #.*reduce-warnings*)
      (multiple-value-bind (%data obj free-fn pool)
          (if rt-p
              (values (foreign-rt-alloc 'sample :count foreign-size :zero-p t)
                      (incudine.util::alloc-rt-object *rt-tuning-pool*)
                      #'safe-foreign-rt-free
                      *rt-tuning-pool*)
              (values (foreign-alloc-sample foreign-size)
                      (incudine.util::alloc-object *tuning-pool*)
                      #'foreign-free
                      *tuning-pool*))
        (let* ((smp-ratios (cffi:inc-pointer %data data-bytes))
               (%aux-data (cffi:inc-pointer smp-ratios
                            (+ data-bytes +foreign-sample-size+))))
          (incudine.util::with-struct-slots
              ((data-ptr description %cents %ratios sample-ratios aux-data size
                real-time-p foreign-free)
               obj tuning)
            (setf data-ptr %data
                  size %size
                  description descr
                  %cents cents
                  %ratios ratios
                  sample-ratios smp-ratios
                  aux-data %aux-data
                  real-time-p rt-p
                  foreign-free free-fn)
            (incudine-finalize obj
              (lambda ()
                (funcall free-fn %data)
                (incudine-object-pool-expand pool 1)))
            (setf (u8-ref aux-data 0) keynum-base)
            (setf (u8-ref aux-data 1) (min degree-index (1- (length cents))))
            (setf (smp-ref aux-data +tuning-freq-base-index+) (sample freq-base))
            (update-tuning-data (tuning-update-sample-ratios obj))
            obj))))))

(defun make-tuning (&key notes file description (keynum-base 69) (freq-base 440)
                    (degree-index 9) (real-time-p (allow-rt-memory-p)))
  "Create and return a new TUNING structure with optional DESCRIPTION.

If NOTES is non-NIL, it is a list of pitch values that are ratios
and/or floating point values (cents). The first note 1/1 (or 0.0) is
implicit. This format is compatible with the Scale file format
described at http://www.huygens-fokker.org/scala/scl_format.html.

If FILE is non-NIL, get the scale from a Scala file.

If NOTES and FILE are NIL, create a 12-tone equal tempered scale.

The reference frequency FREQ-BASE, the related key number KEYNUM-BASE
and DEGREE-INDEX default to 440, 69 and 9 respectively.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type list notes)
           (type (or null string pathname) file)
           (type (or null string) description)
           (type (integer 0 127) keynum-base degree-index)
           (type (real 0 20000) freq-base)
           (type boolean real-time-p))
  (multiple-value-bind (notes num-of-notes description)
      (cond (notes (values notes (length notes) (or description "")))
            (file (load-sclfile file))
            (t (values *default-tuning-notes*
                       (length *default-tuning-notes*)
                       *default-tuning-description*)))
    (%%make-tuning notes num-of-notes keynum-base freq-base degree-index
                   description real-time-p)))

(defmethod print-object ((obj tuning) stream)
  (format stream "#<~S ~S>" (type-of obj) (tuning-description obj)))

(defmethod free ((obj tuning))
  (unless (free-p obj)
    (funcall (tuning-foreign-free obj) (tuning-data obj))
    (incudine-cancel-finalization obj)
    (setf (tuning-data-ptr obj) (null-pointer))
    (setf (tuning-aux-data obj) (null-pointer))
    (setf (tuning-sample-ratios obj) (null-pointer))
    (setf (tuning-size obj) 0)
    (if (tuning-real-time-p obj)
        (incudine.util::free-rt-object obj *rt-tuning-pool*)
        (incudine.util::free-object obj *tuning-pool*))
    (nrt-msg debug "Free ~A" (type-of obj))
    (values)))

(declaim (inline tuning-keynum-base))
(defun tuning-keynum-base (tuning)
  "Return the key number of TUNING used as reference."
  (u8-ref (tuning-aux-data tuning) 0))

(declaim (inline tuning-degree-index))
(defun tuning-degree-index (tuning)
  "Return the scale degree of TUNING used as reference."
  (u8-ref (tuning-aux-data tuning) 1))

(declaim (inline tuning-freq-base))
(defun tuning-freq-base (tuning)
  "Return the reference frequency of TUNING."
  (smp-ref (tuning-aux-data tuning) +tuning-freq-base-index+))

(defun set-tuning-reference (tuning keynum-base freq-base degree-index)
  "Change KEYNUM-BASE, FREQ-BASE and DEGREE-INDEX used as reference
to generate the frequencies of the TUNING."
  (declare (type tuning tuning)
           (type (integer 0 127) keynum-base degree-index)
           (type (real 0 20000) freq-base))
  (incudine-optimize
    (setf (u8-ref (tuning-aux-data tuning) 0) keynum-base)
    (setf (u8-ref (tuning-aux-data tuning) 1) degree-index)
    (reduce-warnings
      (setf (smp-ref (tuning-aux-data tuning) +tuning-freq-base-index+)
            (sample freq-base)))
    (update-tuning-data tuning)))

(declaim (inline tuning-cents))
(defun tuning-cents (tuning)
  "Return the list of pitch values of the TUNING in cents."
  (tuning-%cents tuning))

(declaim (inline tuning-ratios))
(defun tuning-ratios (tuning)
  "Return the list of pitch values of the TUNING as ratios."
  (tuning-%ratios tuning))

(declaim (inline tuning-cps))
(defun tuning-cps (tuning keynum)
  "Return the frequency of the key number KEYNUM in a TUNING.
Setfable."
  (declare (type tuning tuning) (type (integer 0 127) keynum))
  (buffer-value tuning keynum))

(defsetf tuning-cps set-buffer-value)

(defun tuning-update-sample-ratios (tuning)
  (let* ((ratios (tuning-ratios tuning))
         (len (1- (length ratios))))
    (dotimes (i len)
      (setf (smp-ref (tuning-sample-ratios tuning) i)
            (sample (aref ratios (1+ i)))))
    ;; Reciprocal of the last ratio.
    (setf (smp-ref (tuning-sample-ratios tuning)
                   +last-sample-ratio-reciprocal-index+)
          (/ (smp-ref (tuning-sample-ratios tuning) (1- len))))
    tuning))

(defun update-tuning-data (tuning)
  (declare (type tuning tuning)
           #.*standard-optimize-settings*)
  (let* ((data (tuning-data tuning))
         (aux-data (tuning-aux-data tuning))
         (kbase (tuning-keynum-base tuning))
         (degrees (1- (length (tuning-cents tuning))))
         (degree-interval-index (mod (1- (tuning-degree-index tuning))
                                     degrees)))
    (declare (type (integer 0 127) degrees))
    (setf (smp-ref data kbase) (tuning-freq-base tuning))
    ;; 1/1 frequency.
    (setf (smp-ref aux-data +tuning-temp-value-1-index+)
          (* (tuning-freq-base tuning)
             (/ (smp-ref (tuning-sample-ratios tuning) degree-interval-index))))
    (setf (smp-ref aux-data +tuning-temp-value-2-index+)
          (smp-ref aux-data +tuning-temp-value-1-index+))
    ;; Update the frequencies for the keynums minor than KEYNUM-BASE.
    (do ((k (1- kbase) (1- k))
         (deg (mod (1- degree-interval-index) degrees)
              (mod (1- deg) degrees)))
        ((minusp k))
      (declare (type (signed-byte 8) k deg))
      (setf (smp-ref data k) (* (smp-ref aux-data +tuning-temp-value-1-index+)
                                (smp-ref (tuning-sample-ratios tuning) deg)))
      (when (and (zerop deg) (plusp k))
        ;; Update the 1/1 frequency.
        (setf (smp-ref aux-data +tuning-temp-value-1-index+)
              (* (smp-ref aux-data +tuning-temp-value-1-index+)
                 (smp-ref (tuning-sample-ratios tuning)
                          +last-sample-ratio-reciprocal-index+)))))
    ;; Update the frequencies for the keynums major than KEYNUM-BASE.
    (do ((k (1+ kbase) (1+ k))
         (deg (mod (1+ degree-interval-index) degrees)
              (mod (1+ deg) degrees)))
        ((>= k 128))
      (declare (type (unsigned-byte 8) k deg))
      (when (zerop deg)
        ;; Update the 1/1 frequency.
        (setf (smp-ref aux-data +tuning-temp-value-2-index+)
              (smp-ref data (1- k))))
      (setf (smp-ref data k) (* (smp-ref aux-data +tuning-temp-value-2-index+)
                                (smp-ref (tuning-sample-ratios tuning) deg))))
    tuning))

(defun maybe-rewrite-tuning-cents-and-ratios (tuning new-length)
  (unless (= new-length (1- (length (tuning-cents tuning))))
    (with-tuning-cents-and-ratios (cents ratios new-length)
      (setf (tuning-%cents tuning) cents)
      (setf (tuning-%ratios tuning) ratios))))

(defun set-tuning-notes (tuning notes length description)
  (declare (type tuning tuning) (type list notes)
           (type (unsigned-byte 8) length)
           (type string description)
           #.*standard-optimize-settings*)
  (maybe-rewrite-tuning-cents-and-ratios tuning length)
  (scl-notes-to-cents-and-ratios notes (tuning-cents tuning)
                                 (tuning-ratios tuning))
  (tuning-update-sample-ratios tuning)
  (setf (tuning-description tuning) description)
  (update-tuning-data tuning))

(defun check-tuning-notes (notes)
  (every (lambda (x) (typep x '(or single-float positive-rational))) notes))

(defun set-tuning (tuning notes-or-file &optional description keynum-base
                   freq-base degree-index)
  "Change the notes and optionally the DESCRIPTION, the reference
frequency FREQ-BASE, KEYNUM-BASE and DEGREE-INDEX of a TUNING structure.

If NOTES-OR-FILE is the path of a Scala file, import the notes from
that file."
  (declare (type tuning tuning)
           (type (or list string pathname) notes-or-file)
           (type (or string null) description)
           (type (or (integer 0 127) null) keynum-base degree-index)
           (type (or (real 0 20000) null) freq-base))
  (incudine-optimize
    (when keynum-base
      (setf (u8-ref (tuning-aux-data tuning) 0) keynum-base))
    (when degree-index
      (setf (u8-ref (tuning-aux-data tuning) 1) degree-index))
    (when freq-base
      (reduce-warnings
        (setf (smp-ref (tuning-aux-data tuning) +tuning-freq-base-index+)
              (sample freq-base))))
    (if (listp notes-or-file)
        (if (check-tuning-notes notes-or-file)
            (set-tuning-notes tuning notes-or-file (length notes-or-file)
                              (or description ""))
            (msg error "incorrect note list ~A" notes-or-file))
        (multiple-value-bind (notes len descr)
            (load-sclfile notes-or-file)
          (declare (type (unsigned-byte 8) len))
          (set-tuning-notes tuning notes len descr)))))

;;; We can use the ears to directly set the frequencies of a TUNING
;;; from the keynum START to the keynum END. Then we call TUNING-NOTES-FROM-DATA
;;; to update the TUNING (cents, ratios and all the other frequencies).
(defun tuning-notes-from-data (tuning start end &optional description
                               (significand-error 5.e-6))
  "Compute the notes of the scale from the frequencies stored in a
TUNING structure, starting from the key number START and ending to the
key number END. The ratio between the frequencies in START and END is
the last TUNING ratio. If SIGNIFICAND-ERROR is non-zero (default), try
to minimize the TUNING ratios by introducing an error (default 0.0005%)
in the significand of the floating point numbers."
  (declare (type tuning tuning) (type (integer 0 127) start end)
           (type (or null string) description))
  (when (> end start)
    (let ((len (- end start)))
      (declare (type (integer 0 127) len)
               #.*standard-optimize-settings*)
      (maybe-rewrite-tuning-cents-and-ratios tuning len)
      ;; Probably unnecessary, but it is safer to explicitly set the first slot.
      (setf (aref (tuning-cents tuning) 0) 0.0)
      (setf (aref (tuning-ratios tuning) 0) 1)
      (loop for k from (1+ start) to end
            for i from 1
            with data = (tuning-data tuning) do
              (setf (aref (tuning-ratios tuning) i)
                    (reduce-warnings
                      (incudine.util:rationalize*
                        ;; Integers too large with double precision.
                        (coerce (/ (smp-ref data k) (smp-ref data start))
                                'single-float)
                        (coerce significand-error 'single-float))))
              (setf (aref (tuning-cents tuning) i)
                    (* (log (aref (tuning-ratios tuning) i) 2) 1200.0)))
      (when description
        (setf (tuning-description tuning) description))
      (update-tuning-data (tuning-update-sample-ratios tuning)))))

(defun minimize-tuning-ratios (tuning &optional (significand-error 5.e-6))
  "Try to minimize the TUNING ratios by introducing an error in the
significand of the floating point numbers. The error is 0.0005% by default."
  (declare (type tuning tuning))
  (set-tuning tuning
              (mapcar (lambda (x)
                        (incudine.util:rationalize*
                          x (coerce significand-error 'single-float)))
                      (cdr (coerce (tuning-ratios tuning) 'list)))
              (tuning-description tuning)))

(declaim (inline scl-string))
(defun scl-string (string)
  (string-trim '(#\space #\tab) string))

(declaim (inline scl-comment-p))
(defun scl-comment-p (string)
  (char= (char string 0) #\!))

(declaim (inline scl-valid-pitch-p))
(defun scl-valid-pitch-p (value)
  (and (numberp value) (not (minusp value))))

(defun scl-number-dot-p (string)
  (let ((dot-pos (position #\. string)))
    (and dot-pos (not (position #\! string :end dot-pos)))))

(defmacro with-scl-read ((string-var stream) &body body)
  (with-gensyms (l)
    `(loop for ,l = (read-line ,stream t nil t)
           while ,l do
             (let ((,string-var (scl-string ,l)))
               (unless (scl-comment-p ,string-var)
                 ,@body)))))

(defun scl-description (stream)
  (with-scl-read (str stream) (return str)))

(defun scl-num-of-notes (stream)
  (with-scl-read (str stream) (return (read-from-string str))))

(defun read-pitch-from-string (string)
  (let ((value (read-from-string string)))
    (when (scl-valid-pitch-p value)
      (if (floatp value)
          value
          (if (scl-number-dot-p string)
              (coerce value 'single-float)
              value)))))

(defun scl-read-notes (num-of-notes stream)
  (declare (type (unsigned-byte 8) num-of-notes)
           (type stream stream))
  (let ((acc))
    (with-scl-read (str stream)
      (let ((value (read-pitch-from-string str)))
        (when value
          (push value acc)
          (when (zerop (decf num-of-notes))
            (return (nreverse acc))))))))

(defun load-sclfile (path)
  "Return the pitch values, the number of notes and the description of
the scale stored in the Scala file PATH."
  (declare (type (or string pathname) path))
  (with-open-file (scl path)
    (let ((descr (scl-description scl)))
      (when descr
        (let ((n (scl-num-of-notes scl)))
          (when (and (numberp n) (plusp n))
            (let ((notes (scl-read-notes n scl)))
              (when (and notes (= (length notes) n))
                (values notes n descr)))))))))

(defun scl-notes-to-cents-and-ratios (notes cents ratios)
  (declare (type list notes)
           (type (simple-array single-float (*)) cents)
           (type (simple-array positive-rational (*)) ratios)
           #.*standard-optimize-settings*)
  (setf (aref cents 0) 0.0)
  (setf (aref ratios 0) 1)
  (loop for pch in notes
        for i from 1 do
          (multiple-value-bind (cent ratio)
              (reduce-warnings
                (if (floatp pch)
                    (values pch (rationalize (expt 2 (* (the single-float pch)
                                                        1/1200))))
                    (values (* (the single-float (log pch 2)) 1200.0) pch)))
            (setf (aref cents i) cent)
            (setf (aref ratios i) ratio)))
  (values cents ratios))

(defun scl-cent-prevalence-p (cent ratio)
  (and (zerop (rem (* 1000 cent) 1)) (> (numerator ratio) 99)))

(defun scl-cents-prevalence-p (tuning)
  (do ((i 1 (1+ i))
       (len (1- (length (tuning-cents tuning))))
       (cents 0))
      ((>= i len) (> (/ cents len) 1/2))
    (when (scl-cent-prevalence-p (aref (tuning-cents tuning) i)
                                 (aref (tuning-ratios tuning) i))
      (incf cents))))

(defun tuning-ratio-or-cent-string (tuning index cents-prevalence-p)
  (let ((r (aref (tuning-ratios tuning) index))
        (c (aref (tuning-cents tuning) index)))
    (cond ((and (zerop (rem c 1))
                (or (not (integerp r)) cents-prevalence-p))
           (format nil "~,1F" c))
          ((integerp r) (format nil "~D/1" r))
          ((scl-cent-prevalence-p c r) (format nil "~,3F" c))
          (t (format nil "~A" r)))))

(defun tuning-save (tuning path)
  "Save pitch values and description of a TUNING structure to the file PATH
in Scale file format."
  (declare (type tuning tuning) (type (or string pathname) path))
  (with-open-file (scl path :direction :output :if-exists :supersede)
    (let ((degrees (1- (length (tuning-cents tuning)))))
      (format scl "! ~A~%!~%~A~%~D~%!~%" (file-namestring path)
              (tuning-description tuning) degrees)
      (loop for i from 1 to degrees
            with cents-prevalence-p = (scl-cents-prevalence-p tuning)
            do (write-line (tuning-ratio-or-cent-string tuning i
                                                        cents-prevalence-p)
                           scl))
      (pathname path))))

(defun copy-tuning (tuning)
  "Return a copy of TUNING."
  (declare (type tuning tuning))
  (if (free-p tuning)
      (msg error "Unusable tuning.")
      (let ((new (make-tuning
                   :notes (coerce (tuning-ratios tuning) 'list)
                   :description (copy-seq (tuning-description tuning))
                   :keynum-base (tuning-keynum-base tuning)
                   :freq-base (tuning-freq-base tuning)
                   :degree-index (tuning-degree-index tuning)
                   :real-time-p (tuning-real-time-p tuning))))
        (foreign-copy-samples (tuning-data new) (tuning-data tuning)
                              (tuning-size tuning))
        (setf (tuning-%cents new) (copy-seq (tuning-%cents tuning)))
        new)))

(defvar *tuning-et12* (make-tuning))
(declaim (type tuning *tuning-et12*))

(defvar *default-tuning* *tuning-et12*
  "Default TUNING structure.")
(declaim (type tuning *default-tuning*))

(declaim (inline tuning-keynum-base-range))
(defun tuning-keynum-base-range (tuning)
  (values (the (integer 0 127)
            (- (tuning-keynum-base tuning)
               (tuning-degree-index tuning)))
          (the non-negative-fixnum
            (1- (length (tuning-cents tuning))))))

(defun tuning-start-index (tuning octave)
  "Return the keynum related to the first degree of TUNING after the
start of OCTAVE.  For convention, octave zero starts with one cycle
for second.  Example: octave 8 starts with 256 Hz (about middle C)."
  (declare (type (integer 0 14) octave))
  (let ((cps (ash 1 octave)))
    (declare (type fixnum cps))
    (labels ((tuncps (index)
               (sample->fixnum (tuning-cps tuning index)))
             (get-start (index steps)
               (declare (type fixnum index steps))
               (let ((prev (prev-start index steps)))
                 (if (< prev index)
                     prev
                     (next-start index steps))))
             (prev-start (curr steps)
               (declare (type fixnum curr steps))
               (let ((prev (- curr steps)))
                 (declare (type fixnum prev))
                 (if (or (< prev 0) (< (tuncps prev) cps))
                     curr
                     (prev-start prev steps))))
             (next-start (curr steps)
               (declare (type fixnum curr steps))
               (cond ((>= curr 127) 127)
                     ((>= (tuncps curr) cps) curr)
                     (t (next-start (+ curr steps) steps)))))
      (if (<= (tuncps 127) cps)
          127
          (multiple-value-call #'get-start
            (tuning-keynum-base-range tuning))))))

(defun decode-pitch-class (pch)
  (declare (type (single-float 0.0 15.0) pch))
  (multiple-value-bind (oct pch) (truncate pch)
    (declare (type (integer 0 14) oct))
    (multiple-value-bind (index frac) (truncate (* pch 100))
      (declare (type (integer 0 99) index))
      (values oct index frac))))

(defun pch->keynum (pitch-class &optional (tuning *default-tuning*))
  "Convert from PITCH-CLASS value to key number. The second returned
value is the fractional part. PITCH-CLASS is a floating point number
xx.yy[z]* where

    xx  = octave number from 0 to 14
    yy  = scale degree from 0 to 99
    z*  = fractional part 0.z* to interpolate between yy and (+ yy 1)

Note: if the returned key number is used without the fractional part, it
is necessary to avoid round off problems by adding 1e-6 to PITCH-CLASS
before the conversion.

Example with ET12 scale:

         pch   | keynum |  frac
     ----------+--------+--------
      8.00     |   60   |  0.000
      8.09     |   69   |  0.000
      8.10     |   70   |  0.000
      8.095    |   69   |  0.500
      8.12     |   71   |  0.999
      8.120001 |   72   |  0.000
      9.00     |   72   |  0.000
"
  (declare (type (single-float 0.0 15.0) pitch-class))
  (multiple-value-bind (min-oct degree0) (%keynum->pch 0 tuning)
    (declare (type fixnum min-oct degree0))
    (multiple-value-bind (oct index frac) (decode-pitch-class pitch-class)
      (declare (type fixnum oct index) (type single-float frac))
      (when (and (= oct min-oct) (plusp degree0))
        (decf index (1- (length (tuning-cents tuning)))))
      (when (>= oct min-oct)
        (incf index (the fixnum (tuning-start-index tuning oct)))
        (when (minusp index)
          (setf index 0 frac 0.0)))
      (values (min index 127) frac))))

(defconstant +cs-cpspch-table-size+ 8192)
(defconstant +cs-cpspch-a440-tuning-factor+ 1.02197486) ; 440 / 2^(8 + 9/12)

(defvar *cs-cpspch-table*
  (let ((size +cs-cpspch-table-size+))
    (make-array size
      :initial-contents (loop for i below size
                              collect (* (expt 2.0 (/ i size))
                                         +cs-cpspch-a440-tuning-factor+)))))

;;; Table lookup used by Csound's cpspch opcode.
(defun cs-cpspch (oct frac)
  (declare (type (integer 0 14) oct) (type single-float frac))
  (let ((frac (* frac 25/3))
        (size +cs-cpspch-table-size+))
    (flet ((cpsoctl (n)
             (declare (type fixnum n))
             (* (the fixnum (ash 1 (ash n -13)))
                (the single-float
                  (svref *cs-cpspch-table* (logand n (1- size)))))))
      (cpsoctl (truncate (the (single-float 0.0 262144.0)
                           (* (+ oct frac) size)))))))

;;; Compatible with Csound's cpspch opcode if TUNING is NIL or a TUNING
;;; struct that represents a 12-tone equally-tempered scale.
(defun pch->cps (pitch-class &optional (tuning *default-tuning*))
  "Convert from PITCH-CLASS value to cycles-per-second.  If TUNING is NIL,
the table lookup is the same used by Csound's cpspch opcode.  If TUNING is
a TUNING structure, PITCH-CLASS is a floating point number xx.yy[z]* where

    xx  = octave number from 0 to 14
    yy  = scale degree from 0 to 99
    z*  = fractional part 0.z* to interpolate between yy and (+ yy 1)

Example with ET12 scale:

       pch  |  cps
     -------+-------
      8.00  |  261
      8.09  |  440
      8.095 |  453
      8.10  |  466
      8.12  |  523
      9.00  |  523
"
  (declare (type (single-float 0.0 15.0) pitch-class)
           (type (or tuning null) tuning))
  (if tuning
      (multiple-value-bind (keynum frac) (pch->keynum pitch-class tuning)
        (declare (type (integer 0 127) keynum) (type single-float frac))
        (if (= keynum 127)
            (tuning-cps tuning 127)
            (+ (* (tuning-cps tuning keynum) (- 1.0 frac))
               (* (tuning-cps tuning (1+ keynum)) frac))))
      (multiple-value-call #'cs-cpspch (floor pitch-class))))

(defun %keynum->pch (keynum &optional (tuning *default-tuning*))
  (declare (type (integer 0 127) keynum) (type tuning tuning))
  (multiple-value-bind (start length) (tuning-keynum-base-range tuning)
    (let ((delta (- keynum start)))
      (declare (type fixnum delta))
      (let* ((degree (mod delta length))
             (keynum0 (+ (- start degree) delta))
             (oct-offset -1))
        (declare (type fixnum degree keynum0 oct-offset))
        (when (minusp keynum0)
          (incf keynum0 length)
          (decf oct-offset 1))
        (values (+ (integer-length (sample->fixnum (tuning-cps tuning keynum0)))
                   oct-offset)
                degree)))))

(declaim (inline keynum->pch))
(defun keynum->pch (keynum &optional (tuning *default-tuning*))
  "Convert from key number KEYNUM to pitch class value.

The TUNING structure defaults to *DEFAULT-TUNING*."
  (declare (type (integer 0 127) keynum) (type tuning tuning))
  (multiple-value-bind (oct degree) (%keynum->pch keynum tuning)
    (+ oct (* degree .01))))

(defun tuning-nearest-keynum (freq &optional (tuning *default-tuning*))
  (declare (type real freq) (type tuning tuning))
  (let ((freq (reduce-warnings (floor freq))))
    (declare (type fixnum freq))
    (labels ((tquantize (i delta)
               (declare (type (integer 0 127) i delta))
               (if (or (= i 0) (= i 127))
                   i
                   (let* ((prev (tuncps (1- i)))
                          (next (tuncps (1+ i)))
                          (%curr-delta (- freq (tuncps i)))
                          (curr-delta (abs %curr-delta)))
                     (if (and (< prev freq) (<= freq next))
                         (cond ((< (- freq prev) curr-delta) (values (1- i) nil))
                               ((< (- next freq) curr-delta) (values (1+ i) t))
                               (t (values i (minusp %curr-delta))))
                         (tquantize (if (>= (tuncps i) freq)
                                        (- i delta)
                                        (+ i delta))
                                    (ash delta -1))))))
             (tuncps (index)
               (sample->fixnum (tuning-cps tuning index))))
      (cond ((<= freq (tuncps 0)) (values 0 nil))
            ((>= freq (tuncps 127)) (values 127 nil))
            (t (tquantize 64 32))))))

(defun cps->pch (freq &optional (tuning *default-tuning*))
  "Convert from FREQ cycles-per-second to pitch class value.

The TUNING structure defaults to *DEFAULT-TUNING*."
  (declare (type real freq) (type tuning tuning))
  (multiple-value-bind (keynum round-up-p) (tuning-nearest-keynum freq tuning)
    (declare (type (integer 0 127) keynum) (type boolean round-up-p))
    (when round-up-p
      (decf keynum))
    (multiple-value-bind (oct degree) (%keynum->pch keynum tuning)
      (declare (type fixnum oct degree))
      (+ oct (* degree .01)
         (if (and (< keynum 127)
                  (< (tuning-cps tuning keynum)
                     (tuning-cps tuning (1+ keynum))))
             (coerce (* (/ (- freq (tuning-cps tuning keynum))
                           (- (tuning-cps tuning (1+ keynum))
                              (tuning-cps tuning keynum)))
                        .01)
                     'single-float)
             0.0)))))

(defmethod quantize ((obj real) (from tuning) &key)
  (let* ((keynum (tuning-nearest-keynum obj from))
         (value (tuning-cps from keynum)))
    (values value keynum)))
