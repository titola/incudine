;;; Copyright (c) 2015 Tito Latini
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
  (description "" :type string)
  ;; Scale degrees specified as cents.
  (%cents (error "missign tuning cents") :type (simple-array single-float (*)))
  ;; Scale degrees specified as ratios.
  (%ratios (error "missing tuning ratios")
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
  (aux-data (error "missing aux data pointer") :type foreign-pointer))

(define-constant +last-sample-ratio-reciprocal-index+ 128)

(define-constant +tuning-number-of-aux-data+ 5)

(define-constant +tuning-freq-base-index+    1)
(define-constant +tuning-temp-value-1-index+ 2)
(define-constant +tuning-temp-value-2-index+ 3)

(defvar *default-tuning-notes*
  (loop for i from 100 to 1200 by 100 collect (float i)))

(defvar *default-tuning-description* "12-tone equal temperament")

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
                      description real-time-p)
  (multiple-value-bind (cents ratios)
      (with-tuning-cents-and-ratios (cents ratios length)
        (scl-notes-to-cents-and-ratios notes cents ratios))
    (let* ((size 128)
           (data-bytes (* size +foreign-sample-size+))
           ;; DATA: size
           ;; SUM-RATIOS plus last-sample-ratio-reciprocal: (1+ size)
           ;; AUX-DATA: (1- +tuning-number-of-aux-data+)
           (foreign-size (+ (* size 2) +tuning-number-of-aux-data+))
           (data (if real-time-p
                     (foreign-rt-alloc 'sample :count foreign-size :zero-p t)
                     (foreign-alloc-sample foreign-size)))
           (smp-ratios (cffi:inc-pointer data data-bytes))
           (aux-data (cffi:inc-pointer smp-ratios
                                       (+ data-bytes +foreign-sample-size+)))
           (free-function (if real-time-p
                              #'safe-foreign-rt-free
                              #'foreign-free))
           (obj (%make-tuning
                 :description (or description "")
                 :%cents cents
                 :%ratios ratios
                 :data data
                 :aux-data aux-data
                 :sample-ratios smp-ratios
                 :size size
                 :real-time-p (and real-time-p (rt-thread-p))
                 :foreign-free free-function)))
      (tg:finalize obj (lambda () (funcall free-function data)))
      (setf (u8-ref aux-data 0) keynum-base)
      (setf (u8-ref aux-data 1) (min degree-index (1- (length cents))))
      (setf (smp-ref aux-data +tuning-freq-base-index+) (sample freq-base))
      (update-tuning-data (tuning-update-sample-ratios obj))
      obj)))

(defun make-tuning (&key notes file description (keynum-base 69)
                    (freq-base 440) (degree-index 9) real-time-p)
  (declare (type list notes)
           (type (or null string pathname) file)
           (type (or null string) description)
           (type (integer 0 127) keynum-base degree-index)
           (type (real 0 20000) freq-base)
           (type boolean real-time-p))
  (multiple-value-bind (description num-of-notes notes)
      (cond (notes (values (or description "") (length notes) notes))
            (file (load-sclfile file))
            (t (values *default-tuning-description*
                       (length *default-tuning-notes*)
                       *default-tuning-notes*)))
    (%%make-tuning notes num-of-notes keynum-base freq-base degree-index
                   description real-time-p)))

(defmethod print-object ((obj tuning) stream)
  (with-slots (frames channels sample-rate) obj
    (format stream "#<~S ~S>" (type-of obj) (tuning-description obj))))

(defmethod free ((obj tuning))
  (unless (null-pointer-p (tuning-aux-data obj))
    (setf (tuning-aux-data obj) (null-pointer)))
  (unless (null-pointer-p (tuning-sample-ratios obj))
    (setf (tuning-sample-ratios obj) (null-pointer)))
  (call-next-method))

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

(declaim (inline tuning-keynum-base))
(defun tuning-keynum-base (tuning)
  (u8-ref (tuning-aux-data tuning) 0))

(declaim (inline tuning-degree-index))
(defun tuning-degree-index (tuning)
  (u8-ref (tuning-aux-data tuning) 1))

(declaim (inline tuning-freq-base))
(defun tuning-freq-base (tuning)
  (smp-ref (tuning-aux-data tuning) +tuning-freq-base-index+))

(defun set-tuning-reference (tuning keynum-base freq-base degree-index)
  "Change KEYNUM-BASE, FREQ-BASE and DEGREE-INDEX used as reference
to generate the TUNING frequencies."
  (declare (type tuning tuning)
           (type (integer 0 127) keynum-base degree-index)
           (type (real 0 20000) freq-base)
           #.*standard-optimize-settings*)
  (setf (u8-ref (tuning-aux-data tuning) 0) keynum-base)
  (setf (u8-ref (tuning-aux-data tuning) 1) degree-index)
  (reduce-warnings
    (setf (smp-ref (tuning-aux-data tuning) +tuning-freq-base-index+)
          (sample freq-base)))
  (update-tuning-data tuning))

(declaim (inline tuning-cents))
(defun tuning-cents (tuning)
  (tuning-%cents tuning))

(declaim (inline tuning-ratios))
(defun tuning-ratios (tuning)
  (tuning-%ratios tuning))

(declaim (inline tuning-cps))
(defun tuning-cps (tuning keynum)
  (declare (type tuning tuning) (type (integer 0 127) keynum))
  (buffer-value tuning keynum))

(defsetf tuning-cps set-buffer-value)

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

(defun set-tuning (tuning notes-or-file)
  "Change the notes of a TUNING structure. If NOTES-OR-FILE is the
path of a .scl file, import the notes from this file."
  (declare (type tuning tuning)
           (type (or list string pathname) notes-or-file)
           #.*standard-optimize-settings*)
  (if (listp notes-or-file)
      (if (check-tuning-notes notes-or-file)
          (set-tuning-notes tuning notes-or-file (length notes-or-file) "")
          (msg error "incorrect note list ~A" notes-or-file))
      (multiple-value-bind (descr len notes)
          (load-sclfile notes-or-file)
        (declare (type (unsigned-byte 8) len))
        (set-tuning-notes tuning notes len descr))))

;;; We can use the ears to directly set the frequencies in a TUNING
;;; from the keynum START to the keynum END. Then we call TUNING-NOTES-FROM-DATA
;;; to update the TUNING (cents, ratios and all the other frequencies).
(defun tuning-notes-from-data (tuning start end &optional description)
  (declare (type tuning tuning) (type (integer 0 127) start end)
           (type (or null string) description)
           #.*standard-optimize-settings*)
  "Compute the notes of the scale from the frequencies stored in a
TUNING structure, starting from the keynum START and ending to the
keynum END. The ratio between the frequencies in START and END is the
last TUNING ratio."
  (when (> end start)
    (let ((len (- end start)))
      (declare (type (integer 0 127) len))
      (maybe-rewrite-tuning-cents-and-ratios tuning len)
      ;; Probably unnecessary, but it is safer to explicitly set the first slot.
      (setf (aref (tuning-cents tuning) 0) 0.0)
      (setf (aref (tuning-ratios tuning) 0) 1)
      (loop for k from (1+ start) to end
            for i from 1
            with data = (tuning-data tuning) do
              (setf (aref (tuning-ratios tuning) i)
                    (reduce-warnings
                      (rationalize
                        ;; Integers too large with double precision.
                        (coerce (/ (smp-ref data k) (smp-ref data start))
                                'single-float))))
              (setf (aref (tuning-cents tuning) i)
                    (* (log (aref (tuning-ratios tuning) i) 2) 1200.0)))
      (when description
        (setf (tuning-description tuning) description))
      (update-tuning-data tuning))))

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

(defun load-sclfile (filespec)
  "Return the description, the number of notes and the pitch values of a
scale stored in FILESPEC in scale file format."
  (declare (type (or string pathname) filespec))
  (with-open-file (scl filespec)
    (let ((descr (scl-description scl)))
      (when descr
        (let ((n (scl-num-of-notes scl)))
          (when (and (numberp n) (plusp n))
            (let ((notes (scl-read-notes n scl)))
              (when (and notes (= (length notes) n))
                (values descr n notes)))))))))

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
  "Save the notes of a TUNING structure in scale file format."
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

(defvar *tuning-et12* (make-tuning))
(declaim (type tuning *tuning-et12*))

(defvar *default-tuning* *tuning-et12*)
(declaim (type tuning *default-tuning*))
