;;; Copyright (c) 2013-2020 Tito Latini
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

(in-package :incudine.config)

(define-constant +incudine-major+ 0)
(define-constant +incudine-minor+ 9)
(define-constant +incudine-patch+ 34)

(in-package :incudine.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *use-foreign-sample-p*
    (eq incudine.config:*sample-type* 'double-float))

  (cond (*use-foreign-sample-p*
         (defvar *foreign-sample-type* :double)
         (cffi:defctype sample :double)
         (pushnew :double-samples *features*)
         (define-constant most-negative-sample most-negative-double-float)
         (define-constant most-positive-sample most-positive-double-float)
         (define-constant least-negative-sample least-negative-double-float)
         (define-constant least-positive-sample least-positive-double-float))
        (t
         (defvar *foreign-sample-type* :float)
         (setf *foreign-sample-pool-size* 1)
         (cffi:defctype sample :float)
         (pushnew :float-samples *features*)
         (define-constant most-negative-sample most-negative-single-float)
         (define-constant most-positive-sample most-positive-single-float)
         (define-constant least-negative-sample least-negative-single-float)
         (define-constant least-positive-sample least-positive-single-float)))

  (pushnew (case incudine.config:*audio-driver*
             (:jack :jack-audio)
             (:dummy :dummy-audio)
             (otherwise :portaudio))
           *features*)

  (pushnew :incudine *features*)

  (deftype sample (&optional min max)
    `(,incudine.config:*sample-type* ,min ,max))

  (define-constant +sample-zero+ (coerce 0 'sample)
    :documentation "Zero coerced to SAMPLE.")

  (deftype positive-sample () `(sample ,least-positive-sample))

  (deftype non-negative-sample () `(sample ,+sample-zero+))

  (deftype negative-sample () `(sample * ,least-negative-sample))

  (deftype non-positive-sample () `(sample * ,+sample-zero+))

  (deftype frame ()
    "Type designator for a foreign array of samples."
    'foreign-pointer)

  (defun sample (number)
    "Coerce NUMBER to type SAMPLE."
    (coerce number 'sample))

  (define-compiler-macro sample (number)
    (if (constantp number)
        (coerce (eval number) 'sample)
        `(coerce ,number 'sample)))

  (defun force-sample-format (x)
    (declare (type real x))
    (cond ((typep x 'sample) x)
          ((floatp x)
           ;; Avoid coercing from SINGLE-FLOAT to DOUBLE-FLOAT
           (let ((str (write-to-string x))
                 (*read-default-float-format* incudine.config:*sample-type*))
             (values (read-from-string str))))
          (t (coerce x 'sample))))

  (define-constant +twopi+ (coerce (* 2 pi) 'sample)
    :documentation "Two PI coerced to SAMPLE.")

  (define-constant +half-pi+ (coerce (* 0.5 pi) 'sample)
    :documentation "Half PI coerced to SAMPLE")

  (define-constant +rtwopi+ (/ 1.0 +twopi+)
    :documentation "The inverse of two PI coerced to SAMPLE.")

  (define-constant +log001+ (log (force-sample-format 0.001))
    :documentation "Natural logarithm of 0.001 coerced to SAMPLE.")

  (define-constant +sqrt2+ (sqrt (force-sample-format 2.0))
    :documentation "Square root of two coerced to SAMPLE.")

  (define-constant +pointer-size+ (foreign-type-size :pointer)
    :documentation "Size in bytes of the foreign pointer.")

  (define-constant +foreign-sample-size+ (foreign-type-size 'sample)
    :documentation "Size in bytes of the foreign type SAMPLE.")

  (define-constant +foreign-complex-size+ (* 2 (foreign-type-size 'sample))
    :documentation "Size in bytes of a foreign complex number.")

  (define-constant +pointer-address-type+
      (alexandria:make-keyword (format nil "INT~D"
                                       (* +pointer-size+ 8)))
    :documentation "Foreign address type. Should be one of :INT32, :INT64.")

  (define-constant +table-maxlen+ #x1000000
    :documentation "Maximum table length. It is assumed to be a power of two.")

  (define-constant +max-lobits+ (integer-length (1- +table-maxlen+))
    :documentation "Number of bits needed to represent the bitmask +PHASE-MASK+.")

  (define-constant +phase-mask+ (1- +table-maxlen+)
    :documentation "The bitmask calculated as +TABLE-MAXLEN+ minus 1.")

  (define-constant +rad2inc+ (* +table-maxlen+ +rtwopi+)
    :documentation "Division of +TABLE-MAXLEN+ by two PI.")

  (defvar *sample-rate* (force-sample-format incudine.config::*sample-rate*)
    "Sample rate in Hz.")
  (declaim (type sample *sample-rate*))

  (defvar *sample-duration* (/ 1.0 *sample-rate*)
    "Duration of one sample in seconds.")
  (declaim (type sample *sample-duration*))

  (defvar *sample-duration-msec* (* *sample-duration* 1000))
  (declaim (type sample *sample-duration-msec*))

  (defvar *sound-velocity*
    (force-sample-format incudine.config::*sound-velocity*)
    "Velocity of the sound in m/s at 22 degrees Celsius, 1 atmosfera.")
  (declaim (type sample *sound-velocity*))

  (defvar *r-sound-velocity* (/ 1.0 *sound-velocity*)
    "The inverse of the sound velocity in s/m at 22 degrees Celsius, 1 atmosfera.")
  (declaim (type sample *r-sound-velocity*))

  (defvar *sample-rate-hook* nil
    "A list of function designators which are called in an unspecified
order when the sample rate is changed via SET-SAMPLE-RATE or
SET-SAMPLE-DURATION.")
  (declaim (type list *sample-rate-hook*))

  (defvar *sound-velocity-hook* nil
    "A list of function designators which are called in an unspecified
order when the sound velocity is changed via SET-SOUND-VELOCITY.")
  (declaim (type list *sound-velocity-hook*))

  (defvar *block-size-hook* nil
    "A list of function designators which are called in an unspecified
order when the block size is changed via SET-RT-BLOCK-SIZE.")
  (declaim (type list *block-size-hook*))

  (defvar *cps2inc* (* +table-maxlen+ *sample-duration*)
    "Division of +TABLE-MAXLEN+ by the sample rate.")
  (declaim (type sample *cps2inc*))

  (defvar *pi-div-sr* (coerce (* pi *sample-duration*) 'sample)
    "Division of PI by the sample rate.")
  (declaim (type sample *pi-div-sr*))

  (defvar *minus-pi-div-sr* (- *pi-div-sr*)
    "Division of minus PI by the sample rate.")
  (declaim (type sample *minus-pi-div-sr*))

  (defvar *twopi-div-sr* (* 2 *pi-div-sr*)
    "Division of two PI by the sample rate.")
  (declaim (type sample *twopi-div-sr*))

  (defvar *sr-div-twopi* (* *sample-rate* +rtwopi+)
    "Division of the sample rate by two PI.")
  (declaim (type sample *sr-div-twopi*))

  (defvar *log001-div-sr* (/ +log001+ *sample-rate*))
  (declaim (type sample *log001-div-sr*))

  (defvar *sample-rate-deps-to-hook-p* nil)

  (defun sample-rate-deps-update-to-hook ()
    (unless *sample-rate-deps-to-hook-p*
      (pushnew (lambda ()
                 (setf *sample-duration-msec* (* 1000 *sample-duration*)
                       *cps2inc* (* +table-maxlen+ *sample-duration*)
                       *pi-div-sr* (coerce (* pi *sample-duration*) 'sample)
                       *minus-pi-div-sr* (- *pi-div-sr*)
                       *twopi-div-sr* (* 2 *pi-div-sr*)
                       *sr-div-twopi* (* *sample-rate* +rtwopi+)
                       *log001-div-sr* (/ +log001+ *sample-rate*)))
               *sample-rate-hook*)
      (setf *sample-rate-deps-to-hook-p* t)))

  (sample-rate-deps-update-to-hook)

  (defmacro with-local-sample-rate ((sample-rate) &body body)
    `(let* ((*sample-rate* (coerce ,sample-rate 'sample))
            (*sample-duration* (/ 1.0 *sample-rate*))
            (*cps2inc* (* +table-maxlen+ *sample-duration*))
            (*pi-div-sr* (coerce (* pi *sample-duration*) 'sample))
            (*minus-pi-div-sr* (- *pi-div-sr*))
            (*twopi-div-sr* (* 2 *pi-div-sr*))
            (*sr-div-twopi* (* *sample-rate* +rtwopi+))
            (*log001-div-sr* (/ +log001+ *sample-rate*)))
       ,@body))

  (defmacro with-struct-slots ((slots instance struct-name &optional package)
                               &body body)
    (with-gensyms (obj)
      `(let ((,obj ,instance))
         (symbol-macrolet
             ,(mapcar (lambda (slot-name)
                        `(,slot-name (,(alexandria:format-symbol
                                         (or package (symbol-package struct-name))
                                         "~A-~A" struct-name slot-name)
                                       ,obj)))
                      slots)
           ,@body))))

  (declaim (inline next-power-of-two))
  (defun next-power-of-two (n)
    "Return the next power of two of the positive integer N."
    (declare (type positive-fixnum n))
    (the positive-fixnum (ash 1 (integer-length (1- n)))))

  (declaim (inline power-of-two-p))
  (defun power-of-two-p (n)
    "Whether the positive integer N is a power of two."
    (declare (type positive-fixnum n))
    (zerop (logand n (1- n))))

  (defvar *default-table-size* 65536)
  (declaim (type non-negative-fixnum *default-table-size*))

  (defvar *default-bpm* 60)
  (declaim (type alexandria:positive-real *default-bpm*))

  (defvar *rt-edf-heap-size* 1024)
  (declaim (type non-negative-fixnum *rt-edf-heap-size*))

  (defvar *nrt-edf-heap-size* 65536)
  (declaim (type non-negative-fixnum *nrt-edf-heap-size*))

  (macrolet ((force-pow-of-two (value)
               `(when (and (numberp ,value) (not (power-of-two-p ,value)))
                  (setf ,value (next-power-of-two ,value)))))
    (force-pow-of-two *default-table-size*)
    (force-pow-of-two *rt-edf-heap-size*)
    (force-pow-of-two *nrt-edf-heap-size*)))

(defvar *client-name* "incudine")
(declaim (type string *client-name*))

(defvar *max-number-of-channels* 1024)
(declaim (type non-negative-fixnum *max-number-of-channels*))

(defvar *rt-priority*  68
  "Priority of the real-time thread.")

(defvar *nrt-priority* 60
  "Priority of the non-real-time thread.")

(defvar *fast-nrt-priority*
  (let ((prio (1+ *nrt-priority*)))
    (if (= prio *rt-priority*) *nrt-priority* prio))
  "Priority of the fast non-real-time thread.")

(defvar *receiver-default-priority* 63)

(defvar *max-buffer-size* 1024)

(defvar *frames-per-buffer* 256)

(defvar *sndfile-buffer-size* 1024)
(declaim (type (unsigned-byte 24) *sndfile-buffer-size*))

(defvar *default-header-type* #-darwin "wav" #+darwin "aiff")

(defvar *default-data-format* "pcm-24")

(defvar *rt-thread* nil
  "Real-time thread or NIL.")
(declaim (type (or bt:thread null) *rt-thread*))

(defvar *nrt-thread* nil
  "Non-real-time thread or NIL.")
(declaim (type (or bt:thread null) *nrt-thread*))

(defvar *fast-nrt-thread* nil
  "Fast-non-real-time thread or NIL.

The fast-non-real-time thread is used for fast communication with the
real-time thread.")
(declaim (type (or bt:thread null) *fast-nrt-thread*))

(defvar *allow-rt-memory-pool-p* t
  "Whether the foreign memory pool is usable from the real-time thread.")
(declaim (type boolean *allow-rt-memory-pool-p*))

(defvar *standard-optimize-settings* '(optimize speed (safety 0)))
(declaim (type cons *standard-optimize-settings*))

(in-package :incudine)

(defstruct (rt-params (:copier nil))
  (lock (bordeaux-threads:make-lock "RT-THREAD"))
  (driver incudine.config:*audio-driver*)
  (priority *rt-priority*)
  (frames-per-buffer incudine.config:*frames-per-buffer*)
  (status :stopped))

(defvar *rt-thread-start-hook* nil
  "A list of function designators which are called in an unspecified
order when the real-time thread starts.")
(declaim (type list *rt-thread-start-hook*))

(defvar *rt-thread-exit-hook* nil
  "A list of function designators which are called in an unspecified
order when the real-time thread exits.")
(declaim (type list *rt-thread-exit-hook*))

(defvar *initialize-hook* nil)
(declaim (type list *initialize-hook*))
