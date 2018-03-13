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

(in-package :incudine.config)

(define-constant +incudine-major+ 0)
(define-constant +incudine-minor+ 9)
(define-constant +incudine-patch+ 16)

(in-package :incudine.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *use-foreign-sample-p* (eq *sample-type* 'double-float))

  (unless *use-foreign-sample-p* (setf *foreign-sample-pool-size* 1))

  (cond ((eq *sample-type* 'double-float)
         (defvar *foreign-sample-type* :double)
         (cffi:defctype sample :double)
         (pushnew :double-samples *features*)
         (define-constant most-negative-sample most-negative-double-float)
         (define-constant most-positive-sample most-positive-double-float)
         (define-constant least-negative-sample least-negative-double-float)
         (define-constant least-positive-sample least-positive-double-float))
        (t
         (defvar *foreign-sample-type* :float)
         (cffi:defctype sample :float)
         (pushnew :float-samples *features*)
         (define-constant most-negative-sample most-negative-single-float)
         (define-constant most-positive-sample most-positive-single-float)
         (define-constant least-negative-sample least-negative-single-float)
         (define-constant least-positive-sample least-positive-single-float)))

  (pushnew (case *audio-driver*
             (:jack :jack-audio)
             (:dummy :dummy-audio)
             (otherwise :portaudio))
           *features*)

  (pushnew :incudine *features*)

  (deftype sample (&optional min max) `(,*sample-type* ,min ,max))

  (define-constant +sample-zero+ (coerce 0 'sample))

  (deftype positive-sample () `(sample ,least-positive-sample))

  (deftype non-negative-sample () `(sample ,+sample-zero+))

  (deftype negative-sample () `(sample * ,least-negative-sample))

  (deftype non-positive-sample () `(sample * ,+sample-zero+))

  (deftype frame () 'foreign-pointer)

  (defun force-sample-format (x)
    (declare (type real x))
    (cond ((typep x 'sample) x)
          ((floatp x)
           ;; Avoid coercing from SINGLE-FLOAT to DOUBLE-FLOAT
           (let ((str (write-to-string x))
                 (*read-default-float-format* *sample-type*))
             (values (read-from-string str))))
          (t (coerce x 'sample))))

  (define-constant +twopi+ (coerce (* 2 pi) 'sample))
  (define-constant +half-pi+ (coerce (* 0.5 pi) 'sample))
  (define-constant +rtwopi+ (/ 1.0 +twopi+))
  (define-constant +log001+ (log (force-sample-format 0.001)))
  (define-constant +sqrt2+ (sqrt (force-sample-format 2.0)))

  (define-constant +pointer-size+ (foreign-type-size :pointer))
  (define-constant +foreign-sample-size+ (foreign-type-size 'sample))
  (define-constant +foreign-complex-size+ (* 2 (foreign-type-size 'sample)))

  (define-constant +pointer-address-type+
      (alexandria:make-keyword (format nil "INT~D"
                                       (* +pointer-size+ 8))))

  (define-constant +table-maxlen+ #x1000000)
  (define-constant +max-lobits+ (floor (log +table-maxlen+ 2)))
  (define-constant +phase-mask+ (1- +table-maxlen+))
  (define-constant +rad2inc+ (* +table-maxlen+ +rtwopi+))

  (defvar *sample-rate* (force-sample-format incudine.config::*sample-rate*))
  (declaim (type sample *sample-rate*))

  (defvar *sample-duration* (/ 1.0 *sample-rate*))
  (declaim (type sample *sample-duration*))

  (defvar *sample-duration-msec* (* *sample-duration* 1000))
  (declaim (type sample *sample-duration-msec*))

  (defvar *sound-velocity*
    (force-sample-format incudine.config::*sound-velocity*))
  (declaim (type sample *sound-velocity*))

  (defvar *r-sound-velocity* (/ 1.0 *sound-velocity*))
  (declaim (type sample *r-sound-velocity*))

  (defvar sample-rate-hook nil)
  (declaim (type list sample-rate-hook))

  (defvar sample-duration-hook nil)
  (declaim (type list sample-duration-hook))

  (defvar sound-velocity-hook nil)
  (declaim (type list sound-velocity-hook))

  (defvar *cps2inc* (* +table-maxlen+ *sample-duration*))
  (declaim (type sample *cps2inc*))

  (defvar *pi-div-sr* (coerce (* pi *sample-duration*) 'sample))
  (declaim (type sample *pi-div-sr*))

  (defvar *minus-pi-div-sr* (- *pi-div-sr*))
  (declaim (type sample *minus-pi-div-sr*))

  (defvar *twopi-div-sr* (* 2 *pi-div-sr*))
  (declaim (type sample *twopi-div-sr*))

  (defvar *sample-rate-deps-to-hook-p* nil)

  (defun sample-rate-deps-update-to-hook ()
    (unless *sample-rate-deps-to-hook-p*
      (pushnew (lambda ()
                 (setf *sample-duration-msec* (* 1000 *sample-duration*)
                       *cps2inc* (* +table-maxlen+ *sample-duration*)
                       *pi-div-sr* (coerce (* pi *sample-duration*) 'sample)
                       *minus-pi-div-sr* (- *pi-div-sr*)
                       *twopi-div-sr* (* 2 *pi-div-sr*)))
               sample-duration-hook)
      (setf *sample-rate-deps-to-hook-p* t)))

  (sample-rate-deps-update-to-hook)

  (defmacro with-local-sample-rate ((sample-rate) &body body)
    `(let* ((*sample-rate* (coerce ,sample-rate 'sample))
            (*sample-duration* (/ 1.0 *sample-rate*))
            (*cps2inc* (* +table-maxlen+ *sample-duration*))
            (*pi-div-sr* (coerce (* pi *sample-duration*) 'sample))
            (*minus-pi-div-sr* (- *pi-div-sr*))
            (*twopi-div-sr* (* 2 *pi-div-sr*)))
       ,@body))

  (defmacro with-struct-slots ((slots instance struct-name) &body body)
    (with-gensyms (obj)
      `(let ((,obj ,instance))
         (symbol-macrolet
             ,(mapcar (lambda (slot-name)
                        `(,slot-name (,(alexandria:format-symbol
                                         (symbol-package struct-name)
                                         "~A-~A" struct-name slot-name)
                                       ,obj)))
                      slots)
           ,@body))))

  (declaim (inline next-power-of-two))
  (defun next-power-of-two (n)
    (declare (type positive-fixnum n))
    (the positive-fixnum (ash 1 (integer-length (1- n)))))

  (declaim (inline power-of-two-p))
  (defun power-of-two-p (n)
    "Is the number N a power of two?"
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

(defvar *rt-priority*  68)

(defvar *nrt-priority* 60)

(defvar *fast-nrt-priority*
  (let ((prio (1+ *nrt-priority*)))
    (if (= prio *rt-priority*) *nrt-priority* prio)))

(defvar *receiver-default-priority* 63)

(defvar *max-buffer-size* 1024)

(defvar *frames-per-buffer* 256)

(defvar *sndfile-buffer-size* 1024)
(declaim (type (unsigned-byte 24) *sndfile-buffer-size*))

(defvar *default-header-type* #-darwin "wav" #+darwin "aiff")

(defvar *default-data-format* "pcm-24")

(defvar *rt-thread* nil)
(declaim (type (or bt:thread null) *rt-thread*))

(defvar *nrt-thread* nil)
(declaim (type (or bt:thread null) *nrt-thread*))

(defvar *fast-nrt-thread* nil)
(declaim (type (or bt:thread null) *fast-nrt-thread*))

(defvar *allow-rt-memory-pool-p* t)
(declaim (type boolean *allow-rt-memory-pool-p*))

(defvar *standard-optimize-settings* '(optimize speed (safety 0)))
(declaim (type cons *standard-optimize-settings*))

(in-package :incudine)

(defstruct (rt-params (:copier nil))
  (driver *audio-driver*)
  (priority *rt-priority*)
  (frames-per-buffer incudine.util::*frames-per-buffer*)
  (status :stopped))

(defvar *rt-params* (make-rt-params))

(defvar *rt-thread-start-hook* nil)
(declaim (type list *rt-thread-start-hook*))

(defvar *rt-thread-exit-hook* nil)
(declaim (type list *rt-thread-exit-hook*))

(defvar *initialize-hook* nil)
(declaim (type list *initialize-hook*))
