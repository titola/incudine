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

(in-package :incudine.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *config-loaded-p* nil)

  (declaim (special *sample-rate*
                    *sample-type*
                    *use-foreign-sample-p*
                    *frames-per-buffer*
                    *client-name*
                    *max-number-of-channels*
                    *number-of-input-bus-channels*
                    *number-of-output-bus-channels*
                    *number-of-bus-channels*
                    *rt-edf-heap-size*
                    *nrt-edf-heap-size*
                    *rt-priority*
                    *nrt-priority*
                    *receiver-default-priority*
                    *max-number-of-nodes*
                    *default-table-size*
                    *fade-curve*
                    *sound-velocity*
                    *standard-optimize-settings*
                    *foreign-sample-pool-size*
                    *foreign-rt-memory-pool-size*
                    *sndfile-buffer-size*
                    *bounce-to-disk-guard-size*
                    *default-header-type*
                    *default-data-format*))

  (unless *config-loaded-p*
    (setf *config-loaded-p*
          (load (merge-pathnames ".incudinerc" (user-homedir-pathname)))))

  (setf *use-foreign-sample-p* (or *use-foreign-sample-p*
                                   (eq *sample-type* 'double-float)))

  (unless *use-foreign-sample-p*
    (setf *foreign-sample-pool-size* 1))

  (cond ((eq *sample-type* 'double-float)
         (defvar *foreign-sample-type* :double)
         (cffi:defctype sample :double)
         (define-constant least-negative-sample least-negative-double-float)
         (define-constant least-positive-sample least-positive-double-float))
        (t
         (defvar *foreign-sample-type* :float)
         (cffi:defctype sample :float)
         (define-constant least-negative-sample least-negative-single-float)
         (define-constant least-positive-sample least-positive-single-float)))

  (defvar *audio-driver* :portaudio)

  (deftype sample (&optional min max)
    `(,*sample-type* ,min ,max))

  (define-constant +sample-zero+ (coerce 0 'sample))

  (deftype positive-sample () `(sample ,least-positive-sample))

  (deftype non-negative-sample () `(sample ,+sample-zero+))

  (deftype negative-sample () `(sample * ,least-negative-sample))

  (deftype non-positive-sample () `(sample * ,+sample-zero+))

  (deftype frame () 'foreign-pointer)

  (define-constant +twopi+ (* 2 (coerce pi 'sample)))
  (define-constant +half-pi+ (* 0.5 (coerce pi 'sample)))
  (define-constant +rtwopi+ (/ 1.0 +twopi+))
  (define-constant +log001+ (log (coerce 0.001 'sample)))
  (define-constant +sqrt2+ (sqrt (coerce 2.0 'sample)))

  (define-constant +pointer-size+ (foreign-type-size :pointer))
  (define-constant +foreign-sample-size+ (foreign-type-size 'sample))
  (define-constant +foreign-complex-size+ (* 2 (foreign-type-size 'sample)))

  (define-constant +table-maxlen+ #x1000000)
  (define-constant +max-lobits+ (floor (log +table-maxlen+ 2)))
  (define-constant +phase-mask+ (1- +table-maxlen+))
  (define-constant +rad2inc+ (* +table-maxlen+ +rtwopi+))

  (setq *sample-rate* (coerce (or *sample-rate* 48000) 'sample))
  (declaim (type sample *sample-rate*))

  (defvar *sample-duration* (/ 1.0 *sample-rate*))
  (declaim (type sample *sample-duration*))

  ;;; Velocity of the sound at 22°C, 1 atmosfera
  (setf *sound-velocity* (coerce (or *sound-velocity* 345) 'sample))
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

  (defvar *cps2inc-to-hook-p* nil)

  (defun add-cps2inc-update-to-hook ()
    (unless *cps2inc-to-hook-p*
      (pushnew (lambda ()
                 (setf *cps2inc* (* +table-maxlen+ *sample-duration*)))
               sample-duration-hook)
      (setf *cps2inc-to-hook-p* t)))

  (add-cps2inc-update-to-hook)

  (declaim (inline next-power-of-two))
  (defun next-power-of-two (n)
    (declare (type positive-fixnum n))
    (setf n (- n 1)
          n (logior n (ash n -1))
          n (logior n (ash n -2))
          n (logior n (ash n -4))
          n (logior n (ash n -8))
          n (logior n (ash n -16)))
    (the positive-fixnum (+ n 1)))

  (declaim (inline power-of-two-p))
  (defun power-of-two-p (n)
    "Is the number N a power of two?"
    (declare (type positive-fixnum n))
    (zerop (logand n (1- n))))

  (macrolet ((force-pow-of-two (value)
               `(when (and (numberp ,value)
                           (not (power-of-two-p ,value)))
                  (setf ,value (next-power-of-two ,value)))))
    (force-pow-of-two *default-table-size*)
    (force-pow-of-two *rt-edf-heap-size*)
    (force-pow-of-two *nrt-edf-heap-size*)))

(defvar *client-name* "Incudine")
(declaim (type (simple-array character) *client-name*))

(defvar *default-table-size* 65536)
(declaim (type non-negative-fixnum *default-table-size*))

(defvar *max-number-of-channels* 1024)
(declaim (type non-negative-fixnum *max-number-of-channels*))

(defvar *rt-priority*  68)

(defvar *nrt-priority* 60)

(defvar *fast-nrt-priority* (let ((prio (1+ *nrt-priority*)))
                              (if (= prio *rt-priority*)
                                  *nrt-priority*
                                  prio)))

(defvar *receiver-default-priority* 63)

(defvar *frames-per-buffer* 256)

(defvar *sndfile-buffer-size* 1024)
(declaim (type (integer 1 1000000) *sndfile-buffer-size*))

(defvar *default-header-type* #-darwin "wav" #+darwin "aiff")

(defvar *default-data-format* "float")

(defvar *rt-thread* nil)
(declaim (type (or bt:thread null) *rt-thread*))

(defvar *nrt-thread* nil)
(declaim (type (or bt:thread null) *nrt-thread*))

(defvar *fast-nrt-thread* nil)
(declaim (type (or bt:thread null) *fast-nrt-thread*))

(defvar *standard-optimize-settings* '(optimize speed (safety 0)))

(in-package :incudine)

(defstruct (rt-params (:copier nil))
  (driver            *audio-driver*)
  (priority          *rt-priority*)
  (frames-per-buffer *frames-per-buffer*)
  (status            :stopped))

(defvar *rt-params* (make-rt-params))

(defvar *initialize-hook* nil)
(declaim (type list *initialize-hook*))
