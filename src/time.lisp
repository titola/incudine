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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *incudine-readtable* (copy-readtable))
  (defvar *set-readtable-p* t)

  (when *set-readtable-p*
    (push (lambda ()
            (unless (eq *readtable* *incudine-readtable*)
              (setf *readtable* *incudine-readtable*)))
          *initialize-hook*)
    (setf *set-readtable-p* nil)))

(defstruct (tempo (:constructor %make-tempo)
                  (:copier nil))
  (bpm-ptr (error "Missing BPM") :type foreign-pointer))

(defun make-tempo (bpm)
  (let* ((bpm (sample bpm))
         (ptr (foreign-alloc 'sample :count 2
                             :initial-contents `(,bpm ,(/ (sample 60) bpm))))
         (obj (%make-tempo :bpm-ptr ptr)))
    (tg:finalize obj (lambda () (foreign-free ptr)))
    obj))

;;; Default tempo
(defvar *tempo* (make-tempo 60))
(declaim (type tempo *tempo*))

(declaim (inline bpm))
(defun bpm (tempo)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t)
    (mem-ref (tempo-bpm-ptr tempo) 'sample)))

(declaim (inline set-bpm))
(defun set-bpm (tempo bpm)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t)
    (setf #1=(mem-ref (tempo-bpm-ptr tempo) 'sample)
          (sample bpm))
    (setf (mem-ref (tempo-bpm-ptr tempo) 'sample +foreign-sample-size+)
          (/ (sample 60) #1#))
    bpm))

(defsetf bpm set-bpm)

(declaim (inline bps))
(defun bps (tempo)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t)
    (mem-ref (tempo-bpm-ptr tempo) 'sample +foreign-sample-size+)))

(declaim (inline set-bps))
(defun set-bps (tempo bps)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t)
    (setf #1=(mem-ref (tempo-bpm-ptr tempo) 'sample +foreign-sample-size+)
          (sample bps))
    (setf (mem-ref (tempo-bpm-ptr tempo) 'sample)
          (/ (sample 60) #1#))
    bps))

(defsetf bps set-bps)

(defmethod print-object ((obj tempo) stream)
  (format stream "#<TEMPO ~,2F>" (bpm obj)))

(defvar *sample-counter* (foreign-alloc 'sample
                                        :initial-element +sample-zero+))
(declaim (type foreign-pointer *sample-counter*))

(declaim (inline now))
(defun now ()
  (mem-ref *sample-counter* 'sample))

(declaim (inline tempo-sync))
(defun tempo-sync (period)
  "Get the time synchronized to PERIOD."
  (incudine.external::%tempo-sync *sample-counter* (sample period)))

(defmacro case-char (char &body cases)
  (with-gensyms (c)
    `(let ((,c ,char))
       (declare (ignorable ,c))
       (cond ,@(mapcar (lambda (x)
                         `(,(if (eq (car x) 'otherwise)
                                t
                                `(char= ,c ,(car x)))
                            ,@(cdr x)))
                       cases)))))

(defun parse-time-unit (string arg)
  (declare (type simple-string string))
  (case-char (char string 0)
    ;; b.*  -> beats    (the optional ARG is a TEMPO instance)
    (#\b `(* incudine.util:*sample-rate* (bps ,(or arg '*tempo*))))
    ;; s    -> seconds
    ;; se.* -> seconds
    ;; sa.* -> samples
    (#\s (if (> (length string) 1)
             (case-char (char string 1)
               (#\e 'incudine.util:*sample-rate*)
               (#\a 1.0)
               (otherwise (error "Unknown time unit ~S" string)))
             'incudine.util:*sample-rate*))
    ;; m    -> meters   (the optional ARG is the velocity of the sound [m/s])
    ;; me.* -> meters   "            "            "            "            "
    ;; mi.* -> minutes
    ;; ms.* -> msec
    (#\m (if (> (length string) 1)
             (case-char (char string 1)
               (#\s '(* 1.0e-3 incudine.util:*sample-rate*))
               (#\e `(* ,(if arg (/ 1.0 arg) 'incudine.util:*r-sound-velocity*)
                        incudine.util:*sample-rate*))
               (#\i '(* 60.0 incudine.util:*sample-rate*))
               (otherwise (error "Unknown time unit ~S" string)))
             `(* ,(if arg (/ 1.0 arg) 'incudine.util:*r-sound-velocity*)
                 incudine.util:*sample-rate*)))
    ;; h.*  -> hours
    (#\h '(*     3600.0 incudine.util:*sample-rate*))
    ;; d.*  -> days
    (#\d '(*    86400.0 incudine.util:*sample-rate*))
    ;; w.*  -> weeks
    (#\w '(*   604800.0 incudine.util:*sample-rate*))
    (otherwise (error "Unknown time unit ~S" string))))

(defun split-unit-time-string (stream)
  (declare (type stream stream))
  (let ((count 1) lst acc)
    (do ((prev #\space curr)
         (curr (read-char stream) (read-char stream)))
        ((char= curr #\])
         (when lst
           (push (coerce (nreverse lst) 'string) acc)))
      (if (char= curr #\space)
          (when (char/= prev #\space)
            (push (coerce (nreverse lst) 'string) acc)
            (setf lst nil)
            (if (= count 3)
                (return)
                (incf count)))
          (push curr lst)))
    (nreverse acc)))

(defun parse-time-string (stream subchar arg)
  (declare #.*standard-optimize-settings*
           (type stream stream)
           (ignore subchar arg))
  (let ((str-list (split-unit-time-string stream)))
    (declare (type list str-list))
    (if (null str-list)
        +sample-zero+
        (let ((mult (locally
                        (declare #.*reduce-warnings*)
                        (sample (read-from-string (first str-list)))))
              (time-unit-str (second str-list)))
          (if (null time-unit-str)
              mult
              (let ((tempo (third str-list)))
              `(* ,mult ,(parse-time-unit time-unit-str
                                          (when tempo
                                            (read-from-string tempo))))))))))

(set-dispatch-macro-character #\# #\[ #'parse-time-string
  *incudine-readtable*)
