;;; Copyright (c) 2013-2014 Tito Latini
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
  (defvar *set-readtable-p* t)

  (when *set-readtable-p*
    (push (lambda ()
            (setf *readtable* (copy-readtable *readtable*))
            (set-sharp-square-bracket-syntax))
          *initialize-hook*)
    (setf *set-readtable-p* nil)))

(defstruct (tempo (:constructor %make-tempo) (:copier nil))
  (bpm-ptr (error "Missing BPM") :type foreign-pointer))

(defun make-tempo (bpm)
  (let* ((bpm (sample bpm))
         (ptr (foreign-alloc 'sample :count 2
                             :initial-contents `(,bpm ,(/ (sample 60) bpm))))
         (obj (%make-tempo :bpm-ptr ptr)))
    (tg:finalize obj (lambda () (foreign-free ptr)))
    obj))

;;; Default tempo
(defvar *tempo* (make-tempo *default-bpm*))
(declaim (type tempo *tempo*))

(declaim (inline bpm))
(defun bpm (tempo)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t)
    (smp-ref (tempo-bpm-ptr tempo) 0)))

(declaim (inline set-bpm))
(defun set-bpm (tempo bpm)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t)
    (setf #1=(smp-ref (tempo-bpm-ptr tempo) 0)
          (sample bpm))
    (setf (smp-ref (tempo-bpm-ptr tempo) 1)
          (/ (sample 60) #1#))
    bpm))

(defsetf bpm set-bpm)

(declaim (inline bps))
(defun bps (tempo)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t) (smp-ref (tempo-bpm-ptr tempo) 1)))

(declaim (inline set-bps))
(defun set-bps (tempo bps)
  (declare (type tempo tempo))
  (rt-eval (:return-value-p t)
    (setf #1=(smp-ref (tempo-bpm-ptr tempo) 1)
          (sample bps))
    (setf (smp-ref (tempo-bpm-ptr tempo) 0)
          (/ (sample 60) #1#))
    bps))

(defsetf bps set-bps)

(defmethod print-object ((obj tempo) stream)
  (format stream "#<TEMPO ~,2F>" (bpm obj)))

(defvar *sample-counter* (foreign-alloc 'sample :initial-element +sample-zero+))
(declaim (type foreign-pointer *sample-counter*))

(declaim (inline now))
(defun now ()
  (smp-ref *sample-counter* 0))

(declaim (inline incf-time))
(defun incf-sample-counter ()
  (incf (smp-ref *sample-counter* 0) 1.0)
  (values))

(declaim (inline reset-timer))
(defun reset-sample-counter ()
  (setf (smp-ref *sample-counter* 0) +sample-zero+)
  (values))

(declaim (inline tempo-sync))
(defun tempo-sync (period)
  "Get the time synchronized to PERIOD."
  (incudine.external::%tempo-sync *sample-counter* (sample period)))

(defstruct (tempo-envelope (:constructor %make-tempo-envelope)
                           (:copier nil))
  (bps (error "Missing BPS envelope") :type envelope)
  (time-warp (null-pointer) :type foreign-pointer)
  (points 0 :type non-negative-fixnum))

(defmethod print-object ((obj tempo-envelope) stream)
  (let ((bps-env (tempo-envelope-bps obj)))
    (format stream "#<~S :POINTS ~D :LOOP-NODE ~D :RELEASE-NODE ~D>"
            (type-of obj)
            (tempo-envelope-points obj) (envelope-loop-node bps-env)
            (envelope-release-node bps-env))))

(defmacro make-tempo-envelope (&whole args bpms beats &key curve (loop-node -1)
                               (release-node -1) restart-level real-time-p)
  (declare (ignore beats curve loop-node release-node restart-level
                   real-time-p))
  (with-gensyms (tempo-env bps-env bpm points twarp-data)
    `(let* ((,bps-env (make-envelope (mapcar (lambda (,bpm)
                                               (/ ,(sample 60) ,bpm)) ,bpms)
                                     ,@(cddr args)))
            (,points (envelope-points ,bps-env))
            (,twarp-data (foreign-alloc 'sample :count ,points))
            (,tempo-env (%make-tempo-envelope :bps ,bps-env
                                              :time-warp ,twarp-data
                                              :points ,points)))
       (tg:finalize ,tempo-env (lambda () (foreign-free ,twarp-data)))
       (fill-time-warp-data ,twarp-data ,bps-env)
       ,tempo-env)))

(defmethod free ((obj tempo-envelope))
  (unless (null-pointer-p #1=(tempo-envelope-time-warp obj))
    (foreign-free #1#)
    (tg:cancel-finalization obj)
    (setf #1# (null-pointer))
    (free (tempo-envelope-bps obj))
    (setf (tempo-envelope-points obj) 0))
  (values))

(declaim (inline integrate-linear-curve))
(defun integrate-linear-curve (y0 y1 x)
  (let ((x2 (* x x)))
    (* 0.5 (+ (* x2 y1) (* (- (* 2 x) x2) y0)))))

(declaim (inline integrate-exp-curve))
(defun integrate-exp-curve (y0 y1 x)
  (let* ((c0 (/ y1 y0))
         (c1 (log c0)))
    (* y0 (- (/ (expt c0 x) c1) (/ 1.0 c1)))))

(declaim (inline integrate-sine-curve))
(defun integrate-sine-curve (y0 y1 x)
  (/ (- (+ (* (- y1 y0) (sin (* pi x)))
           (* (- (* (- pi) y1) (* pi y0)) x)))
     +twopi+))

(declaim (inline integrate-welch-curve))
(defun integrate-welch-curve (y0 y1 x)
  (let* ((c0 (* pi x))
         (c1 (* 0.5 c0)))
    (/ (if (>= y1 y0)
           (let ((c2 (* 2 (cos c1))))
             (+ (* (- c2 2) y1) (* (+ (- (- c2) c0) 2) y0)))
           (let ((c2 (* 2 (sin c1))))
             (- (* (- c2 c0) y1) (* c2 y0))))
       (- pi))))

(declaim (inline integrate-square-curve))
(defun integrate-square-curve (y0 y1 x)
  (let* ((x2 (* x x))
         (x3 (* x2 x)))
    (/ (+ (* (- x3) y1) (* (- (* 2 x3) (* 3 x2)) (sqrt y0) (sqrt y1))
          (* (- (+ (- x3) (* 3 x2)) (* 3 x)) y0))
       -3.0)))

(declaim (inline integrate-cubic-curve))
(defun integrate-cubic-curve (y0 y1 x)
  (let* ((x2 (* x x))
         (x3 (* x2 x))
         (x4 (* x2 x2))
         (c0 (expt y0 1/3))
         (c1 (expt y1 1/3)))
    (* -0.25 (+ (* (- x4) y1) (* (- (* 3 x4) (* 4 x3)) c0 c1 c1)
                (* (+ (* -3 x4) (* 8 x3) (* -6 x2)) (* c0 c0 c1))
                (* (+ x4 (* -4 x3) (* 6 x2) (* -4 x)) y0)))))

(declaim (inline integrate-custom-curve))
(defun integrate-custom-curve (y0 y1 curve x)
  (let* ((c0 (exp curve))
         (c1 (- (* curve c0) curve))
         (cx (* curve x))
         (c2 (exp cx)))
    (/ (+ (* (- c2 cx) y1) (* (- (* cx c0) c2) y0) (- y0 y1)) c1)))

(defun segment-time-warp (y0 y1 curve t1 x)
  (* t1 (if (or (= y0 y1) (eq curve +seg-step-func+))
            (* y0 x)
            (curve-case curve
              (+seg-lin-func+ (integrate-linear-curve y0 y1 x))
              (+seg-exp-func+ (integrate-exp-curve y0 y1 x))
              (+seg-sine-func+ (integrate-sine-curve y0 y1 x))
              (+seg-welch-func+ (integrate-welch-curve y0 y1 x))
              (+seg-square-func+ (integrate-square-curve y0 y1 x))
              (+seg-cubic-func+ (integrate-cubic-curve y0 y1 x))
              (otherwise (integrate-custom-curve y0 y1 curve x))))))

(defun fill-time-warp-data (twarp-data bps-env)
  (let* ((data (envelope-data bps-env))
         (points (envelope-points bps-env))
         (twarp0 (segment-time-warp (smp-ref data 0) (smp-ref data 2)
                                    (smp-ref data 3) (smp-ref data 1) 1)))
    (setf (smp-ref twarp-data 0) +sample-zero+
          (smp-ref twarp-data 1) twarp0)
    (unless (< points 3)
      (loop for i from 4 to (* (1- points) 3) by 3
            for twdata-index from 2
            for y0 = (smp-ref data 2) then y1
            for y1 = (smp-ref data (+ i 1))
            for t0 = twarp0 then time
            for t1 = (segment-time-warp y0 y1
                                        (smp-ref data (+ i 2))
                                        (smp-ref data i) 1)
            for time = (+ t0 t1) do
           (setf (smp-ref twarp-data twdata-index) time)))
    (values)))

(defmacro set-tempo-envelope (&whole args env bpms beats &key curve
                              (loop-node -1) (release-node -1)
                              restart-level)
  (declare (ignore beats curve loop-node release-node restart-level))
  (with-gensyms (bps-env twarp-data bpm points)
    `(let ((,bps-env (tempo-envelope-bps ,env))
           (,twarp-data (tempo-envelope-time-warp ,env)))
       (set-envelope ,bps-env (mapcar (lambda (,bpm) (/ ,(sample 60) ,bpm))
                                      ,bpms)
                     ,@(cdddr args))
       (let ((,points (envelope-points ,bps-env)))
         (unless (= ,points #1=(tempo-envelope-points ,env))
           (setf #1# ,points)
           (foreign-realloc-sample ,twarp-data ,points)
           (tg:cancel-finalization ,env)
           (tg:finalize ,env (lambda () (foreign-free ,twarp-data))))
         (fill-time-warp-data ,twarp-data ,bps-env)
         ,env))))

(defun %time-at (tempo-env beats)
  (declare (type tempo-envelope tempo-env) (type (real 0) beats))
  (if (zerop beats)
      +sample-zero+
      (with-samples ((pos-time beats)
                     (delta-time 0.0)
                     (t0 0.0)
                     (t1 0.0))
        (let* ((last-point-index (1- (tempo-envelope-points tempo-env)))
               (bps-env (tempo-envelope-bps tempo-env))
               (data (envelope-data bps-env))
               (twarp-data (tempo-envelope-time-warp tempo-env)))
          (declare #.*standard-optimize-settings* #.*reduce-warnings*)
          (labels ((look (p index)
                     (declare (type non-negative-fixnum p index))
                     (setf delta-time (smp-ref data (- index 2)))
                     (setf t0 t1)
                     (incf t1 delta-time)
                     (if (< pos-time t1)
                         (+ (smp-ref twarp-data (1- p))
                            (segment-time-warp
                              (smp-ref data (if (= p 1) 0 (- index 4)))
                              (smp-ref data (- index 1))
                              (smp-ref data index)
                              delta-time (/ (- pos-time t0) delta-time)))
                         (if (< p last-point-index)
                             (look (1+ p) (+ index 3))
                             (let ((last-time (smp-ref twarp-data
                                                       last-point-index)))
                               (+ last-time (* (smp-ref data (- index 1))
                                               (- pos-time t1))))))))
            (look 1 3))))))

(declaim (inline time-at))
(defun time-at (tempo-env beats &optional (offset 0))
  (if (zerop offset)
      (%time-at tempo-env beats)
      (- (%time-at tempo-env (+ offset beats))
         (%time-at tempo-env offset))))

(declaim (inline bps-at))
(defun bps-at (tempo-env beats)
  (declare (type tempo-envelope tempo-env) (type (real 0) beats))
  (envelope-at (tempo-envelope-bps tempo-env) beats))

(declaim (inline bpm-at))
(defun bpm-at (tempo-env beats)
  (declare (type tempo-envelope tempo-env) (type (real 0) beats))
  (/ 60.0 (bps-at tempo-env beats)))

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

(defun parse-time-unit (string mult arg0 arg1)
  (declare (type simple-string string))
  (case-char (char string 0)
    ;; b.*  -> beats
    (#\b `(* incudine.util:*sample-rate*
             ,(if arg1
                  ;; ARG0 is a TEMPO-ENVELOPE and ARG1 is the
                  ;; start time in beats
                  `(time-at ,arg0 (incudine.util:sample ,mult) ,arg1)
                  ;; ARG0 is an instance of TEMPO
                  `(* (incudine.util:sample ,mult) (bps ,(or arg0 '*tempo*))))))
    ;; s    -> seconds
    ;; se.* -> seconds
    ;; sa.* -> samples
    (#\s `(* ,mult ,(if (> (length string) 1)
                        (case-char (char string 1)
                          (#\e 'incudine.util:*sample-rate*)
                          (#\a (incudine.util:sample 1.0))
                          (otherwise (error "Unknown time unit ~S" string)))
                        'incudine.util:*sample-rate*)))
    ;; m    -> meters   (the optional ARG0 is the velocity of the sound [m/s])
    ;; me.* -> meters   "            "            "             "            "
    ;; mi.* -> minutes
    ;; ms.* -> msec
    (#\m `(* ,mult ,(if (> (length string) 1)
                        (case-char (char string 1)
                          (#\s '(* 1.0e-3 incudine.util:*sample-rate*))
                          (#\e `(* ,(if arg0
                                        (/ 1.0 arg0)
                                        'incudine.util:*r-sound-velocity*)
                                   incudine.util:*sample-rate*))
                          (#\i '(* 60.0 incudine.util:*sample-rate*))
                          (otherwise (error "Unknown time unit ~S" string)))
                        `(* ,(if arg0
                                 (/ 1.0 arg0)
                                 'incudine.util:*r-sound-velocity*)
                            incudine.util:*sample-rate*))))
    ;; h.*  -> hours
    (#\h `(* ,mult 3600.0 incudine.util:*sample-rate*))
    ;; d.*  -> days
    (#\d `(* ,mult 86400.0 incudine.util:*sample-rate*))
    ;; w.*  -> weeks
    (#\w `(* ,mult 604800.0 incudine.util:*sample-rate*))
    (otherwise (error "Unknown time unit ~S" string))))

(defun split-unit-time-string (stream)
  (declare (type stream stream))
  (let ((count 1) lst acc)
    (do ((prev #\space curr)
         (curr (read-char stream) (read-char stream)))
        ((char= curr #\])
         (when lst (push (coerce (nreverse lst) 'string) acc)))
      (if (char= curr #\space)
          (when (char/= prev #\space)
            (push (coerce (nreverse lst) 'string) acc)
            (setf lst nil)
            (if (= count 4) (return) (incf count)))
          (push curr lst)))
    (nreverse acc)))

(defun parse-time-string (stream subchar arg)
  (declare #.*standard-optimize-settings*
           (type stream stream) (ignore subchar arg))
  (let ((str-list (split-unit-time-string stream)))
    (declare (type list str-list))
    (if (null str-list)
        +sample-zero+
        (let ((mult (reduce-warnings (read-from-string (first str-list))))
              (time-unit-str (second str-list)))
          (if (null time-unit-str)
              mult
              (destructuring-bind (&optional tempo beats) (cddr str-list)
                (parse-time-unit time-unit-str mult
                                 (if tempo (read-from-string tempo))
                                 (if beats (read-from-string beats)))))))))

(defmacro enable-sharp-square-bracket-syntax ()
  `(eval-when (:compile-toplevel :execute)
     (setf *readtable* (copy-readtable *readtable*))
     (set-sharp-square-bracket-syntax)))

(defun set-sharp-square-bracket-syntax ()
  (set-dispatch-macro-character #\# #\[ #'parse-time-string))
