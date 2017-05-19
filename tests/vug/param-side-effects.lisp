(in-package :incudine-tests)

(define-ugen param-side-effects-1 sample ((list list) duration (base real)
                                          (location fixnum))
  (:defaults '(0 0 1 1 2 0) 4 7 0)
  (:accessors (location))
  (with ((env (breakpoints->local-env list :base base :duration 1.0)))
    ;; ENVELOPE inlined to avoid a failure if it is vug-compiled with
    ;; return-type SAMPLE (return-type T is ok in this case because we get
    ;; the values by using MULTIPLE-VALUE-BIND with type declarations).
    (declare (inline envelope))
    (multiple-value-bind (e i) (envelope env 1 duration #'free location)
      (declare (sample e) (fixnum i))
      ;; LOCATION changes during the performance but ENVELOPE doesn't depend
      ;; on this performance-time setting because it uses WITH-FOLLOW to
      ;; update the code after SET-CONTROL. This technique is useful to set
      ;; and get the updated location by using the same control name.
      (setf location i)
      e)))

(define-ugen param-side-effects-2 sample ((list list) duration (base real)
                                          (location fixnum))
  (:defaults '(0 0 1 1 2 0) 4 7 0)
  (:accessors (location))
  (with ((env (breakpoints->local-env list :base base :duration 1.0))
         ;; Rebinding to inhibit side effects.
         (pos location))
    (declare (fixnum pos) (inline envelope))
    (multiple-value-bind (e i) (envelope env 1 duration #'free location)
      (declare (sample e) (fixnum i))
      (setf pos i)
      e)))

(dsp! param-side-effects-3 (freq phase)
  (vuglet ((rorrim (in low high)
             (with-samples ((x0 (* low 2))
                            (x1 (* high 2))
                            (x in))
               (maybe-expand x)
               (loop (cond ((> x high) (setf x (- x1 x)))
                           ((< x low) (setf x (- x0 x)))
                           (t (return x)))))))
    (with-samples ((inc (* freq *twopi-div-sr*)))
      (initialize (setf phase +half-pi+))
      ;; The input of VUG RORRIM is PHASE, a control with side effects,
      ;; so the variable X in RORRIM is performance-time.
      (out (* .5 (- (rorrim phase 0 pi) +half-pi+)))
      (incf phase inc)
      (cond ((> phase +twopi+) (decf phase +twopi+))
            ((minusp phase) (incf phase +twopi+))))))

(with-ugen-test (param-side-effects.1)
    (with-ugen-instance (u param-side-effects-1)
      (let ((pos (list (param-side-effects-1-location u))))
        (loop repeat 100 do (funcall (ugen-perf-function u)))
        (push (param-side-effects-1-location u) pos)
        (set-param-side-effects-1-location u 1000)
        (push (param-side-effects-1-location u) pos)
        (loop repeat 100 do (funcall (ugen-perf-function u)))
        (push (param-side-effects-1-location u) pos)
        (nreverse pos)))
  (0 99 1000 1099))

(with-ugen-test (param-side-effects.2)
    (with-ugen-instance (u param-side-effects-2)
      (let ((pos (list (param-side-effects-2-location u))))
        (loop repeat 100 do (funcall (ugen-perf-function u)))
        (push (param-side-effects-2-location u) pos)
        (set-param-side-effects-2-location u 1000)
        (push (param-side-effects-2-location u) pos)
        (loop repeat 100 do (funcall (ugen-perf-function u)))
        (push (param-side-effects-2-location u) pos)
        (nreverse pos)))
  (0 0 1000 1000))

(with-dsp-test (param-side-effects.3
      :md5 #(51 81 0 169 234 213 207 194 109 202 77 53 135 96 13 33))
  (param-side-effects-3 6 0))
