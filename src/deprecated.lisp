(in-package :incudine)

(define-condition incudine-deprecation-warning (simple-warning) ())

(defvar *deprecated* nil)

(defmacro deprecated-function ((name args &rest body)
                               &key run-time-message-p date package
                               (replacement (car body)))
  (with-gensyms (form rest)
    `(progn
       (export ',name ,(or package *package*))
       (pushnew (list ',name :from ,date) *deprecated*)
       (defun ,name ,args
         ,@(if run-time-message-p
               `((incudine::deprecated-msg ',name ',replacement)))
         ,@body)
       (define-compiler-macro ,name (&whole ,form &rest ,rest)
         (declare (ignore ,rest))
         (warn (make-condition 'incudine-deprecation-warning
                 :format-control "~A is deprecated,~%use ~A instead."
                 :format-arguments (list ',name ',replacement)))
         ,form))))

(defmacro deprecated-macro ((name args &rest body)
                            &key date package (replacement (car body)))
  (multiple-value-bind (decl form)
      (incudine.util::separate-declaration body)
    `(progn
       (export ',name ,(or package *package*))
       (pushnew (list ',name :from ,date) *deprecated*)
       (defmacro ,name ,args ,@decl
         (warn (make-condition 'incudine-deprecation-warning
           :format-control "~A is deprecated,~%use ~A instead."
           :format-arguments (list ',name ',replacement)))
         ,@form))))

(defun deprecated-msg (old new)
  (msg warn "~A is deprecated,~%~6Tuse ~A instead." old new))

;;;---------------------[ Deprecated symbols ]----------------------

(in-package :incudine.util)

(incudine::deprecated-function
   (thread-set-priority (thread priority)
     (setf (thread-priority thread) priority))
   :date 20180201
   :package "INCUDINE.UTIL"
   :run-time-message-p t)

(in-package :incudine.vug)

(incudine::deprecated-macro
   (make-local-buffer (&whole whole frames &key (channels 1) file offset
                       sample-rate initial-contents fill-function)
     (declare (ignore frames channels file offset sample-rate initial-contents
                      fill-function))
     `(incudine:make-buffer ,@(cdr whole)))
   :replacement incudine:make-buffer
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-envelope (&whole whole levels times &key curve base
                         (loop-node -1) (release-node -1) restart-level)
     (declare (ignore levels times curve base loop-node release-node
                      restart-level))
     `(incudine:make-envelope ,@(cdr whole)))
   :replacement incudine:make-envelope
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-linen (&whole whole attack-time sustain-time release-time
                      &key (level 1) (curve :lin) base restart-level)
     (declare (ignore attack-time sustain-time release-time level curve base
                      restart-level))
     `(incudine:make-linen ,@(cdr whole)))
   :replacement incudine:make-linen
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-perc (&whole whole attack-time release-time
                     &key (level 1) (curve -4) base restart-level)
     (declare (ignore attack-time release-time level curve base restart-level))
     `(incudine:make-perc ,@(cdr whole)))
   :replacement incudine:make-perc
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-cutoff (&whole whole release-time
                       &key (level 1) (curve :exp) base restart-level)
     (declare (ignore release-time level curve base restart-level))
     `(incudine:make-cutoff ,@(cdr whole)))
   :replacement incudine:make-cutoff
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-asr (&whole whole attack-time sustain-level release-time
                    &key (curve -4) base restart-level)
     (declare (ignore attack-time sustain-level release-time curve base
                      restart-level))
     `(incudine:make-asr ,@(cdr whole)))
   :replacement incudine:make-asr
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-adsr (&whole whole attack-time decay-time sustain-level release-time
                     &key (peak-level 1) (curve -4) base restart-level)
     (declare (ignore attack-time decay-time sustain-level release-time
                      peak-level curve base restart-level))
     `(incudine:make-adsr ,@(cdr whole)))
   :replacement incudine:make-adsr
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-dadsr (&whole whole delay-time attack-time decay-time
                      sustain-level release-time
                      &key (peak-level 1) (curve -4) base restart-level)
     (declare (ignore delay-time attack-time decay-time sustain-level
                      release-time peak-level curve base restart-level))
     `(incudine:make-dadsr ,@(cdr whole)))
   :replacement incudine:make-dadsr
   :date 20180330
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (breakpoints->local-env (&whole whole bp-seq
                            &key curve base scaler offset duration (loop-node -1)
                            (release-node -1) restart-level)
     (declare (ignore bp-seq curve base scaler offset duration loop-node
                      release-node restart-level))
     `(incudine:breakpoints->env ,@(cdr whole)))
   :replacement incudine:breakpoints->env
   :date 20180330
   :package "INCUDINE.VUG")
