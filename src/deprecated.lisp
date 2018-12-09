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
                 :format-control "~S is deprecated,~%use ~S instead."
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
           :format-control "~S is deprecated,~%use ~S instead."
           :format-arguments (list ',name ',replacement)))
         ,@form))))

(defun deprecated-msg (old new)
  (msg warn "~S is deprecated,~%~6Tuse ~S instead." old new))

;;;---------------------[ Deprecated symbols ]----------------------

(incudine::deprecated-function
   (set-envelope (env levels times &key curve base (loop-node -1)
                  (release-node -1) (restart-level nil restart-level-p))
     (when restart-level-p
       (setf (envelope-restart-level env) restart-level))
     (edit-envelope env levels times :curve curve :base base
                    :loop-node loop-node :release-node release-node))
   :date 20180331
   :package "INCUDINE"
   :replacement edit-envelope
   :run-time-message-p t)

(incudine::deprecated-function
   (linen (obj attack-time sustain-time release-time
           &key (level 1) (curve :lin) base)
     (edit-envelope obj :linen (list attack-time sustain-time release-time)
                    :peak-level level :curve curve :base base))
   :date 20180331
   :package "INCUDINE"
   :run-time-message-p t)

(incudine::deprecated-function
   (perc (obj attack-time release-time &key (level 1) (curve -4) base)
     (edit-envelope obj :perc (list attack-time release-time) :peak-level level
                    :curve curve :base base))
   :date 20180331
   :package "INCUDINE"
   :run-time-message-p t)

(incudine::deprecated-function
   (cutoff (obj release-time &key (level 1) (curve :exp) base)
     (edit-envelope obj :cutoff release-time :peak-level level :curve curve
                    :base base))
   :date 20180331
   :package "INCUDINE"
   :run-time-message-p t)

(incudine::deprecated-function
   (asr (obj attack-time sustain-level release-time &key (curve -4) base)
     (edit-envelope obj :asr (list attack-time sustain-level release-time)
                    :curve curve :base base))
   :date 20180331
   :package "INCUDINE"
   :run-time-message-p t)

(incudine::deprecated-function
   (adsr (obj attack-time decay-time sustain-level release-time
          &key (peak-level 1) (curve -4) base)
     (edit-envelope obj :adsr
                    (list attack-time decay-time sustain-level release-time)
                    :peak-level peak-level :curve curve :base base))
   :date 20180331
   :package "INCUDINE"
   :run-time-message-p t)

(incudine::deprecated-function
   (dadsr (obj delay-time attack-time decay-time sustain-level release-time
           &key (peak-level 1) (curve -4) base)
     (edit-envelope obj :dadsr
                    (list delay-time attack-time decay-time sustain-level
                          release-time)
                    :peak-level peak-level :curve curve :base base))
   :date 20180331
   :package "INCUDINE"
   :run-time-message-p t)

(in-package :incudine.util)

(incudine::deprecated-function
   (thread-set-priority (thread priority)
     (setf (thread-priority thread) priority))
   :date 20180201
   :package "INCUDINE.UTIL"
   :run-time-message-p t)

(incudine::deprecated-function
   (db->lin (value) (db->linear value))
   :replacement incudine.util:db->linear
   :date 20181209
   :package "INCUDINE.UTIL")

(incudine::deprecated-function
   (lin->db (value) (linear->db value))
   :replacement incudine.util:linear->db
   :date 20181209
   :package "INCUDINE.UTIL")

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
                      &key (level 1) restart-level)
     (declare (ignore attack-time sustain-time release-time level restart-level))
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

(incudine::deprecated-macro
   (make-local-fft (size &optional (window-size size) window-function)
     `(incudine.analysis:make-fft ,size
        :window-size ,window-size
        :window-function ,(or window-function '(gen:sine-window))))
   :replacement incudine.analysis:make-fft
   :date 20180403
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-ifft (size &optional (window-size size) window-function)
     `(incudine.analysis:make-ifft ,size
        :window-size ,window-size
        :window-function ,(or window-function '(gen:sine-window))))
   :replacement incudine.analysis:make-ifft
   :date 20180403
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (make-local-abuffer (analysis-object)
     `(incudine.analysis:make-abuffer ,analysis-object))
   :replacement incudine.analysis:make-abuffer
   :date 20180403
   :package "INCUDINE.VUG")

(incudine::deprecated-macro
   (done-self () `(done-p))
   :replacement incudine:done-p
   :date 20180512
   :package "INCUDINE.VUG")
