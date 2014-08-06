(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun deprecated-msg (old new)
    (terpri *logger-stream*)
    (msg warn "~A is deprecated, use ~A instead." old new)))

;;; 20130904
(defmacro synth-seq (&rest functions)
  (incudine::deprecated-msg 'synth-seq 'dsp-seq)
  `(dsp-seq ,@functions))

;;; 20131110
(defmacro scofile->sexp (path &optional fname)
  (incudine::deprecated-msg 'scofile->sexp 'regofile->sexp)
  `(regofile->sexp ,path ,fname))

;;; 20131110
(defmacro scofile->function (path &optional fname)
  (incudine::deprecated-msg 'scofile->function 'regofile->function)
  `(regofile->function ,path ,fname))

;;; 20131110
(defmacro scofile->lispfile (rego-file &optional fname lisp-file)
  (incudine::deprecated-msg 'scofile->lispfile 'regofile->lispfile)
  `(regofile->lispfile ,rego-file ,fname ,lisp-file))

(in-package :incudine.util)

;;; 20130927
(defmacro data-ref (data index)
  (incudine::deprecated-msg 'data-ref 'smp-ref)
  `(smp-ref ,data ,index))

(in-package :incudine.vug)

;;; 20130904
(defmacro defsynth (name args &body body)
  (incudine::deprecated-msg 'defsynth 'dsp!)
  `(dsp! ,name ,args ,@body))

;;; 20130904
(defmacro defsynth-debug (name args &body body)
  (incudine::deprecated-msg 'defsynth-debug 'dsp-debug)
  `(dsp-debug ,name ,args ,@body))

;;; 20130904
(defun all-synth-names ()
  (incudine::deprecated-msg 'all-synth-names 'all-dsp-names)
  (all-dsp-names))

;;; 20130904
(defun free-synth-instances (&optional name)
  (incudine::deprecated-msg 'free-synth-instances
                            'free-dsp-instances)
  (free-dsp-instances name))

;;; 20130904
(defun destroy-synth (name)
  (incudine::deprecated-msg 'destroy-synth 'destroy-dsp)
  (destroy-dsp name))

;;; 20130904
(defmacro synth-node ()
  (incudine::deprecated-msg 'synth-node 'dsp-node)
  `(dsp-node))

(in-package :incudine.vug)

;;; 20131114
(defmacro frame-value-bind (vars frame &body body)
  (incudine::deprecated-msg 'frame-value-bind 'multiple-sample-bind)
  `(multiple-sample-bind ,vars ,frame ,@body))

(in-package :incudine.voicer)

;;; 20140806
(defmacro scale-midi-event-amp (midi-event mult)
  (incudine::deprecated-msg 'scale-midi-event-amp 'scale-midi-amp)
  `(scale-midi-amp ,midi-event ,mult))
