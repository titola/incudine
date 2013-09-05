(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun deprecated-msg (old new)
    (terpri *logger-stream*)
    (msg warn "~A is deprecated, use ~A instead." old new)))

;;; 20130904
(defmacro synth-seq (&rest functions)
  (incudine::deprecated-msg 'synth-seq 'dsp-seq)
  `(dsp-seq ,@functions))

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
