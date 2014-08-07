(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun deprecated-msg (old new)
    (terpri *logger-stream*)
    (msg warn "~A is deprecated, use ~A instead." old new)))

(in-package :incudine.voicer)

;;; 20140806
(defmacro scale-midi-event-amp (midi-event mult)
  (incudine::deprecated-msg 'scale-midi-event-amp 'scale-midi-amp)
  `(scale-midi-amp ,midi-event ,mult))
