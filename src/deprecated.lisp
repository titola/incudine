(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun deprecated-msg (old new)
    (msg warn "~A is deprecated, use ~A instead." old new)))

(in-package :incudine.vug)

;;; 20150124
(defmacro one-pole (in coef)
  (incudine::deprecated-msg 'one-pole 'pole*)
  `(pole* ,in ,coef))

;;; 20150124
(defmacro one-zero (in coef)
  (incudine::deprecated-msg 'one-zero 'zero*)
  `(zero* ,in ,coef))

;;; 20150311
(defmacro generic-rate (gate start-offset in)
  (incudine::deprecated-msg 'generic-rate 'snapshot)
  `(snapshot ,gate ,start-offset ,in))

(in-package :incudine.voicer)

;;; 20150209
(defmacro fill-freq-vector (function midi-event)
  (incudine::deprecated-msg 'fill-freq-vector 'fill-freq-table)
  `(fill-freq-table ,function ,midi-event))

;;; 20150209
(defmacro fill-amp-vector (function midi-event)
  (incudine::deprecated-msg 'fill-amp-vector 'fill-amp-table)
  `(fill-amp-table ,function ,midi-event))
