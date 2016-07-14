(in-package :incudine)

(defun deprecated-msg (old new)
  (msg warn "~A is deprecated, use ~A instead." old new))

(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   ;; 20160714
   '(envgen x-line)))

(define-vug envgen ((env envelope) gate time-scale (done-action function))
  (initialize
    (nrt-funcall (lambda () (incudine::deprecated-msg "ENVGEN" "ENVELOPE"))))
  (envelope env gate time-scale done-action))

(define-vug x-line (start end dur (done-action function))
  (initialize
    (nrt-funcall (lambda () (incudine::deprecated-msg "X-LINE" "EXPON"))))
  (expon start end dur done-action))
