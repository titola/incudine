(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun deprecated-msg (old new)
    (msg warn "~A is deprecated, use ~A instead." old new)))
