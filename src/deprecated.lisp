(in-package :incudine)

(defun deprecated-msg (old new)
  (msg warn "~A is deprecated, use ~A instead." old new))
