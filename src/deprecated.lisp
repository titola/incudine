(in-package :incudine)

(defun deprecated-msg (old new)
  (msg warn "~A is deprecated, use ~A instead." old new))

(in-package :incudine.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   ;; 20180201
   '(thread-set-priority)))

(defun thread-set-priority (thread priority)
  (incudine::deprecated-msg "THREAD-SET-PRIORITY"
                            "(SETF (THREAD-PRIORITY THREAD) PRIO)")
  (setf (thread-priority thread) priority))
