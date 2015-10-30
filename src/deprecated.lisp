(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun deprecated-msg (old new)
    (msg warn "~A is deprecated, use ~A instead." old new)))

(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   ;; 20151029
   '(make-int32-array make-int64-array make-uint32-array make-uint64-array)))

(defmacro make-int32-array (&whole whole size &key zero-p initial-element
                            initial-contents)
  (declare (ignore size zero-p initial-element initial-contents))
  (incudine::deprecated-msg 'make-int32-array 'make-i32-array)
  `(make-i32-array ,@(cdr whole)))

(defmacro make-int64-array (&whole whole size &key zero-p initial-element
                            initial-contents)
  (declare (ignore size zero-p initial-element initial-contents))
  (incudine::deprecated-msg 'make-int64-array 'make-i64-array)
  `(make-i64-array ,@(cdr whole)))

(defmacro make-uint32-array (&whole whole size &key zero-p initial-element
                             initial-contents)
  (declare (ignore size zero-p initial-element initial-contents))
  (incudine::deprecated-msg 'make-uint32-array 'make-u32-array)
  `(make-u32-array ,@(cdr whole)))

(defmacro make-uint64-array (&whole whole size &key zero-p initial-element
                             initial-contents)
  (declare (ignore size zero-p initial-element initial-contents))
  (incudine::deprecated-msg 'make-uint64-array 'make-u64-array)
  `(make-u64-array ,@(cdr whole)))
