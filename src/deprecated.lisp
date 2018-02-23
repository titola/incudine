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
                 :format-control "~A is deprecated,~%use ~A instead."
                 :format-arguments (list ',name ',replacement)))
         ,form))))

(defun deprecated-msg (old new)
  (msg warn "~A is deprecated,~%~6Tuse ~A instead." old new))

;;;---------------------[ Deprecated symbols ]----------------------

(in-package :incudine.util)

(incudine::deprecated-function
   (thread-set-priority (thread priority)
     (setf (thread-priority thread) priority))
   :date 20180201
   :package "INCUDINE.UTIL"
   :run-time-message-p t)
