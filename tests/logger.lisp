(in-package :incudine-tests)

(deftest logger.1
    (flet ((res (x)
             (setf (logger-level) x)
             (eq (logger-level) x)))
      (let ((incudine.util::*logger-mask* 0))
        (values (res :error) (res :warn) (res :info) (res :debug))))
  T T T T)

(deftest logger.2
    (flet ((res (x)
             (setf (logger-level) x)
             (mapcar (lambda (x)
                       (plusp (logand x incudine.util::*logger-mask*)))
                     (list incudine.util::+logger-error+
                           incudine.util::+logger-warn+
                           incudine.util::+logger-info+
                           incudine.util::+logger-debug+))))
      (let ((incudine.util::*logger-mask* 0))
        (values (res :error) (res :warn) (res :info) (res :debug))))
  (T NIL NIL NIL)
  (T T NIL NIL)
  (T T T NIL)
  (T T T T))
