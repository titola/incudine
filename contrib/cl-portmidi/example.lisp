(defpackage :portmidi-example (:use :cl))

(in-package :portmidi-example)

(pm:initialize)

(pm:get-default-input-device-id)
(getf (pm:get-device-info *) :name)
(pm:print-devices-info :input)

(defvar midiin (pm:open ***))

;;; A simple receiver

(let ((pm-state nil)
      (recv-function (lambda (time msg)
                       (multiple-value-bind (status data1 data2)
                           (pm:decode-message msg)
                         (format t "TIME ~D STATUS ~D DATA1 ~D DATA2 ~D~%"
                                 time status data1 data2)
                         (force-output)))))
  (declare (type boolean pm-state)
           (type function recv-function))

  (defun recv-start (stream)
    (declare (optimize speed (safety 0))
             (type pm:stream stream))
    (pm:with-receiver (pm-state stream msg time)
      (funcall recv-function time msg)))

  (defun set-recv-function (fn)
    (declare (type function fn))
    (setf recv-function fn))

  (defun recv-stop ()
    (setf pm-state nil)
    (recv-status))

  (defun recv-status ()
    (if pm-state :RUNNING :STOPPED)))

(recv-status)
(recv-start midiin)
(recv-status)
(recv-stop)

(pm:terminate)
