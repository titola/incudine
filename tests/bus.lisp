(in-package :incudine-tests)

(defmacro with-bus-test ((&rest rest) &body body)
  (declare (ignore rest))
  `(let ((incudine::*bus-channels* incudine::*nrt-bus-channels*)
         (incudine::*output-pointer* incudine::*nrt-bus-channels*)
         (incudine::*input-pointer* (cffi:inc-pointer incudine::*nrt-bus-channels*
                                                 (* *number-of-output-bus-channels*
                                                    #.(cffi:foreign-type-size 'sample))))
         (incudine::*bus-pointer* (cffi:inc-pointer incudine::*nrt-bus-channels*
                                               (* (+ *number-of-input-bus-channels*
                                                     *number-of-output-bus-channels*)
                                                  #.(cffi:foreign-type-size 'sample))))
         (incudine::*output-peak-values* incudine::*nrt-output-peak-values*)
         (incudine::*out-of-range-counter* incudine::*nrt-out-of-range-counter*))
     (incudine::zeroes-nrt-bus-channels)
     (multiple-value-prog1 (progn ,@body)
       (incudine::zeroes-nrt-bus-channels))))

(deftest bus.1
    (with-bus-test ()
      (let ((b0 (two-decimals (bus 0))))
        (setf (bus 0) .25)
        (values b0 (two-decimals (bus 0)))))
  0.0 0.25)

(deftest audio-out.1
    (with-bus-test ()
      (let ((out0 (two-decimals (audio-out 0))))
        (setf (audio-out 0) .25)
        (values out0 (two-decimals (audio-out 0)))))
  0.0 0.25)

(deftest audio-out.2
    (with-bus-test ()
      (incudine::%reset-peak-meters)
      (setf (audio-out 0) .75)
      (let ((test1 (two-decimals (peak-info 0))))
        (incudine::update-peak-values 0)
        (values test1 (two-decimals (peak-info 0)))))
  0.0 0.75)
