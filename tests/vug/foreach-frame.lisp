(in-package :incudine-tests)

(dsp! foreach-frame-test-1 ()
  ;; FOREACH-FRAME binds a VUG-VARIABLE to NOW to initialize a local
  ;; sample counter. There is a bug if that variable is not preserved
  ;; because it is replaced by the global sample counter.
  (foreach-frame)
  ;; Global sample counter.
  (out (now)))

(deftest foreach-frame.1
    (let ((*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 8)
        (foreach-frame-test-1))
      (loop for i below 8
            collect (sample->fixnum (buffer-value *buffer-test-c1* i))))
  (0 1 2 3 4 5 6 7))
