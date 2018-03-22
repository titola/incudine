(in-package :incudine-tests)

(dsp! channels-test-1 ()
  (foreach-frame
    (out (+ (audio-in 0) (audio-in 1) (audio-in 2) (audio-in 3))
         (+ (audio-in 4) (audio-in 5) (audio-in 6) (audio-in 7)))))

;; 8 inputs, 2 outputs
(deftest channels.1
    (with-cleanup
      (let ((input (make-buffer 100 :channels 8
              :initial-contents (loop for i below 100 collect i)))
            (*logger-stream* *null-output*))
        (bounce-to-buffer (*buffer-test-c2* :input-buffer input :frames 24)
          (channels-test-1))
        (loop for i below 24
              collect (sample->fixnum (buffer-value *buffer-test-c2* i)))))
  (6 22 38 54 70 86 102 118 134 150 166 182 198 214 230 246 262 278
   294 310 326 342 358 374))
