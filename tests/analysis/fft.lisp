(in-package :incudine-tests)

(defun fill-fft-input-test (obj)
  (loop for i from 1 to (fft-size obj) do
          (setf (fft-input obj) (sample i))))

(defun ifft-output-list (obj length)
  (loop for i below length collect (sample->fixnum (ifft-output obj))))

(deftest fft.1
    (with-cleanup
      (let* ((fft (make-fft 16 :window-function #'rectangular-window))
             (abuf (make-abuffer fft))
             (ifft (make-ifft 16 :window-function #'rectangular-window)))
        (fill-fft-input-test fft)
        (compute-ifft ifft abuf)
        (ifft-output-list ifft 32)))
  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(deftest fft-circular-shift.1
    (with-cleanup
      (let* ((fft (make-fft 16 :window-function #'rectangular-window))
             (abuf (make-abuffer fft))
             (ifft (make-ifft 16 :window-function #'rectangular-window)))
        (flet ((shift-test (obj n)
                 (fill-fft-input-test fft)
                 (if (fft-p obj) (circular-shift obj n))
                 (compute-ifft ifft (compute-abuffer abuf t) t)
                 (if (ifft-p obj) (circular-shift obj n))
                 (ifft-output-list ifft 32)))
          (values (shift-test fft 5)
                  (shift-test ifft 5)
                  (shift-test fft -7)
                  (shift-test ifft -7)))))
  (12 13 14 15 16 1 2 3 4 5 6 7 8 9 10 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (12 13 14 15 16 1 2 3 4 5 6 7 8 9 10 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (8 9 10 11 12 13 14 15 16 1 2 3 4 5 6 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (8 9 10 11 12 13 14 15 16 1 2 3 4 5 6 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
