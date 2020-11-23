(in-package :incudine-tests)

(dsp! conditional-expansion-1 ()
  (with ((p (counter 7 20))
         (y 0))
    (initialize
      ;; P is 7 before the first cycle.
      (setf y p))
    ;; First cycle: P remains 7.
    (setf y p)
    ;; First cycle: P is updated to 8.
    (update p)
    (out p y)))

(dsp! conditional-expansion-2 ()
  (with ((p (counter 7 20))
         (y0 0)
         (y 0))
    (initialize
      ;; Y0 is 7 before the first cycle.
      (setf y0 p))
    ;; First cycle: Y0 is 7.
    ;; Second cycle: Y0 remains 7.
    (setf y y0)
    ;; First cycle: Y0 remains 7 (P is not updated).
    ;; Second cycle: Y0 is 8.
    (setf y0 p)
    (out p y)))

(dsp! no-conditional-expansion-1 ()
  (with ((p (counter 7 20))
         (y 0))
    ;; First cycle: P is set to 7.
    (setf y p)
    ;; First cycle: P is updated to 8.
    (update p)
    (out p y)))

(deftest conditional-expansion.1
    (progn
      (bounce-to-buffer (*buffer-test-c2* :frames 8)
        (conditional-expansion-1))
      (subseq (buffer->list *buffer-test-c2*) 0 16))
  (8.0d0 7.0d0 10.0d0 9.0d0 12.0d0 11.0d0 14.0d0 13.0d0
   16.0d0 15.0d0 18.0d0 17.0d0 20.0d0 19.0d0 20.0d0 20.0d0))

(deftest conditional-expansion.2
    (progn
      (bounce-to-buffer (*buffer-test-c2* :frames 8)
        (conditional-expansion-2))
      (subseq (buffer->list *buffer-test-c2*) 0 16))
  (7.0d0 7.0d0 8.0d0 7.0d0 9.0d0 8.0d0 10.0d0 9.0d0
   11.0d0 10.0d0 12.0d0 11.0d0 13.0d0 12.0d0 14.0d0 13.0d0))

(deftest no-conditional-expansion.1
    (progn
      (bounce-to-buffer (*buffer-test-c2* :frames 8)
        (no-conditional-expansion-1))
      (subseq (buffer->list *buffer-test-c2*) 0 16))
  (8.0d0 7.0d0 10.0d0 9.0d0 12.0d0 11.0d0 14.0d0 13.0d0 16.0d0
   15.0d0 18.0d0 17.0d0 20.0d0 19.0d0 20.0d0 20.0d0))
