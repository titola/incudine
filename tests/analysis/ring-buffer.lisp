(in-package :incudine-tests)

(deftest analysis-ring-input-buffer.1
    (with-cleanup
      (let ((rb (ana::make-ring-input-buffer
                  ;; round to next power of two (16)
                  10)))
        (assert (= (ana::ring-buffer-size rb) 16))
        (dotimes (i 20)
          (ana::ring-input-buffer-put (random (sample 1)) rb))
        (ana::ring-buffer-head rb)))
  4)

(deftest analysis-ring-input-buffer.2
    (with-cleanup
      (let* ((rb-size 16)
             (rb (ana::make-ring-input-buffer rb-size))
             (b (make-buffer 20))
             (write-head 7)
             (res nil))
        (flet ((rb-test (items)
                 (incudine.external:foreign-zero-sample (buffer-data b) rb-size)
                 (ana::copy-from-ring-buffer (buffer-data b) rb items)
                 (push (mapcar #'sample->fixnum (buffer->list b)) res)))
          (dotimes (i rb-size)
            (ana::ring-input-buffer-put (sample i) rb))
          ;; ring buffer size < items, write head = 0
          (rb-test 20)
          ;; ring buffer size = items, write head = 0
          (rb-test rb-size)
          ;; ring buffer size > items, write head = 0
          (rb-test 11)
          ;; ring buffer size < items, write head = 7
          (loop for i from 100 repeat write-head do
                  (ana::ring-input-buffer-put (sample i) rb))
          (push (ana::ring-input-buffer-head rb) res)
          (rb-test 20)
          ;; ring buffer size = items, write head = 7
          (rb-test rb-size)
          ;; ring buffer size > items, write head = 7
          (rb-test 13)
          (rb-test 4)
          (nreverse res))))
  ((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0 0 0 0)
   (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0 0 0 0)
   (5 6 7 8 9 10 11 12 13 14 15 0 0 0 0 0 0 0 0 0)
   7
   (7 8 9 10 11 12 13 14 15 100 101 102 103 104 105 106 0 0 0 0)
   (7 8 9 10 11 12 13 14 15 100 101 102 103 104 105 106 0 0 0 0)
   (10 11 12 13 14 15 100 101 102 103 104 105 106 0 0 0 0 0 0 0)
   (103 104 105 106 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(deftest analysis-ring-output-buffer.1
    (with-cleanup
      (let ((rb (ana::make-ring-output-buffer
                  ;; round to next power of two (16)
                  10)))
        (assert (= (ana::ring-buffer-size rb) 16))
        (loop for i below 16 do
                (setf (smp-ref (ana::ring-buffer-data rb) i) (sample i)))
        (loop for i below 20
              collect (sample->fixnum (ana::ring-output-buffer-next rb)))))
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0 0 0 0))

(deftest analysis-ring-output-buffer.2
    (with-cleanup
      (let* ((rb-size 16)
             (rb (ana::make-ring-output-buffer rb-size))
             (b (make-buffer rb-size
                             :initial-contents (alexandria:iota rb-size)))
             (res nil))
        (flet ((rb-test (items)
                 (ana::copy-to-ring-output-buffer rb (buffer-data b) items)
                 (push (loop for i below rb-size
                             collect (sample->fixnum
                                       (ana::ring-output-buffer-next rb)))
                       res)))
          ;; ring buffer size = items
          (rb-test rb-size)
          ;; ring buffer size < items
          (rb-test (+ rb-size 7))
          ;; ring buffer size > items
          (rb-test 10)
          (nreverse res))))
  ((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
   (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
   (0 1 2 3 4 5 6 7 8 9 0 0 0 0 0 0)))
