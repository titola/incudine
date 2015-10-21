(in-package :incudine-tests)

(deftest buffer.1
    (let* ((buf (make-buffer 8))
           (lst (unless (free-p buf)
                  (mapcar #'sample->fixnum (buffer->list buf)))))
      (free buf)
      (values lst (free-p buf)))
  (0 0 0 0 0 0 0 0) t)

(deftest buffer-value.1
    (let* ((buf (make-buffer 8 :initial-contents '(0 1 2 3 4 5 6 7)))
           (lst (unless (free-p buf)
                  (mapcar #'sample->fixnum (buffer->list buf)))))
      (unless (free-p buf)
        (setf (buffer-value buf 4) 123))
      (values lst (sample->fixnum (buffer-value buf 4))))
  (0 1 2 3 4 5 6 7) 123)

(deftest fill-buffer.1
    (let ((buf (make-buffer 8)))
      (fill-buffer buf '(1 2 3 4) :start 4)
      (unless (free-p buf)
        (mapcar #'sample->fixnum (buffer->list buf))))
  (0 0 0 0 1 2 3 4))

(deftest fill-buffer.2
    (let ((buf (make-buffer 8)))
      (fill-buffer buf '(1 2 3 4) :start 4 :end 6)
      (unless (free-p buf)
        (mapcar #'sample->fixnum (buffer->list buf))))
  (0 0 0 0 1 2 0 0))

(deftest fill-buffer.3
    (let ((buf (make-buffer 8)))
      (fill-buffer buf #(1 2 3 4) :start 4 :end 6 :normalize-p t)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.0 0.0 0.0 0.0 0.5 1.0 0.0 0.0))

(deftest fill-buffer.4
    (let ((buf (make-buffer 8
                 :fill-function (lambda (arr size)
                                  (dotimes (i size)
                                    (setf (cffi:mem-aref arr 'sample i)
                                          (/ (coerce i 'sample) size)))))))
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.0 0.12 0.25 0.38 0.5 0.62 0.75 0.88))

(deftest fill-buffer.5
    (let ((buf (make-buffer 8)))
      (fill-buffer buf (lambda (arr size)
                         (dotimes (i size)
                           (setf (cffi:mem-aref arr 'sample i)
                                 (/ (coerce i 'sample) size))))
                   :start 4 :end 8)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.0 0.0 0.0 0.0 0.0 0.25 0.5 0.75))

(deftest map-buffer.1
    (let ((buf (make-buffer 8)))
      (fill-buffer buf '(0 1 2 3 4 5 6 7))
      (map-buffer (lambda (index value)
                    (* value value (if (< index 5) 1 value)))
                  buf)
      (unless (free-p buf)
        (mapcar #'sample->fixnum (buffer->list buf))))
  (0 1 4 9 16 125 216 343))

(deftest map-into-buffer.1
    (let ((buf1 (make-buffer 8 :initial-contents '(0 1 2 3 4 5 6 7)))
          (buf2 (make-buffer 8 :initial-contents '(100 101 102 103 104 105 106 107))))
        (map-into-buffer buf1 #'+ buf1 buf2)
        (unless (free-p buf1)
          (mapcar #'sample->fixnum (buffer->list buf1))))
  (100 102 104 106 108 110 112 114))

(deftest scale-buffer.1
    (let ((buf (make-buffer 8 :initial-contents '(100 105 110 115 120 125 130 135))))
      (scale-buffer buf .05)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (5.0 5.25 5.5 5.75 6.0 6.25 6.5 6.75))

(deftest normalize-buffer.1
    (let ((buf (make-buffer 8 :initial-contents '(10 20 30 40 50 60 70 80))))
      (normalize-buffer buf 1)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.12 0.25 0.38 0.5 0.62 0.75 0.88 1.0))

(deftest rescale-buffer.1
    (let ((buf (make-buffer 8 :initial-contents '(10 20 30 40 50 60 70 80))))
      (rescale-buffer buf 0 1000)
      (unless (free-p buf)
        (mapcar #'sample->fixnum (buffer->list buf))))
  (0 142 285 428 571 714 857 999))

(deftest with-buffer.1
    (with-buffers ((buf 64 :fill-function (gen:partials '(16 0 8 0 4 0 2 0 1)))
                   (win 64 :fill-function (gen:sine-window)))
      (mapcar #'sample->fixnum
              (buffer->list (map-into-buffer buf (lambda (x y) (* x y 10000))
                                             buf win))))
  (0 221 782 1444 1981 2314 2516 2713 2966 3233 3427 3514 3558 3668
   3901 4207 4476 4638 4745 4933 5306 5837 6377 6793 7109 7520 8219
   9146 9848 9613 7841 4442 0 -4432 -7803 -9542 -9750 -9031 -8095
   -7385 -6962 -6632 -6207 -5663 -5130 -4751 -4551 -4429 -4253
   -3977 -3665 -3423 -3295 -3224 -3110 -2895 -2612 -2339 -2107
   -1859 -1491 -965 -392 0))

(deftest buffer-circular-shift.1
    (with-buffer (buf 8 :initial-contents '(1 2 3 4 5 6 7 8))
      (flet ((circ-test (n)
               (circular-shift buf n)
               (mapcar #'sample->fixnum (buffer->list buf))))
        (values (circ-test -3) (circ-test 3) (circ-test 0) (circ-test 8)
                (circ-test 126) (circ-test -126))))
  (4 5 6 7 8 1 2 3)
  (1 2 3 4 5 6 7 8)
  (1 2 3 4 5 6 7 8)
  (1 2 3 4 5 6 7 8)
  (3 4 5 6 7 8 1 2)
  (1 2 3 4 5 6 7 8))

(deftest sort-buffer.1
    (with-buffer (buf 16 :initial-contents '(860 758 750 889 300 991 5 993 954
                                             299 99 549 196 282 351 16))
      (mapcar #'sample->fixnum (buffer->list (sort-buffer buf))))
  (5 16 99 196 282 299 300 351 549 750 758 860 889 954 991 993))
