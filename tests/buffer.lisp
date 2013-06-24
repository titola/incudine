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

(deftest set-buffer.1
    (let ((buf (make-buffer 8)))
      (set-buffer-data buf '(1 2 3 4) :start 4)
      (unless (free-p buf)
        (mapcar #'sample->fixnum (buffer->list buf))))
  (0 0 0 0 1 2 3 4))

(deftest set-buffer.2
    (let ((buf (make-buffer 8)))
      (set-buffer-data buf '(1 2 3 4) :start 4 :end 6)
      (unless (free-p buf)
        (mapcar #'sample->fixnum (buffer->list buf))))
  (0 0 0 0 1 2 0 0))

(deftest set-buffer.3
    (let ((buf (make-buffer 8)))
      (set-buffer-data buf #(1 2 3 4) :start 4 :end 6 :normalize-p t)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.0 0.0 0.0 0.0 0.5 1.0 0.0 0.0))

(deftest set-buffer.4
    (let ((buf (make-buffer 8 :fill-function (lambda (arr size)
                                               (dotimes (i size)
                                                 (setf (cffi:mem-aref arr 'sample i)
                                                       (/ (coerce i 'sample) size)))))))
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.0 0.12 0.25 0.38 0.5 0.62 0.75 0.88))

(deftest set-buffer.5
    (let ((buf (make-buffer 8)))
      (set-buffer-data buf (lambda (arr size)
                             (dotimes (i size)
                               (setf (cffi:mem-aref arr 'sample i)
                                     (/ (coerce i 'sample) size))))
                       :start 4 :end 8)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.0 0.0 0.0 0.0 0.0 0.25 0.5 0.75))

(deftest map-buffer.1
    (let ((buf (make-buffer 8)))
      (setf (data buf) '(0 1 2 3 4 5 6 7))
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

(deftest buffer-scale.1
    (let ((buf (make-buffer 8 :initial-contents '(100 105 110 115 120 125 130 135))))
      (scale buf .05)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (5.0 5.25 5.5 5.75 6.0 6.25 6.5 6.75))

(deftest buffer-normalize.1
    (let ((buf (make-buffer 8 :initial-contents '(10 20 30 40 50 60 70 80))))
      (normalize buf 1)
      (unless (free-p buf)
        (mapcar #'two-decimals (buffer->list buf))))
  (0.12 0.25 0.38 0.5 0.62 0.75 0.88 1.0))

(deftest buffer-rescale.1
    (let ((buf (make-buffer 8 :initial-contents '(10 20 30 40 50 60 70 80))))
      (rescale buf 0 1000)
      (unless (free-p buf)
        (mapcar #'sample->fixnum (buffer->list buf))))
  (0 142 285 428 571 714 857 999))
