(in-package :incudine-tests)

(defmacro with-fifo-test ((var size) &body body)
  `(let ((,var (incudine::make-fifo :buffer-size ,size :name "test fifo 1 2 3")))
     ,@body))

(deftest ring-buffer.1
    (with-fifo-test (fifo 8)
      (values (incudine::fifo-empty-p fifo)
              (incudine::fifo-buffer-size fifo)
              (incudine::fifo-name fifo)))
  T 8 "test fifo 1 2 3")

(deftest ring-buffer.2
    (with-fifo-test (fifo 8)
      (loop for i to 10 do (incudine::enqueue i fifo))
      (let ((test1 (incudine::fifo-empty-p fifo)))
        (incudine::fifo-flush fifo)
        (values test1 (incudine::fifo-empty-p fifo))))
  NIL T)

(deftest ring-buffer.3
    (let ((acc))
      (with-fifo-test (fifo 8)
        (mapc (lambda (x)
                (incudine::enqueue-function (lambda () (push x acc)) fifo))
              (loop for i to 10 collect i))
        (let ((test1 (incudine::fifo-empty-p fifo)))
          (incudine::fifo-perform-functions fifo)
          (values test1 (incudine::fifo-empty-p fifo) acc))))
  NIL T (6 5 4 3 2 1 0))
