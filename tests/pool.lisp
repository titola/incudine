(in-package :incudine-tests)

;;;; CONS-POOL

(defun cons-pool-default-expand-func (pool &optional delta)
  (expand-cons-pool pool delta))

(defun cons-pool-expand-func-2 (pool &optional delta)
  (let ((i 0))
    (expand-cons-pool pool delta (list (incf i)))))

(defmacro with-cons-pool-test ((var size &optional (grow 4)
                                (expand-func #'cons-pool-default-expand-func))
                               &body body)
  `(let ((,var (make-cons-pool
                 :data (make-list ,size)
                 :size ,size
                 :expand-function ,expand-func
                 :grow ,grow)))
     ,@body))

(deftest cons-pool.1
    (with-cons-pool-test (pool 16)
      (let* ((lst (loop repeat 9 append (cons-pool-pop-cons pool)))
             (remain (cons-pool-size pool)))
        (loop repeat 9 do (cons-pool-push-cons pool (cons nil nil)))
        (values (and (listp lst) (= (length lst) 9))
                (= remain 7) (= (cons-pool-size pool) 16)
                (= (length (incudine.util::cons-pool-data pool)) 16))))
  T T T T)

(deftest cons-pool.2
    (with-cons-pool-test (pool 16 7)
      (loop repeat 18 do (cons-pool-pop-cons pool))
      (let ((remain1 (cons-pool-size pool)))
        (loop repeat 5 do (cons-pool-pop-cons pool))
        (let ((remain2 (cons-pool-size pool)))
          (cons-pool-pop-cons pool)
          (values remain1 remain2 (cons-pool-size pool)))))
  5 0 6)

(deftest cons-pool.3
    (with-cons-pool-test (pool 16)
      (let* ((lst (cons-pool-pop-list pool 7))
             (test1 (= (cons-pool-size pool)
                       (length (incudine.util::cons-pool-data pool))
                       9))
             (test2 (and (listp lst) (= (length lst) 7))))
        (cons-pool-push-list pool lst)
        (values test1 test2
                (= (cons-pool-size pool) 16)
                (= (length (incudine.util::cons-pool-data pool)) 16))))
  T T T T)

(deftest cons-pool.4
    (with-cons-pool-test (pool 16 8)
      (flet ((pool-size-test (n)
               (= (cons-pool-size pool)
                  (length (incudine.util::cons-pool-data pool))
                  n)))
      (cons-pool-pop-list pool 18)
      (let ((test1 (pool-size-test 16)))
        (loop repeat 3 do (cons-pool-pop-list pool 6))
        (values test1 (pool-size-test 4)))))
  T T)

(deftest cons-pool.5
    (with-cons-pool-test (pool 16 8)
      (cons-pool-push-list pool (cons-pool-pop-list pool 20))
      (values (cons-pool-size pool)
              (length (incudine.util::cons-pool-data pool))))
  36 36)

(deftest cons-pool.6
    (with-cons-pool-test (pool 8 8 #'cons-pool-expand-func-2)
      (cons-pool-pop-list pool 8)
      (when (zerop (cons-pool-size pool))
        (cons-pool-pop-list pool 8)))
  ((8) (7) (6) (5) (4) (3) (2) (1)))

;;;; TLIST

(defun tlist-test-result (tl)
  (values (tlist-left tl) (tlist-right tl) (tlist-empty-p tl)))

(defmacro with-tlist-test ((tl-var pool-var) &body body)
  `(with-cons-pool-test (,pool-var 16)
     (let ((,tl-var (make-tlist ,pool-var)))
       ,@body
       (tlist-test-result ,tl-var))))

(deftest tlist.1
    (with-tlist-test (tl pool))
  NIL NIL T)

(deftest tlist.2
    (with-tlist-test (tl pool)
      (tlist-add-left tl 123 pool))
  123 123 NIL)

(deftest tlist.3
    (with-tlist-test (tl pool)
      (tlist-add-right tl 123 pool))
  123 123 NIL)

(deftest tlist.4
    (with-tlist-test (tl pool)
      (tlist-add-left tl 123 pool)
      (tlist-add-right tl 321 pool))
  123 321 NIL)

(deftest tlist.5
    (with-tlist-test (tl pool)
      (loop for i from 1 to 10 do (tlist-add-left tl i pool)))
  10 1 NIL)

(deftest tlist.6
    (with-tlist-test (tl pool)
      (loop for i from 1 to 10 do (tlist-add-right tl i pool)))
  1 10 NIL)

(deftest tlist.7
    (let ((acc1) (acc2) (test1) (test2))
      (with-tlist-test (tl pool)
        (loop for i below 5 do (tlist-add-right tl i pool))
        (loop repeat 5 do (push (tlist-remove-left tl pool) acc1))
        (setf test1 (tlist-empty-p tl))
        (loop for i below 5 do (tlist-add-left tl i pool))
        (loop repeat 5 do (push (tlist-remove-left tl pool) acc2))
        (setf test2 (tlist-empty-p tl)))
      (values acc1 acc2 test1 test2))
  (4 3 2 1 0)
  (0 1 2 3 4)
  T T)

;;; FOREIGN RT MEMORY POOL

(deftest foreign-rt-alloc.1
    (let* ((size (get-rt-memory-free-size))
           (ptr (foreign-rt-alloc :int))
           (test1 (cffi:null-pointer-p ptr))
           (test2 (= size (get-rt-memory-free-size))))
      (foreign-rt-free ptr)
      (values test1 test2 (= size (get-rt-memory-free-size))))
  NIL NIL T)

(deftest foreign-rt-alloc.2
    (let* ((size (get-rt-memory-free-size))
           (ptr (foreign-rt-alloc :int :count 8
                                  :initial-contents (loop for i below 8
                                                          collect i)))
           (lst (unless (cffi:null-pointer-p ptr)
                  (loop for i below 8 collect (cffi:mem-aref ptr :int i)))))
      (foreign-rt-free ptr)
      (values lst (= size (get-rt-memory-free-size))))
  (0 1 2 3 4 5 6 7) T)

(deftest foreign-rt-alloc.3
    (let* ((size (get-rt-memory-free-size))
           (ptr (foreign-rt-alloc :int :count 8)))
      (unless (cffi:null-pointer-p ptr)
        (foreign-rt-realloc ptr :int :count 16
                            :initial-contents (loop for i below 16
                                                    collect i)))
      (let ((lst (unless (cffi:null-pointer-p ptr)
                   (loop for i below 16 collect (cffi:mem-aref ptr :int i)))))
        (foreign-rt-free ptr)
        (values lst (= size (get-rt-memory-free-size)))))
  (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) T)

(deftest foreign-rt-alloc.4
    (let* ((size (get-foreign-sample-free-size))
           (ptr (incudine.util::foreign-rt-alloc-sample 8 t))
           (lst (unless (cffi:null-pointer-p ptr)
                  (loop for i below 8 collect (cffi:mem-aref ptr 'sample i))))
           (test1 (= size (get-foreign-sample-free-size))))
      (incudine.util::foreign-rt-free-sample ptr)
      (values lst test1 (= size (get-foreign-sample-free-size))))
  #.(loop repeat 8 collect (coerce 0 'sample)) NIL T)

(deftest foreign-rt-alloc.5
    (let* ((size (get-foreign-sample-free-size))
           (ptr (incudine.util::foreign-rt-alloc-sample 2048)))
      (unless (cffi:null-pointer-p ptr)
        (incudine.util::foreign-rt-realloc-sample ptr 8192 t))
      (let ((test1 (unless (cffi:null-pointer-p ptr)
                     (dotimes (i 8192 t)
                       (unless (zerop (cffi:mem-aref ptr 'sample i))
                         (return nil))))))
        (incudine.util::foreign-rt-free-sample ptr)
        (values test1 (= size (get-foreign-sample-free-size)))))
  T T)

;;; FOREIGN NRT MEMORY POOL

(deftest foreign-nrt-alloc.1
    (let ((size (get-nrt-memory-used-size))
          (remain (get-nrt-memory-free-size)))
      (values
        (with-samples (a b c d)
          (let ((used (- (get-nrt-memory-used-size) size)))
            (and (plusp used)
                 (= used (- remain (get-nrt-memory-free-size))))))
        (zerop (- (get-nrt-memory-used-size) size))
        (zerop (- remain (get-nrt-memory-free-size)))))
  T T T)
