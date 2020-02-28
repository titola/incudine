(in-package :incudine-tests)

(cffi:defcstruct foreign-ringbuffer
  (buf :pointer)
  (write-head :unsigned-int)
  (read-head :unsigned-int))

(defun foreign-ringbuffer-slot-value (rb slot-name)
  (cffi:foreign-slot-value rb '(:struct foreign-ringbuffer) slot-name))

(cffi:defcfun ("incudine_ringbuffer_create" foreign-ringbuffer-create) :pointer
  (size :unsigned-int))

(cffi:defcfun ("incudine_ringbuffer_free" foreign-ringbuffer-free) :void
  (rb :pointer))

(cffi:defcfun ("incudine_ringbuffer_reset" foreign-ringbuffer-reset) :void
  (rb :pointer))

(cffi:defcfun ("incudine_ringbuffer_empty" foreign-ringbuffer-empty-p) :boolean
  (rb :pointer))

(cffi:defcfun ("incudine_ringbuffer_write" foreign-ringbuffer-write) :unsigned-int
  (rb :pointer)
  (src :pointer)
  (bytes :unsigned-int))

(cffi:defcfun ("incudine_ringbuffer_write_space" foreign-ringbuffer-write-space)
    :unsigned-int
  (rb :pointer))

(cffi:defcfun ("incudine_ringbuffer_write_advance" foreign-ringbuffer-write-advance)
    :void
  (rb :pointer)
  (bytes :unsigned-int))

(cffi:defcfun ("incudine_ringbuffer_read" foreign-ringbuffer-read) :unsigned-int
  (rb :pointer)
  (dest :pointer)
  (bytes :unsigned-int))

(cffi:defcfun ("incudine_ringbuffer_read_space" foreign-ringbuffer-read-space)
    :unsigned-int
  (rb :pointer))

(cffi:defcfun ("incudine_ringbuffer_read_advance" foreign-ringbuffer-read-advance)
    :void
  (rb :pointer)
  (bytes :unsigned-int))

(cffi:defcfun ("incudine_ringbuffer_peek" foreign-ringbuffer-peek) :unsigned-int
  (rb :pointer)
  (dest :pointer)
  (bytes :unsigned-int))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *foreign-ringbuffer-input-test*
    #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)))

(define-constant +foreign-ringbuffer-size+
  (next-power-of-two (length *foreign-ringbuffer-input-test*)))

(defmacro with-foreign-ringbuffer ((var &optional size) &body body)
  (with-gensyms (len)
    `(let ((,var nil)
           (,len ,(or size +foreign-ringbuffer-size+)))
       (unwind-protect
            (progn
              (setf ,var (foreign-ringbuffer-create ,len))
              (unless (cffi:null-pointer-p ,var)
                ,@body))
         (and ,var (foreign-ringbuffer-free ,var))))))

(deftest foreign-ringbuffer.1
    (let ((rb-size +foreign-ringbuffer-size+))
      (with-foreign-ringbuffer (rb)
        (values
          (foreign-ringbuffer-slot-value rb 'write-head)
          (foreign-ringbuffer-slot-value rb 'read-head)
          (foreign-ringbuffer-empty-p rb)
          (= (foreign-ringbuffer-write-space rb) (- rb-size 1))
          (= (foreign-ringbuffer-read-space rb) 0)
          (progn
            (foreign-ringbuffer-write-advance rb 7)
            (foreign-ringbuffer-read-advance rb 5)
            (foreign-ringbuffer-slot-value rb 'write-head))
          (foreign-ringbuffer-slot-value rb 'read-head)
          (foreign-ringbuffer-empty-p rb)
          (= (foreign-ringbuffer-write-space rb) (- rb-size 3))
          (= (foreign-ringbuffer-read-space rb) 2)
          (let ((bytes (+ rb-size 3)))
            (foreign-ringbuffer-write-advance rb bytes)
            (foreign-ringbuffer-read-advance rb bytes)
            (foreign-ringbuffer-slot-value rb 'write-head))
          (foreign-ringbuffer-slot-value rb 'read-head)
          (progn
            (foreign-ringbuffer-reset rb)
            (foreign-ringbuffer-slot-value rb 'write-head))
          (foreign-ringbuffer-slot-value rb 'read-head)
          (foreign-ringbuffer-empty-p rb))))
  0 0 T T T 7 5 NIL T T 10 8 0 0 T)

(deftest foreign-ringbuffer.2
    (let* ((input *foreign-ringbuffer-input-test*)
           (len (length input))
           (acc nil))
      (with-foreign-ringbuffer (rb)
        (cffi:with-foreign-object (src :unsigned-char len)
          (cffi:with-foreign-object (dest :unsigned-char len)
            (flet ((append-dest ()
                     (dotimes (i len)
                       (push (cffi:mem-aref dest :unsigned-char i) acc))))
            (dotimes (i len)
              (setf (cffi:mem-aref src :unsigned-char i)
                    (svref input i))
              (setf (cffi:mem-aref dest :unsigned-char i) 0))
            (foreign-ringbuffer-write rb src len)
            (foreign-ringbuffer-peek rb dest len)
            (append-dest)
            (foreign-ringbuffer-read-advance rb 7)
            (foreign-ringbuffer-read rb dest len)
            (append-dest)
            (foreign-ringbuffer-write rb src len)
            (foreign-ringbuffer-read rb dest len)
            (append-dest)))))
      (nreverse acc))
  (1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
   8  9 10 11 12 13 14 15 16 17 18 19 20 21 15 16 17 18 19 20 21
   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21))
