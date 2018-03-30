(in-package :incudine-tests)

(define-constant +sample-rate-test+ (sample 48000))

;;; The duration of the test is 5 seconds.
(define-constant +vug-test-duration-sec+ 5)

;;; Duration of the test in samples.
(define-constant +vug-test-duration+ (floor (* +vug-test-duration-sec+
                                               +sample-rate-test+)))

(define-constant +vug-test-max-channels+ 5)

(define-constant +vug-test-duration-bytes+ (* +vug-test-duration+
                                              +foreign-sample-size+))

(define-constant +vug-test-max-duration-bytes+ (* +vug-test-max-channels+
                                                  +vug-test-duration-bytes+))

(defvar *buffer-test-c1* (make-buffer +vug-test-duration+)
  "Buffer for the test with 1 output channel.")
(declaim (type buffer *buffer-test-c1*))

(defvar *buffer-test-c2* (make-buffer +vug-test-duration+ :channels 2)
  "Buffer for the test with 2 output channels.")
(declaim (type buffer *buffer-test-c2*))

(defvar *buffer-test-c5* (make-buffer +vug-test-duration+ :channels 5)
  "Buffer for the test with 5 output channels.")
(declaim (type buffer *buffer-test-c5*))

(defvar *delay-buffer-test* (make-buffer (ash 1 16))
  "Buffer to test the delay lines.")
(declaim (type buffer *delay-buffer-test*))

(defun make-fir-filter-buffer-test (size sinc-beta kaiser-beta)
  (let ((b (make-buffer size :fill-function (gen:sinc sinc-beta))))
    (with-buffer (w size :fill-function (gen:kaiser kaiser-beta))
      (map-into-buffer b #'* b w))))

(defvar *filter-buffer-test-1* (make-fir-filter-buffer-test 103 17 12)
  "Buffer for FIR filter test.")
(declaim (type buffer *filter-buffer-test-1*))

(defvar *filter-buffer-test-2* (make-fir-filter-buffer-test 102 11 22)
  "Buffer for FIR filter test.")
(declaim (type buffer *filter-buffer-test-2*))

(defvar *filter-pvbuffer-test-1*
  (with-buffers ((b 1024 :fill-function (gen:sinc 23))
                 (w 1024 :fill-function (gen:kaiser 12)))
    (map-into-buffer b #'* b w)
    (buffer->pvbuffer b 64))
  "Buffer for FIR filter test with partitioned convolution.")
(declaim (type pvbuffer *filter-pvbuffer-test-1*))

(defvar *byte-vector-test* (make-array +vug-test-max-duration-bytes+
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 0)
  "Sequence of bytes used to check MD5 checksums.")
(declaim (type (vector (unsigned-byte 8)) *byte-vector-test*))

(defun md5sum-buffer-test (buffer &optional byte-order)
  (declare (type buffer buffer))
  (let ((bytes (* (buffer-size buffer) +foreign-sample-size+))
        (data (buffer-data buffer)))
    (when (<= bytes +vug-test-max-duration-bytes+)
      (md5sum-sequence
        (if (eq byte-order #-little-endian :little-endian
                           #+little-endian :big-endian)
            (loop for i below bytes by +foreign-sample-size+ do
                    ;; Reverse the order of the bits.
                    (loop for j below (ash +foreign-sample-size+ -1)
                          for m = (+ i j)
                          for n from (+ i (1- +foreign-sample-size+)) downto 0 do
                            (setf (aref *byte-vector-test* m)
                                  (cffi:mem-aref data :unsigned-char n))
                            (setf (aref *byte-vector-test* n)
                                  (cffi:mem-aref data :unsigned-char m)))
                  finally (return *byte-vector-test*))
            (dotimes (i bytes *byte-vector-test*)
              (setf (aref *byte-vector-test* i)
                    (cffi:mem-aref data :unsigned-char i))))
        :start 0 :end bytes))))

(defun scale-and-round-buffer (buffer mult)
  (map-buffer (lambda (index value)
                (setf (buffer-value buffer index)
                      (fround (* value mult))))
              buffer))

(defun dsp-test-header (name)
  (fresh-line)
  (format t "~%DSP test: ~A~%" name)
  (force-output))

(defmacro with-dsp-test ((name &key bindings (channels 1) input-buffer
                          (mult 1d7) (byte-order :little-endian) md5)
                         &body body)
  ;; Tested only with double float samples.
  (when (eq *sample-type* 'double-float)
    (let ((output-buffer (format-symbol *package* "*BUFFER-TEST-C~D*"
                                        channels)))
      `(deftest ,name
           (with-local-logger (*standard-output*
                               (if (eq (logger-level) :debug) :debug :info)
                               (or (logger-time) :sec))
             (let ,bindings
               (dsp-test-header ',name)
               (md5sum-buffer-test
                 ;; Scale and round the values before the MD5 checksums because
                 ;; the precision of the sample could slightly vary on different
                 ;; machines (i.e. due to the conversions from/to the data
                 ;; registers of the x87 FPU on x86, from double extended
                 ;; precision to double precision and vice versa).
                 ;; By default MULT is 1d7 (140 dB).
                 (scale-and-round-buffer
                   (bounce-to-buffer (,output-buffer
                                      ,@(and input-buffer
                                             `(:input-buffer ,input-buffer))
                                      :sample-rate +sample-rate-test+)
                     ,@body)
                   ,mult)
                 ,byte-order)))
           ,md5))))

(defmacro with-ugen-test ((name) form &body body)
  `(deftest ,name
       (incudine.util::with-local-sample-rate (+sample-rate-test+) ,form)
     ,@body))

(define-vug noise-test (amp)
  (with-samples ((mult (* amp #.(/ (sample #x7fffff)))))
    (* (- (~ (logand (+ 17711 (* it 9227465)) #xffffff) :type fixnum) #x7fffff)
       mult)))

(define-vug vug-test-1 (freq amp phase)
  (sine freq amp phase))

;;; Test name, arguments and types.
(deftest vug.1
    (let ((vug (vug 'vug-test-1)))
      (if vug
          (values t (incudine.vug::vug-name vug) (incudine.vug::vug-args vug)
                  (incudine.vug::vug-arg-types vug))))
  t vug-test-1 (freq amp phase) (sample sample sample))

(define-vug vug-test-2 ((buf buffer) rate (offset sample)
                        (loop-p boolean) (done-action function))
  "Simple test: arguments with different types."
  (buffer-play buf rate offset loop-p done-action))

;;; Test arguments and types.
(deftest vug.2
    (let ((vug (vug 'vug-test-2)))
      (if vug
          (values t (incudine.vug::vug-args vug)
                  (incudine.vug::vug-arg-types vug))))
  t (buf rate offset loop-p done-action)
  (buffer sample sample boolean function))

;;; Test RENAME-VUG and DESTROY-VUG.
(deftest vug.3
    (progn
      (define-vug vug-test-3 () (out 0))
      (let ((n1 (incudine.vug::vug-name (vug 'vug-test-3))))
        (rename-vug 'vug-test-3 'vug-test-4)
        (let ((n2 (incudine.vug::vug-name (vug 'vug-test-4))))
          (destroy-vug 'vug-test-4)
          (values n1 n2 (null (vug 'vug-test-3)) (null (vug 'vug-test-4))))))
  vug-test-3 vug-test-4 t t)
