;;; Copyright (c) 2013-2019 Tito Latini
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :incudine)

(defstruct (buffer-base (:include incudine-object))
  (data-ptr (null-pointer) :type foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (real-time-p nil :type boolean)
  (foreign-free #'foreign-free :type function))

(defstruct (buffer (:include buffer-base)
                   (:constructor %make-buffer)
                   (:copier nil))
  "Buffer type."
  (mask 0 :type non-negative-fixnum)
  ;; LOBITS, LOMASK and LODIV used with the oscillators
  ;; that require power-of-two tables
  (lobits 0 :type (integer 0 #.+max-lobits+))
  (lomask 0 :type non-negative-fixnum)
  (lodiv (sample 1) :type sample)
  (frames 0 :type non-negative-fixnum)
  (channels 1 :type non-negative-fixnum)
  (sample-rate *sample-rate* :type sample)
  (file nil :type (or pathname null))
  (textfile-p nil :type boolean))

(declaim (inline buffer-data))
(defun buffer-data (obj)
  "Return the foreign pointer to the buffer data."
  (buffer-data-ptr obj))

(setf
  (documentation 'buffer-p 'function)
  "Return T if object is of type BUFFER."
  (documentation 'buffer-size 'function)
  "Return the buffer size."
  (documentation 'buffer-frames 'function)
  "Return the number of the sample frames of the buffer."
  (documentation 'buffer-channels 'function)
  "Return the number of the channels of the buffer."
  (documentation 'buffer-sample-rate 'function)
  "Return the buffer sample rate."
  (documentation 'buffer-file 'function)
  "If the buffer is created with BUFFER-LOAD or MAKE-BUFFER with
a non-NIL FILE argument, return the pathname.")

(define-constant +buffer-pool-initial-size+ 4000)

(defvar *buffer-pool*
  (make-incudine-object-pool +buffer-pool-initial-size+ #'%make-buffer nil))
(declaim (type incudine-object-pool *buffer-pool*))

(defvar *rt-buffer-pool*
  (make-incudine-object-pool +buffer-pool-initial-size+ #'%make-buffer t))
(declaim (type incudine-object-pool *rt-buffer-pool*))

(defun calc-buffer-mask (size)
  (declare (type non-negative-fixnum size))
  (if (power-of-two-p size)
      (1- size)
      (let ((half (ash size -1)))
        (if (power-of-two-p half)
            (- size 2)
            (- (next-power-of-two half) 1)))))

(defun calc-lobits (size)
  (declare (type non-negative-fixnum size))
  (if (>= size +table-maxlen+)
      0
      (- #.(integer-length +table-maxlen+) (integer-length size))))

(defun update-buffer (obj frm chans bufsize data &optional sr rt-p free-fn)
  (let* ((%lobits (calc-lobits bufsize))
         (value (ash 1 %lobits)))
    (declare (type non-negative-fixnum %lobits value))
    (incudine.util::with-struct-slots
        ((data-ptr size mask lobits lomask lodiv frames channels
          sample-rate real-time-p foreign-free)
         obj buffer)
      (setf frames frm
            channels chans
            size bufsize
            data-ptr data
            mask (calc-buffer-mask bufsize)
            lobits %lobits
            lomask (1- value)
            lodiv (if (zerop %lobits) +sample-zero+ (/ (sample 1) value)))
      (when sr
        (setf sample-rate (reduce-warnings (sample sr))
              real-time-p rt-p
              foreign-free free-fn))
      obj)))

(defun %%make-buffer (frm chans srate rt-p)
  (declare (type non-negative-fixnum frm chans)
           (type alexandria:positive-real srate)
           (type boolean rt-p))
  (let ((bufsize (the non-negative-fixnum (* frm chans)))
        (rt-p (and rt-p *allow-rt-memory-pool-p*)))
    (declare (type non-negative-fixnum bufsize))
    (multiple-value-bind (%data obj free-fn pool)
        (reduce-warnings
          (if rt-p
              (values (foreign-rt-alloc 'sample :count bufsize :zero-p t)
                      (incudine.util::alloc-rt-object *rt-buffer-pool*)
                      #'safe-foreign-rt-free
                      *rt-buffer-pool*)
              (values (foreign-alloc-sample bufsize)
                      (incudine.util::alloc-object *buffer-pool*)
                      #'foreign-free
                      *buffer-pool*)))
      (declare (type foreign-pointer %data) (type buffer obj)
               (type incudine-object-pool pool))
      (handler-case
          (update-buffer obj frm chans bufsize %data srate rt-p free-fn)
        (condition (c)
          (funcall free-fn %data)
          (incudine-object-pool-expand pool 1)
          (error c)))
      (incudine-finalize obj
        (lambda ()
          (funcall free-fn %data)
          (incudine-object-pool-expand pool 1))))))

(defmethod print-object ((obj buffer) stream)
  (format stream "#<~S :FRAMES ~D :CHANNELS ~D :SR ~F>"
          (type-of obj)
          (if (free-p obj) 0 (buffer-frames obj))
          (buffer-channels obj)
          (buffer-sample-rate obj)))

(defmethod free-p ((obj buffer-base))
  (null-pointer-p (buffer-base-data-ptr obj)))

(declaim (inline buffer-value))
(defun buffer-value (buffer index)
  "Return the buffer value stored at the sample frame INDEX. Setfable."
  (declare (type buffer-base buffer) (type non-negative-fixnum index))
  (smp-ref (buffer-base-data-ptr buffer) index))

(declaim (inline set-buffer-value))
(defun set-buffer-value (buffer index value)
  (declare (type buffer-base buffer) (type non-negative-fixnum index)
           (type real value))
  (setf (smp-ref (buffer-base-data-ptr buffer) index) (sample value)))

(defsetf buffer-value set-buffer-value)

;;; Check if FILE is a text file that contains numbers.
;;; Returns T and the number of the counted values, or NIL if the file is
;;; not valid.
;;;
;;; There is not the parsing of the numbers; a text file is valid if it
;;; contains numbers separated with spaces, tabs or newlines.
;;;
;;; It is possible to use line comments that begin with the ';' character.
(defun check-numeric-textfile (file)
  (flet ((valid-char-p (code)
           (or ;; [0-9]
               (< 47 code 58)
               ;;            \t \n \f \r sp  +  -  .  D  E  d   e
               (member code '(9 10 12 13 32 43 45 46 68 69 100 101))))
         (separator-p (code)
           (member code '(9 10 12 13 32)))
         (newline-p (code)
           (member code '(10 12 13))))
    (with-open-file (f file :element-type '(unsigned-byte 8))
      (loop for code = (read-byte f nil nil)
            with sep = t and count = 0
            while code do
           (when (= code 59)
             ;; Skip line comment
             (unless sep (setf sep t))
             (loop do (setf code (read-byte f nil nil))
                   until (newline-p code)))
           (if (valid-char-p code)
               (if sep
                   (unless (separator-p code)
                     (setf sep nil)
                     (incf count))
                   (if (separator-p code) (setf sep t)))
               (return (values nil count)))
            finally (return (values t count))))))

(defun buffer-import-textfile (buffer path offset buffer-start channels size)
  (with-open-file (f path)
    (when (plusp offset)
      ;; Skip the first OFFSET frames
      (loop for frame from 0
            while (< frame offset) do
           (loop repeat channels do (read f nil nil))))
    (let ((*read-default-float-format* incudine.config:*sample-type*))
      (loop with count = buffer-start
            while (< count size) do
           (loop repeat channels do
                (flet ((check-value (x)
                         (typecase x
                           (number x)
                           (null +sample-zero+)
                           (t (nrt-msg warn "bad value (~A) in ~A" x path)
                              +sample-zero+))))
                  (setf (buffer-value buffer count)
                        (check-value (read f nil nil))))
                (incf count))))))

(defun buffer-load-textfile (path offset frames channels sample-rate)
  (multiple-value-bind (valid-p size) (check-numeric-textfile path)
    (when valid-p
      (let* ((channels (max channels 1))
             (frames (or frames (- (ceiling (/ size channels)) offset))))
        (when (plusp frames)
          (let ((buf (make-buffer frames :channels channels
                                  :sample-rate (sample sample-rate))))
            (buffer-import-textfile buf path offset 0 channels
                                    (* frames channels))
            (setf (buffer-file buf)
                  (if (pathnamep path) path (pathname path)))
            (setf (buffer-textfile-p buf) t)
            buf))))))

(declaim (inline headerless-sf-info))
(defun headerless-sf-info (sample-rate channels data-format)
  (sf:make-info :sample-rate (floor sample-rate)
                :channels channels
                :format (sf:get-format (list "raw" (or data-format "double")))))

(defmacro with-open-sndfile ((var path offset frames channels sample-rate
                              headerless-p data-format) &body body)
  `(if (probe-file ,path)
       (let ((,channels (or ,channels 1))
             (,sample-rate (or ,sample-rate *sample-rate*)))
         (sf:with-open (,var ,path
                        :info (when ,headerless-p
                                (headerless-sf-info ,sample-rate ,channels
                                                    ,data-format)))
           (if (sf:sndfile-null-p ,var)
               ;; Try to load a text file
               (or (buffer-load-textfile ,path ,offset ,frames ,channels
                                         ,sample-rate)
                   (incudine-error (sf:strerror ,var)))
               ,@body)))
       (incudine-error "file ~S not found" (namestring ,path))))

(defun buffer-load (path &key (offset 0) frames (channel -1) channels
                    sample-rate headerless-p data-format)
  "Create a new buffer by loading the file PATH (string or pathname).

If PATH represents a sound file, load that file starting from OFFSET
sample frame (defaults to 0).

If PATH corresponds to a text file that contains numbers, create a buffer
with that values. There is not the parsing of the numbers, a text file is
valid if it contains numbers separated with spaces, tabs or newlines.
It is possible to use line comments that begin with the ';' character.

If FRAMES is non-NIL, create a buffer with FRAMES sample frames.

If CHANNEL is  greater than or equal to zero, load that channel (zero based)
of the sound file, otherwise import all the channels (default).

The number of channels is CHANNELS or the number of channels of the
sound file if CHANNELS is NIL (default).

If HEADERLESS-P is T, load a headerless file with sample type DATA-FORMAT
(defaults to \"double\")

SAMPLE-RATE is *SAMPLE-RATE* by default."
  (declare (type (or string pathname) path) (type fixnum channel)
           (type non-negative-real offset))
  (let ((offset (floor offset))
        (frames (and frames (floor frames)))
        (path (truename path)))
    (declare (type non-negative-fixnum offset)
             (type (or non-negative-fixnum null) frames))
    (with-open-sndfile (sf path offset frames channels sample-rate
                        headerless-p data-format)
      (let* ((info (sf:info sf))
             (channels (sf:channels info))
             (%frames (- (sf:frames info) offset))
             (frames (if frames (min (floor frames) %frames) %frames)))
        (declare (type non-negative-fixnum channels)
                 (type fixnum %frames frames))
        (when (plusp frames)
          (let ((buffer (make-buffer frames
                          :channels (if (minusp channel) channels 1))))
            (declare (type buffer buffer) #.*standard-optimize-settings*)
            (sf:seek sf offset 0)
            (if (minusp channel)
                ;; All channels
                (sndfile-to-buffer (buffer-data buffer) sf
                                   frames channels 0 *sndfile-buffer-size*)
                ;; The buffer is mono
                (let ((channel-map `((,(min channel (1- channels)) 0))))
                  (map-sndfile-ch-to-buffer (buffer-data buffer) sf frames
                                            channels (buffer-channels buffer) 0
                                            *sndfile-buffer-size* channel-map
                                            1)))
            (setf (buffer-file buffer) path)
            (reduce-warnings
              (setf (buffer-sample-rate buffer) (sample (sf:sample-rate info))))
            buffer))))))

(defmacro writef-sample (sndfile ptr items)
  `(#+double-samples sf:writef-double
    #-double-samples sf:writef-float
    ,sndfile ,ptr ,items))

(declaim (inline save-data-to-textfile))
(defun save-data-to-textfile (data path size)
  (with-open-file (f path :direction :output :if-exists :supersede)
    (dotimes (i size)
      (write-line (format nil "~F" (smp-ref data i)) f))))

(defun buffer-save (buf path &key (start 0) (end 0) sample-rate
                    textfile-p (header-type *default-header-type*)
                    (data-format *default-data-format*))
  "Save the buffer data of BUF to the file PATH.

START specifies an offset into the buffer and marks the beginning
position of that buffer. END marks the position following the last
value of the buffer.

If SAMPLE-RATE is non-NIL, it replaces the sample rate of the buffer.

If TEXTFILE-P is T, save the buffer data to a text file.

HEADER-TYPE defaults to *DEFAULT-HEADER-TYPE*.

DATA-FORMAT defaults to *DEFAULT-DATA-FORMAT*."
  (declare (type buffer buf) (type (or string pathname) path)
           (type non-negative-real start end)
           (type (or positive-real null) sample-rate))
  (let* ((offset (floor (if (< 0 start (buffer-frames buf)) start 0)))
         (max-frames (- (buffer-frames buf) offset))
         (frames (floor (if (and (plusp end) (> end offset))
                            (min (- end offset) max-frames)
                            max-frames)))
         (sr (floor (or sample-rate (buffer-sample-rate buf))))
         (data (if (plusp offset)
                   (inc-pointer (buffer-data buf)
                                (* offset (buffer-channels buf)
                                   +foreign-sample-size+))
                   (buffer-data buf))))
    (declare (type non-negative-fixnum offset max-frames frames sr))
    (if textfile-p
        (save-data-to-textfile data path (* (buffer-channels buf) frames))
        (sf:with-open (sf path
                       :info (sf:make-info
                               :frames frames
                               :sample-rate sr
                               :channels (buffer-channels buf)
                               :format (sf:get-format
                                         (list header-type data-format)))
                       :mode sf:sfm-write)
          (writef-sample sf data frames)))
    buf))

(defmethod free ((obj buffer))
  (unless (free-p obj)
    (funcall (buffer-base-foreign-free obj) (buffer-base-data-ptr obj))
    (incudine-cancel-finalization obj)
    (setf (buffer-base-data-ptr obj) (null-pointer))
    (setf (buffer-base-size obj) 0)
    (setf (buffer-file obj) nil)
    (if (buffer-real-time-p obj)
        (incudine.util::free-rt-object obj *rt-buffer-pool*)
        (incudine.util::free-object obj *buffer-pool*))
    (nrt-msg debug "Free ~A" (type-of obj))
    (values)))

(defun copy-buffer (buffer)
  "Return a copy of BUFFER."
  (declare (type buffer buffer))
  (if (free-p buffer)
      (incudine-error "The buffer is unusable.")
      (let ((new (make-buffer (buffer-frames buffer)
                              :channels (buffer-channels buffer)
                              :sample-rate (buffer-sample-rate buffer)
                              :real-time-p (allow-rt-memory-p))))
        (foreign-copy-samples (buffer-data new) (buffer-data buffer)
                              (buffer-size buffer))
        (copy-struct-slots buffer (file textfile-p) buffer new)
        new)))

(defun resize-buffer (buffer frames &optional channels)
  "Resize BUFFER to FRAMES sample frames.

If CHANNELS is non-NIL, the resized buffer is created with that number
of channels."
  (declare (type buffer buffer) (type non-negative-fixnum frames)
           (type (or non-negative-fixnum null) channels))
  (if (or (free-p buffer)
          (and (= (buffer-frames buffer) frames)
               (or (null channels)
                   (= (buffer-channels buffer) channels))))
      buffer
      (let* ((old-channels (buffer-channels buffer))
             (old-size (buffer-size buffer))
             (channels (or channels old-channels))
             (size (* frames channels))
             (free-fn (buffer-foreign-free buffer))
             (pool (if (buffer-real-time-p buffer)
                       *rt-buffer-pool*
                       *buffer-pool*)))
        (declare (type non-negative-fixnum old-channels old-size channels size)
                 (type function free-fn) (type incudine-object-pool pool)
                 #.*standard-optimize-settings*)
        (let ((data (reduce-warnings
                      (if (buffer-real-time-p buffer)
                          (rt-eval (:return-value-p t)
                            (foreign-rt-alloc 'sample :count size :zero-p t))
                          (foreign-alloc-sample size)))))
          (declare (type foreign-pointer data))
          (loop for i of-type non-negative-fixnum below size by channels
                for j of-type non-negative-fixnum below old-size by old-channels
                do (dochannels (ch channels)
                     (setf (smp-ref data (+ i ch))
                           (if (< ch old-channels)
                               (smp-ref (buffer-data buffer) (+ j ch))
                               +sample-zero+))))
          (funcall free-fn (buffer-data buffer))
          (incudine-cancel-finalization buffer)
          (incudine-finalize buffer
            (lambda ()
              (funcall free-fn data)
              (incudine-object-pool-expand pool 1)))
          (update-buffer buffer frames channels size data)))))

(defun map-buffer (function buffer)
  "Destructively modifies BUFFER to contain the results of applying
FUNCTION to each buffer value. The function arguments are the index
and the buffer value."
  (declare (type function function) (type buffer-base buffer))
  (dotimes (i (buffer-base-size buffer) buffer)
    (declare #.*standard-optimize-settings* #.*reduce-warnings*)
    (setf #1=(buffer-value buffer i) (funcall function i #1#))))

;;; Like MAP-INTO but for the BUFFERs
(defun map-into-buffer (result-buffer function &rest buffers)
  "Destructively modifies RESULT-BUFFER to contain the results of
applying FUNCTION to each element in the argument BUFFERS in turn

FUNCTION is a function of as many arguments as there are buffers."
  (declare (type function function) (type buffer-base result-buffer))
  (let ((size (reduce #'min
                      (mapcar #'buffer-base-size (cons result-buffer buffers)))))
    (declare (type non-negative-fixnum size)
             #.*standard-optimize-settings* #.*reduce-warnings*)
    (flet ((compute-value (i)
             (cond ((null buffers) (funcall function))
                   ((cdr buffers)
                    (reduce function
                            (mapcar (lambda (x) (buffer-value x i)) buffers)))
                   (t (funcall function (buffer-value (car buffers) i))))))
      (dotimes (i size result-buffer)
        (setf (buffer-value result-buffer i) (compute-value i))))))

(declaim (inline scale-buffer))
(defun scale-buffer (buffer mult)
  "Multiply the buffer values by MULT."
  (declare (type buffer-base buffer) (type real mult))
  (map-buffer (lambda (index value)
                (declare (ignore index))
                (* value mult))
              buffer))

(defun normalize-buffer (buffer value)
  "Scale the buffer values to be between -VALUE and VALUE."
  (declare (type buffer-base buffer) (type real value))
  (let ((data (buffer-base-data-ptr buffer))
        (size (buffer-base-size buffer)))
    (declare (type positive-fixnum size))
    (labels ((norm (index max)
               (declare (type non-negative-fixnum index)
                        (type sample max))
               (if (= index size)
                   max
                   (norm (1+ index)
                         (max (abs (smp-ref data index)) max)))))
      (let ((max (norm 1 (abs (smp-ref data 0)))))
        (if (zerop max)
            buffer
            (scale-buffer buffer (/ value max)))))))

(defun rescale-buffer (buffer min max)
  "Rescale the buffer values to be between MIN and MAX."
  (declare (type buffer-base buffer) (type real min max))
  (let ((data (buffer-base-data-ptr buffer))
        (size (buffer-base-size buffer)))
    (declare (type positive-fixnum size))
    (labels ((resc (index old-min old-max)
               (declare (type non-negative-fixnum index)
                        (type sample old-min old-max))
               (if (= index size)
                   (values (/ (sample 1) (- old-max old-min)) old-min)
                   (let ((value (smp-ref data index)))
                     (resc (1+ index)
                           (min value old-min)
                           (max value old-max))))))
      (multiple-value-bind (old-delta old-min)
          (resc 1 (smp-ref data 0) (smp-ref data 0))
        (map-buffer (lambda (index value)
                      (declare (ignore index))
                      (+ min (* (- max min) old-delta (- value old-min))))
                    buffer)))))

(defun sort-buffer (buffer)
  "Destructively sort BUFFER and return it."
  (incudine.util:sort-samples
    (buffer-base-data-ptr buffer) (buffer-base-size buffer))
  buffer)

(defmethod circular-shift ((obj buffer-base) n &key)
  (foreign-circular-shift (buffer-base-data-ptr obj) 'sample
                          (buffer-base-size obj) n)
  obj)

(defmethod quantize ((obj real) (from buffer-base) &key)
  (quantize-from-vector obj from smp-ref buffer-base-size
                        (buffer-base-data-ptr from)))

(defmethod quantize ((obj buffer-base) (from real)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function smp-ref
                   buffer-base-size (buffer-base-data-ptr obj) sample))

(defmethod quantize ((obj buffer-base) (from buffer-base)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function smp-ref
                   buffer-base-size (buffer-base-data-ptr obj) sample))

(defmethod quantize ((obj buffer-base) (from simple-vector)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function smp-ref
                   buffer-base-size (buffer-base-data-ptr obj) sample))

(defmethod quantize ((obj buffer-base) (from simple-array)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function smp-ref
                   buffer-base-size (buffer-base-data-ptr obj) sample))

(defmethod quantize ((obj simple-vector) (from buffer-base)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function svref length obj))

(defmethod quantize ((obj simple-array) (from buffer-base)
                     &key (start 0) end filter-function)
  (quantize-vector obj from start end filter-function aref length obj))

(defun buffer->array (buf)
  "Create a new array of type (simple-array sample (*)) with the
content of the buffer."
  (declare (type buffer-base buf))
  (let* ((size (buffer-base-size buf))
         (arr (make-array size :element-type 'sample)))
    (dotimes (i size arr)
      (setf (aref arr i) (buffer-value buf i)))))

(defun buffer->list (buf)
  "Create a new list with the content of the buffer."
  (declare (type buffer-base buf))
  (loop for i below (buffer-base-size buf) collect (buffer-value buf i)))

(defun set-buffer-from-textfile (buffer path start buffer-start buffer-end)
  (declare (type (or string pathname)) (type buffer buffer)
           (type non-negative-real start buffer-start buffer-end))
  (multiple-value-bind (valid-p size) (check-numeric-textfile path)
    (when valid-p
      (let* ((channels (buffer-channels buffer))
             (start (floor start))
             (buffer-start (floor buffer-start))
             (buffer-end (floor buffer-end))
             (frames (min (- buffer-end buffer-start)
                          (- (ceiling (/ size channels)) start))))
        (declare (type channel-number channels)
                 (type non-negative-fixnum start buffer-start buffer-end
                       frames))
        (when (plusp frames)
          (buffer-import-textfile buffer path start buffer-start
                                  channels (+ buffer-start
                                              (* frames channels))))))))

(defun check-channel-map (cmap cmap-size sf-channels buf-channels)
  (declare (type list cmap)
           (type positive-fixnum cmap-size sf-channels buf-channels))
  (cond ((> cmap-size sf-channels)
         (incudine-error "channel-map size greater than sndfile channels"))
        ((> cmap-size buf-channels)
         (incudine-error "channel-map size greater than buffer channels"))
        ((some (lambda (x)
                 (cond
                   ((>= (the non-negative-fixnum (first x)) sf-channels)
                    (incudine-error
                      "wrong channel-map; max value for the source is ~D"
                      (1- sf-channels)) t)
                   ((>= (the non-negative-fixnum (second x)) buf-channels)
                    (incudine-error
                      "wrong channel-map; max value for the destination is ~D"
                      (1- buf-channels)) t)))
               cmap))
        (t t)))

(defun map-sndfile-ch-to-buffer (data sndfile frames channels buf-channels
                                 data-offset chunk-size channel-map
                                 channel-map-size)
  (with-foreign-array (dest :int (* 2 channel-map-size))
    (let ((src (cffi:inc-pointer dest
                                 (the non-negative-fixnum
                                   (* channel-map-size
                                      (the non-negative-fixnum
                                        (cffi:foreign-type-size :int)))))))
      (labels ((rec (index chmap)
                 (declare (type non-negative-fixnum index) (type list chmap))
                 (cond ((null chmap)
                        (incudine.external::%map-sndfile-ch-to-buffer
                          data sndfile frames channels buf-channels
                          data-offset chunk-size dest src channel-map-size))
                       (t (setf (mem-aref src :int index) (caar chmap)
                                (mem-aref dest :int index) (cadar chmap))
                          (rec (1+ index) (cdr chmap))))))
        (rec 0 channel-map)))))

(defun set-buffer-from-sndfile (buffer path start buffer-start buffer-end
                                &optional channel-map headerless-p data-format)
  (declare (type buffer buffer) (type (or string pathname) path)
           (type non-negative-fixnum start buffer-start buffer-end)
           (type list channel-map))
  (if (probe-file path)
      (sf:with-open (sf (truename path)
                     :info (when headerless-p
                             (headerless-sf-info
                               (buffer-sample-rate buffer)
                               (if channel-map
                                   (1+ (reduce #'max channel-map :key #'first))
                                   (buffer-channels buffer))
                               data-format)))
        (if (sf:sndfile-null-p sf)
            ;; Perhaps it is a numeric text file
            (set-buffer-from-textfile buffer path start buffer-start buffer-end)
            (let* ((info (sf:info sf))
                   (channels (sf:channels info))
                   (selected-frames (min (- buffer-end buffer-start)
                                         (- (sf:frames info) start))))
              (declare (type non-negative-fixnum channels)
                       (type fixnum selected-frames))
              (when (plusp selected-frames)
                (incudine-optimize
                  (sf:seek sf start 0)
                  (cond (channel-map
                         (let ((channel-map-size (length channel-map)))
                           (when (check-channel-map channel-map channel-map-size
                                                    channels
                                                    (buffer-channels buffer))
                             (map-sndfile-ch-to-buffer
                               (buffer-data buffer) sf selected-frames
                               channels (buffer-channels buffer) buffer-start
                               *sndfile-buffer-size* channel-map
                               channel-map-size))))
                        ((= (buffer-channels buffer) channels)
                         (sndfile-to-buffer (buffer-data buffer) sf
                                            selected-frames channels
                                            buffer-start *sndfile-buffer-size*))
                        (t (let ((channel-map
                                  (loop for src below channels
                                        for dest below (buffer-channels buffer)
                                        collect `(,src ,dest))))
                             (nrt-msg debug "use channel-map ~A" channel-map)
                             (map-sndfile-ch-to-buffer
                               (buffer-data buffer) sf selected-frames
                               channels (buffer-channels buffer) buffer-start
                               *sndfile-buffer-size* channel-map
                               (min channels (buffer-channels buffer))))))))))
        buffer)
      (incudine-error "file ~S not found" (namestring path))))

(defun fill-buffer (buffer obj &key (start 0) end (sndfile-start 0)
                    channel-map (normalize-p nil normalize-pp)
                    headerless-p data-format)
  "If OBJ is a function, fill the buffer to contain the results of
applying OBJ. The function arguments are the foreign pointer to the
buffer data and the buffer size (i.e. GEN routines are valid functions).

If OBJ is a list, a vector or an ENVELOPE struct, it is used to fill
the buffer.

If OBJ is of type string or pathname, load that file. If the file is
a sound file, load OBJ starting from SNDFILE-START sample frame
(defaults to 0). If HEADERLESS-P is T, load a headerless file with
sample type DATA-FORMAT (defaults to \"double\").  If OBJ corresponds
to a text file that contains numbers, fill the buffer with that values.
There is not the parsing of the numbers, a text file is valid if it
contains numbers separated with spaces, tabs or newlines. It is
possible to use line comments that begin with the ';' character.

START specifies an offset into the buffer and marks the beginning
position of that buffer. END marks the position following the last
value of the buffer.

CHANNEL-MAP is a list of lists (soundfile-channel buffer-channel)
used to define the mapping between the sound file channel data and
the buffer channel data. For example, CHANNEL-MAP '((1 0) (0 1))
fills the first two buffer channel data with the swapped channels of
a stereo sound file.

If NORMALIZE-P is T, normalize the buffer data between -1 and 1."
  (declare (type buffer buffer) (type boolean normalize-p)
           (type non-negative-fixnum start sndfile-start))
  (macrolet ((loop-sequence (clause seq)
                (with-gensyms (i j max scale)
                  `(let ((end (if end (min end size) size)))
                     (loop for ,i from start below end
                           for ,j ,clause ,seq
                           for ,max = 0 then (max (abs ,j) ,max) do
                          (setf (buffer-value buffer ,i) ,j)
                           finally (when (and normalize-p (/= ,max 1))
                                     (loop for ,i from start below end
                                           for ,j ,clause ,seq
                                           with ,scale = (/ 1.0 ,max) do
                                          (setf (buffer-value buffer ,i)
                                                (* (buffer-value buffer ,i)
                                                   ,scale)))))))))
    (let ((size (buffer-size buffer)))
      (unless (free-p buffer)
        (cond ((functionp obj)
               (let ((chunk-size (- (if end (min end size) size) start)))
                 (when (plusp chunk-size)
                   (multiple-value-bind (c-array mult norm-p)
                       (funcall obj (inc-pointer (buffer-data buffer)
                                                 (* start
                                                    +foreign-sample-size+))
                                chunk-size)
                     (declare (ignore c-array))
                     (let ((norm-p (if normalize-pp normalize-p norm-p)))
                       (when (and norm-p (numberp mult) (/= mult 1))
                         (scale-buffer buffer mult)))))))
              ((consp obj) (loop-sequence in obj))
              ((or (stringp obj) (pathnamep obj))
               (set-buffer-from-sndfile buffer obj sndfile-start start
                                        (if end
                                            (min end (buffer-frames buffer))
                                            (buffer-frames buffer))
                                        channel-map headerless-p data-format))
              ((vectorp obj) (loop-sequence across obj))
              ((envelope-p obj)
               (fill-buffer buffer (gen:envelope obj)
                            :start start :end end
                            :normalize-p normalize-p))))
      buffer)))

(defun make-buffer (frames &key (channels 1) file (offset 0)
                    (sample-rate *sample-rate*) (real-time-p (allow-rt-memory-p))
                    initial-contents fill-function (start 0) end normalize-p)
  "Create a new buffer with FRAMES sample frames.

If FILE is non-NIL, copy the sample frames of a soundfile starting from
OFFSET sample frame.

INITIAL-CONTENTS is used to initialize the contents of the buffer from START
to END frame.

FILL-FUNCTION is a function used to fill the buffer from START to END frame.
The function arguments are the foreign pointer to the buffer data and the
buffer size (i.e. GEN routines are valid functions).

If NORMALIZE-P is T, normalize the initial content of the buffer.

Set REAL-TIME-P to NIL to disallow real-time memory pools."
  (declare (type non-negative-real frames start offset sample-rate)
           (type (or non-negative-real null) end)
           (type non-negative-fixnum channels)
           (type boolean real-time-p normalize-p)
           (type (or function null) fill-function))
  (flet ((new-from-file (frm ch f os sr)
           (set-buffer-from-sndfile (%%make-buffer frm ch sr real-time-p)
                                    f os 0 frm)))
    (if file
        (if (zerop frames)
            (if (zerop offset)
                (buffer-load file)
                (let ((info (sf:info file)))
                  (new-from-file (max 1 (- (sf:frames info) offset))
                                 (sf:channels info)
                                 file offset sample-rate)))
            (new-from-file frames channels file offset sample-rate))
        (let ((buf (%%make-buffer frames channels sample-rate real-time-p))
              (value (or initial-contents fill-function)))
          (when value
            (if normalize-p
                (fill-buffer buf value :start start :end end
                             :normalize-p normalize-p)
                (fill-buffer buf value :start start :end end)))
          buf))))

(defmacro with-buffer ((var frames &rest args) &body body)
  "Bind VAR to a newly allocated BUFFER structure with dynamic extent
during BODY.

FRAMES and the other keyword arguments ARGS are passed to MAKE-BUFFER."
  `(let ((,var (make-buffer ,frames ,@args)))
     (declare (type buffer ,var))
     (maybe-unwind-protect (progn ,@body) (free ,var))))

(defmacro with-buffers (bindings &body body)
  "Create bindings to newly allocated BUFFER structures with dynamic extent
during BODY.

BINDINGS is a list of lists

    (var frames &rest args)

where VAR is the variable bound to a buffer, FRAMES and the other keyword
arguments ARGS are passed to MAKE-BUFFER."
  (let ((vars (mapcar #'car bindings)))
    `(let ,(mapcar (lambda (x) `(,(car x) (make-buffer ,@(cdr x)))) bindings)
       ,(and vars `(declare (type buffer ,@vars)))
       (maybe-unwind-protect (progn ,@body)
         ,@(mapcar (lambda (x) `(free ,x)) vars)))))

;;; Frequently used waveforms

(defvar *sine-table* (make-buffer *default-table-size*
                                  :fill-function (gen:partials '(1)))
  "BUFFER structure of size *DEFAULT-TABLE-SIZE* with a single cycle sinusoid.")
(declaim (type buffer *sine-table*))

(defvar *cosine-table* (make-buffer *default-table-size*
                                    :fill-function (gen:partials '((1 1 .25))))
  "BUFFER structure of size *DEFAULT-TABLE-SIZE* with a single cycle cosinusoid.")
(declaim (type buffer *cosine-table*))
