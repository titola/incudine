;;; Copyright (c) 2013-2014 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (buffer (:constructor %make-buffer)
                     (:copier nil))
    (data (null-pointer) :type foreign-pointer)
    (size 0 :type non-negative-fixnum)
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
    (textfile-p nil :type boolean)
    (real-time-p nil :type boolean)
    (foreign-free #'foreign-free :type function)))

(declaim (inline calc-buffer-mask))
(defun calc-buffer-mask (size)
  (declare (type non-negative-fixnum size))
  (if (power-of-two-p size)
      (1- size)
      (let ((half (ash size -1)))
        (if (power-of-two-p half)
            (- size 2)
            (- (next-power-of-two half) 1)))))

(declaim (inline %%make-buffer))
(defun %%make-buffer (frames channels sample-rate real-time-p)
  (let* ((size (the non-negative-fixnum
                 (* frames channels)))
         (data (if real-time-p
                   (foreign-rt-alloc 'sample :count size :zero-p t)
                   (foreign-alloc-sample size)))
         (lobits (calc-lobits size))
         (value (ash 1 lobits))
         (free-function (if real-time-p
                            #'safe-foreign-rt-free
                            #'foreign-free))
         (obj (%make-buffer
               :data data
               :size size
               :mask (calc-buffer-mask size)
               :lobits lobits
               :lomask (1- value)
               :lodiv (if (zerop lobits)
                          +sample-zero+
                          (/ (sample 1) value))
               :frames frames
               :channels channels
               :sample-rate (sample sample-rate)
               :real-time-p real-time-p
               :foreign-free free-function)))
    (tg:finalize obj (lambda () (funcall free-function data)))
    obj))

(defmethod print-object ((obj buffer) stream)
  (with-slots (frames channels sample-rate) obj
    (format stream "#<~S :FRAMES ~D :CHANNELS ~D :SR ~F>"
            (type-of obj)
            (if (free-p obj) 0 (buffer-frames obj))
            (buffer-channels obj)
            (buffer-sample-rate obj))))

(defmethod free-p ((obj buffer))
  (null-pointer-p (buffer-data obj)))

(declaim (inline buffer-value))
(defun buffer-value (buffer index)
  (declare (type buffer buffer) (type non-negative-fixnum index))
  (smp-ref (buffer-data buffer) index))

(declaim (inline set-buffer-value))
(defun set-buffer-value (buffer index value)
  (declare (type buffer buffer) (type non-negative-fixnum index)
           (type real value))
  (setf (smp-ref (buffer-data buffer) index)
        (sample value)))

(defsetf buffer-value set-buffer-value)

(defun check-numeric-textfile (file)
  "Check if FILE is a text file that contains numbers.
Returns T and the number of the counted values, or NIL if the file is
not valid.

There is not the parsing of the numbers, a text file is valid if it
contains numbers or spaces or tabs or newlines.

It is possible to use line comments that begin with the `;' char."
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
             (unless sep
               (setf sep t))
             (loop do (setf code (read-byte f nil nil))
                   until (newline-p code)))
           (if (valid-char-p code)
               (if sep
                   (unless (separator-p code)
                     (setf sep nil)
                     (incf count))
                   (if (separator-p code)
                       (setf sep t)))
               (return (values nil count)))
            finally (return (values t count))))))

(defun buffer-import-textfile (buffer path offset buffer-start channels size)
  (with-open-file (f path)
    (when (plusp offset)
      ;; Skip the first OFFSET frames
      (loop for frame from 0
            while (< frame offset) do
           (dotimes (ch channels)
             (read f nil nil))))
    (let ((*read-default-float-format* *sample-type*))
      (loop with count = buffer-start
            while (< count size) do
           (dotimes (ch channels)
             (let ((value (read f nil nil)))
               (setf (buffer-value buffer count)
                     (typecase value
                       (number value)
                       (null +sample-zero+)
                       (t (nrt-msg warn "bad value (~A) in ~A" value path)
                          +sample-zero+))))
             (incf count))))))

(defun buffer-load-textfile (path &key (offset 0) frames (channels 1)
                             (sample-rate *sample-rate*))
  (multiple-value-bind (valid-p size)
      (check-numeric-textfile path)
    (when valid-p
      (let* ((offset (if (floatp offset) (sample->fixnum offset) offset))
             (channels (if (plusp channels) channels 1))
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

(defmacro with-open-sndfile ((var path offset frames channels sample-rate
                             &rest rest) &body body)
  `(if (probe-file ,path)
       (sf:with-open (,var ,path ,@rest)
         (if (sf:sndfile-null-p ,var)
             ;; Try to load a text file
             (or (buffer-load-textfile ,path :offset ,offset
                                       :frames ,frames
                                       :channels (or ,channels 1)
                                       :sample-rate (or ,sample-rate
                                                        *sample-rate*))
                 (nrt-msg error (sf:strerror ,var)))
             ,@body))
       (nrt-msg error "file ~S not found" (namestring ,path))))

(defun buffer-load (path &key (offset 0) frames (channel -1) channels
                    sample-rate)
  (declare (type (or string pathname) path) (type fixnum channel))
  (with-open-sndfile (sf path offset frames channels sample-rate)
    (let* ((offset (if (floatp offset) (sample->fixnum offset) offset))
           (info (sf:info sf))
           (channels (sf:channels info))
           (%frames (- (sf:frames info) offset))
           (frames (if frames
                       (min (if (floatp frames)
                                (sample->fixnum frames)
                                frames)
                            %frames)
                       %frames)))
      (declare (type non-negative-fixnum %frames frames channels offset))
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
                (map-sndfile-ch-to-buffer (buffer-data buffer) sf frames channels
                                          (buffer-channels buffer) 0
                                          *sndfile-buffer-size* channel-map 1)))
          (setf (buffer-file buffer)
                (if (pathnamep path) path (pathname path)))
          (locally
              (declare #.*reduce-warnings*)
              (setf (buffer-sample-rate buffer)
                    (sample (sf:sample-rate info))))
          buffer)))))

(defmacro writef-sample (sndfile ptr items)
  `(#+double-samples sf:writef-double
    #-double-samples sf:writef-float
    ,sndfile ,ptr ,items))

(declaim (inline save-data-to-textfile))
(defun save-data-to-textfile (data path size)
  (with-open-file (f path :direction :output :if-exists :supersede)
    (dotimes (i size)
      (write-line (incudine.util::sample->string (smp-ref data i)) f))))

(defun buffer-save (buf path &key (start 0) (end 0) sample-rate
                    textfile-p (header-type *default-header-type*)
                    (data-format *default-data-format*))
  (declare (type buffer buf) (type (or string pathname) path)
           (type alexandria:non-negative-real start end)
           (type (or alexandria:positive-real null) sample-rate))
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
                              :frames frames :sample-rate sr
                              :channels (buffer-channels buf)
                              :format (sf:get-format
                                       (list header-type data-format)))
                       :mode sf:sfm-write)
          (writef-sample sf data frames)))
    buf))

(defmethod free ((obj buffer))
  (unless (free-p obj)
    (funcall (buffer-foreign-free obj)
             (buffer-data obj))
    (tg:cancel-finalization obj)
    (setf (buffer-data obj) (null-pointer))
    (values)))

;;; FUNCTION has two arguments: the index and the value of the buffer
(defun map-buffer (function buffer)
  (declare #.*standard-optimize-settings*
           (type function function) (type buffer buffer)
           #.*reduce-warnings*)
  (dotimes (i (buffer-size buffer) buffer)
    (setf #1=(buffer-value buffer i)
          (funcall function i #1#))))

;;; Like MAP-INTO but for the BUFFERs
(defun map-into-buffer (result-buffer function &rest buffers)
  (declare #.*standard-optimize-settings*
           (type function function) (type buffer result-buffer)
           #.*reduce-warnings*)
  (let ((size (reduce #'min
                      (mapcar #'buffer-size (cons result-buffer buffers)))))
    (declare (type non-negative-fixnum size))
    (dotimes (i size result-buffer)
      (setf (buffer-value result-buffer i)
            (reduce function
                    (mapcar (lambda (x) (buffer-value x i))
                            buffers))))))

(defun scale-buffer (buffer mult)
  (declare (type buffer buffer) (type real mult))
  (map-buffer (lambda (index value)
                (declare (ignore index))
                (* value mult))
              buffer))

(defun normalize-buffer (buffer norm-value)
  (declare (type buffer buffer) (type real norm-value))
  (let ((data (buffer-data buffer))
        (size (buffer-size buffer)))
    (declare (type positive-fixnum size))
    (labels ((norm (index max)
               (declare (type non-negative-fixnum index)
                        (type sample max))
               (if (= index size)
                   max
                   (norm (1+ index)
                         (max (smp-ref data index) max)))))
      (scale-buffer buffer (/ norm-value (norm 1 (smp-ref data 0)))))))

(defun rescale-buffer (buffer min max)
  (declare (type buffer buffer) (type real min max))
  (let ((data (buffer-data buffer))
        (size (buffer-size buffer)))
    (declare (type positive-fixnum size))
    (labels ((resc (index old-min old-max)
               (declare (type non-negative-fixnum index)
                        (type sample old-min old-max))
               (if (= index size)
                   (values (/ (sample 1) (- old-max old-min))
                           old-min)
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

(declaim (inline buffer->list))
(defun buffer->list (buf)
  (declare (type buffer buf))
  (loop for i below (buffer-size buf) collect (buffer-value buf i)))

(defun set-buffer-from-textfile (buffer path start buffer-start buffer-end)
  (declare (type (or string pathname)) (type buffer buffer)
           (type non-negative-fixnum start buffer-start buffer-end))
  (multiple-value-bind (valid-p size)
      (check-numeric-textfile path)
    (when valid-p
      (let* ((channels (buffer-channels buffer))
             (frames (min (- buffer-end buffer-start)
                          (- (ceiling (/ size channels)) start))))
        (declare (type channel-number channels)
                 (type non-negative-fixnum frames))
        (when (plusp frames)
          (buffer-import-textfile buffer path start buffer-start
                                  channels (+ buffer-start
                                              (* frames channels))))))))

(declaim (inline check-channel-map))
(defun check-channel-map (cmap cmap-size sf-channels buf-channels)
  (declare (type list cmap)
           (type positive-fixnum cmap-size sf-channels buf-channels))
  (cond ((> cmap-size sf-channels)
         (nrt-msg error "channel-map size greater than sndfile channels"))
        ((> cmap-size buf-channels)
         (nrt-msg error "channel-map size greater than buffer channels"))
        ((some (lambda (x)
                 (cond ((>= (the non-negative-fixnum (first x))
                            sf-channels)
                        (nrt-msg error "wrong channel-map; max value for the source is ~D"
                                 (1- sf-channels)) t)
                       ((>= (the non-negative-fixnum (second x))
                            buf-channels)
                        (nrt-msg error "wrong channel-map; max value for the destination is ~D"
                                 (1- buf-channels)) t)))
               cmap))
        (t t)))

(defun map-sndfile-ch-to-buffer (data sndfile frames channels buf-channels
                                 data-offset chunk-size channel-map channel-map-size)
  (cffi:with-foreign-object (dest :int (* 2 channel-map-size))
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
                                &optional channel-map)
  (declare (type buffer buffer) (type (or string pathname) path)
           (type non-negative-fixnum start buffer-start buffer-end)
           (type list channel-map))
  (if (probe-file path)
      (sf:with-open (sf path)
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
                (locally (declare #.*standard-optimize-settings*)
                  (sf:seek sf start 0)
                  (cond (channel-map
                         (let ((channel-map-size (length channel-map)))
                           (when (check-channel-map channel-map channel-map-size channels
                                   (buffer-channels buffer))
                             (map-sndfile-ch-to-buffer (buffer-data buffer) sf selected-frames
                               channels (buffer-channels buffer)
                               buffer-start *sndfile-buffer-size*
                               channel-map channel-map-size))))
                        ((= (buffer-channels buffer) channels)
                         (sndfile-to-buffer (buffer-data buffer) sf selected-frames
                                            channels buffer-start *sndfile-buffer-size*))
                        (t (let ((channel-map (loop for src below channels
                                                    for dest below (buffer-channels buffer)
                                                    collect `(,src ,dest))))
                             (nrt-msg debug "use channel-map ~A" channel-map)
                             (map-sndfile-ch-to-buffer (buffer-data buffer) sf selected-frames
                               channels (buffer-channels buffer)
                               buffer-start *sndfile-buffer-size*
                               channel-map (min channels
                                                (buffer-channels buffer))))))))))
        buffer)
      (nrt-msg error "file ~S not found" (namestring path))))

(defun fill-buffer (buffer values &key (start 0) end (sndfile-start 0)
                    channel-map (normalize-p nil normalize-pp))
  (declare (type buffer buffer) (type boolean normalize-p)
           (type non-negative-fixnum start sndfile-start))
  (macrolet ((loop-sequence (clause seq)
                (with-gensyms (i j max scale)
                  `(let ((end (if end (min end size) size)))
                     (loop for ,i from start below end
                           for ,j ,clause ,seq
                           for ,max = 0 then (max ,j ,max) do
                          (setf (buffer-value buffer ,i) ,j)
                         finally (when (and normalize-p (/= ,max 1))
                                   (loop for ,i from start below end
                                         for ,j ,clause ,seq
                                         with ,scale = (/ 1.0 ,max) do
                                        (setf (buffer-value buffer ,i)
                                              (* (buffer-value buffer ,i) ,scale)))))))))
    (let ((size (buffer-size buffer)))
      (when (and (free-p buffer) (plusp size))
        (setf (buffer-data buffer) (foreign-alloc-sample (buffer-size buffer))))
      (unless (free-p buffer)
        (cond ((functionp values)
               (let ((chunk-size (- (if end (min end size) size)
                                    start)))
                 (when (plusp chunk-size)
                   (multiple-value-bind (c-array mult norm-p)
                       (funcall values (inc-pointer
                                        (buffer-data buffer)
                                        (* start +foreign-sample-size+))
                                chunk-size)
                     (declare (ignore c-array))
                     (let ((norm-p (if normalize-pp normalize-p norm-p)))
                       (when (and norm-p
                                  (numberp mult)
                                  (/= mult 1))
                         (scale-buffer buffer mult)))))))
              ((consp values) (loop-sequence in values))
              ((or (stringp values) (pathnamep values))
               (set-buffer-from-sndfile buffer values sndfile-start start
                                        (if end
                                            (min end (buffer-frames buffer))
                                            (buffer-frames buffer))
                                        channel-map))
              ((vectorp values) (loop-sequence across values))
              ((envelope-p values)
               (fill-buffer buffer (gen:envelope values)
                            :start start :end end
                            :normalize-p normalize-p))))
      buffer)))

(defun make-buffer (frames &key (channels 1) file (offset 0)
                    (sample-rate *sample-rate*) real-time-p
                    initial-contents fill-function (start 0) end
                    normalize-p)
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

;;; Frequently used waveforms

(defvar *sine-table* (make-buffer *default-table-size*
                                  :fill-function (gen:partials '(1))))
(declaim (type buffer *sine-table*))

(defvar *cosine-table* (make-buffer *default-table-size*
                                    :fill-function (gen:partials '((1 1 .25)))))
(declaim (type buffer *cosine-table*))
