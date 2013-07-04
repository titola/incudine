;;; Copyright (c) 2013 Tito Latini
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
    (lodiv #.(coerce 1.0 'sample) :type sample)
    (frames 0 :type non-negative-fixnum)
    (channels 1 :type non-negative-fixnum)
    (sample-rate *sample-rate* :type sample)
    (file nil :type (or pathname null))
    (real-time-p nil :type boolean)
    (foreign-free #'foreign-free :type function)))

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
                            #'foreign-rt-free
                            #'foreign-free))
         (obj (%make-buffer
               :data data
               :size size
               :mask (if (power-of-two-p size)
                         (1- size)
                         (1- (next-power-of-two (ash size -1))))
               :lobits lobits
               :lomask (1- value)
               :lodiv (if (zerop lobits)
                          +sample-zero+
                          (/ (coerce 1.0 'sample) value))
               :frames frames
               :channels channels
               :sample-rate sample-rate
               :real-time-p real-time-p
               :foreign-free free-function)))
    (tg:finalize obj (lambda ()
                       (rt-eval-if (real-time-p)
                         (funcall free-function data))))
    obj))

(defmethod print-object ((obj buffer) stream)
  (with-slots (frames channels sample-rate) obj
    (format stream "#<~S :FRAMES ~D :CHANNELS ~D :SR ~F>"
            (type-of obj)
            (if (free-p obj) 0 (buffer-frames obj))
            (buffer-channels obj)
            (buffer-sample-rate obj))))

(defgeneric free-p (obj))

(defmethod free-p ((obj buffer))
  (null-pointer-p (buffer-data obj)))

(defmacro with-sndfile-open ((var path &rest rest) &body body)
  `(if (probe-file ,path)
       (sf:with-open (,var ,path ,@rest)
         (if (sf:sndfile-null-p ,var)
             (nrt-msg error (sf:strerror ,var))
             ,@body))
       (nrt-msg error "file ~S not found" (namestring ,path))))

(defun buffer-load (path &key (offset 0) frames (channel -1))
  (declare (type (or string pathname) path) (type fixnum channel))
  (with-sndfile-open (sf path)
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
                    (coerce (sf:sample-rate info) 'sample)))
          buffer)))))

(defmethod free ((obj buffer))
  (unless (free-p obj)
    (funcall (buffer-foreign-free obj)
             (buffer-data obj))
    (tg:cancel-finalization obj)
    (setf (buffer-data obj) (null-pointer))
    (values)))

(declaim (inline buffer-value))
(defun buffer-value (buffer index)
  (declare (type buffer buffer) (type non-negative-fixnum index))
  (data-ref (buffer-data buffer) index))

(declaim (inline set-buffer-value))
(defun set-buffer-value (buffer index value)
  (declare (type buffer buffer) (type non-negative-fixnum index)
           (type real value))
  (setf (data-ref (buffer-data buffer) index)
        (coerce value 'sample)))

(defsetf buffer-value set-buffer-value)

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

(defgeneric scale (obj mult))

(defmethod scale ((obj buffer) (mult real))
  (map-buffer (lambda (index value)
                (declare (ignore index))
                (* value mult))
              obj))

(defgeneric normalize (obj norm-value))

(defmethod normalize ((obj buffer) (norm-value real))
  (let* ((data (buffer-data obj))
         (max (data-ref data 0))
         (size (buffer-size obj)))
    (declare (type positive-fixnum size))
    (do* ((i 1 (1+ i))
          (value (data-ref data i) (data-ref data i)))
         ((= i size))
      (declare (type positive-fixnum i))
      (when (> value max) (setf max value)))
    (let ((mult (/ norm-value max)))
      (scale obj mult))))

(defgeneric rescale (obj min max))

(defmethod rescale ((obj buffer) (min real) (max real))
  (let* ((data (buffer-data obj))
         (old-min (data-ref data 0))
         (old-max old-min)
         (size (buffer-size obj)))
    (declare (type positive-fixnum size))
    (do* ((i 1 (1+ i))
          (value (data-ref data i) (data-ref data i)))
         ((= i size))
      (declare (type positive-fixnum i))
      (when (> value old-max) (setf old-max value))
      (when (< value old-min) (setf old-min value)))
    (let ((old-delta (/ 1.0d0 (- old-max old-min)))
          (new-delta (- max min)))
      (map-buffer (lambda (index value)
                    (declare (ignore index))
                    (+ min (* new-delta old-delta (- value old-min))))
                  obj))))

(declaim (inline buffer->list))
(defun buffer->list (buf)
  (declare (type buffer buf))
  (loop for i below (buffer-size buf) collect (buffer-value buf i)))

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
  (let ((double-size (* channel-map-size)))
    (declare (type non-negative-fixnum double-size))
    (cffi:with-foreign-object (dest :int double-size)
      (let ((src (cffi:inc-pointer dest
                                   (the non-negative-fixnum
                                     (* double-size
                                        (the non-negative-fixnum
                                          (cffi:foreign-type-size :int)))))))
        (do ((i 0 (1+ i))
             (l channel-map (cdr l)))
            ((null l))
          (declare (type non-negative-fixnum i) (type list l))
          (let ((map (car l)))
            (setf (cffi:mem-aref src :int i) (first map)
                  (cffi:mem-aref dest :int i) (second map))))
        (incudine.external::%map-sndfile-ch-to-buffer data sndfile frames channels
                                                      buf-channels data-offset chunk-size
                                                      dest src channel-map-size)))))

(defun set-buffer-from-sndfile (buffer path start buffer-start buffer-end
                                &optional channel-map)
  (declare (type buffer buffer) (type (or string pathname) path)
           (type non-negative-fixnum start buffer-start buffer-end)
           (type list channel-map))
  (with-sndfile-open (sf path)
    (unless (sf:sndfile-null-p sf)
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
                                                                  (buffer-channels buffer))))))))))))

(defun set-buffer-data (buffer values &key (start 0) end
                        (sndfile-start 0) channel-map normalize-p)
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
                   (multiple-value-bind (c-array mult normalize-p)
                       (funcall values (inc-pointer
                                        (buffer-data buffer)
                                        (* start +foreign-sample-size+))
                                chunk-size)
                     (declare (ignore c-array))
                     (when (and normalize-p
                                (numberp mult)
                                (/= mult 1))
                       (scale buffer mult))))))
              ((consp values) (loop-sequence in values))
              ((or (stringp values) (pathnamep values))
               (set-buffer-from-sndfile buffer values sndfile-start start
                                        (if end
                                            (min end (buffer-frames buffer))
                                            (buffer-frames buffer))
                                        channel-map))
              ((vectorp values) (loop-sequence across values))
              ((envelope-p values)
               (set-buffer-data buffer (gen:envelope values)
                                :start start :end end
                                :normalize-p normalize-p))))
      buffer)))

(defgeneric data (obj))

(defmethod data ((obj buffer))
  (buffer-data obj))

(defgeneric (setf data) (values obj))

(defmethod (setf data) (values (obj buffer))
  (set-buffer-data obj values))

(defun make-buffer (frames &key (channels 1) file (offset 0)
                    (sample-rate *sample-rate*) real-time-p
                    initial-contents fill-function (start 0) end
                    normalize-p)
  (flet ((new-from-file (frm ch f os sr)
           (let ((buf (%%make-buffer frm ch sr real-time-p)))
             (declare (type buffer buf))
             (set-buffer-from-sndfile buf f os 0 frm)
             buf)))
    (if file
        (if (zerop frames)
            (if (zerop offset)
                (buffer-load file)
                (let* ((info (sf:info file))
                       (frames (max 1 (- (sf:frames info) offset)))
                       (channels (sf:channels info)))
                  (new-from-file frames channels file offset sample-rate)))
            (new-from-file frames channels file offset sample-rate))
        (let ((buf (%%make-buffer frames channels sample-rate real-time-p))
              (value (or initial-contents fill-function)))
          (when value
            (set-buffer-data buf value :start start :end end
                             :normalize-p normalize-p))
          buf))))

;;; Frequently used waveforms

(defvar *sine-table* (make-buffer *default-table-size*
                                  :fill-function (gen:partials '(1))))
(declaim (type buffer *sine-table*))

(defvar *cosine-table* (make-buffer *default-table-size*
                                    :fill-function (gen:partials '((1 1 .25)))))
(declaim (type buffer *cosine-table*))


;; DEBUG
;; (defun dump-buffer-values (buf)
;;   (with-open-file (f "/tmp/buffer_dump.txt"
;;                      :direction :output :if-exists :supersede)
;;     (dotimes (i (buffer-size buf))
;;       (format f "~F~%" (buffer-value buf i)))))
