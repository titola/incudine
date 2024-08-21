;;; Copyright (c) 2013-2024 Tito Latini
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :sndfile)

(defclass info ()
  ((frames :initform 0
           :initarg :frames
           :type (unsigned-byte 64)
           :accessor frames)
   (sample-rate :initform 48000
                :initarg :sample-rate
                :type non-negative-fixnum
                :accessor sample-rate)
   (channels :initform 1
             :initarg :channels
             :type non-negative-fixnum
             :accessor channels)
   (format :initform 0
           :initarg :format
           :type non-negative-fixnum
           :accessor format)
   (sections :initform 1
             :initarg :sections
             :type non-negative-fixnum
             :accessor sections)
   (seekable :initform t
             :initarg :seekable
             :type boolean
             :accessor seekable)))

(defclass format-info ()
  ((format :initform 0
           :initarg :format
           :type fixnum
           :accessor format)
   (name :initform "none"
         :initarg :name
         :type string
         :accessor name)
   (extension :initform ""
              :initarg :extension
              :type string
              :accessor extension)))

(defclass dither-info ()
  ((type :initform 0
         :initarg :type
         :type fixnum
         :accessor type)
   (level :initform 0.0d0
          :initarg :level
          :type double-float
          :accessor level)
   (name :initform "none"
         :initarg :name
         :type string
         :accessor name)))

(defclass embed-file-info ()
  ((offset :initform 0
           :initarg :offset
           :type (unsigned-byte 64)
           :accessor offset)
   (length :initform 0
           :initarg :length
           :type (unsigned-byte 64)
           :accessor length)))

(defclass instr-loop ()
  ((mode :initform 0
         :initarg :mode
         :type fixnum
         :accessor mode)
   (start :initform 0
          :initarg :start
          :type unsigned-byte
          :accessor start)
   (end :initform 0
        :initarg :end
        :type unsigned-byte
        :accessor end)
   (count :initform 0
          :initarg :count
          :type unsigned-byte
          :accessor count)))

(defclass instrument ()
  ((gain :initform 0
         :initarg :gain
         :type fixnum
         :accessor gain)
   (basenote :initform 0
             :initarg :basenote
             :type (unsigned-byte 8)
             :accessor basenote)
   (detune :initform 0
           :initarg :detune
           :type (unsigned-byte 8)
           :accessor detune)
   (velocity-lo :initform 0
                :initarg :velocity-lo
                :type (unsigned-byte 8)
                :accessor velocity-lo)
   (velocity-hi :initform 0
                :initarg :velocity-hi
                :type (unsigned-byte 8)
                :accessor velocity-hi)
   (key-lo :initform 0
           :initarg :key-lo
           :type (unsigned-byte 8)
           :accessor key-lo)
   (key-hi :initform 0
           :initarg :key-hi
           :type (unsigned-byte 8)
           :accessor key-hi)
   (loop-count :initform 0
               :initarg :loop-count
               :type fixnum
               :accessor loop-count)
   (loops :initform (make-array 16
                      :initial-contents (loop repeat 16
                                              collect (make-instance
                                                        'instr-loop)))
          :initarg :loops :type simple-vector :accessor loops)))

(defclass loop-info ()
  ((time-sig-num :initform 0
                 :initarg :time-sig-num
                 :type fixnum
                 :accessor time-sig-num)
   (time-sig-den :initform 0
                 :initarg :time-sig-den
                 :type fixnum
                 :accessor time-sig-den)
   (loop-mode :initform 0
              :initarg :loop-mode
              :type fixnum
              :accessor loop-mode)
   (num-beats :initform 0
              :initarg :num-beats
              :type fixnum
              :accessor num-beats)
   (bpm :initform 60f0
        :initarg :bpm
        :type single-float
        :accessor bpm)
   (root-key :initform 0
             :initarg :root-key
             :type fixnum
             :accessor root-key)
   (future :initform (make-array 6 :initial-element 0)
           :initarg :future
           :type simple-vector
           :accessor future)))

(defclass broadcast-info ()
  ((description :initform "none"
                :initarg :description
                :type string
                :accessor description)
   (originator :initform "none"
               :initarg :originator
               :type string
               :accessor originator)
   (originator-reference :initform "none"
                         :initarg :originator-reference
                         :type string
                         :accessor originator-reference)
   (originator-date :initform "none"
                    :initarg :originator-date
                    :type string
                    :accessor originator-date)
   (originator-time :initform "none"
                    :initarg :originator-time
                    :type string
                    :accessor originator-time)
   (time-reference-low :initform 0
                       :initarg :time-reference-low
                       :type unsigned-byte
                       :accessor time-reference-low)
   (time-reference-high :initform 0
                        :initarg :time-reference-high
                        :type unsigned-byte
                        :accessor time-reference-high)
   (version :initform 0
            :initarg :version
            :type fixnum
            :accessor version)
   (umid :initform "none"
         :initarg :umid
         :type string
         :accessor umid)
   (reserved :initform "none"
             :initarg :reserved
             :type string
             :accessor reserved)
   (coding-history-size :initform 0
                        :initarg :coding-history-size
                        :type unsigned-byte
                        :accessor coding-history-size)
   (coding-history :initform "none"
                   :initarg :coding-history
                   :type string
                   :accessor coding-history)))

(defclass virtual-io ()
  ((get-filelen :initform (cffi:null-pointer)
                :initarg :get-filelen
                :type cffi:foreign-pointer
                :accessor get-filelen)
   (seek :initform (cffi:null-pointer)
         :initarg :seek
         :type cffi:foreign-pointer
         :accessor virtual-io-seek)
   (read :initform (cffi:null-pointer)
         :initarg :read
         :type cffi:foreign-pointer
         :accessor virtual-io-read)
   (write :initform (cffi:null-pointer)
          :initarg :write
          :type cffi:foreign-pointer
          :accessor virtual-io-write)
   (tell :initform (cffi:null-pointer)
         :initarg :tell
         :type cffi:foreign-pointer
         :accessor virtual-io-tell)))

(declaim (inline make-sndinfo))
(defun make-sndinfo (&optional pointer)
  (%make-sndinfo
    :pointer (or pointer
                 (cffi:foreign-alloc :int8
                   :count (cffi:foreign-type-size '(:struct info))
                   :initial-element 0))))

(defun make-info (&key (frames 0) (sample-rate 48000) (channels 1)
                  (format 0) (sections 0) (seekable t))
  (make-instance 'info :frames frames :sample-rate sample-rate
                 :channels channels :format format :sections sections
                 :seekable seekable))

(defun info-to-sndinfo (info)
  (let* ((ptr (cffi:foreign-alloc '(:struct info)))
         (sinfo (make-pointer-wrapper ptr)))
    (cffi:with-foreign-slots ((frames sample-rate channels format sections
                               seekable)
                              ptr (:struct info))
      (with-slots ((fr frames) (sr sample-rate) (chans channels)
                   (fmt format) (sec sections) (seek-p seekable)) info
        (setf frames fr
              sample-rate sr
              channels chans
              format fmt
              sections sec
              seekable (if seek-p 1 0))
        sinfo))))

(defun sndinfo-to-info (sndinfo &optional info)
  (cffi:with-foreign-slots ((frames sample-rate channels format
                            sections seekable)
                            (pointer sndinfo) (:struct info))
    (if info
        (with-slots ((fr frames) (sr sample-rate) (chans channels) (fmt format)
                     (sec sections) (seek-p seekable)) info
          (setf fr frames
                sr sample-rate
                chans channels
                fmt format
                sec sections
                seek-p (= seekable 1))
          info)
        (make-info :frames frames :sample-rate sample-rate
                   :channels channels :format format
                   :sections sections :seekable (= seekable 1)))))

(defun instrument-to-foreign-instrument (instr)
  (let* ((ptr (cffi:foreign-alloc '(:struct instrument)))
         (foreign-instr (make-pointer-wrapper ptr)))
    (cffi:with-foreign-slots ((gain basenote detune velocity-lo velocity-hi
                               key-lo key-hi loop-count loops)
                              ptr (:struct instrument))
      (with-slots ((g gain) (bn basenote) (vlo velocity-lo) (vhi velocity-hi)
                   (klo key-lo) (khi key-hi) (lc loop-count) (l loops)) instr
        (setf gain g
              basenote bn
              velocity-lo vlo
              velocity-hi vhi
              key-lo klo
              key-hi khi
              loop-count lc)
        (dotimes (i 16)
          (let ((iloop (cffi:foreign-slot-value ptr '(:struct instrument)
                                                'loops))
                (loop (svref l i)))
              (setf (cffi:mem-aref iloop :int) (mode loop)
                    (cffi:mem-aref iloop :int 1) (start loop)
                    (cffi:mem-aref iloop :int 2) (end loop)
                    (cffi:mem-aref iloop :int 3) (count loop))))
        foreign-instr))))

(defun duration (info)
  (declare (info info))
  (with-slots (frames sample-rate) info
    (coerce (/ frames sample-rate) 'float)))

;;; An alternative frame size calculation requires a file descriptor
;;; as argument:
;;;
;;;     (sf:seek sf 1 SF:SEEK-SET)
;;;     (sb-posix:lseek fd 0 SB-POSIX:SEEK-CUR)
;;;
;;; then close fd or reset position.
(defun frame-size (sndfile)
  (let ((frames (sf:frames (sf:info sndfile))))
    (declare (cl:type non-negative-fixnum frames))
    (assert (> frames 0))
    (locally (declare (optimize speed (safety 0)))
      (values
        (the fixnum
          (floor (the (unsigned-byte 64)
                   (sf:length (sf:get-embed-file-info sndfile)))
                 frames))))))

(defvar *default-format*
  (get-format (list #-darwin "wav" #+darwin "aiff" "pcm-24")))

(defun open (path-or-fd &key (mode sfm-read) info close-fd-p)
  (declare (cl:type (or string pathname integer) path-or-fd)
           (cl:type integer mode)
           (cl:type (or info null) info)
           (cl:type boolean close-fd-p))
  (when (pathnamep path-or-fd)
    (setf path-or-fd (#+sbcl sb-ext:native-namestring
                      #-sbcl namestring
                      path-or-fd)))
  (let* ((sfinfo (if info
                     (info-to-sndinfo info)
                     (make-sndinfo)))
         (sf (if (numberp path-or-fd)
                 (open-fd path-or-fd mode sfinfo (if close-fd-p true false))
                 (%open path-or-fd mode sfinfo))))
    (when info
      (sndinfo-to-info sfinfo info))
    (free sfinfo)
    sf))

(defmacro with-open ((var path-or-fd &key info (mode sfm-read) close-fd-p)
                     &body body)
  `(let ((,var (open ,path-or-fd :mode ,mode :info ,info
                     :close-fd-p ,close-fd-p)))
     (unwind-protect (progn ,@body)
       (when ,var (close ,var)))))

(defun command-to-int (command-number)
  (cffi:with-foreign-object (i :int)
    (command (%make-sndfile) command-number i (cffi:foreign-type-size :int))
    (cffi:mem-ref i :int)))

(defun command-to-sf-count (sndfile command-number value)
  (cffi:with-foreign-object (count 'sf-count)
    (setf (cffi:mem-ref count 'sf-count) value)
    (command sndfile command-number count (cffi:foreign-type-size 'sf-count))))

(defun command-to-double (sndfile command-number)
  (cffi:with-foreign-object (d :double)
    (let ((res (command sndfile command-number d
                        (cffi:foreign-type-size :double))))
      (values (cffi:mem-ref d :double) res))))

(defun command-to-format-info (command-number format-number)
  (cffi:with-foreign-object (fmt-info '(:struct format-info))
    (cffi:with-foreign-slots ((format name extension) fmt-info
                              (:struct format-info))
      (setf format format-number)
      (when (zerop (command (%make-sndfile) command-number fmt-info
                            (cffi:foreign-type-size '(:struct format-info))))
        (make-instance 'format-info :format format :name name
                       :extension extension)))))

(defun command-set-bool (sndfile command-number bool)
  (declare (boolean bool))
  (command sndfile command-number (cffi:null-pointer) (if bool 1 0))
  bool)

(defun get-lib-version ()
  (cffi:with-foreign-object (str :char 32)
    (command (%make-sndfile) SFC-GET-LIB-VERSION str 32)
    (values (cffi:foreign-string-to-lisp str))))

(defun get-log-info (sndfile)
  (cffi:with-foreign-object (str :char 2048)
    (command sndfile SFC-GET-LOG-INFO str 2048)
    (values (cffi:foreign-string-to-lisp str))))

(defun get-current-info (sndfile)
  (let ((sfinfo (make-pointer-wrapper (cffi:foreign-alloc '(:struct info)))))
    (command sndfile SFC-GET-CURRENT-SF-INFO (pointer sfinfo)
             (cffi:foreign-type-size '(:struct info)))
    (prog1 (sndinfo-to-info sfinfo)
      (free sfinfo))))

(defun info (sndfile)
  (if (sndfile-p sndfile)
      (get-current-info sndfile)
      (with-open (sf sndfile)
        (get-current-info sf))))

(defun get-norm-double (sndfile)
  (= 1 (command sndfile SFC-GET-NORM-DOUBLE (cffi:null-pointer) 0)))

(defun get-norm-float (sndfile)
  (= 1 (command sndfile SFC-GET-NORM-FLOAT (cffi:null-pointer) 0)))

(defun set-norm-double (sndfile norm-p)
  (command-set-bool sndfile SFC-SET-NORM-DOUBLE norm-p))

(defun set-norm-float (sndfile norm-p)
  (command-set-bool sndfile SFC-SET-NORM-FLOAT norm-p))

(defun set-scale-float-int-read (sndfile bool)
  (command-set-bool sndfile SFC-SET-SCALE-FLOAT-INT-READ bool))

(defun set-scale-int-float-write (sndfile bool)
  (command-set-bool sndfile SFC-SET-SCALE-INT-FLOAT-WRITE bool))

(defun get-simple-format-count ()
  (command-to-int SFC-GET-SIMPLE-FORMAT-COUNT))

(defun get-simple-format (format-number)
  (command-to-format-info SFC-GET-SIMPLE-FORMAT format-number))

(defun get-format-info (format)
  (command-to-format-info SFC-GET-FORMAT-INFO format))

(defun get-format-major-count ()
  (command-to-int SFC-GET-FORMAT-MAJOR-COUNT))

(defun get-format-major (format-number)
  (command-to-format-info SFC-GET-FORMAT-MAJOR format-number))

(defun get-format-subtype-count ()
  (command-to-int SFC-GET-FORMAT-SUBTYPE-COUNT))

(defun get-format-subtype (format-number)
  (command-to-format-info SFC-GET-FORMAT-SUBTYPE format-number))

(defun calc-signal-max (sndfile)
  (command-to-double sndfile SFC-CALC-SIGNAL-MAX))

(defun calc-norm-signal-max (sndfile)
  (command-to-double sndfile SFC-CALC-NORM-SIGNAL-MAX))

(defun calc-max-all-channels (sndfile)
  (command-to-double sndfile SFC-CALC-MAX-ALL-CHANNELS))

(defun calc-norm-max-all-channels (sndfile)
  (command-to-double sndfile SFC-CALC-NORM-MAX-ALL-CHANNELS))

(defun get-signal-max (sndfile)
  (command-to-double sndfile SFC-GET-SIGNAL-MAX))

(defun get-max-all-channels (sndfile)
  (command-to-double sndfile SFC-GET-MAX-ALL-CHANNELS))

(defun set-add-peak-chunk (sndfile peak-chunk-p)
  (command-set-bool sndfile SFC-SET-ADD-PEAK-CHUNK peak-chunk-p))

(defun set-add-header-pad-chunk (sndfile pad-chunk-p)
  (command-set-bool sndfile SFC-SET-ADD-HEADER-PAD-CHUNK pad-chunk-p))

(defun update-header-now (sndfile)
  (command sndfile SFC-UPDATE-HEADER-NOW (cffi:null-pointer) 0))

(defun set-update-header-auto (sndfile bool)
  (command-set-bool sndfile SFC-SET-UPDATE-HEADER-AUTO bool))

(defun file-truncate (sndfile frames)
  (command-to-sf-count sndfile SFC-FILE-TRUNCATE frames))

(defun set-raw-start-offset (sndfile offset)
  (command-to-sf-count sndfile SFC-SET-RAW-START-OFFSET offset))

(defun set-dither-on-write (sndfile bool)
  (command-set-bool sndfile SFC-SET-DITHER-ON-WRITE bool))

(defun set-dither-on-read (sndfile bool)
  (command-set-bool sndfile SFC-SET-DITHER-ON-READ bool))

(defun get-dither-info-count ()
  (command-to-int SFC-GET-DITHER-INFO-COUNT))

(defun get-dither-info (number)
  (cffi:with-foreign-object (dinfo '(:struct dither-info))
    (cffi:with-foreign-slots ((type level name) dinfo (:struct dither-info))
      (setf type number)
      (when (zerop (command (%make-sndfile) SFC-GET-DITHER-INFO dinfo
                            (cffi:foreign-type-size '(:struct dither-info))))
        (make-instance 'dither-info :type type :level level :name name)))))

(defun get-embed-file-info (sndfile)
  (cffi:with-foreign-object (ef-info '(:struct embed-file-info))
    (cffi:with-foreign-slots ((offset length) ef-info (:struct embed-file-info))
      (when (zerop (command sndfile SFC-GET-EMBED-FILE-INFO ef-info
                            (cffi:foreign-type-size
                              '(:struct embed-file-info))))
        (make-instance 'embed-file-info :offset offset :length length)))))

(defun set-clipping (sndfile bool)
  (command-set-bool sndfile SFC-SET-CLIPPING bool))

(defun get-clipping (sndfile)
  (= 1 (command sndfile SFC-GET-CLIPPING (cffi:null-pointer) 0)))

(defun instrument-loops (loops-ptr count)
  (loop for i below 16
        if (< i count)
        collect (cffi:with-foreign-slots
                    ((mode start end count)
                     (cffi:mem-aref loops-ptr '(:struct instr-loop) i)
                     (:struct instr-loop))
                  (make-instance 'instr-loop :mode mode :start start :end end
                                 :count count))
        else collect (make-instance 'instr-loop)))

(defun get-instrument (sndfile)
  (cffi:with-foreign-object (instr '(:struct instrument))
    (cffi:with-foreign-slots ((gain basenote detune velocity-lo velocity-hi
                               key-lo key-hi loop-count loops)
                              instr (:struct instrument))
      (when (= 1 (command sndfile SFC-GET-INSTRUMENT instr
                          (cffi:foreign-type-size '(:struct instrument))))
        (make-instance 'instrument
          :gain gain :basenote basenote :detune detune :velocity-lo velocity-lo
          :velocity-hi velocity-hi :key-lo key-lo :key-hi key-hi
          :loop-count loop-count
          :loops (make-array 16
                   :initial-contents (instrument-loops loops loop-count)))))))

(defun set-instrument (sndfile instr)
  (let ((i (instrument-to-foreign-instrument instr)))
    (prog1 (= 1 (command sndfile SFC-SET-INSTRUMENT (pointer i)
                         (cffi:foreign-type-size '(:struct instrument))))
      (free i))))

(defun get-loop-info (sndfile)
  (cffi:with-foreign-object (linfo '(:struct loop-info))
    (cffi:with-foreign-slots ((time-sig-num time-sig-den loop-mode num-beats bpm
                               root-key future) linfo (:struct loop-info))
      (when (= 1 (command sndfile SFC-GET-LOOP-INFO linfo
                          (cffi:foreign-type-size '(:struct loop-info))))
        (make-instance 'loop-info
          :time-sig-num time-sig-num :time-sig-den time-sig-den
          :loop-mode loop-mode :num-beats num-beats :bpm bpm
          :root-key root-key
          :future (make-array 6
                    :initial-contents (loop for i below 6
                                            collect (cffi:mem-aref future :int
                                                                   i))))))))
