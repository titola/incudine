;;; Copyright (c) 2013-2019 Tito Latini
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
   (bpm :initform 60.0
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

(defvar *default-format*
  (get-format (list #-darwin "wav" #+darwin "aiff" "pcm-24")))

(defun open (path-or-fd &key (mode sfm-read) info close-fd-p)
  (declare (cl:type (or string pathname integer) path-or-fd)
           (cl:type integer mode)
           (cl:type (or info null) info)
           (cl:type boolean close-fd-p))
  (when (pathnamep path-or-fd)
    (setf path-or-fd (namestring path-or-fd)))
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
    (command (%make-sndfile) #x1000 str 32)
    (values (cffi:foreign-string-to-lisp str))))

(defun get-log-info (sndfile)
  (cffi:with-foreign-object (str :char 2048)
    (command sndfile #x1001 str 2048)
    (values (cffi:foreign-string-to-lisp str))))

(defun get-current-info (sndfile)
  (let ((sfinfo (make-pointer-wrapper (cffi:foreign-alloc '(:struct info)))))
    (command sndfile #x1002 (pointer sfinfo)
             (cffi:foreign-type-size '(:struct info)))
    (prog1 (sndinfo-to-info sfinfo)
      (free sfinfo))))

(defun info (sndfile)
  (if (sndfile-p sndfile)
      (get-current-info sndfile)
      (with-open (sf sndfile)
        (get-current-info sf))))

(defun get-norm-double (sndfile)
  (= 1 (command sndfile #x1010 (cffi:null-pointer) 0)))

(defun get-norm-float (sndfile)
  (= 1 (command sndfile #x1011 (cffi:null-pointer) 0)))

(defun set-norm-double (sndfile norm-p)
  (command-set-bool sndfile #x1012 norm-p))

(defun set-norm-float (sndfile norm-p)
  (command-set-bool sndfile #x1013 norm-p))

(defun set-scale-float-int-read (sndfile bool)
  (command-set-bool sndfile #x1014 bool))

(defun set-scale-int-float-write (sndfile bool)
  (command-set-bool sndfile #x1015 bool))

(defun get-simple-format-count ()
  (command-to-int #x1020))

(defun get-simple-format (format-number)
  (command-to-format-info #x1021 format-number))

(defun get-format-info (format)
  (command-to-format-info #x1028 format))

(defun get-format-major-count ()
  (command-to-int #x1030))

(defun get-format-major (format-number)
  (command-to-format-info #x1031 format-number))

(defun get-format-subtype-count ()
  (command-to-int #x1032))

(defun get-format-subtype (format-number)
  (command-to-format-info #x1033 format-number))

(defun calc-signal-max (sndfile)
  (command-to-double sndfile #x1040))

(defun calc-norm-signal-max (sndfile)
  (command-to-double sndfile #x1041))

(defun calc-max-all-channels (sndfile)
  (command-to-double sndfile #x1042))

(defun calc-norm-max-all-channels (sndfile)
  (command-to-double sndfile #x1043))

(defun get-signal-max (sndfile)
  (command-to-double sndfile #x1044))

(defun get-max-all-channels (sndfile)
  (command-to-double sndfile #x1045))

(defun set-add-peak-chunk (sndfile peak-chunk-p)
  (command-set-bool sndfile #x1050 peak-chunk-p))

(defun set-add-header-pad-chunk (sndfile pad-chunk-p)
  (command-set-bool sndfile #x1051 pad-chunk-p))

(defun update-header-now (sndfile)
  (command sndfile #x1060 (cffi:null-pointer) 0))

(defun set-update-header-auto (sndfile bool)
  (command-set-bool sndfile #x1061 bool))

(defun file-truncate (sndfile frames)
  (command-to-sf-count sndfile #x1080 frames))

(defun set-raw-start-offset (sndfile offset)
  (command-to-sf-count sndfile #x1090 offset))

(defun set-dither-on-write (sndfile bool)
  (command-set-bool sndfile #x10a0 bool))

(defun set-dither-on-read (sndfile bool)
  (command-set-bool sndfile #x10a1 bool))

(defun get-dither-info-count ()
  (command-to-int #x10a2))

(defun get-dither-info (number)
  (cffi:with-foreign-object (dinfo '(:struct dither-info))
    (cffi:with-foreign-slots ((type level name) dinfo (:struct dither-info))
      (setf type number)
      (when (zerop (command (%make-sndfile) #x10a3 dinfo
                            (cffi:foreign-type-size '(:struct dither-info))))
        (make-instance 'dither-info :type type :level level :name name)))))

(defun get-embed-file-info (sndfile)
  (cffi:with-foreign-object (ef-info '(:struct embed-file-info))
    (cffi:with-foreign-slots ((offset length) ef-info (:struct embed-file-info))
      (when (zerop (command sndfile #x10b0 ef-info
                            (cffi:foreign-type-size
                              '(:struct embed-file-info))))
        (make-instance 'embed-file-info :offset offset :length length)))))

(defun set-clipping (sndfile bool)
  (command-set-bool sndfile #x10c0 bool))

(defun get-clipping (sndfile)
  (= 1 (command sndfile #x10c1 (cffi:null-pointer) 0)))

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
      (when (= 1 (command sndfile #x10d0 instr
                          (cffi:foreign-type-size '(:struct instrument))))
        (make-instance 'instrument
          :gain gain :basenote basenote :detune detune :velocity-lo velocity-lo
          :velocity-hi velocity-hi :key-lo key-lo :key-hi key-hi
          :loop-count loop-count
          :loops (make-array 16
                   :initial-contents (instrument-loops loops loop-count)))))))

(defun set-instrument (sndfile instr)
  (let ((i (instrument-to-foreign-instrument instr)))
    (prog1 (= 1 (command sndfile #x10d1 (pointer i)
                         (cffi:foreign-type-size '(:struct instrument))))
      (free i))))

(defun get-loop-info (sndfile)
  (cffi:with-foreign-object (linfo '(:struct loop-info))
    (cffi:with-foreign-slots ((time-sig-num time-sig-den loop-mode num-beats bpm
                               root-key future) linfo (:struct loop-info))
      (when (= 1 (command sndfile #x10e0 linfo
                          (cffi:foreign-type-size '(:struct loop-info))))
        (make-instance 'loop-info
          :time-sig-num time-sig-num :time-sig-den time-sig-den
          :loop-mode loop-mode :num-beats num-beats :bpm bpm
          :root-key root-key
          :future (make-array 6
                    :initial-contents (loop for i below 6
                                            collect (cffi:mem-aref future :int
                                                                   i))))))))
