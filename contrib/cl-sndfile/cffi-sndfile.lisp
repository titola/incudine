;;; Copyright (c) 2013-2022 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library sndfile
    (:darwin "libsndfile.dylib")
    (:unix "libsndfile.so")
    (:cygwin "cygsndfile-0.dll")
    (t (:default "libsndfile")))

  (defun load-sndfile-library ()
    (cffi:use-foreign-library sndfile))

  (unless (cffi:foreign-library-loaded-p 'sndfile)
    (load-sndfile-library)))

(cffi:defctype sf-count :int64)
;; Alias.
(cffi:defctype count :int64)

(cffi:defcstruct info
  (frames sf-count)
  (sample-rate :int)
  (channels :int)
  (format :int)
  (sections :int)
  (seekable :int))

(defstruct (pointer-wrapper (:constructor %make-pointer-wrapper))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (sndfile (:include pointer-wrapper)
                    (:constructor %make-sndfile)
                    (:copier nil)))

(defstruct (sndinfo (:include pointer-wrapper)
                    (:constructor %make-sndinfo)
                    (:copier nil)))

(declaim (inline pointer))
(defun pointer (obj)
  (pointer-wrapper-pointer obj))

(declaim (inline finalize))
(defun finalize (obj function)
  #+sbcl (sb-ext:finalize obj function :dont-save t)
  #-sbcl (tg:finalize obj function))

(declaim (inline cancel-finalization))
(defun cancel-finalization (obj)
  #+sbcl (sb-ext:cancel-finalization obj)
  #-sbcl (tg:cancel-finalization obj))

(defun close (sfile)
  (declare (sndfile sfile))
  (let ((result (%close (sndfile-pointer sfile))))
    (setf (sndfile-pointer sfile) (cffi:null-pointer))
    (cancel-finalization sfile)
    (unless (zerop result)
      (error-generic result))
    (values)))

(declaim (inline make-sndfile))
(defun make-sndfile (pointer)
  (let ((obj (%make-sndfile :pointer pointer)))
    (unless (cffi:null-pointer-p pointer)
      (finalize obj (lambda () (%close pointer))))
    obj))

(declaim (inline make-sndinfo))
(defun make-sndinfo (&optional pointer)
  (%make-sndinfo
    :pointer (or pointer
                 (cffi:foreign-alloc :int8
                   :count (cffi:foreign-type-size '(:struct info))
                   :initial-element 0))))

(defun make-pointer-wrapper (pointer)
  (let ((obj (%make-pointer-wrapper :pointer pointer)))
    (finalize obj (lambda () (cffi:foreign-free pointer)))
    obj))

(defun free (obj)
  (let ((ptr (pointer obj)))
    (unless (cffi:null-pointer-p ptr)
      (cffi:foreign-free ptr)
      (setf (pointer-wrapper-pointer obj) (cffi:null-pointer))
      (cancel-finalization obj))
    (values)))

(defun sndfile-null-p (sndfile)
  (declare (sndfile sndfile))
  (cffi:null-pointer-p (sndfile-pointer sndfile)))

(cffi:define-foreign-type sndfile-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser sndfile))

(cffi:define-foreign-type pointer-wrapper-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser pointer-wrapper))

(cffi:define-foreign-type sndinfo-type (pointer-wrapper-type)
  ()
  (:simple-parser sndinfo))

(defmethod cffi:translate-from-foreign (ptr (type sndfile-type))
  (make-sndfile ptr))

(defmethod cffi:translate-from-foreign (ptr (type sndinfo-type))
  (make-pointer-wrapper ptr))

(defmethod cffi:translate-to-foreign (handle (type sndfile-type))
  (slot-value handle 'pointer))

(defmethod cffi:translate-to-foreign (handle (type sndinfo-type))
  (slot-value handle 'pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *formats* (make-hash-table :test 'equal))
  (declaim (cl:type hash-table *formats*))

  (macrolet
      ((define-constants (lst)
         `(progn ,@(mapcar (lambda (x) (cons 'define-constant x)) lst)))
       (define-format-constants (lst)
         `(progn
            ,@(mapcar (lambda (x)
                        (let ((key (string-downcase
                                     (subseq (symbol-name (car x))
                                             #.(cl:length "FORMAT-"))
                                     :end (when (member (car x)
                                                  '(FORMAT-MPEG-LAYER-I
                                                    FORMAT-MPEG-LAYER-II
                                                    FORMAT-MPEG-LAYER-III))
                                            #.(cl:length "MPEG-LAYER-")))))
                          `(progn
                             ,(cons 'define-constant x)
                             (setf (gethash ,key *formats*) ,(car x)))))
                      lst))))
    (define-format-constants
        (;; Major formats.
         (FORMAT-WAV        #x010000)
         (FORMAT-AIFF       #x020000)
         (FORMAT-AU         #x030000)
         (FORMAT-RAW        #x040000)
         (FORMAT-PAF        #x050000)
         (FORMAT-SVX        #x060000)
         (FORMAT-NIST       #x070000)
         (FORMAT-VOC        #x080000)
         (FORMAT-IRCAM      #x0A0000)
         (FORMAT-W64        #x0B0000)
         (FORMAT-MAT4       #x0C0000)
         (FORMAT-MAT5       #x0D0000)
         (FORMAT-PVF        #x0E0000)
         (FORMAT-XI         #x0F0000)
         (FORMAT-HTK        #x100000)
         (FORMAT-SDS        #x110000)
         (FORMAT-AVR        #x120000)
         (FORMAT-WAVEX      #x130000)
         (FORMAT-SD2        #x160000)
         (FORMAT-FLAC       #x170000)
         (FORMAT-CAF        #x180000)
         (FORMAT-WVE        #x190000)
         (FORMAT-OGG        #x200000)
         (FORMAT-MPC2K      #x210000)
         (FORMAT-RF64       #x220000)
         (FORMAT-MPEG       #x230000)
         ;; Subtypes from here on.
         (FORMAT-PCM-S8     #x0001)
         (FORMAT-PCM-16     #x0002)
         (FORMAT-PCM-24     #x0003)
         (FORMAT-PCM-32     #x0004)
         (FORMAT-PCM-U8     #x0005)
         (FORMAT-FLOAT      #x0006)
         (FORMAT-DOUBLE     #x0007)
         (FORMAT-ULAW       #x0010)
         (FORMAT-ALAW       #x0011)
         (FORMAT-IMA-ADPCM  #x0012)
         (FORMAT-MS-ADPCM   #x0013)
         (FORMAT-GSM610     #x0020)
         (FORMAT-VOX-ADPCM  #x0021)
         (FORMAT-NMS-ADPCM-16 #x0022)
         (FORMAT-NMS-ADPCM-24 #x0023)
         (FORMAT-NMS-ADPCM-32 #x0024)
         (FORMAT-G721-32    #x0030)
         (FORMAT-G723-24    #x0031)
         (FORMAT-G723-40    #x0032)
         (FORMAT-DWVW-12    #x0040)
         (FORMAT-DWVW-16    #x0041)
         (FORMAT-DWVW-24    #x0042)
         (FORMAT-DWVW-N     #x0043)
         (FORMAT-DPCM-8     #x0050)
         (FORMAT-DPCM-16    #x0051)
         (FORMAT-VORBIS     #x0060)
         (FORMAT-OPUS       #x0064)
         (FORMAT-ALAC-16    #x0070)
         (FORMAT-ALAC-20    #x0071)
         (FORMAT-ALAC-24    #x0072)
         (FORMAT-ALAC-32    #x0073)
         (FORMAT-MPEG-LAYER-I   #x0080)
         (FORMAT-MPEG-LAYER-II  #x0081)
         (FORMAT-MPEG-LAYER-III #x0082)
         ;; Endian-ness options.
         (ENDIAN-FILE       #x00000000)
         (ENDIAN-LITTLE     #x10000000)
         (ENDIAN-BIG        #x20000000)
         (ENDIAN-CPU        #x30000000)
         (FORMAT-SUBMASK    #x0000FFFF)
         (FORMAT-TYPEMASK   #x0FFF0000)
         (FORMAT-ENDMASK    #x30000000)))
    (define-constants
        (;; String types that can be set and read from files.
         (STR-TITLE         #x01)
         (STR-COPYRIGHT     #x02)
         (STR-SOFTWARE      #x03)
         (STR-ARTIST        #x04)
         (STR-COMMENT       #x05)
         (STR-DATE          #x06)
         (STR-ALBUM         #x07)
         (STR-LICENSE       #x08)
         (STR-TRACKNUMBER   #x09)
         (STR-GENRE         #x10)
         ;; True and false.
         (FALSE 0)
         (TRUE  1)
         ;; Modes for opening files.
         (SFM-READ            #x10)
         (SFM-WRITE           #x20)
         (SFM-RDWR            #x30)
         (AMBISONIC-NONE      #x40)
         (AMBISONIC-B-FORMAT  #x41)
         ;; Seek offset.
         (SEEK-SET 0)
         (SEEK-CUR 1)
         (SEEK-END 2)
         ;; Error number.
         (ERR-NO-ERROR              0)
         (ERR-UNRECOGNISED-FORMAT   1)
         (ERR-SYSTEM                2)
         (ERR-MALFORMED-FILE        3)
         (ERR-UNSUPPORTED-ENCODING  4)
         ;; Channel map values.
         (CHANNEL-MAP-INVALID                 0)
         (CHANNEL-MAP-MONO                    1)
         (CHANNEL-MAP-LEFT                    2)
         (CHANNEL-MAP-RIGHT                   3)
         (CHANNEL-MAP-CENTER                  4)
         (CHANNEL-MAP-FRONT-LEFT              5)
         (CHANNEL-MAP-FRONT-RIGHT             6)
         (CHANNEL-MAP-FRONT-CENTER            7)
         (CHANNEL-MAP-REAR-CENTER             8)
         (CHANNEL-MAP-REAR-LEFT               9)
         (CHANNEL-MAP-REAR-RIGHT             10)
         (CHANNEL-MAP-LFE                    11)
         (CHANNEL-MAP-FRONT-LEFT-OF-CENTER   12)
         (CHANNEL-MAP-FRONT-RIGHT-OF-CENTER  13)
         (CHANNEL-MAP-SIDE-LEFT              14)
         (CHANNEL-MAP-SIDE-RIGHT             15)
         (CHANNEL-MAP-TOP-CENTER             16)
         (CHANNEL-MAP-TOP-FRONT-LEFT         17)
         (CHANNEL-MAP-TOP-FRONT-RIGHT        18)
         (CHANNEL-MAP-TOP-FRONT-CENTER       19)
         (CHANNEL-MAP-TOP-REAR-LEFT          20)
         (CHANNEL-MAP-TOP-REAR-RIGHT         21)
         (CHANNEL-MAP-TOP-REAR-CENTER        22)
         (CHANNEL-MAP-AMBISONIC-B-W          23)
         (CHANNEL-MAP-AMBISONIC-B-X          24)
         (CHANNEL-MAP-AMBISONIC-B-Y          25)
         (CHANNEL-MAP-AMBISONIC-B-Z          26)
         (CHANNEL-MAP-MAX                    27)
         ;; Bitrate mode values.
         (BITRATE-MODE-CONSTANT      0)
         (BITRATE-MODE-AVERAGE       1)
         (BITRATE-MODE-VARIABLE      2)
         ;; Dither.
         (SFD-DEFAULT-LEVEL          0)
         (SFD-CUSTOM-LEVEL  #x40000000)
         (SFD-NO-DITHER            500)
         (SFD-WHITE                501)
         (SFD-TRIANGULAR-PDF       502)
         ;; Loop mode field.
         (LOOP-NONE        800)
         (LOOP-FORWARD     801)
         (LOOP-BACKWARD    802)
         (LOOP-ALTERNATING 803)
         ;; Command numbers.
         (SFC-GET-LIB-VERSION            #x1000)
         (SFC-GET-LOG-INFO               #x1001)
         (SFC-GET-CURRENT-SF-INFO        #x1002)
         (SFC-GET-NORM-DOUBLE            #x1010)
         (SFC-GET-NORM-FLOAT             #x1011)
         (SFC-SET-NORM-DOUBLE            #x1012)
         (SFC-SET-NORM-FLOAT             #x1013)
         (SFC-SET-SCALE-FLOAT-INT-READ   #x1014)
         (SFC-SET-SCALE-INT-FLOAT-WRITE  #x1015)
         (SFC-GET-SIMPLE-FORMAT-COUNT    #x1020)
         (SFC-GET-SIMPLE-FORMAT          #x1021)
         (SFC-GET-FORMAT-INFO            #x1028)
         (SFC-GET-FORMAT-MAJOR-COUNT     #x1030)
         (SFC-GET-FORMAT-MAJOR           #x1031)
         (SFC-GET-FORMAT-SUBTYPE-COUNT   #x1032)
         (SFC-GET-FORMAT-SUBTYPE         #x1033)
         (SFC-CALC-SIGNAL-MAX            #x1040)
         (SFC-CALC-NORM-SIGNAL-MAX       #x1041)
         (SFC-CALC-MAX-ALL-CHANNELS      #x1042)
         (SFC-CALC-NORM-MAX-ALL-CHANNELS #x1043)
         (SFC-GET-SIGNAL-MAX             #x1044)
         (SFC-GET-MAX-ALL-CHANNELS       #x1045)
         (SFC-SET-ADD-PEAK-CHUNK         #x1050)
         (SFC-SET-ADD-HEADER-PAD-CHUNK   #x1051)
         (SFC-UPDATE-HEADER-NOW          #x1060)
         (SFC-SET-UPDATE-HEADER-AUTO     #x1061)
         (SFC-FILE-TRUNCATE              #x1080)
         (SFC-SET-RAW-START-OFFSET       #x1090)
         (SFC-SET-DITHER-ON-WRITE        #x10A0)
         (SFC-SET-DITHER-ON-READ         #x10A1)
         (SFC-GET-DITHER-INFO-COUNT      #x10A2)
         (SFC-GET-DITHER-INFO            #x10A3)
         (SFC-GET-EMBED-FILE-INFO        #x10B0)
         (SFC-SET-CLIPPING               #x10C0)
         (SFC-GET-CLIPPING               #x10C1)
         (SFC-GET-CUE-COUNT              #x10CD)
         (SFC-GET-CUE                    #x10CE)
         (SFC-SET-CUE                    #x10CF)
         (SFC-GET-INSTRUMENT             #x10D0)
         (SFC-SET-INSTRUMENT             #x10D1)
         (SFC-GET-LOOP-INFO              #x10E0)
         (SFC-GET-BROADCAST-INFO         #x10F0)
         (SFC-SET-BROADCAST-INFO         #x10F1)
         (SFC-GET-CHANNEL-MAP-INFO       #x1100)
         (SFC-SET-CHANNEL-MAP-INFO       #x1101)
         (SFC-RAW-DATA-NEEDS-ENDSWAP     #x1110)
         (SFC-WAVEX-SET-AMBISONIC        #x1200)
         (SFC-WAVEX-GET-AMBISONIC        #x1201)
         (SFC-RF64-AUTO-DOWNGRADE        #x1210)
         (SFC-SET-VBR-ENCODING-QUALITY   #x1300)
         (SFC-SET-COMPRESSION-LEVEL      #x1301)
         (SFC-SET-OGG-PAGE-LATENCY-MS    #x1302)
         (SFC-SET-OGG-PAGE-LATENCY       #x1303)
         (SFC-GET-BITRATE-MODE           #x1304)
         (SFC-SET-BITRATE-MODE           #x1305)
         (SFC-SET-CART-INFO              #x1400)
         (SFC-GET-CART-INFO              #x1401)
         (SFC-SET-ORIGINAL-SAMPLERATE    #x1500)
         (SFC-GET-ORIGINAL-SAMPLERATE    #x1501)
         (SFC-TEST-IEEE-FLOAT-REPLACE    #x6001)
         (SFC-SET-ADD-DITHER-ON-WRITE    #x1070)
         (SFC-SET-ADD-DITHER-ON-READ     #x1071)

         (COUNT-MAX #x7FFFFFFFFFFFFFFF)))))

(defun check-format (fmt min max)
  (let ((value (gethash fmt *formats*)))
    (and value (<= min value max))))

(defun major-format-p (fmt)
  (check-format fmt FORMAT-WAV FORMAT-MPEG))

(defun minor-format-p (fmt)
  (check-format fmt FORMAT-PCM-S8 FORMAT-MPEG-LAYER-III))

(defun get-format (fmt-list)
  (flet ((value (x)
           (or (gethash x *formats*) 0)))
    (destructuring-bind (major &optional (sample 0) (endian "file")) fmt-list
      (logior (value major) (value sample) (value endian)))))

(defun decode-format (format)
  (let ((major (logand format SF:FORMAT-TYPEMASK))
        (subtype (logand format SF:FORMAT-SUBMASK))
        (endian (svref #("file" "little" "big" "cpu")
                       (logand format SF:FORMAT-ENDMASK)))
        (header-type "unknown")
        (data-format "unknown")
        (found 0))
    (maphash (lambda (k v)
               (cond ((= v major) (setf header-type k) (incf found))
                     ((= v subtype) (setf data-format k) (incf found)))
               (when (> found 1)
                 (return-from decode-format
                   (values header-type data-format endian))))
             sf::*formats*)))

(cffi:defctype sf-count :int64)
;; Alias.
(cffi:defctype count :int64)

(cffi:defcstruct format-info
  (format :int)
  (name :string)
  (extension :string))

(cffi:defcstruct dither-info
  (type :int)
  (level :double)
  (name :string))

(cffi:defcstruct embed-file-info
  (offset sf-count)
  (length sf-count))

(cffi:defcstruct instr-loop
  (mode :int)
  (start :unsigned-int)
  (end :unsigned-int)
  (count :unsigned-int))

(cffi:defcstruct instrument
  (gain :int)
  (basenote :char)
  (detune :char)
  (velocity-lo :char)
  (velocity-hi :char)
  (key-lo :char)
  (key-hi :char)
  (loop-count :int)
  (loops :int :count 64))

(cffi:defcstruct loop-info
  (time-sig-num :short)
  (time-sig-den :short)
  (loop-mode :int)
  (num-beats :int)
  (bpm :float)
  (root-key :int)
  (future :int :count 6))

(cffi:defcstruct broadcast-info
  (description :char :count 256)
  (originator :char :count  32)
  (originator-reference :char :count  32)
  (origination-date :char :count  10)
  (origination-time :char :count   8)
  (time-reference-low :unsigned-int)
  (time-reference-high :unsigned-int)
  (version :short)
  (umid :char :count  64)
  (reserved :char :count 190)
  (coding-history-size :unsigned-int)
  (coding-history :char :count 256))

(cffi:defcstruct virtual-io
  (get-filelen :pointer)
  (seek :pointer)
  (read :pointer)
  (write :pointer)
  (tell :pointer))

(cffi:defcfun ("sf_open" %open) sndfile
  (path :string)
  (mode :int)
  (sfinfo sndinfo))

(cffi:defcfun ("sf_open_fd" open-fd) sndfile
  (fd :int)
  (mode :int)
  (sfinfo sndinfo)
  (close-desc :int))

(cffi:defcfun ("sf_open_virtual" open-virtual) sndfile
  (sfvirtual :pointer)
  (mode :int)
  (sfinfo sndinfo)
  (user-data :pointer))

(cffi:defcfun ("sf_error" error) :int
  (sndfile sndfile))

(cffi:defcfun ("sf_strerror" strerror) :string
  (sndfile sndfile))

(cffi:defcfun ("sf_error_number" error-number) :string
  (errnum :int))

(declaim (inline command))
(cffi:defcfun ("sf_command" command) :int
  (sndfile sndfile)
  (command :int)
  (data :pointer)
  (datasize :int))

(cffi:defcfun ("sf_format_check" format-check) :boolean
  (info sndinfo))

(cffi:defcfun ("sf_seek" seek) sf-count
  (sndfile sndfile)
  (frames sf-count)
  (whence :int))

(cffi:defcfun ("sf_set_string" set-string) :int
  (sndfile sndfile)
  (str-type :int)
  (str :string))

(cffi:defcfun ("sf_get_string" get-string) :string
  (sndfile sndfile)
  (str-type :int))

(cffi:defcfun ("sf_version_string" version-string) :string)

(cffi:defcfun ("sf_read_raw" read-raw) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (bytes sf-count))

(cffi:defcfun ("sf_write_raw" write-raw) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (bytes sf-count))

(cffi:defcfun ("sf_readf_short" readf-short) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_writef_short" writef-short) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_readf_int" readf-int) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_writef_int" writef-int) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_readf_float" readf-float) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_writef_float" writef-float) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_readf_double" readf-double) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_writef_double" writef-double) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (frames sf-count))

(cffi:defcfun ("sf_read_short" read-short) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_write_short" write-short) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_read_int" read-int) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_write_int" write-int) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_read_float" read-float) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_write_float" write-float) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_read_double" read-double) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_write_double" write-double) sf-count
  (sndfile sndfile)
  (ptr :pointer)
  (items sf-count))

(cffi:defcfun ("sf_close" %close) :int
  (sndfile :pointer))

(cffi:defcfun ("sf_write_sync" write-sync) :void
  (sndfile sndfile))
