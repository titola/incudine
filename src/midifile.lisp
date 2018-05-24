;;; Copyright (c) 2016-2018 Tito Latini
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

(in-package :midifile)

(defvar *write-buffer-expand-size* 4000)
(declaim (type non-negative-fixnum *write-buffer-expand-size*))

(deftype data ()
  "Type designator for a vector of octets."
  '(simple-array (unsigned-byte 8) (*)))

(declaim (inline data))
(defun data (&rest octets)
  "Return a vector of OCTETS."
  (coerce octets 'data))

(define-condition midifile-error (incudine-simple-error) ()
  (:documentation "All types of error conditions about MIDI files
inherit from this condition.

Subtype of INCUDINE-SIMPLE-ERROR."))

(define-condition midifile-parse-error (midifile-error)
  ((string :initarg :string :reader midifile-error-string)
   (position :initarg :position :reader midifile-error-position)
   (track :initarg :track :reader midifile-error-track))
  (:report (lambda (condition stream)
             (cl:format stream "~A at position ~D (track ~D)."
                        (midifile-error-string condition)
                        (midifile-error-position condition)
                        (midifile-error-track condition))))
  (:documentation "Signaled if there is a MIDI file parsing error."))

(define-condition invalid-running-status (midifile-parse-error)
  ((byte :initarg :status-byte :reader invalid-running-status-byte))
  (:report (lambda (condition stream)
             (cl:format stream "Running status with previous status byte ~X ~
                                at position ~D (track ~D)."
                        (invalid-running-status-byte condition)
                        (midifile-error-position condition)
                        (midifile-error-track condition))))
  (:documentation "Signaled if the MIDI file parser encounters an
invalid running status.

Subtype of MIDIFILE-PARSE-ERROR."))

(define-condition invalid-track-chunk-length (midifile-parse-error) ()
  (:documentation "Signaled if the MIDI file parser encounters a
header-chunk with invalid length of track data.

Subtype of MIDIFILE-PARSE-ERROR."))

(define-condition invalid-variable-length-quantity (midifile-parse-error) ()
  (:documentation "Signaled if the MIDI file parser encounters an
invalid variable-length-quantity.

Subtype of MIDIFILE-PARSE-ERROR."))

(defmacro %midifile-error (format-control &rest format-arguments)
  `(incudine::%simple-error 'midifile-error ,format-control ,@format-arguments))

(declaim (inline read-two-bytes))
(defun read-two-bytes (stream)
  (logior (ash (read-byte stream) 8) (read-byte stream)))

(declaim (inline read-four-bytes))
(defun read-four-bytes (stream)
  (logior (ash (read-two-bytes stream) 16) (read-two-bytes stream)))

(declaim (inline write-two-bytes))
(defun write-two-bytes (integer stream)
  (write-byte (logand (ash integer -8) #xff) stream)
  (write-byte (logand integer #xff) stream)
  integer)

(declaim (inline write-four-bytes))
(defun write-four-bytes (integer stream)
  (write-two-bytes (ash integer -16) stream)
  (write-two-bytes integer stream)
  integer)

(declaim (inline skip-bytes))
(defun skip-bytes (n stream)
  (dotimes (i n) (read-byte stream)))

(declaim (inline chunk-type-and-length))
(defun chunk-type-and-length (stream)
  (values (read-four-bytes stream) (read-four-bytes stream)))

(defmacro check-chunk-type (value string)
  `(= ,value ,(reduce #'+ (map 'list (lambda (x y) (ash (char-code x) y))
                               string '(24 16 8 0)))))

(declaim (inline get-timecode))
(defun get-timecode (x)
  (let ((msb (logand (ash x -8) #xff)))
    (assert (member msb '(#xe2 #xe3 #xe7 #xe8) :test #'=))
    (values (- (logorc1 #xff msb)) ; 24, 25, 29 or 30 fps
            (logand x #xff))))     ; sub-frame resolution

(declaim (inline variable-length-quantity-bytes))
(defun variable-length-quantity-bytes (value)
  (max 1 (ceiling (integer-length value) 7)))

;;; Pool of adjustable buffers for MIDIFILE:STREAM.
(defvar *buffer-pool* nil)
(declaim (type list *buffer-pool*))

(defvar *buffer-pool-lock* (bt:make-lock "MIDIFILE-BUFFER-POOL"))

(defun make-buffer (size)
  (declare (type positive-fixnum size))
  (flet ((find-optimal-buffer (size)
           (multiple-value-bind (buffer pool)
               (cond ((rest *buffer-pool*)
                      (setf *buffer-pool*
                            (sort *buffer-pool* #'< :key #'length))
                      (let ((res (find-if (lambda (x) (>= (length x) size))
                                          *buffer-pool*)))
                        (if res
                            (values res (delete res *buffer-pool*))
                            ;; Reuse the buffer with largest size.
                            (values (first (last *buffer-pool*))
                                    (butlast *buffer-pool*)))))
                     (t
                      (first *buffer-pool*)))
             (setf *buffer-pool* pool)
             buffer)))
    (bt:with-lock-held (*buffer-pool-lock*)
      (if *buffer-pool*
          (let ((buf (find-optimal-buffer size)))
            (when (> size (length buf))
              (adjust-array buf size))
            buf)
          (make-array size :element-type '(unsigned-byte 8) :adjustable t
                      :initial-element 0)))))

(defun add-to-buffer-pool (buf)
  (declare (type (vector (unsigned-byte 8)) buf))
  (bt:with-lock-held (*buffer-pool-lock*)
    (setf *buffer-pool* (cons buf *buffer-pool*))))

(defun release-cached-buffers ()
  "Release the cached adjustable buffers for MIDIFILE:STREAM."
  (bt:with-lock-held (*buffer-pool-lock*)
    (setf *buffer-pool* nil)))

(defstruct tempo
  "MIDI file tempo type."
  (time 0 :type non-negative-fixnum)
  (seconds 0.0d0 :type double-float)
  (microseconds-per-quarter-note 500000 :type non-negative-fixnum))

(defstruct stream
  "MIDI file stream type."
  (fd-stream (incudine-missing-arg "Missing stream.") :type cl:stream)
  (open-p nil :type boolean)
  (pathname (incudine-missing-arg "Missing pathname.") :type pathname)
  (direction :input :type (member :input :output))
  (format 0 :type non-negative-fixnum)
  (number-of-tracks 0 :type non-negative-fixnum)
  ;; Current track is zero based.
  (current-track 0 :type non-negative-fixnum)
  (track-time 0 :type non-negative-fixnum)
  (ppqn 0 :type non-negative-fixnum)
  (smpte-format 0 :type non-negative-fixnum)
  (ticks-per-frame 0 :type non-negative-fixnum)
  (buffer (make-buffer 8) :type (vector (unsigned-byte 8))))

(defstruct (input-stream (:include stream))
  "MIDI file input stream type."
  (position 0 :type non-negative-fixnum)
  (track-bytes-remain 0 :type non-negative-fixnum)
  (last-message-length 0 :type non-negative-fixnum)
  (last-delta-time 0 :type non-negative-fixnum)
  (tempo-map (make-array 1 :adjustable t :fill-pointer 1
                         :initial-element (make-tempo))
             :type vector)
  (tempo-index 0 :type non-negative-fixnum)
  (next-tempo-time 0 :type non-negative-fixnum))

(defstruct (output-stream (:include stream))
  "MIDI file output stream type."
  (buffer-index 0 :type non-negative-fixnum)
  (last-status 0 :type (unsigned-byte 8)))

(setf
  (documentation 'input-stream-p 'function)
  "Return T if object is of type MIDIFILE:INPUT-STREAM."
  (documentation 'output-stream-p 'function)
  "Return T if object is of type MIDIFILE:OUTPUT-STREAM.")

(defmethod print-object ((obj stream) stream)
  (multiple-value-bind (k v)
      (if (plusp (stream-ppqn obj))
          (values :ppqn (stream-ppqn obj))
          (values :fps (stream-smpte-format obj)))
    (cl:format stream "#<MIDIFILE:~A-STREAM :NUMBER-OF-TRACKS ~D ~S ~D>"
               (stream-direction obj) (stream-number-of-tracks obj) k v)))

(declaim (inline open-p))
(defun open-p (mf)
  "Whether MF is an open MIDIFILE:STREAM."
  (stream-open-p mf))

(declaim (inline path))
(defun path (mf)
  "Return the pathname of the MIDIFILE:STREAM MF."
  (stream-pathname mf))

(declaim (inline current-track))
(defun current-track (mf)
  "Return the current track of the MIDIFILE:STREAM MF."
  (stream-current-track mf))

;;; Utilities to get the MIDI data after MIDIFILE:READ-EVENT

(declaim (inline event-delta-time))
(defun event-delta-time (mf)
  "Return the delta time in pulses of the last event read from the
MIDIFILE:INPUT-STREAM MF."
  (input-stream-last-delta-time mf))

(declaim (inline event-time))
(defun event-time (mf)
  "Return the time in pulses of the last event read from a
MIDIFILE:INPUT-STREAM."
  (stream-track-time mf))

(declaim (inline message-length))
(defun message-length (mf)
  "Return the length of the MIDI message read from the
MIDIFILE:INPUT-STREAM MF."
  (input-stream-last-message-length mf))

(declaim (inline message-buffer))
(defun message-buffer (mf)
  "Return the buffer that contains the MIDI message read from the
MIDIFILE:INPUT-STREAM MF and the length of that message in bytes."
  (values (stream-buffer mf) (input-stream-last-message-length mf)))

(declaim (inline message-status))
(defun message-status (mf)
  "Return the status byte of the MIDI message read from the
MIDIFILE:INPUT-STREAM MF."
  (aref (input-stream-buffer mf) 0))

(declaim (inline message-data1))
(defun message-data1 (mf)
  "Return the first data byte of the MIDI message read from the
MIDIFILE:INPUT-STREAM MF."
  (aref (input-stream-buffer mf) 1))

(declaim (inline message-data2))
(defun message-data2 (mf)
  "Return the second data byte of the MIDI message read from the
MIDIFILE:INPUT-STREAM MF."
  (aref (input-stream-buffer mf) 2))

;;; Utilities to simplify the creation of a MIDI message.

;; Copy of JACMIDI:MESSAGE.
(declaim (inline message))
(defun message (status data1 data2)
  "Encode a short MIDI message into four bytes."
  (declare (type (unsigned-byte 8) status data1 data2))
  (the (unsigned-byte 32)
    #+little-endian
    (logior (ash data2 16) (ash data1 8) status)
    #-little-endian
    (logior (ash status 24) (ash data1 16) (ash data2 8))))

(declaim (inline tempo-message))
(defun tempo-message (bpm)
  "Return the octets of a MIDI tempo change of BPM beats per minute."
  (declare (type (real 4 60000000) bpm))
  (let ((mpqn (round (/ 6e7 bpm))))
    (declare (type (unsigned-byte 24) mpqn))
    (data #xFF #x51 #x03
          (ldb (byte 8 16) mpqn)
          (ldb (byte 8 8) mpqn)
          (ldb (byte 8 0) mpqn))))

(defun string-message (meta-event-type string)
  "Return the octets of a text-based Meta event of type META-EVENT-TYPE."
  (declare (type (unsigned-byte 8) meta-event-type) (type simple-string string))
  (let* ((str-octets (incudine.util::string-to-octets string))
         (slen (length str-octets))
         (len-bytes (variable-length-quantity-bytes slen)))
    (declare (type data str-octets) (type non-negative-fixnum slen len-bytes))
    (assert (<= 1 len-bytes 4))
    (let* ((len (+ 2 len-bytes slen))
           (octets (make-array len :element-type '(unsigned-byte 8))))
      (declare (type non-negative-fixnum len) (type data octets))
      (setf (aref octets 0) #xff)
      (setf (aref octets 1) meta-event-type)
      (do ((i 2 (1+ i))
           (pos (* (1- len-bytes) 7) (- pos 7)))
          ((= pos 0)
           (setf (aref octets i) (logand #x7f slen))
           (do ((j (1+ i) (1+ j))
                (c 0 (1+ c)))
               ((>= j len) octets)
             (setf (aref octets j) (aref str-octets c))))
        (setf (aref octets i) (logior #x80 (ldb (byte 7 pos) slen)))))))

(defgeneric read-header (obj)
  (:documentation "Read the header of a MIDI file and return four values:
format, number of tracks, ppqn-or-smpte-format and ticks-per-frame."))

;;; MIDI file header: "MThd" 0x00000006 <format> <ntracks> <division>
(defmethod read-header ((obj cl:stream))
  (multiple-value-bind (type size) (chunk-type-and-length obj)
    (assert (and (check-chunk-type type "MThd") (>= size 6)))
    (let ((format (read-two-bytes obj)))
      (assert (<= 0 format 2))
      (let ((ntracks (read-two-bytes obj))
            (division (read-two-bytes obj)))
        (when (= format 0)
          (assert (= ntracks 1)))
        (multiple-value-bind (ppqn-or-smpte-format ticks-per-frame)
            (if (logbitp 15 division)
                ;; Time-code-based time.
                (get-timecode division)
                ;; Metrical time.
                division)
          (when (> size 6)
            (skip-bytes (- size 6) obj)
            (warn "the last ~D bytes of the MIDI file header chunk are ignored."
                  (- size 6)))
          (values format ntracks ppqn-or-smpte-format ticks-per-frame
                  ;; Used to update MIDIFILE:INPUT-STREAM-POSITION
                  (+ 8 size)))))))

(defmethod read-header ((obj midifile:stream))
  (values (stream-format obj)
          (stream-number-of-tracks obj)
          (if (plusp (stream-ppqn obj))
              (stream-ppqn obj)
              (stream-smpte-format obj))
          (stream-ticks-per-frame obj)))

(defmethod read-header ((obj pathname))
  (with-open-file (f obj :element-type '(unsigned-byte 8))
    (read-header f)))

(defmethod read-header ((obj string))
  (read-header (truename obj)))

(defgeneric format (obj)
  (:documentation "Return the format of a MIDI file."))

(defmethod format ((obj pathname))
  (values (read-header obj)))

(defmethod format ((obj string))
  (format (truename obj)))

(defmethod format ((obj stream))
  (stream-format obj))

(defgeneric number-of-tracks (obj)
  (:documentation "Return the number of tracks of a MIDI file."))

(defmethod number-of-tracks ((obj pathname))
  (multiple-value-bind (fmt ntracks) (read-header obj)
    (declare (ignore fmt))
    ntracks))

(defmethod number-of-tracks ((obj string))
  (number-of-tracks (truename obj)))

(defmethod number-of-tracks ((obj stream))
  (stream-number-of-tracks obj))

(defgeneric ppqn (obj)
  (:documentation "Return the pulses per quarter note of a MIDI file."))

(defmethod ppqn ((obj pathname))
  (multiple-value-bind (fmt ntracks ppqn) (read-header obj)
    (declare (ignore fmt ntracks))
    ppqn))

(defmethod ppqn ((obj string))
  (ppqn (truename obj)))

(defmethod ppqn ((obj stream))
  (stream-ppqn obj))

(defgeneric smpte (obj)
  (:documentation
    "Return two values: smpte-format and ticks-per-frame of a MIDI file."))

(defmethod smpte ((obj pathname))
  (multiple-value-bind (fmt ntracks smpte-format ticks-per-frame)
      (read-header obj)
    (declare (ignore fmt ntracks))
    (if ticks-per-frame
        (values smpte-format ticks-per-frame)
        (values 0 0))))

(defmethod smpte ((obj string))
  (smpte (truename obj)))

(defmethod smpte ((obj stream))
  (values (stream-smpte-format obj) (stream-ticks-per-frame obj)))

(declaim (inline meta-event-p))
(defun meta-event-p (mf)
  (= (aref (input-stream-buffer mf) 0) #xff))

(declaim (inline tempo-event-p))
(defun tempo-event-p (mf)
  (and (meta-event-p mf) (= (aref (input-stream-buffer mf) 1) #x51)))

(defun end-of-track-p (mf)
  (declare (type midifile:stream mf))
  (if (output-stream-p mf)
      (let ((i (output-stream-buffer-index mf)))
        (and (= (output-stream-last-status mf) #xff)
             (> i 2)
             (= (aref (stream-buffer mf) (- i 2)) #x2f)))
      (and (meta-event-p mf) (= (aref (input-stream-buffer mf) 1) #x2f))))

(defgeneric write-header (obj &key)
  (:documentation "Write the header-chunk of a MIDI file.

Default values for defined keywords:
    :FORMAT 0
    :NUMBER-OF-TRACKS 1
    :PPQN-OR-SMPTE-FORMAT 480
    :TICKS-PER-FRAME 0"))

(defmethod write-header ((obj cl:stream) &key (format 0) (number-of-tracks 1)
                         (ppqn-or-smpte-format 480) (ticks-per-frame 0))
  (declare (type (unsigned-byte 16) format number-of-tracks ppqn-or-smpte-format)
           (type (unsigned-byte 8) ticks-per-frame))
  (multiple-value-bind (div1 div0)
      (if (plusp ticks-per-frame)
          (values (- (logorc1 #xff ppqn-or-smpte-format)) ticks-per-frame)
          (values (ash ppqn-or-smpte-format -8)
                  (logand ppqn-or-smpte-format #xff)))
    (length (write-sequence
              (data #x4d #x54 #x68 #x64            ; "MThd"
                    0    0    0    6               ; header-chunk-length
                    0 (logand format #xff)         ; format
                    (ash number-of-tracks -8)      ; ntracks
                    (logand number-of-tracks #xff)
                    div1 div0)                     ; division
              obj))))

(defmethod write-header ((obj midifile:output-stream) &key)
  (write-header (stream-fd-stream obj)
                :format (stream-format obj)
                :number-of-tracks (stream-number-of-tracks obj)
                :ppqn-or-smpte-format (if (plusp (stream-ppqn obj))
                                          (stream-ppqn obj)
                                          (stream-smpte-format obj))
                :ticks-per-frame (stream-ticks-per-frame obj)))

(defun end-of-track (mf &optional (beats 0))
  "Write a MIDI End Of Track Meta event with absolute time BEATS.
If BEATS is 0 (default), write the event with delta time zero.

End Of Track Meta event is automatically added if necessary before to
write a track by calling MIDIFILE:NEXT-TRACK or MIDIFILE:CLOSE.

Multiple End Of Track Meta events are ignored."
  (declare (type midifile:output-stream mf) (type non-negative-real beats))
  (if (end-of-track-p mf)
      (output-stream-buffer-index mf)
      (write-short-event mf beats (message #xff #x2f 0) 3)))

(defun write-track (mf)
  (declare (type midifile:output-stream mf))
  (let ((len (end-of-track mf))
        (buf (stream-buffer mf))
        (s (stream-fd-stream mf)))
    (declare (type (unsigned-byte 32) len))
    (assert (plusp len))
    ;; "MTrk"
    (write-four-bytes #x4d54726b s)
    ;; track-chunk-length
    (write-four-bytes len s)
    ;; track-data
    (prog1 (write-sequence buf s :end (output-stream-buffer-index mf))
      (setf (stream-track-time mf) 0)
      (setf (output-stream-buffer-index mf) 0))))

(defgeneric next-track (obj)
  (:documentation "Return the number (zero based) of the next track
or NIL if OBJ is of type MIDIFILE:INPUT-STREAM and there aren't other
tracks to read.
Write the current track if OBJ is of type MIDIFILE:OUTPUT-STREAM."))

(defun %next-track (mf &optional (offset 0))
  (declare (type midifile:input-stream) (type non-negative-fixnum offset))
  (let ((stream (stream-fd-stream mf)))
    (when (plusp offset)
      (skip-bytes offset stream)
      (incf (input-stream-position mf) offset))
    (multiple-value-bind (type size) (chunk-type-and-length stream)
      (incf (input-stream-position mf) 8)
      (if (check-chunk-type type "MTrk")
          size
          (%next-track mf size)))))

(declaim (inline update-next-tempo-time))
(defun update-next-tempo-time (mf next-index)
  (setf (input-stream-next-tempo-time mf)
        (if (> (length (input-stream-tempo-map mf)) next-index)
            (tempo-time (aref (input-stream-tempo-map mf) next-index))
            0)))

(defmethod next-track ((mf midifile:input-stream))
  (when (and (plusp (stream-format mf))
             (< (stream-current-track mf)
                (1- (stream-number-of-tracks mf))))
    (let ((size (%next-track mf (input-stream-track-bytes-remain mf))))
      (declare (type non-negative-fixnum size))
      (when (plusp size)
        (setf (input-stream-track-bytes-remain mf) size)
        (setf (aref (stream-buffer mf) 0) #xff)
        (setf (stream-track-time mf) 0)
        (setf (input-stream-tempo-index mf) 0)
        (update-next-tempo-time mf 1)
        (incf (stream-current-track mf))))))

(defmethod next-track ((mf midifile:output-stream))
  (cond ((stream-open-p mf)
         (write-track mf)
         (incf (stream-number-of-tracks mf))
         (incf (stream-current-track mf)))
        (t 0)))

(defun open (filename &key (direction :input) (if-exists :error)
             format (ppqn 480) (buffer-size *write-buffer-expand-size*))
  "Create and return a new MIDIFILE:STREAM.

DIRECTION is :INPUT (default) or :OUTPUT to return a MIDIFILE:INPUT-STREAM
or a MIDIFILE:OUTPUT-STREAM respectively.

IF-EXISTS should be one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
:OVERWRITE, :APPEND, :SUPERSEDE or NIL. The default is :ERROR.

If DIRECTION is :OUTPUT, the MIDI file FORMAT defaults to 0 and PPQN (Pulses
Per Quarter Note) defaults to 480."
  (declare (type (or pathname string) filename)
           (type (member :input :output) direction)
           (type (or (integer 0 2) null) format)
           (type non-negative-fixnum ppqn))
  (let* ((input-p (eq direction :input))
         (pathname (if input-p
                       (truename filename)
                       (make-pathname :defaults filename)))
         (mf nil)
         (buffer (make-buffer (if input-p 8 (max 16 buffer-size))))
         (stream (cl:open pathname :element-type '(unsigned-byte 8)
                          :direction direction :if-exists if-exists)))
    (handler-case
        (multiple-value-bind (format ntracks ppqn-or-smpte ticks-per-frame
                              header-size)
            (if input-p
                (read-header stream)
                (values (or format 0) 1 ppqn nil))
          (setf mf (funcall (if input-p #'make-input-stream #'make-output-stream)
                     :fd-stream stream
                     :open-p t
                     :pathname pathname
                     :direction direction
                     :format format
                     :number-of-tracks ntracks
                     :buffer buffer
                     :ppqn (if ticks-per-frame 0 ppqn-or-smpte)
                     :smpte-format (if ticks-per-frame ppqn-or-smpte 0)
                     :ticks-per-frame (or ticks-per-frame 0)))
          (cond (input-p
                 (setf (input-stream-position mf) header-size)
                 (setf (input-stream-track-bytes-remain mf) (%next-track mf)))
                (t
                 ;; If the number of tracks will change, the header will be
                 ;; updated during MIDIFILE:CLOSE.
                 (write-header mf)))
          (incudine-finalize mf
            (lambda () (cl:close stream) (add-to-buffer-pool buffer))
            nil)
          mf)
      (condition (c)
        (if mf
            (close mf)
            (progn (cl:close stream) (add-to-buffer-pool buffer)))
        (error c)))))

(defun close (mf)
  "Close a MIDIFILE:STREAM.
If MF is a MIDIFILE:OUTPUT-STREAM, write the current track."
  (let ((f (stream-fd-stream mf)))
    (when (open-stream-p f)
      (when (midifile:output-stream-p mf)
        ;; Write the current track.
        (write-track mf))
      (cl:close f)
      (incudine-cancel-finalization mf)
      (when (and (midifile:output-stream-p mf)
                 (> (stream-number-of-tracks mf) 1))
        ;; Update format and number of tracks in header-chunk.
        (when (zerop (stream-format mf))
          (setf (stream-format mf) 1))
        (with-open-file (f (stream-pathname mf) :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :overwrite)
          (setf (stream-fd-stream mf) f)
          (write-header mf)))
      (setf (stream-open-p mf) nil)
      (add-to-buffer-pool (stream-buffer mf)))
      mf))

(defmethod incudine:free-p ((obj midifile:stream))
  (not (open-stream-p (stream-fd-stream obj))))

(defmethod incudine:free ((obj midifile:stream))
  (midifile:close obj))

(defmacro with-open-midifile ((midifile-stream filespec &rest options) &body body)
  "Use MIDIFILE:OPEN to create a MIDIFILE:STREAM. When control leaves the body,
either normally or abnormally, the MIDIFILE:STREAM is automatically closed."
  `(let ((,midifile-stream (open ,filespec ,@options)))
     (unwind-protect (progn ,@body) (close ,midifile-stream))))

(declaim (inline update-byte-counters))
(defun update-byte-counters (mf bytes)
  (incf (input-stream-position mf) bytes)
  (decf (input-stream-track-bytes-remain mf) bytes))

(declaim (inline read-variable-length-quantity))
(defun read-variable-length-quantity (mf)
  (do* ((bytes 1 (1+ bytes))
        (b (read-byte (stream-fd-stream mf))
           (read-byte (stream-fd-stream mf)))
        (v (logand b #x7f) (+ (ash v 7) (logand b #x7f))))
       ((< b #x80) (values v bytes))
    (declare (type (unsigned-byte 8) bytes b) (type non-negative-fixnum v))
    (when (> bytes 4)
      (let ((pos (1+ (input-stream-position mf))))
        (unless (> bytes (input-stream-track-bytes-remain mf))
          ;; We can try to continue with the next track.
          (update-byte-counters mf bytes))
        (error 'invalid-variable-length-quantity
               :string "Variable length quantity in more than four bytes"
               :position pos :track (stream-current-track mf))))))

(defun read-delta-time (mf)
  (when (plusp (input-stream-track-bytes-remain mf))
    (multiple-value-bind (delta-time bytes)
        (read-variable-length-quantity mf)
      (if (< bytes (input-stream-track-bytes-remain mf))
          (update-byte-counters mf bytes)
          (error 'invalid-track-chunk-length
                 :string "Wrong track-chunk-length"
                 :position (1+ (input-stream-position mf))
                 :track (input-stream-current-track mf)))
      (when (plusp delta-time)
        (incf (stream-track-time mf) delta-time)
        (when (and (plusp (input-stream-next-tempo-time mf))
                   (> (stream-track-time mf)
                      (input-stream-next-tempo-time mf)))
          (update-next-tempo-time mf (1+ (incf (input-stream-tempo-index mf))))))
      (setf (input-stream-last-delta-time mf) delta-time))))

(declaim (inline read-status-byte))
(defun read-status-byte (mf)
  (let ((s (read-byte (input-stream-fd-stream mf))))
    (setf (aref (input-stream-buffer mf) (logxor (ash s -7) 1)) s)
    s))

(declaim (inline read-into-message-buffer))
(defun read-into-message-buffer (mf start end)
  (read-sequence (input-stream-buffer mf) (input-stream-fd-stream mf)
                 :start start :end end))

(defun continue-running-status (mf)
  (let ((status-byte (aref (stream-buffer mf) 0)))
    (cond ((< status-byte #xf0)
           (when (or (< status-byte #xc0) (> status-byte #xdf))
             (setf (aref (stream-buffer mf) 2)
                   (read-byte (input-stream-fd-stream mf))))
           (input-stream-last-message-length mf))
          (t
           (let ((pos (1+ (input-stream-position mf))))
             (unless (zerop (input-stream-track-bytes-remain mf))
               (update-byte-counters mf 1))
             (error 'invalid-running-status
                    :status-byte status-byte :position pos
                    :track (input-stream-current-track mf)))))))

(defun continue-midi-event-two-bytes (mf)
  (setf (aref (input-stream-buffer mf) 1) (read-byte (stream-fd-stream mf)))
  2)

(defun continue-midi-event-three-bytes (mf)
  (read-into-message-buffer mf 1 3))

(defun meta-or-sysex-end (mf buffer-start)
  (declare (type midifile:input-stream)
           (type (unsigned-byte 8) buffer-start))
  (loop for i of-type (unsigned-byte 8) from buffer-start
        for b = (read-byte (stream-fd-stream mf))
        for len of-type non-negative-fixnum = (logand b #x7f)
                                            then (+ (ash len 7) (logand b #x7f))
        with max-bytes of-type (unsigned-byte 8) = (+ buffer-start 3)
        do (when (> i max-bytes)
             (let ((bytes (- i buffer-start))
                   (pos (1+ (input-stream-position mf))))
               (unless (> bytes (input-stream-track-bytes-remain mf))
                 (update-byte-counters mf bytes))
               (error 'invalid-variable-length-quantity
                      :string "Variable length quantity in more than four bytes"
                      :position pos :track (input-stream-current-track mf))))
           (setf (aref (stream-buffer mf) i) b)
        until (< b #x80)
        finally (return (let ((start (1+ i)))
                          (values start (+ start len))))))

(defun continue-meta-or-sysex (mf)
  (multiple-value-bind (start end)
      (cond ((meta-event-p mf)
             (setf (aref (stream-buffer mf) 1)
                   (read-byte (stream-fd-stream mf)))
             (when (end-of-track-p mf)
               (setf (aref (stream-buffer mf) 2)
                     (read-byte (stream-fd-stream mf)))
               (update-byte-counters mf 3)
               (return-from continue-meta-or-sysex 3))
             (meta-or-sysex-end mf 2))
            (t
             (meta-or-sysex-end mf 1)))
    (when (> end (length (stream-buffer mf)))
      (adjust-array (stream-buffer mf) end))
    (read-into-message-buffer mf start end)))

(declaim (inline current-tempo))
(defun current-tempo (mf)
  (aref (input-stream-tempo-map mf) (input-stream-tempo-index mf)))

(declaim (inline microseconds-per-quarter-note))
(defun microseconds-per-quarter-note (mf)
  (tempo-microseconds-per-quarter-note (current-tempo mf)))

(declaim (inline event-beats))
(defun event-beats (mf &optional (output-type-spec 'double-float))
  "Return the time in beats of the last event read from a
MIDIFILE:INPUT-STREAM.

OUTPUT-TYPE-SPEC is the type of the returned value and defaults to
DOUBLE-FLOAT."
  (declare (type midifile:input-stream mf))
  (assert (plusp (stream-ppqn mf)))
  (coerce (/ (stream-track-time mf) (stream-ppqn mf))
          output-type-spec))

(defun event-seconds (mf)
  "Return the time in seconds of the last event read from a
MIDIFILE:INPUT-STREAM."
  (declare (type midifile:input-stream mf))
  (assert (plusp (stream-ppqn mf)))
  (let ((te (current-tempo mf)))
    (+ (tempo-seconds te)
       (* (tempo-microseconds-per-quarter-note te)
          1d-6
          (/ (- (stream-track-time mf) (tempo-time te))
             (stream-ppqn mf))))))

(defun update-tempo-map (mf)
  (let* ((buf (stream-buffer mf))
         (usecs (+ (ash (aref buf 3) 16) (ash (aref buf 4) 8) (aref buf 5)))
         (curr (current-tempo mf)))
    (cond ((or ;; Discard a tempo change out of the global tempo track.
               (plusp (input-stream-next-tempo-time mf))
               ;; Ignore the duplicate.
               (= (tempo-microseconds-per-quarter-note curr) usecs))
           nil)
          ((= (stream-track-time mf) (tempo-time curr))
           ;; Multiple changes at the same time: the last closes the door.
           (setf (tempo-microseconds-per-quarter-note curr) usecs))
          (t
           (setf (input-stream-tempo-index mf)
                 (vector-push-extend
                   (make-tempo :time (stream-track-time mf)
                               :seconds (event-seconds mf)
                               :microseconds-per-quarter-note usecs)
                   (input-stream-tempo-map mf) 4))))
    (values)))

(defun read-event (mf)
  "Read the next event from a MIDIFILE:INPUT-STREAM and return the
status byte."
  (declare (type midifile:input-stream mf))
  (when (read-delta-time mf)
    (let* ((g #'continue-running-status)
           (o #'continue-midi-event-two-bytes)
           (i #'continue-midi-event-three-bytes)
           (a #'continue-meta-or-sysex)
           (status (read-status-byte mf))
           (index (max (- (ash status -4) 7) 0)))
      (setf (input-stream-last-message-length mf)
            (funcall (svref (vector g i i i i o o i a) index) mf))
      (let ((size (if (zerop index)
                      ;; Running status.
                      (1- (input-stream-last-message-length mf))
                      (input-stream-last-message-length mf))))
        (cond ((end-of-track-p mf)
               ;; Note: byte counters are updated.
               (next-track mf))
              ((< size (input-stream-track-bytes-remain mf))
               (update-byte-counters mf size))
              (t
               (error 'invalid-track-chunk-length
                      :string "Wrong track-chunk-length"
                      :position (1+ (input-stream-position mf))
                      :track (input-stream-current-track mf))))
        (when (and (tempo-event-p mf)
                   ;; Ignore tempo-events if time-code-based time.
                   (plusp (stream-ppqn mf)))
          (update-tempo-map mf))
        (message-status mf)))))

(declaim (inline write-delta-time))
(defun write-delta-time (pulses octets &optional (start 0))
  (declare (type non-negative-fixnum pulses start)
           (type (or data (vector (unsigned-byte 8))) octets))
  (let ((event-time-bytes (variable-length-quantity-bytes pulses)))
    (do ((i start (1+ i))
         (pos (* (1- event-time-bytes) 7) (- pos 7)))
        ((= pos 0)
         (setf (aref octets i) (logand #x7f pulses))
         (+ start event-time-bytes))
      (declare (type non-negative-fixnum i pos))
      (setf (aref octets i) (logior #x80 (ldb (byte 7 pos) pulses))))))

(defun write-event-time (mf beats)
  (declare (type midifile:output-stream mf)
           (type non-negative-real beats))
  (let ((delta-time (max 0 (- (round (* beats (output-stream-ppqn mf)))
                              (output-stream-track-time mf)))))
    (declare (type non-negative-fixnum delta-time))
    (incf (output-stream-track-time mf) delta-time)
    (write-delta-time delta-time (stream-buffer mf)
                      (output-stream-buffer-index mf))))

(defun ensure-buffer-space (mf event-size)
  (let ((size (array-total-size (stream-buffer mf))))
    (when (> (+ (output-stream-buffer-index mf) event-size 1) size)
      (adjust-array (stream-buffer mf) (+ size *write-buffer-expand-size*)))
    mf))

(declaim (inline short-message-to-octets))
(defun short-message-to-octets (value octets &optional (start 0) (size 4))
  (do ((i 0 (1+ i))
       (j start (1+ j))
       #+little-endian (pos 0 (+ pos 8))
       #-little-endian (pos 24 (- pos 8)))
      ((>= i size) j)
    (declare (type non-negative-fixnum i j pos))
    (setf (aref octets j) (ldb (byte 8 pos) value))))

(defun write-short-event (mf beats msg size)
  "Write a MIDI event with the message MSG of size SIZE encoded into
four bytes to the MIDIFILE:OUTPUT-STREAM MF.

BEATS is the absolute time of the message in beats. If the event
precedes the last event, it is added with delta-time zero."
  (declare (type midifile:output-stream mf)
           (type non-negative-real beats)
           (type (unsigned-byte 32) msg)
           (type (integer 1 4) size))
  (assert (stream-open-p mf))
  (ensure-buffer-space mf 8)
  (let ((status (logand #+little-endian msg
                        #-little-endian (ash msg -24)
                        #xff)))
    (cond ((and (= status (output-stream-last-status mf))
                (< status #xf0))
           ;; Running status.
           #+little-endian (setf msg (ash msg -8))
           #-little-endian (ash (logand msg #xffffff) 8)
           (decf size))
          (t
           (setf (output-stream-last-status mf) status)))
    (setf (output-stream-buffer-index mf)
          (short-message-to-octets msg (stream-buffer mf)
                                   (write-event-time mf beats)
                                   size))))

(defun write-event (mf beats data &key (start 0) end)
  "Write a MIDI event with the message stored in the octets DATA
to the MIDIFILE:OUTPUT-STREAM MF.

BEATS is the absolute time of the message in beats. If the event
precedes the last event, it is added with delta-time zero.

The octets DATA are optionally bounded by START and END."
  (declare (type midifile:output-stream mf)
           (type non-negative-real beats)
           (type data data)
           (type non-negative-fixnum start)
           (type (or non-negative-fixnum null) end))
  (assert (stream-open-p mf))
  (let ((len (if end (- end start) (length data))))
    (declare (type non-negative-fixnum len))
    (setf (output-stream-last-status mf) (aref data start))
    (cond ((plusp len)
           (ensure-buffer-space mf (+ 4 len))
           (do ((i start (1+ i))
                (j (write-event-time mf beats) (1+ j)))
               ((>= i (+ start len))
                (setf (output-stream-buffer-index mf) j))
             (declare (type non-negative-fixnum i j))
             (setf (aref (stream-buffer mf) j) (aref data i))))
          (t (output-stream-buffer-index mf)))))

(defun write-tempo-track (mf tempo-envelope)
  "Write a tempo track to a MIDIFILE:OUTPUT-STREAM MF with the tempo
changes obtained from a INCUDINE:TEMPO-ENVELOPE.

It fails if the current track contains events at non-zero time.

An error is thrown if a curve of TEMPO-ENVELOPE is not a step function."
  (declare (type midifile:output-stream mf)
           (type incudine:tempo-envelope tempo-envelope))
  (let* ((spb-env (incudine::tempo-envelope-spb tempo-envelope))
         (points (incudine:envelope-points spb-env)))
    ;; Allowing meta-events just at time zero before the tempo changes.
    (unless (zerop (stream-track-time mf))
      (%midifile-error "WRITE-TEMPO-TRACK works from time zero."))
    (do ((i 1 (1+ i)))
        ((>= i points))
      (unless (eq (incudine:envelope-curve spb-env i) :step)
        (%midifile-error "WRITE-TEMPO-TRACK requires a TEMPO-ENVELOPE structure ~
                          with step function curves.")))
    (do* ((i 0 (1+ i))
          (beats 0 (+ beats (incudine:envelope-time spb-env i))))
         ((>= i points))
      (write-event mf beats
                   (tempo-message (incudine:bpm-at tempo-envelope beats))))
    (next-track mf)))

(defgeneric tempo (obj)
  (:documentation "If the MIDI file contains more than one tempo event,
return two lists: the values in BPM and the delta-times of the changes
in beats (useful to create a INCUDINE:TEMPO-ENVELOPE structure).
If there aren't changes, return the tempo in BPM."))

(defmethod tempo ((obj midifile:input-stream))
  (flet ((bpm (te)
           (/ (round (/ 6e9 (tempo-microseconds-per-quarter-note te))) 100.0)))
    (let* ((tempo-map (input-stream-tempo-map obj))
           (len (length tempo-map)))
      (assert (plusp len))
      (if (= len 1)
          (bpm (aref tempo-map 0))
          (loop for i below (length tempo-map)
                for prev-time = 0 then (tempo-time j)
                for j = (aref tempo-map i)
                for bpms = (list (bpm j)) then (cons (bpm j) bpms)
                for beats = nil then (cons (float (/ (- (tempo-time j) prev-time)
                                                     (stream-ppqn obj)))
                                           beats)
                finally (return (values (nreverse bpms) (nreverse beats))))))))

(defmethod tempo ((obj pathname))
  (with-open-midifile (mf obj)
    (loop while (read-event mf))
    (tempo mf)))

(defmethod tempo ((obj string))
  (tempo (truename obj)))
