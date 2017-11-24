;;; Copyright (c) 2013-2017 Tito Latini
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

(deftype midi-input-stream ()
  #-jack-midi 'pm:input-stream
  #+jack-midi '(or pm:input-stream jackmidi:input-stream))

(deftype midi-output-stream ()
  #-jack-midi 'pm:output-stream
  #+jack-midi '(or pm:output-stream jackmidi:output-stream))

(declaim (inline portmidi-write-short))
(defun portmidi-write-short (stream msg)
  (pm:write-short stream (if (rt-thread-p) (portmidi-time) 0) msg))

(declaim (inline portmidi-write-sysex))
(defun portmidi-write-sysex (stream msg-ptr)
  (cffi:foreign-funcall "Pm_WriteSysEx"
                        :pointer (pm:stream-pointer stream)
                        pm:timestamp (if (rt-thread-p) (portmidi-time) 0)
                        :pointer msg-ptr :int))

(declaim (inline midiout))
(defun midiout (status data1 data2 stream)
  "Send a generic MIDI message to a MIDI output stream."
  (declare (type (unsigned-byte 8) status data1 data2)
           (type midi-output-stream stream))
  #-jack-midi
  (portmidi-write-short stream (pm:message status data1 data2))
  #+jack-midi
  (if (pm:output-stream-p stream)
      (portmidi-write-short stream (pm:message status data1 data2))
      (jackmidi:write-short stream (jackmidi:message status data1 data2) 3))
  stream)

(declaim (inline midi-write-sysex))
#-jack-midi
(defun midi-write-sysex (stream msg-ptr size)
  (declare (ignore size))
  (portmidi-write-sysex stream msg-ptr))

#+jack-midi
(defun midi-write-sysex (stream msg-ptr size)
  (if (pm:output-stream-p stream)
      (portmidi-write-sysex stream msg-ptr)
      (rt-eval () (jackmidi:foreign-write stream msg-ptr size)))
  stream)

(defun sysex-sequence->foreign-array (seq)
  (declare (type sequence seq))
  (let* ((size (length seq))
         (fix-last-p (/= (the fixnum (elt seq (1- size))) #xF7))
         (obj (make-foreign-array (if fix-last-p (1+ size) size)
                                  :unsigned-char
                                  :initial-contents seq)))
    (when fix-last-p
      (setf (u8-ref (foreign-array-data obj) size) #xF7))
    obj))

(declaim (inline midiout-sysex))
(defun midiout-sysex (seq stream)
  "Send a MIDI SysEx message to a MIDI OUTPUT STREAM."
  (declare (type sequence seq) (type midi-output-stream stream))
  (let ((obj (sysex-sequence->foreign-array seq)))
    (unwind-protect
         (midi-write-sysex stream (foreign-array-data obj)
                           (length seq))
      (free obj))))

;;; MIDI Tuning messages

(define-constant +midi-bulk-tuning-dump-buffer-size+ 408)
(define-constant +midi-bulk-tuning-dump-name-index+ 6)
(define-constant +midi-bulk-tuning-dump-name-length+ 16)
(define-constant +midi-bulk-tuning-dump-freq-data-index+ 22)
(define-constant +midi-bulk-tuning-dump-checksum-index+ 406)

(defun position-nearest-et12-frequency (tuning keynum)
  (declare (type tuning tuning) (type (unsigned-byte 8) keynum))
  (let ((freq (tuning-cps tuning keynum)))
    (cond ((< freq (tuning-cps *tuning-et12* 0)) 0)
          ((>= freq (tuning-cps *tuning-et12* 127)) 127)
          (t (loop for k from 1 below 128
                   if (< freq (tuning-cps *tuning-et12* k))
                   return (1- k))))))

(defun tuning-et12-cents-offset (tuning)
  (coerce (* (log (/ (tuning-cps tuning 0) 8.1758) 2) 1200) 'single-float))

(declaim (inline midi-tuning-fraction-bytes))
(defun midi-tuning-fraction-bytes (cents-diff)
  (if (plusp cents-diff)
      (let ((value (round (* cents-diff (ash 1 14) 0.01))))
        (declare (type (unsigned-byte 14) value))
        ;; Two parts of the 14 bits message.
        (values (ldb (byte 7 7) value) (ldb (byte 7 0) value)))
      (values 0 0)))

(declaim (inline midi-four-bytes))
(defun midi-four-bytes (b0 b1 b2 b3)
  (declare (type (unsigned-byte 8) b0 b1 b2 b3))
  #+little-endian
  (logior (ash b3 24) (ash b2 16) (ash b1 8) b0)
  #-little-endian
  (logior (ash b0 24) (ash b1 16) (ash b2 8) b3))

;;; Bulk tuninig dump:
;;;
;;;     F0 7E DEVICE-ID 08 01 PROGRAM TUNINGNAME (XX YY ZZ)x128 CHECKSUM F7
;;;
(declaim (inline prepare-midi-bulk-tuning-dump-buffer))
(defun prepare-midi-bulk-tuning-dump-buffer (buffer device-id program)
  (declare (type foreign-pointer buffer)
           (type (unsigned-byte 8) device-id program))
  (setf (u32-ref buffer 0) (midi-four-bytes #xF0 #x7E device-id 8))
  (setf (u8-ref buffer 4) 1)
  (setf (u8-ref buffer 5) program)
  (setf (u8-ref buffer #.(1- +midi-bulk-tuning-dump-buffer-size+)) #xF7))

(defun set-midi-bulk-tuning-name (buffer name)
  (declare (type foreign-pointer buffer) (type string name))
  (let ((len (length name)))
    (cffi:with-foreign-string (str name)
      (foreign-copy (cffi:inc-pointer buffer +midi-bulk-tuning-dump-name-index+)
                    str (min len +midi-bulk-tuning-dump-name-length+)))
    (unless (>= len +midi-bulk-tuning-dump-name-length+)
      (incudine.external:foreign-set
        (cffi:inc-pointer buffer (+ len +midi-bulk-tuning-dump-name-index+))
        0 (- +midi-bulk-tuning-dump-name-length+ len)))
    name))

(defun midi-dump-checksum (buffer size)
  (declare (type foreign-pointer buffer) (type non-negative-fixnum size)
           (ignore size))
  (labels ((checksum (i sum)
             (declare (type non-negative-fixnum i sum))
             (if (< i +midi-bulk-tuning-dump-checksum-index+)
                 (checksum (1+ i) (logxor sum (u8-ref buffer i)))
                 (logand sum #x7F))))
    (checksum 1 0)))

(defun midi-bulk-tuning-dump (tuning stream device-id program checksum-function)
  (declare (type midi-output-stream stream) (type tuning tuning)
           (type (unsigned-byte 8) device-id program)
           (type function checksum-function)
           #.*standard-optimize-settings*)
  (with-foreign-array (buf :char #.+midi-bulk-tuning-dump-buffer-size+)
    (prepare-midi-bulk-tuning-dump-buffer buf device-id program)
    (set-midi-bulk-tuning-name buf (tuning-description tuning))
    (labels ((set-freqs (k i j degrees os)
               (declare (type (unsigned-byte 8) k i j degrees)
                        (type single-float os))
               (when (< k 128)
                 (let ((xx (position-nearest-et12-frequency tuning k)))
                   (declare (type (unsigned-byte 8) xx))
                   (multiple-value-bind (yy zz)
                       (midi-tuning-fraction-bytes
                         (- (+ os (aref (tuning-cents tuning) i)) (* xx 100)))
                     (declare (type (unsigned-byte 8) yy zz))
                     (setf (u8-ref buf j) xx)
                     (setf (u8-ref buf (+ j 1)) yy)
                     (setf (u8-ref buf (+ j 2)) zz)))
                 (let ((i (mod (1+ k) degrees)))
                   (set-freqs (1+ k) i (+ j 3) degrees
                              (if (zerop i)
                                  ;; Increment the offset with the last interval.
                                  (+ os (aref (tuning-cents tuning) degrees))
                                  os))))))
      (set-freqs 0 0 +midi-bulk-tuning-dump-freq-data-index+
                 (1- (length (tuning-cents tuning)))
                 (tuning-et12-cents-offset tuning))
      (setf (u8-ref buf +midi-bulk-tuning-dump-checksum-index+)
            (funcall checksum-function buf +midi-bulk-tuning-dump-buffer-size+))
      (midi-write-sysex stream buf +midi-bulk-tuning-dump-buffer-size+))))

(defmacro with-midi-single-note-tuning-change-buffer ((buf-var device-id program)
                                                      &body body)
  ;; Single note tuning change message:
  ;;
  ;;     F0 7F DEVICE-ID 08 02 PROGRAM 01 KEYNUM XX YY ZZ F7
  ;;
  `(with-foreign-array (,buf-var :char 12)
     (setf (u32-ref ,buf-var 0) (midi-four-bytes #xF0 #x7F ,device-id 8))
     (setf (u32-ref ,buf-var 1) (midi-four-bytes 2 ,program 1 0))
     ,@body))

(defun midi-128-single-note-tuning (tuning stream device-id program)
  (declare (type midi-output-stream stream) (type tuning tuning)
           (type (unsigned-byte 8) device-id program)
           #.*standard-optimize-settings*)
  (with-midi-single-note-tuning-change-buffer (buf device-id program)
    (labels ((send (k i degrees os)
               (declare (type (unsigned-byte 8) k i degrees)
                        (type single-float os))
               (when (< k 128)
                 (setf (u8-ref buf 7) k)
                 (let ((xx (position-nearest-et12-frequency tuning k)))
                   (declare (type (unsigned-byte 8) xx))
                   (multiple-value-bind (yy zz)
                       (midi-tuning-fraction-bytes
                         (- (+ os (aref (tuning-cents tuning) i)) (* xx 100)))
                     (declare (type (unsigned-byte 8) yy zz))
                     (setf (u32-ref buf 2) (midi-four-bytes xx yy zz #xF7))
                     (midi-write-sysex stream buf 12)))
                 (sleep .0001)
                 (let ((i (mod (1+ k) degrees)))
                   (send (1+ k) i degrees
                         (if (zerop i)
                             ;; Increment the offset with the last interval.
                             (+ os (aref (tuning-cents tuning) degrees))
                             os))))))
      (send 0 0 (1- (length (tuning-cents tuning)))
            (tuning-et12-cents-offset tuning))
      stream)))

(defun midi-tuning-sysex (tuning stream &optional (device-id 0) (program 0)
                          single-note-tuning-p
                          (checksum-function #'midi-dump-checksum))
  "Send a bulk tuning dump message to a MIDI output STREAM. The new
frequencies are related to a TUNING structure. If SINGLE-NOTE-TUNING-P
is non-NIL, send 128 single note tuning change messages instead.
The optional CHECKSUM-FUNCTION requires two arguments, the foreign
buffer used for the MIDI SysEx message and the buffer size. It is
useful if the manufacturer implements a different checksum."
  (if single-note-tuning-p
      (midi-128-single-note-tuning tuning stream device-id program)
      (midi-bulk-tuning-dump tuning stream device-id program
                             checksum-function)))

(declaim (inline valid-midi-bulk-tuning-dump-p))
(defun valid-midi-bulk-tuning-dump-p (sysex-ptr device-id sub-id-2)
  (declare (type foreign-pointer sysex-ptr)
           (type (or (unsigned-byte 8) null) device-id)
           (type (unsigned-byte 8) sub-id-2))
  (and #+little-endian (= (logand (u32-ref sysex-ptr 0) #xff00ffff) #x08007ef0)
       #-little-endian (= (logand (u32-ref sysex-ptr 0) #xffff00ff) #xf07e0008)
       (or (null device-id)
           (= (u8-ref sysex-ptr 2) device-id)
           (= (u8-ref sysex-ptr 2) #x7f))
       ;; sub-id 2 (bulk dump reply)
       (= (u8-ref sysex-ptr sub-id-2) 1)))

(declaim (inline midi-bulk-tuning-program))
(defun midi-bulk-tuning-program (stream)
  ;; First PmEvent: F0 7E DEVICE-ID 08 [timestamp (4 bytes)] 01 PROGRAM ...
  (pm:with-input-sysex-event (ptr stream) (u8-ref ptr 9)))

(defun set-tuning-from-portmidi (obj stream device-id)
  (declare (type (or tuning function) obj) (type pm:input-stream stream)
           (type (or (unsigned-byte 8) null) device-id)
           #.*standard-optimize-settings*)
  (pm:with-input-sysex-event (ptr stream)
    (when (and (>= (pm:input-stream-events-remain stream)
                   (ash +midi-bulk-tuning-dump-buffer-size+ -2))
               (valid-midi-bulk-tuning-dump-p ptr device-id 8))
      (let ((tuning (if (functionp obj)
                        (funcall obj (midi-bulk-tuning-program stream))
                        obj)))
        (declare (type (or tuning null) tuning))
        (when tuning
          (cffi:with-foreign-object
              (name :char (1+ +midi-bulk-tuning-dump-name-length+))
            (let ((ret (cffi:foreign-funcall "set_freqs_from_midi" :pointer ptr
                                             :pointer (tuning-data tuning)
                                             :pointer name :int)))
              (declare (type fixnum ret))
              (cond ((zerop ret)
                     (setf (tuning-description tuning)
                           (reduce-warnings (cffi:foreign-string-to-lisp name)))
                     (msg debug "received MIDI bulk tuning dump"))
                    (t (msg warn "MIDI bulk tuning dump failed at index ~D" ret)
                       tuning)))))))))

#+jack-midi
(defun set-tuning-from-jackmidi (obj stream &optional device-id)
  (declare (type (or tuning function) obj) (type jackmidi:input-stream stream)
           (type (or (unsigned-byte 8) null) device-id)
           #.*standard-optimize-settings*)
  (multiple-value-bind (ptr size) (jackmidi:input-stream-sysex-pointer stream)
    (declare (type foreign-pointer ptr) (type non-negative-fixnum size))
    (when (and (= size +midi-bulk-tuning-dump-buffer-size+)
               (valid-midi-bulk-tuning-dump-p ptr device-id 4))
      (let ((tuning (if (functionp obj)
                        ;; Program number as argument.
                        (funcall obj (u8-ref ptr 5))
                        obj)))
        (declare (type (or tuning null) tuning))
        (when tuning
          (let ((ret (jackmidi::set-freqs-from-midi-data-format
                       (tuning-data tuning)
                       (cffi:inc-pointer ptr
                         +midi-bulk-tuning-dump-freq-data-index+)
                       128)))
            (declare (type fixnum ret))
            (cond ((zerop ret)
                   (setf (tuning-description tuning)
                         (reduce-warnings
                           (cffi:foreign-string-to-lisp ptr
                             :offset +midi-bulk-tuning-dump-name-index+
                             :count +midi-bulk-tuning-dump-name-length+)))
                   (msg debug "received MIDI bulk tuning dump"))
                  (t (msg warn "MIDI bulk tuning dump failed at index ~D" ret)
                     tuning))))))))

(declaim (inline set-tuning-from-midi))
(defun set-tuning-from-midi (obj stream &optional device-id)
    "If OBJ is a TUNING structure, the frequencies and the description
of OBJ are changed with the data received from a MIDI bulk tuning dump
message. If OBJ is a function, it is called with the program number
contained in the MIDI bulk tuning message. The function has to return
the TUNING structure to set or NIL to ignore the MIDI message.
The checksum of the message is ignored."
    (declare (type (or tuning function) obj) (type midi-input-stream stream)
             (type (or (unsigned-byte 8) null) device-id))
    #-jack-midi
    (set-tuning-from-portmidi obj stream device-id)
    #+jack-midi
    (if (pm:input-stream-p stream)
        (set-tuning-from-portmidi obj stream device-id)
        (set-tuning-from-jackmidi obj stream device-id)))
