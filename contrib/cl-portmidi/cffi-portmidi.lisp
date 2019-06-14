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

(in-package :portmidi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *standard-optimize-settings*
    '(optimize speed (safety 0)))

  (cffi:define-foreign-library portmidi
    (:darwin "libportmidi.dylib")
    (:unix "libportmidi.so")
    (:cygwin "cygportmidi-0.dll")
    (t (:default "libportmidi")))

  (defun load-portmidi-library ()
    (cffi:use-foreign-library portmidi))

  (unless (cffi:foreign-library-loaded-p 'portmidi)
    (load-portmidi-library)))

(defstruct (stream (:constructor %make-stream)
                   (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (direction :closed :type (member :input :output :closed))
  (device-id 0 :type non-negative-fixnum)
  (device-interf "none" :type string)
  (device-name "none" :type string)
  (buffer-size 1024 :type positive-fixnum)
  (driver-info (cffi:null-pointer) :type cffi:foreign-pointer)
  (time-proc (cffi:null-pointer) :type cffi:foreign-pointer)
  (time-info (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (input-stream (:include stream) (:copier nil))
  ;; Pointer to the PmEvent that contains the received SysEx message.
  (sysex-pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  ;; Number of the events starting from the received SysEx message.
  (events-remain 0 :type non-negative-fixnum))

(defstruct (output-stream (:include stream) (:copier nil))
  (latency 1 :type non-negative-fixnum))

(declaim (inline finalize))
(defun finalize (obj function)
  #+sbcl (sb-ext:finalize obj function :dont-save t)
  #-sbcl (tg:finalize obj function))

(declaim (inline cancel-finalization))
(defun cancel-finalization (obj)
  #+sbcl (sb-ext:cancel-finalization obj)
  #-sbcl (tg:cancel-finalization obj))

(defmethod print-object ((obj stream) stream)
  (format stream "#<PM:~A-STREAM \"~A - ~A\">"
          (stream-direction obj) (stream-device-interf obj)
          (stream-device-name obj)))

(cffi:define-foreign-type stream-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser stream))

(defmethod cffi:translate-to-foreign (handle (type stream-type))
  (slot-value handle 'pointer))

(cffi:defctype message   :int32)
(cffi:defctype timestamp :int32)

(defmacro define-constants (bindings)
  `(progn
     ,@(mapcar (lambda (bind)
                 `(define-constant ,@bind))
               bindings)))

(define-constants
  ((default-sysex-buffer-size 1024)
   (hdrlength 50)
   (host-error-msg-len 256)
   (no-device -1)
   ;; Filter bit-mask definitions
   (filt-active (ash 1 #xe))
   (filt-sysex (ash 1 0))
   (filt-clock (ash 1 #x8))
   (filt-play (logior (ash 1 #xa)
                      (ash 1 #xc)
                      (ash 1 #xb)))
   (filt-tick (ash 1 #x9))
   (filt-fd (ash 1 #xd))
   (filt-undefined filt-fd)
   (filt-reset (ash 1 #xf))
   (filt-realtime (logior filt-active
                          filt-sysex
                          filt-clock
                          filt-play
                          filt-undefined
                          filt-reset
                          filt-tick))
   (filt-note (logior (ash 1 #x19) (ash 1 #x18)))
   (filt-channel-aftertouch (ash 1 #x1d))
   (filt-poly-aftertouch (ash 1 #x1a))
   (filt-aftertouch (logior filt-channel-aftertouch
                            filt-poly-aftertouch))
   (filt-program (ash 1 #x1c))
   (filt-control (ash 1 #x1b))
   (filt-pitchbend (ash 1 #x1e))
   (filt-mtc (ash 1 1))
   (filt-song-position (ash 1 2))
   (filt-song-select (ash 1 3))
   (filt-tune (ash 1 6))
   (filt-systemcommon (logior filt-mtc
                              filt-song-position
                              filt-song-select
                              filt-tune))))

(cffi:defcenum error
  (:pm-no-data 0)
  (:pm-no-error 0)
  (:pm-got-data 1)
  (:pm-host-error -10000)
  :pm-invalid-device-id
  :pm-insufficient-memory
  :pm-buffer-too-small
  :pm-buffer-overflow
  :pm-bad-ptr
  :pm-bad-data
  :pm-internal-error
  :pm-buffer-max-size)

(cffi:defcstruct device-info
  (struct-version :int)
  (interf         :string)
  (name           :string)
  (input          :int)
  (output         :int)
  (opened         :int))

(cffi:defcstruct event
  (message   message)
  (timestamp timestamp))

(cffi:defcfun ("Pm_Initialize" initialize) error)

(cffi:defcfun ("Pm_Terminate" terminate%) error)

(cffi:defcfun ("Pm_HasHostError" has-host-error) :int
  (stream stream))

(cffi:defcfun ("Pm_GetErrorText" get-error-text) :string
  (errnum error))

(cffi:defcfun ("Pm_GetHostErrorText" get-host-error-text) :void
  (msg :string)
  (len :unsigned-int))

(cffi:defcfun ("Pm_CountDevices" count-devices) :int)

(cffi:defcfun ("Pm_GetDefaultInputDeviceID"
               get-default-input-device-id) :int)

(cffi:defcfun ("Pm_GetDefaultOutputDeviceID"
               get-default-output-device-id) :int)

(cffi:defcfun ("Pm_GetDeviceInfo" get-device-info%) :pointer
  (id :int))

(cffi:defcfun ("Pm_OpenInput" open-input) error
  (stream            :pointer)
  (input-device      :int)
  (input-driver-info :pointer)
  (buffer-size       :int32)
  (time-proc         :pointer)
  (time-info         :pointer))

(cffi:defcfun ("Pm_OpenOutput" open-output) error
  (stream             :pointer)
  (output-device      :int)
  (output-driver-info :pointer)
  (buffer-size        :int32)
  (time-proc          :pointer)
  (time-info          :pointer)
  (latency            :int32))

(cffi:defcfun ("Pm_SetFilter" set-filter) error
  (stream  stream)
  (filters :int32))

(cffi:defcfun ("Pm_SetChannelMask" set-channel-mask) error
  (stream stream)
  (mask   :int))

(cffi:defcfun ("Pm_Abort" abort) error
  (stream stream))

(cffi:defcfun ("Pm_Close" close%) error
  (stream stream))

(cffi:defcfun ("Pm_Synchronize" synchronize) error
  (stream stream))

(cffi:defcfun ("Pm_Read" read%) :int
  (stream stream)
  (buffer :pointer)
  (length :int32))

(cffi:defcfun ("Pm_Poll" poll) error
  (stream stream))

(cffi:defcfun ("Pm_Write" write%) error
  (stream stream)
  (buffer :pointer)
  (length :int32))

(cffi:defcfun ("Pm_WriteShort" write-short) error
  (stream stream)
  (when   timestamp)
  (msg    :int32))

(cffi:defcfun ("Pm_WriteSysEx" write-sysex) error
  (stream stream)
  (when   timestamp)
  (msg    :string))

(declaim (inline message))
(defun message (status data1 data2)
  (declare (type (unsigned-byte 8) status data1 data2))
  (locally (declare #.*standard-optimize-settings*)
    (logior (ash data2 16) (ash data1 8) status)))

(declaim (inline message-status))
(defun message-status (msg)
  (declare (type (unsigned-byte 24) msg))
  (logand msg #xFF))

(declaim (inline message-data1))
(defun message-data1 (msg)
  (declare (type (unsigned-byte 24) msg))
  (locally (declare #.*standard-optimize-settings*)
    (ldb (byte 8 8) msg)))

(declaim (inline message-data2))
(defun message-data2 (msg)
  (declare (type (unsigned-byte 24) msg))
  (locally (declare #.*standard-optimize-settings*)
    (ldb (byte 8 16) msg)))

(declaim (inline decode-message))
(defun decode-message (msg)
  (declare (type (unsigned-byte 24) msg))
  (let ((ash-8 (ldb (byte 16 8) msg)))
    (declare #.*standard-optimize-settings*)
    (values (ldb (byte 8 0) msg)       ; status
            (ldb (byte 8 0) ash-8)     ; data1
            (ldb (byte 8 8) ash-8))))  ; data2

(declaim (inline decode-channel-message))
(defun decode-channel-message (msg)
  (declare (type (unsigned-byte 24) msg))
  (let* ((ash-4 (ldb (byte 20 4) msg))
         (ash-8 (ldb (byte 16 4) ash-4)))
    (declare #.*standard-optimize-settings*)
    (values (ldb (byte 4 0) ash-4)     ; type
            (ldb (byte 4 0) msg)       ; channel
            (ldb (byte 8 0) ash-8)     ; data1
            (ldb (byte 8 8) ash-8))))  ; data2

(declaim (inline before))
(defun before (t1 t2)
  (declare (type (unsigned-byte 32) t1 t2))
  (< t1 t2))

(declaim (inline channel))
(defun channel (chan)
  (declare (type (unsigned-byte 4) chan))
  (ash 1 chan))

(in-package :porttime)

(cffi:defctype timestamp :int32)

(cffi:defcenum error
  (:pt-no-error 0)
  (:pt-host-error -10000)
  :pt-already-started
  :pt-already-stopped
  :pt-insufficient-memory)

(cffi:defcfun ("Pt_Start" start%) error
  (resolution :int)
  (callback   :pointer)
  (userData   :pointer))

(declaim (inline start))
(defun start (&optional (resolution 1) (callback (cffi:null-pointer))
                (user-data (cffi:null-pointer)))
  (declare (type unsigned-byte resolution)
           (type cffi:foreign-pointer callback user-data))
  (start% resolution callback user-data))

(cffi:defcfun ("Pt_Stop" stop) error)
(cffi:defcfun ("Pt_Started" started) :boolean)
(cffi:defcfun ("Pt_Time" time) timestamp)
(cffi:defcfun ("Pt_Sleep" sleep) :void (duration :int32))
