;;; Copyright (c) 2018-2023 Tito Latini
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

#-win32
(progn
  (defun posix-value (&rest flag-constants)
    (reduce #'logior
            (mapcar (lambda (x) (symbol-value (find-symbol x "SB-POSIX")))
                    flag-constants)))

  (defmacro set-termios-flag (term type &key include exclude)
    (let ((termios-flag (format-symbol "SB-POSIX" "TERMIOS-~AFLAG"
                                       (char (symbol-name type) 0))))
      `(setf (,termios-flag ,term)
             (logior (logand (,termios-flag ,term)
                             (lognot (posix-value ,@exclude)))
                     (posix-value ,@include)))))

  (defun serial-speed-constant (value)
    (let ((sym (find-symbol (format nil "B~D" value) "SB-POSIX")))
      (if sym
          (symbol-value sym)
          (incudine-error "Invalid baud rate ~D" value))))

  (defun open-serial-port (port &key (baud-rate 9600) (element-type 'character))
    (declare (type (or pathname string) port)
             (type (integer 0 230400) baud-rate))
    (let ((fd (sb-posix:open (namestring port)
                             (logior sb-posix:o-rdwr sb-posix:o-noctty
                                     sb-posix:o-nonblock))))
      (handler-case
          (let ((term (sb-posix:tcgetattr fd))
                (speed (serial-speed-constant baud-rate)))
            (set-termios-flag term input-modes
              :exclude ("INLCR" "IGNCR" "ICRNL" "IGNBRK" "PARMRK"
                        "INPCK" "ISTRIP" "IXON" "IXOFF"))
            (set-termios-flag term output-modes
              :exclude ("OPOST" "OCRNL"))
            (set-termios-flag term control-modes
              :include ("CLOCAL" "CREAD" "CS8")
              :exclude ("CSIZE" "CSTOPB" "PARENB"))
            (set-termios-flag term local-modes
              :exclude ("ICANON" "ECHO" "ECHOE" "ISIG" "IEXTEN"))
            (sb-posix:cfsetispeed speed term)
            (sb-posix:cfsetospeed speed term)
            (setf (aref (sb-posix:termios-cc term) sb-posix:vmin) 0)
            (setf (aref (sb-posix:termios-cc term) sb-posix:vtime) 0)
            (sb-posix:tcsetattr fd sb-posix:tcsanow term)
            (sb-sys:make-fd-stream fd :input t :output t :buffering :none
                                   :name (format nil "serial port ~A" port)
                                   :element-type element-type))
        (condition (c) (sb-posix:close fd) (error c)))))

  (defun serial-stream-p (stream)
    (and (sb-sys:fd-stream-p stream)
         (cffi:foreign-funcall "isatty" :int (sb-sys:fd-stream-fd stream)
                               :boolean)))

  (defun serial-flush (stream &key (direction :io))
  (declare (type sb-sys:fd-stream stream)
           (type (member :input :output :io) direction))
  (let ((queue-selector sb-posix:tcioflush))
    (cond ((eq direction :output)
           (setq queue-selector sb-posix:tcoflush)
           (clear-output stream))
          (t
           (if (eq direction :input) (setq queue-selector sb-posix:tciflush))
           (clear-input stream)))
    (sb-posix:tcflush (sb-sys:fd-stream-fd stream) queue-selector)
    nil)))

#+win32
(progn
  (cffi:defctype dword :unsigned-long)

  (cffi:defcstruct dcb
    (dcb-length dword)
    (baud-rate dword)
    (byte-size :uint8 :offset 10)
    (parity :uint8)
    (stop-bits :uint8)
    (reserved :uint16 :offset 5))

  (define-constant +dcb-length+ 28)

  (defmacro dcb-value (p slot-name)
    `(cffi:foreign-slot-value ,p '(:struct dcb) ,slot-name))

  (defun open-serial-port (port &key (baud-rate 9600) (element-type 'character))
      (declare (type (or pathname string) port)
               (type (integer 0 230400) baud-rate))
      (unless (member baud-rate
                      '(110 300 600 1200 2400 4800 9600 14400 19200
                        38400 56000 57600 115200 128000 256000)
                      :test #'=)
        (incudine-error "Invalid baud rate ~D" baud-rate))
      (let* ((generic-read  #x80000000)
             (generic-write #x40000000)
             (open-existing 3)
             (file-attribute-normal #x80)
             (invalid-handle-value #XFFFFFFFFFFFFFFFF)
             (onestopbit 0)
             (noparity 0)
             (fd (cffi:foreign-funcall "CreateFileA"
                   :string (namestring port)
                   dword (logior generic-read generic-write)
                   dword 0
                   :pointer (cffi:null-pointer)
                   dword open-existing
                   dword file-attribute-normal
                   :pointer (cffi:null-pointer)
                   :uint64)))
        (when (= fd invalid-handle-value)
          (incudine-error "CreateFileA failed"))
        (handler-case
            (cffi:with-foreign-object (serial-param '(:struct dcb))
              (setf (dcb-value serial-param 'dcb-length) +dcb-length+)
              (unless (cffi:foreign-funcall "GetCommState"
                        :uint64 fd :pointer serial-param :boolean)
                (incudine-error "GetCommState failed"))
              (setf (dcb-value serial-param 'baud-rate) baud-rate)
              (setf (dcb-value serial-param 'byte-size) 8)
              (setf (dcb-value serial-param 'stop-bits) onestopbit)
              (setf (dcb-value serial-param 'parity) noparity)
              (unless (cffi:foreign-funcall "SetCommState"
                        :uint64 fd :pointer serial-param :boolean)
                (incudine-error "SetCommState failed"))
              (sb-sys:make-fd-stream fd :input t :output t :buffering :none
                                     :name (format nil "serial port ~A" port)
                                     :element-type element-type))
          (error (c)
            (cffi:foreign-funcall "CloseHandle" :uint64 fd :int)
            (error c)))))

  (defun serial-stream-p (stream)
    (when (sb-sys:fd-stream-p stream)
      (let ((handle (sb-sys:fd-stream-fd stream))
            (file-type-char 2))
        (and (= (cffi:foreign-funcall "GetFileType" :uint64 handle dword)
                file-type-char)
             (cffi:with-foreign-object (serial-param '(:struct dcb))
               (cffi:foreign-funcall "GetCommState"
                 :uint64 handle :pointer serial-param :boolean))))))

  (defun serial-flush (stream &key (direction :io))
  (declare (type sb-sys:fd-stream stream)
           (type (member :input :output :io) direction))
  (let ((rxclear 8)
        (txclear 4))
    (cffi:foreign-funcall "PurgeComm" :uint64 (sb-sys:fd-stream-fd stream)
      dword (case direction
              (:input rxclear)
              (:output txclear)
              (otherwise (logior rxclear txclear)))
      :boolean)
    nil)))

(setf (documentation 'open-serial-port 'function)
      "Create, open and return a stream that is connected to a serial PORT with
speed BAUD-RATE, 8 data bits, no parity checking and one stop-bit.

BAUD-RATE defaults to 9600.

ELEMENT-TYPE is the type specifier CHARACTER (default) or UNSIGNED-BYTE 8.

Example:

    \(in-package :scratch)

    (defvar *uno* (open-serial-port \"/dev/ttyACM0\"))

    (make-responder *uno*
      (lambda (x)
        (when (numberp x)
          (barrier (:write)
            (setf (bus 0) (sample (the single-float x))))
          nil)))

    (recv-start *uno*
      :read-function (lambda (stream)
                       (let ((str (read-line stream nil nil)))
                         (and str (parse-float str)))))

    ;; stop
    (recv-stop *uno*)
    (remove-all-responders *uno*)
    (remove-receiver *uno*)
    (close *uno*)")

(setf (documentation 'serial-stream-p 'function)
      "Whether STREAM refers to a serial port.")

(setf (documentation 'serial-flush 'function)
      "If DIRECTION is :INPUT or :IO (default), flush data received from
the serial port STREAM but not read.

If DIRECTION is :OUTPUT or :IO, flush data written to STREAM but not
transmitted.")
