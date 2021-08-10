;;; Copyright (c) 2013-2021 Tito Latini
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

(in-package :incudine.util)

(define-constant +logger-error+  1)
(define-constant +logger-warn+   2)
(define-constant +logger-info+   4)
(define-constant +logger-debug+  8)

(define-constant +logger-error-mask+ #b00000001)
(define-constant +logger-warn-mask+  #b00000011)
(define-constant +logger-info-mask+  #b00000111)
(define-constant +logger-debug-mask+ #b00001111)

(defvar *logger-mask* +logger-warn-mask+)
(declaim (type (integer 0 15) *logger-mask*))

(defvar *logger-time* nil)
(declaim (type (member :sec :samp nil) *logger-time*))

(defvar *logger-stream* *error-output*
  "Stream for log messages.")
(declaim (type stream *logger-stream*))

(sb-ext:defglobal *null-output* (make-null-output-stream)
  "Output stream for null output.")

(defvar *logger-force-output-p* t
  "Whether FORCE-OUTPUT is called after a log message.")
(declaim (type boolean *logger-force-output-p*))

(declaim (inline logger-level))
(defun logger-level ()
  "Return the log level. Should be one of :ERROR, :WARN (default),
:INFO or :DEBUG. Setfable."
  (cond ((= *logger-mask* +logger-error-mask+) :error)
        ((= *logger-mask* +logger-warn-mask+) :warn)
        ((= *logger-mask* +logger-info-mask+) :info)
        (t :debug)))

(declaim (inline get-logger-mask))
(defun get-logger-mask (level)
  (case level
    (:error +logger-error-mask+)
    (:warn  +logger-warn-mask+)
    (:info  +logger-info-mask+)
    (otherwise +logger-debug-mask+)))

(declaim (inline set-logger-level))
(defun set-logger-level (level)
  (declare (type (member :error :warn :info :debug) level))
  (setf *logger-mask* (get-logger-mask level))
  level)

(defsetf logger-level set-logger-level)

(declaim (inline logger-time))
(defun logger-time ()
  "Return the time unit used for the log messages. Should be one
of :SEC, :SAMP or NIL (default). Setfable."
  *logger-time*)

(declaim (inline set-logger-time))
(defun set-logger-time (unit)
  (declare (type (member :sec :samp nil) unit))
  (setf *logger-time* unit))

(defsetf logger-time set-logger-time)

(defun default-logger-time-function ()
  "Default function to format the timestamp used for the log messages."
  (let ((seconds-p (eq *logger-time* :sec)))
    (format *logger-stream* "~:[~,1F~;~,3F~] " seconds-p
            (if seconds-p
                (* (incudine:now) *sample-duration*)
                (incudine:now)))))

(defvar *logger-time-function* #'default-logger-time-function)
(declaim (type function  *logger-time-function*))

(declaim (inline logger-time-function))
(defun logger-time-function ()
  "Return the function of no arguments to format the timestamp used
for the log messages. Setfable."
  *logger-time-function*)

(declaim (inline set-logger-time-function))
(defun set-logger-time-function (function)
  (declare (type (or function null) function))
  (setf *logger-time-function*
        (or function #'default-logger-time-function)))

(defsetf logger-time-function set-logger-time-function)

(defmacro* with-logger ((&optional-key stream level (time-unit nil time-unit-p)
                        time-format-function) &rest body)
  "Log LEVEL, TIME-UNIT and TIME-FORMAT-FUNCTION to log messages inside BODY."
  `(let (,@(if stream `((*logger-stream* ,stream)))
         ,@(if level `((*logger-mask* (get-logger-mask ,level))))
         ,@(if time-unit-p `((*logger-time* ,time-unit)))
         ,@(if time-format-function
               `((*logger-time-function* ,time-format-function))))
     ,@body))

(declaim (inline logger-active-p))
(defun logger-active-p (type)
  (logtest (case type
             (error +logger-error+)
             (warn +logger-warn+)
             (info +logger-info+)
             (debug +logger-debug+)
             (otherwise #.(1+ +logger-debug-mask+)))
           *logger-mask*))

(defun %msg (type control-string args)
  (declare (type (member error warn info debug) type)
           (type string control-string)
           (optimize speed (safety 0)))
  (when (logger-active-p type)
    (fresh-line *logger-stream*)
    (when *logger-time*
      (funcall *logger-time-function*))
    (unless (eq type 'info)
      (princ (format nil "~A: " type) *logger-stream*))
    (apply #'format *logger-stream* control-string args)
    (terpri *logger-stream*)
    (when *logger-force-output-p*
      (force-output *logger-stream*))))

(defun %nrt-msg (type control-string &rest args)
  (declare (type (member error warn info debug) type)
           (type string control-string)
           (optimize speed (safety 0)))
  (when (logger-active-p type)
    (flet ((logging ()
             (%msg type control-string args)))
      (if (rt-thread-p)
          (incudine:nrt-funcall #'logging)
          (logging)))))

(defmacro msg (type format-control &rest format-arguments)
  "Produce a formatted log message controlled by FORMAT-CONTROL and
FORMAT-ARGUMENTS.

TYPE should be one of ERROR, WARN, INFO or DEBUG."
  `(%msg ',(ensure-symbol type "INCUDINE.UTIL")
         ,format-control (list ,@format-arguments)))

(defmacro nrt-msg (type format-control &rest format-arguments)
  "Produce a formatted log message in nrt-thread controlled by
FORMAT-CONTROL and FORMAT-ARGUMENTS.

TYPE should be one of ERROR, WARN, INFO or DEBUG."
  `(%nrt-msg ',(ensure-symbol type "INCUDINE.UTIL")
             ,format-control ,@format-arguments))
