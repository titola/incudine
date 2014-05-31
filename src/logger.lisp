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

(in-package :incudine.util)

(defconstant +logger-error+  1)
(defconstant +logger-warn+   2)
(defconstant +logger-info+   4)
(defconstant +logger-debug+  8)

(defconstant +logger-error-mask+ #b00000001)
(defconstant +logger-warn-mask+  #b00000011)
(defconstant +logger-info-mask+  #b00000111)
(defconstant +logger-debug-mask+ #b00001111)

(defvar *logger-mask* +logger-warn-mask+)
(declaim (type (integer 0 15) *logger-mask*))

(defvar *logger-time* nil)
(declaim (type (member :sec :samp nil) *logger-time*))

(defvar *logger-stream* *error-output*)
(declaim (type stream *logger-stream*))

(defvar *logger-force-output* t)
(declaim (type boolean *logger-force-output*))

(declaim (inline logger-level))
(defun logger-level ()
  (cond ((= *logger-mask* +logger-error-mask+) :error)
        ((= *logger-mask* +logger-warn-mask+) :warn)
        ((= *logger-mask* +logger-info-mask+) :info)
        (t :debug)))

(declaim (inline set-logger-level))
(defun set-logger-level (level)
  (declare (type (member :error :warn :info :debug) level))
  (setf *logger-mask*
        (case level
          (:error +logger-error-mask+)
          (:warn  +logger-warn-mask+)
          (:info  +logger-info-mask+)
          (otherwise +logger-debug-mask+)))
  level)

(defsetf logger-level set-logger-level)

(declaim (inline logger-time))
(defun logger-time () *logger-time*)

(declaim (inline set-logger-time))
(defun set-logger-time (unit)
  (declare (type (member :sec :samp nil) unit))
  (setf *logger-time* unit))

(defsetf logger-time set-logger-time)

(declaim (inline default-logger-time-function))
(defun default-logger-time-function ()
  (let ((seconds-p (eq *logger-time* :sec)))
    (format *logger-stream* "~:[~,1F~;~,3F~] " seconds-p
            (if seconds-p
                (* (incudine:now) *sample-duration*)
                (incudine:now)))))

(defvar *logger-time-function* #'default-logger-time-function)
(declaim (type function  *logger-time-function*))

(declaim (inline logger-time-function))
(defun logger-time-function () *logger-time-function*)

(declaim (inline set-logger-time-function))
(defun set-logger-time-function (function)
  (declare (type (or function null) function))
  (setf *logger-time-function*
        (or function #'default-logger-time-function)))

(defsetf logger-time-function set-logger-time-function)

(defmacro logger-active-p (type)
  `(plusp (logand ,(alexandria:format-symbol :incudine.util
                                             "+LOGGER-~A+" type)
                  *logger-mask*)))

(defmacro msg (type &rest rest)
  `(when (logger-active-p ,type)
     (when *logger-time* (funcall *logger-time-function*))
     ,(unless (eq type 'info)
        `(princ ,(format nil "~A: " type) *logger-stream*))
     (format *logger-stream* ,@rest)
     (terpri *logger-stream*)
     (when *logger-force-output* (force-output *logger-stream*))
     nil))

(defmacro nrt-msg (type &rest rest)
  `(progn
     (when (logger-active-p ,type)
       (incudine.edf:at #.+sample-zero+
                        (lambda ()
                          (incudine:nrt-funcall (lambda ()
                                                  (msg ,type ,@rest)))))
       nil)))
