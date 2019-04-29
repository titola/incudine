;;; Copyright (c) 2018-2019 Tito Latini
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

(define-condition incudine-error (error) ()
  (:documentation "All types of Incudine error conditions inherit from
this condition."))

(define-condition incudine-simple-error (incudine-error simple-error) ()
  (:documentation "Subtype of INCUDINE-ERROR and SIMPLE-ERROR."))

(define-condition incudine-compile-error (incudine-simple-error) ()
  (:documentation "Signaled if there is an error during the
compilation of the Incudine library.

Subtype of INCUDINE-SIMPLE-ERROR."))

(define-condition incudine-memory-fault-error (incudine-simple-error) ()
  (:documentation "Signaled if there is an unhandled memory fault.

Subtype of INCUDINE-SIMPLE-ERROR."))

(define-condition incudine-storage-condition (storage-condition
                                              incudine-simple-error)
  ()
  (:documentation "Subtype of INCUDINE-SIMPLE-ERROR and STORAGE-CONDITION."))

(define-condition incudine-network-error (incudine-simple-error) ()
  (:documentation "Conditions that relate to networking interface
should inherit from this type.

Subtype of INCUDINE-SIMPLE-ERROR."))

(define-condition incudine-node-error (incudine-simple-error) ()
  (:documentation "Conditions that relate to the node tree should
inherit from this type.

Subtype of INCUDINE-SIMPLE-ERROR."))

(define-condition incudine-missing-arg (incudine-simple-error) ()
  (:documentation "Signaled if there is a missing argument.

Subtype of INCUDINE-SIMPLE-ERROR."))

(define-condition incudine-unknown-time-unit (incudine-error)
  ((name :initarg :name :reader time-unit-name))
  (:report (lambda (condition stream)
             (format stream "Unknown time unit ~S"
                     (time-unit-name condition))))
  (:documentation "Signaled if the time unit is unknown."))

(define-condition incudine-undefined-object (incudine-error)
  ((name :initarg :name :reader undefined-object-name)))

(define-condition incudine-undefined-vug (incudine-undefined-object) ()
  (:report (lambda (condition stream)
             (format stream "The VUG ~S is undefined."
                     (undefined-object-name condition))))
  (:documentation "Signaled if the virtual unit generator is undefined."))

(define-condition incudine-undefined-ugen (incudine-undefined-object) ()
  (:report (lambda (condition stream)
             (format stream "The UGEN ~S is undefined."
                     (undefined-object-name condition))))
  (:documentation "Signaled if the unit generator is undefined."))

(define-condition incudine-undefined-dsp (incudine-undefined-object) ()
  (:report (lambda (condition stream)
             (format stream "The DSP ~S is undefined."
                     (undefined-object-name condition))))
  (:documentation "Signaled if the DSP is undefined."))

(defmacro %simple-error (datum format-control &rest format-arguments)
  `(error ,datum :format-control ,format-control
          ,@(if format-arguments `(:format-arguments (list ,@format-arguments)))))

(defmacro incudine-error (format-control &rest format-arguments)
  "Signals an INCUDINE-SIMPLE-ERROR controlled by FORMAT-CONTROL
and FORMAT-ARGUMENTS."
  `(%simple-error 'incudine-simple-error ,format-control ,@format-arguments))

(defmacro foreign-alloc-error (format-control &rest format-arguments)
  `(%simple-error 'incudine-storage-condition ,format-control ,@format-arguments))

(defmacro network-error (format-control &rest format-arguments)
  `(%simple-error 'incudine-network-error ,format-control ,@format-arguments))

(defun incudine-missing-arg (datum)
  "Signals an INCUDINE-MISSING-ARG error."
  (%simple-error 'incudine-missing-arg datum))
