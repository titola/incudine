;;; Copyright (c) 2013 Tito Latini
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

(define-condition sndfile-error (simple-error) ())

(define-condition allocation-error (sndfile-error)
  ((object-type :reader object-type-of :initarg :object-type))
  (:report (lambda (condition stream)
             (cl:format stream "Failed object allocation for ~A."
                        (object-type-of condition)))))

(define-condition error-generic (sndfile-error)
  ((errno :reader errno :initarg :errno))
  (:report (lambda (condition stream)
             (princ (error-number (errno condition))
                    stream))))

(defmacro allocation-error (obj-type)
  `(cl:error 'allocation-error :object-type ,obj-type))

(defmacro error-generic (errno)
  `(cl:error 'error-generic :errno ,errno))
