;;; Copyright (c) 2014 Tito Latini
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

(in-package :ladspa)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (cffi:foreign-symbol-pointer "dlopen")
    (cffi:define-foreign-library dl
      (:unix "libdl.so")
      (t (:default "libdl")))
    (cffi:use-foreign-library dl)))

(define-constant RTLD-LAZY          #x00001)
(define-constant RTLD-NOW           #x00002)
(define-constant RTLD-BINDING-MASK  #x00003)
(define-constant RTLD-NOLOAD        #x00004)
(define-constant RTLD-DEEPBIND      #x00008)
(define-constant RTLD-GLOBAL        #x00100)
(define-constant RTLD-LOCAL         #x00000)
(define-constant RTLD-NODELETE      #x01000)

(defvar *dlopen-default-flags* RTLD-NOW)
(declaim (type (unsigned-byte 16) *dlopen-default-flags*))

(cffi:defcfun "dlopen" :pointer (filename :string) (flag :int))

(cffi:defcfun "dlerror" :string)

(cffi:defcfun "dlsym" :pointer (handle :pointer) (symbol :string))

(cffi:defcfun "dlclose" :int (handle :pointer))
