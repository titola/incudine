;;;; -*- Mode: lisp -*-
;;;
;;; Copyright (c) 2013-2018 Tito Latini
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

(in-package :asdf-user)

(defsystem "cl-sndfile"
  :version "1.0"
  :description "Wrapper for libsndfile API"
  :licence "LGPL"
  :author "Tito Latini"
  :depends-on (:alexandria :cffi #-sbcl :trivial-garbage)
  :serial t
  :components ((:static-file "COPYING")
               (:file "package")
               (:file "error")
               (:file "cffi-sndfile")
               (:file "sndfile")))
