;;;; -*- Mode: lisp -*-
;;;
;;; Copyright (c) 2013-2017 Tito Latini
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

(defsystem "lilv"
  :description "Wrapper for Lilv, a C API for using LV2 plugins"
  :version "1.0"
  :author "Tito Latini"
  :licence "LGPL"
  :depends-on (cffi alexandria trivial-garbage)
  :serial t
  :components ((:static-file "COPYING")
               (:file "package")
               (:file "error")
               (:file "cffi-lv2")
               (:file "cffi-lilv")
               (:file "lilv")))
