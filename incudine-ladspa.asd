;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE-LADSPA
;;;
;;; Copyright (c) 2014-2017 Tito Latini
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

(in-package :asdf-user)

(defsystem "incudine-ladspa"
  :version "1.0"
  :description "LADSPA audio plugin interface for Incudine."
  :licence "GPL v2"
  :author "Tito Latini"
  :depends-on (:incudine)
  :components
  ((:module "contrib/cl-ladspa"
    :serial t
    :components
    ((:static-file "COPYING")
     (:file "package")
     (:file "cffi-ladspa")
     (:file "dlopen")
     (:file "ladspa")))
   (:module "src"
    :depends-on ("contrib/cl-ladspa")
    :components
    ((:file "vug/ladspa")))))
