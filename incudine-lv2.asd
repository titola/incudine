;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE-LV2
;;;
;;; Copyright (c) 2013 Tito Latini
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

(defpackage :incudine-lv2-system (:use :cl :asdf))
(in-package :incudine-lv2-system)

(defsystem "incudine-lv2"
  :version "1.0"
  :description "LV2 audio plugin interface for Incudine."
  :licence "GPL v2"
  :author "Tito Latini"
  :depends-on (:incudine)
  :components
  ((:module "contrib/cl-lilv"
    :serial t
    :components
    ((:file "package")
     (:file "error")
     (:file "cffi-lv2")
     (:file "cffi-lilv")
     (:file "lilv")))
   (:module "src"
    :depends-on ("contrib/cl-lilv")
    :serial t
    :components
    ((:file "vug/lv2")))))
