;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE-SND
;;;
;;; Copyright (c) 2015 Tito Latini
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

(defpackage :incudine-snd-system (:use :cl :asdf))
(in-package :incudine-snd-system)

(defsystem "incudine-snd"
  :version "1.0"
  :description "Interface to interact with the sound editor Snd."
  :licence "GPL v2"
  :author "Tito Latini"
  :depends-on (:incudine)
  :components
  ((:module "contrib/cl-snd"
    :serial t
    :components
    ((:static-file "COPYING")
     (:file "package")
     (:file "sbcl" :if-feature :sbcl)
     (:file "snd")
     (:file "snd-incudine")))))
