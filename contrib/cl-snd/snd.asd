;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for SND.
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

(in-package :cl-user)

(defpackage :snd-asd (:use #:cl #:asdf))

(in-package :snd-asd)

(defsystem "snd"
  :description "Interface to interact with the sound editor Snd."
  :version "1.0"
  :author "Tito Latini"
  :licence "GPL v2"
  :serial t
  :components ((:static-file "COPYING")
               (:file "package")
               (:file "sbcl" :if-feature :sbcl)
               (:file "snd")
               (:file "snd-incudine" :if-feature :incudine)))
