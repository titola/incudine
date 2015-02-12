;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for FLUIDSYNTH.
;;;
;;; Copyright (c) 2015 Tito Latini
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

(in-package :cl-user)

(defpackage :fluidsynth-asd (:use #:cl #:asdf))

(in-package :fluidsynth-asd)

(defsystem "fluidsynth"
  :description "Wrapper for FluidSynth SoundFont synthesizer"
  :version "1.0"
  :author "Tito Latini"
  :licence "LGPL"
  :depends-on (:cffi :alexandria :trivial-garbage)
  :serial t
  :components ((:static-file "COPYING")
               (:file "package")
               (:file "cffi-fluidsynth")
               (:file "fluidsynth")
               (:file "tuning" :if-feature :incudine)))
