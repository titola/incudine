;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE.
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

(defpackage :incudine-system (:use :cl :asdf))
(in-package :incudine-system)

(defsystem "incudine"
  :version "0.1"
  :description "Incudine is a Music/DSP programming environment."
  :licence "GPL v2"
  :author "Tito Latini"
  :depends-on (:cffi :alexandria :bordeaux-threads :trivial-garbage)
  :components
  ((:module "contrib/cl-sndfile"
    :serial t
    :components
    ((:file "package")
     (:file "error")
     (:file "cffi-sndfile")
     (:file "sndfile")))
   (:module "contrib/cl-portmidi"
    :serial t
    :components
    ((:file "package")
     (:file "error")
     (:file "cffi-portmidi")
     (:file "portmidi")))
   (:module "src"
    :depends-on ("contrib/cl-sndfile" "contrib/cl-portmidi")
    :components
    ((:file "packages")
     (:file "sample-type" :depends-on ("packages"))
     (:file "audio-driver" :depends-on ("packages"))
     (:file "config" :depends-on ("sample-type" "audio-driver"))
     (:file "logger" :depends-on ("edf-sched"))
     (:file "foreign" :depends-on ("config"))
     (:file "sbcl" :depends-on ("foreign"))
     (:file "pool" :depends-on ("foreign"))
     (:file "util" :depends-on ("pool" "sbcl"))
     (:file "sync-condition" :depends-on ("packages"))
     (:file "fifo" :depends-on ("util" "sync-condition"))
     (:file "edf-sched" :depends-on ("fifo"))
     (:file "time" :depends-on ("logger" "envelope"))
     (:file "int-hash" :depends-on ("util"))
     (:file "bus" :depends-on ("logger"))
     (:file "buffer" :depends-on ("gen/partials" "gen/envelope" "logger"))
     (:file "foreign-array" :depends-on ("util"))
     (:file "envelope" :depends-on ("util"))
     (:file "graph" :depends-on ("time" "logger" "int-hash" "envelope"))
     (:file "rt" :depends-on ("fifo" "bus" "graph"))
     (:file "nrt" :depends-on ("rt"))
     (:file "score" :depends-on ("nrt"))
     (:file "receiver" :depends-on ("vug/midi"))
     (:file "analysis/base" :depends-on ("time" "util" "gen/window"))
     (:file "analysis/fft" :depends-on ("analysis/base"))
     (:file "analysis/pvbuffer" :depends-on ("analysis/fft"))
     (:file "gen/envelope" :depends-on ("envelope"))
     (:file "gen/partials" :depends-on ("util"))
     (:file "gen/polynomial" :depends-on ("util"))
     (:file "gen/window" :depends-on ("util"))
     (:file "gen/random" :depends-on ("config" "foreign"))
     (:file "voicer/base" :depends-on ("vug/dsp"))
     (:file "voicer/midi" :depends-on ("voicer/base" "receiver"))
     (:file "vug/util" :depends-on ("fifo"))
     (:file "vug/vug" :depends-on ("vug/util" "buffer" "foreign-array" "graph"))
     (:file "vug/dsp" :depends-on ("vug/vug"))
     (:file "vug/codegen" :depends-on ("vug/dsp"))
     (:file "vug/util2" :depends-on ("vug/codegen"))
     (:file "vug/buffer" :depends-on ("vug/util2" "buffer" "analysis/base"))
     (:file "vug/in-out" :depends-on ("bus" "vug/util2"))
     (:file "vug/envelope" :depends-on ("gen/envelope" "vug/util2"))
     (:file "vug/oscillator" :depends-on ("vug/buffer"))
     (:file "vug/delay" :depends-on ("vug/buffer"))
     (:file "vug/filter" :depends-on ("vug/codegen"))
     (:file "vug/convolve" :depends-on ("vug/fft" "analysis/pvbuffer"))
     (:file "vug/moog-vcf" :depends-on ("vug/codegen"))
     (:file "vug/noise" :depends-on ("gen/random" "vug/codegen"))
     (:file "vug/chaos" :depends-on ("vug/util2"))
     (:file "vug/gendy" :depends-on ("vug/util2"))
     (:file "vug/pan" :depends-on ("vug/in-out"))
     (:file "vug/midi" :depends-on ("vug/codegen"))
     (:file "vug/mouse" :depends-on ("vug/util2" "vug/filter"))
     (:file "vug/fft" :depends-on ("gen/window" "analysis/fft" "vug/codegen"))
     (:file "vug/spectral" :depends-on ("vug/fft"))
     (:file "deprecated" :depends-on ("vug/in-out"))))
   (:file "src/save-core" :depends-on ("src"))
   (:static-file "COPYING")
   (:static-file "README")
   (:static-file "incudinerc-example")))
