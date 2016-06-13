;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE.
;;;
;;; Copyright (c) 2013-2016 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(load-time-value
            (merge-pathnames "src/features.lisp"
                             (or *compile-file-pathname* *load-pathname*)))))

(in-package :asdf-user)

(defvar *incudine-force-compile-p* nil)

(defmethod perform :before ((o load-op) (c cl-source-file))
  (when *incudine-force-compile-p*
    (compile-file* (component-pathname c))))

(defsystem "incudine"
  :version "0.9.3"
  :description "Incudine is a Music/DSP programming environment."
  :licence "GPL v2"
  :author "Tito Latini"
  :depends-on (:alexandria :bordeaux-threads :cffi :swap-bytes :trivial-garbage)
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
     (:file "compile-clib" :depends-on ("packages"))
     (:module "clib" :depends-on ("compile-clib")
      :pathname ""
      :components ((:static-file "common.h") (:static-file "util.c")
                   (:static-file "mouse.c") (:static-file "mouse.h")
                   (:static-file "nothing.c")
                   (:static-file "rtjack.c") (:static-file "rtjack.h")
                   (:static-file "rtpa.c") (:static-file "rtpa.h")
                   (:static-file "osc/osc.c") (:static-file  "osc/osc.h")
                   (:static-file "cache.lisp"))
      :output-files
      (compile-op (o c)
        (let ((name #-cygwin "libincudine" #+cygwin "cygincudine-0")
              (type (asdf/bundle:bundle-pathname-type :shared-library)))
          (values (list (system-relative-pathname "incudine" (strcat "src/" name)
                                                  :type type))
                  t)))
      :perform (compile-op (o c)
                 (symbol-call :incudine.config '#:compile-c-library)
                 #+(and sbcl x86 (not darwin))
                 (symbol-call :incudine.config '#:fftw-stack-align-test))
      :perform (load-op (o c)
                 (when (symbol-call :incudine.config '#:changed-compiler-options)
                   (setf *incudine-force-compile-p* t)
                   (symbol-call :incudine.config '#:compile-c-library))
                 (symbol-call :cffi '#:load-foreign-library
                              (output-file 'compile-op c))))
     (:file "config" :depends-on ("compile-clib"))
     (:file "logger" :depends-on ("edf-sched"))
     (:file "foreign" :depends-on ("config" "clib"))
     (:file "sbcl" :depends-on ("foreign"))
     (:file "spinlock" :depends-on ("sbcl"))
     (:file "pool" :depends-on ("spinlock"))
     (:file "util" :depends-on ("pool" "sbcl"))
     (:file "sync-condition" :depends-on ("packages"))
     (:file "fifo" :depends-on ("util" "sync-condition"))
     (:file "edf-sched" :depends-on ("fifo"))
     (:file "time" :depends-on ("envelope"))
     (:file "int-hash" :depends-on ("util"))
     (:file "bus" :depends-on ("logger"))
     (:file "buffer" :depends-on ("gen/partials" "gen/envelope"))
     (:file "tuning" :depends-on ("buffer"))
     (:file "foreign-array" :depends-on ("util"))
     (:file "envelope" :depends-on ("logger" "foreign-array"))
     (:file "graph" :depends-on ("time" "logger" "int-hash"))
     (:file "node-pool" :depends-on ("graph"))
     (:file "rt" :depends-on ("fifo" "bus" "graph"
                                     #+jack-audio "jack"
                                     #+jack-midi "jackmidi"
                                     #+portaudio "portaudio"))
     (:file "nrt" :depends-on ("rt"))
     (:file "score" :depends-on ("nrt"))
     (:file "midi" :depends-on ("edf-sched" "tuning" #+jack-midi "jackmidi"))
     (:file "jack" :if-feature :jack-audio :depends-on ("foreign"))
     (:file "jackmidi" :if-feature :jack-midi :depends-on ("fifo" "receiver"))
     (:file "portaudio" :if-feature :portaudio :depends-on ("foreign"))
     (:file "receiver" :depends-on ("vug/midi" "osc/osc"))
     (:file "osc/sbcl-vops" :if-feature (:and :sbcl (:or :x86 :x86-64))
                            :depends-on ("packages"))
     (:file "osc/cffi-osc" :depends-on ("packages"))
     (:file "osc/osc" :depends-on ("util" "osc/cffi-osc" "osc/sbcl-vops"))
     (:file "analysis/maybe-fftw-no-simd"
            :if-feature (:and :sbcl :x86 (:not :darwin))
            :depends-on ("packages"))
     (:file "analysis/base" :depends-on ("time" "gen/window"
                                         #+(and sbcl x86 (not darwin))
                                         "analysis/maybe-fftw-no-simd"))
     (:file "analysis/fft" :depends-on ("analysis/base"))
     (:file "analysis/pvbuffer" :depends-on ("analysis/fft"))
     (:file "gen/envelope" :depends-on ("envelope"))
     (:file "gen/partials" :depends-on ("foreign-array"))
     (:file "gen/polynomial" :depends-on ("foreign-array"))
     (:file "gen/window" :depends-on ("foreign" "foreign-array"))
     (:file "gen/random" :depends-on ("foreign"))
     (:file "gen/analysis" :depends-on ("analysis/fft"))
     (:file "voicer/base" :depends-on ("vug/dsp"))
     (:file "voicer/event" :depends-on ("voicer/base" "receiver"))
     (:file "voicer/midi" :depends-on ("voicer/event" "vug/midi"))
     (:file "vug/util" :depends-on ("fifo"))
     (:file "vug/vug" :depends-on ("vug/util" "buffer" "foreign-array" "node-pool"))
     (:file "vug/dsp" :depends-on ("vug/vug"))
     (:file "vug/codegen" :depends-on ("vug/dsp"))
     (:file "vug/ugen" :depends-on ("vug/codegen"))
     (:file "vug/util2" :depends-on ("vug/codegen"))
     (:file "vug/buffer" :depends-on ("vug/util2" "buffer" "analysis/base"))
     (:file "vug/in-out" :depends-on ("bus" "vug/util2"))
     (:file "vug/envelope" :depends-on ("gen/envelope" "vug/util2"))
     (:file "vug/oscillator" :depends-on ("vug/buffer" "vug/filter"))
     (:file "vug/delay" :depends-on ("vug/buffer"))
     (:file "vug/filter" :depends-on ("vug/codegen"))
     (:file "vug/convolve" :depends-on ("vug/fft" "analysis/pvbuffer"))
     (:file "vug/moog-vcf" :depends-on ("vug/codegen"))
     (:file "vug/noise" :depends-on ("gen/random" "vug/codegen"))
     (:file "vug/chaos" :depends-on ("vug/util2"))
     (:file "vug/gendy" :depends-on ("vug/util2"))
     (:file "vug/pan" :depends-on ("vug/in-out"))
     (:file "vug/note-priority" :depends-on ("vug/codegen"))
     (:file "vug/midi" :depends-on ("tuning" "vug/note-priority"))
     (:file "vug/mouse" :depends-on ("vug/util2" "vug/filter"))
     (:file "vug/fft" :depends-on ("gen/window" "analysis/fft" "vug/codegen"))
     (:file "vug/spectral" :depends-on ("vug/fft"))
     (:file "vug/foreign-plugin" :depends-on ("vug/vug"))
     (:file "deprecated" :depends-on ("vug/in-out"))))
   (:file "src/save-core" :depends-on ("src"))
   (:static-file "COPYING")
   (:static-file "README")
   (:static-file "INSTALL")
   (:static-file "incudinerc-example")
   (:static-file "src/features.lisp")))
