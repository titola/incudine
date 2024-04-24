;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE unit tests.
;;;
;;; Copyright (c) 2013-2023 Tito Latini
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

(defsystem incudine-tests
  :depends-on (:incudine #+sbcl :sb-rt #+sbcl :sb-md5
                         #-sbcl :rt    #-sbcl :md5)
  :components
  ((:module "tests"
    :components
    ((:file "packages")
     (:file "util" :depends-on ("packages"))
     (:file "logger" :depends-on ("util"))
     (:file "pool" :depends-on ("util"))
     (:file "fifo" :depends-on ("util"))
     (:file "edf" :depends-on ("vug/base"))
     (:file "graph" :depends-on ("vug/base"))
     (:file "time" :depends-on ("util"))
     (:file "string" :depends-on ("util"))
     (:file "bus" :depends-on ("util"))
     (:file "channels" :depends-on ("vug/base"))
     (:file "buffer" :depends-on ("util"))
     (:file "soundfile" :depends-on ("util"))
     (:file "foreign-ringbuffer" :depends-on ("util"))
     (:file "tuning" :depends-on ("util"))
     (:file "quantize" :depends-on ("util"))
     (:file "foreign-array" :depends-on ("util"))
     (:file "envelope" :depends-on ("util"))
     (:file "midi" :depends-on ("util"))
     (:file "midifile" :depends-on ("envelope"))
     (:file "osc" :depends-on ("util"))
     (:file "gen-partials" :depends-on ("util"))
     (:file "gen-envelope" :depends-on ("util"))
     (:file "gen-polynomial" :depends-on ("util"))
     (:file "gen-random" :depends-on ("util"))
     (:file "gen-window" :depends-on ("util"))
     (:file "gen-filter" :depends-on ("util"))
     (:file "play-function" :depends-on ("vug/base"))
     (:file "analysis/ring-buffer" :depends-on ("util"))
     (:file "analysis/fft" :depends-on ("util"))
     (:file "vug/base" :depends-on ("util"))
     (:file "vug/ugen-base" :depends-on ("vug/base"))
     (:file "vug/types" :depends-on ("vug/base"))
     (:file "vug/variables" :depends-on ("vug/base"))
     (:file "vug/binding-lambda" :depends-on ("vug/base"))
     (:file "vug/metadata" :depends-on ("vug/base"))
     (:file "vug/initialize" :depends-on ("vug/base"))
     (:file "vug/dsp-node" :depends-on ("vug/base"))
     (:file "vug/control-pointer" :depends-on ("vug/base"))
     (:file "vug/defaults" :depends-on ("vug/base"))
     (:file "vug/foreach-frame" :depends-on ("vug/base"))
     (:file "vug/with-or-without-follow" :depends-on ("vug/base"))
     (:file "vug/reinit" :depends-on ("vug/base"))
     (:file "vug/reducible" :depends-on ("vug/base"))
     (:file "vug/conditional-expansion" :depends-on ("vug/base"))
     (:file "vug/delete-var" :depends-on ("vug/base"))
     (:file "vug/param-side-effects" :depends-on ("vug/base"))
     (:file "vug/frame-slot-names" :depends-on ("vug/base"))
     (:file "vug/vuglet" :depends-on ("vug/base"))
     (:file "vug/non-local-exit" :depends-on ("vug/base"))
     (:file "vug/lambda" :depends-on ("vug/base"))
     (:file "vug/let-over-vug-1" :depends-on ("vug/base"))
     (:file "vug/let-over-vug-2" :depends-on ("vug/base"))
     (:file "vug/local-func-over-vug" :depends-on ("vug/base"))
     (:file "vug/local-time" :depends-on ("vug/base"))
     (:file "vug/shadow-local-functions" :depends-on ("vug/base"))
     (:file "vug/ugen-reinit" :depends-on ("vug/ugen-base"))
     (:file "vug/free-hook" :depends-on ("vug/ugen-base"))
     (:file "vug/dsp-from-dsp" :depends-on ("vug/base"))
     (:file "vug/recursion" :depends-on ("vug/base"))
     (:file "vug/node-out" :depends-on ("vug/base"))
     (:file "vug/delay/delay-1" :depends-on ("vug/base"))
     (:file "vug/delay/delay-2" :depends-on ("vug/base"))
     (:file "vug/with-buffer" :depends-on ("vug/ugen-base"))
     (:file "vug/envelope" :depends-on ("vug/ugen-base"))
     (:file "vug/voicer" :depends-on ("vug/base"))
     (:file "vug/profile" :depends-on ("vug/base"))
     (:file "vug/amplitude/balance" :depends-on ("vug/base"))
     (:file "vug/filter/allpass" :depends-on ("vug/base"))
     (:file "vug/filter/reson" :depends-on ("vug/base"))
     (:file "vug/filter/sharp-t" :depends-on ("vug/base"))
     (:file "vug/filter/svf" :depends-on ("vug/base"))
     (:file "vug/filter/convolve" :depends-on ("vug/base"))
     (:file "vug/generator/osc" :depends-on ("vug/base"))
     (:file "vug/generator/oscr" :depends-on ("vug/base"))
     (:file "vug/generator/impulse" :depends-on ("vug/base"))
     (:file "vug/generator/sine" :depends-on ("vug/base"))
     (:file "vug/parallel-nrt" :depends-on ("vug/generator/osc"))
     (:file "rego/base" :depends-on ("vug/base"))
     (:file "rego/test" :depends-on ("rego/base"))
     (:file "rego/string-test" :depends-on ("rego/base"))
     (:static-file "data.raw")
     (:static-file "data.txt")
     (:static-file "rego/include-1.rego")
     (:static-file "rego/include-2.rego")
     (:static-file "rego/include-3.rego")
     (:static-file "rego/include-loop-1.rego")
     (:static-file "rego/include-loop-2.rego")
     (:static-file "rego/jump-1.rego")
     (:static-file "rego/jump-2.rego")
     (:static-file "rego/loop-1.rego")
     (:static-file "rego/org-mode.rego")
     (:static-file "rego/paral-1.rego")
     (:static-file "rego/paral-2.rego")
     (:static-file "rego/t1.rego")
     (:static-file "rego/t2.rego")
     (:static-file "rego/test-1.sco")
     (:static-file "rego/test-2.sco"))))
  :perform (test-op (o c) (symbol-call '#:incudine-tests '#:run-tests)))
