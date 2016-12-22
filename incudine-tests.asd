;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE unit tests.
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

(defpackage :incudine-tests-system (:use :cl :asdf))
(in-package :incudine-tests-system)

(defsystem incudine-tests
  :depends-on (:incudine #+sbcl :sb-rt #-sbcl :rt #+sbcl :sb-md5)
  :components
  ((:module "tests"
    :components
    ((:file "packages")
     (:file "sbcl" :depends-on ("packages"))
     (:file "util" :depends-on ("sbcl"))
     (:file "logger" :depends-on ("util"))
     (:file "pool" :depends-on ("util"))
     (:file "fifo" :depends-on ("util"))
     (:file "edf" :depends-on ("util"))
     (:file "time" :depends-on ("util"))
     (:file "bus" :depends-on ("util"))
     (:file "buffer" :depends-on ("util"))
     (:file "tuning" :depends-on ("util"))
     (:file "quantize" :depends-on ("util"))
     (:file "foreign-array" :depends-on ("util"))
     (:file "envelope" :depends-on ("util"))
     (:file "midi" :depends-on ("util"))
     (:file "midifile" :depends-on ("envelope"))
     (:file "osc" :depends-on ("util"))
     (:file "gen-partials" :depends-on ("util"))
     (:file "gen-polynomial" :depends-on ("util"))
     (:file "gen-random" :depends-on ("util"))
     (:file "gen-window" :depends-on ("util"))
     (:file "play-function" :depends-on ("vug/base"))
     (:file "vug/base" :depends-on ("util"))
     (:file "vug/ugen-base" :depends-on ("vug/base"))
     (:file "vug/types" :depends-on ("vug/base"))
     (:file "vug/binding-lambda" :depends-on ("vug/base"))
     (:file "vug/initialize" :depends-on ("vug/base"))
     (:file "vug/with-or-without-follow" :depends-on ("vug/base"))
     (:file "vug/reinit" :depends-on ("vug/base"))
     (:file "vug/reducible" :depends-on ("vug/base"))
     (:file "vug/delete-var" :depends-on ("vug/base"))
     (:file "vug/vuglet" :depends-on ("vug/base"))
     (:file "vug/let-over-vug-1" :depends-on ("vug/base"))
     (:file "vug/let-over-vug-2" :depends-on ("vug/base"))
     (:file "vug/local-func-over-vug" :depends-on ("vug/base"))
     (:file "vug/shadow-local-functions" :depends-on ("vug/base"))
     (:file "vug/ugen-reinit" :depends-on ("vug/base"))
     (:file "vug/delay/delay-1" :depends-on ("vug/base"))
     (:file "vug/delay/delay-2" :depends-on ("vug/base"))
     (:file "vug/filter/sharp-t" :depends-on ("vug/base"))
     (:file "vug/filter/svf" :depends-on ("vug/base"))
     (:file "vug/filter/convolve" :depends-on ("vug/base"))
     (:file "vug/generator/osc" :depends-on ("vug/base"))
     (:file "vug/generator/oscr" :depends-on ("vug/base"))
     (:file "vug/generator/impulse" :depends-on ("vug/base"))
     (:file "vug/generator/sine" :depends-on ("vug/base"))
     (:file "rego/base" :depends-on ("vug/base"))
     (:file "rego/test" :depends-on ("rego/base"))))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :incudine-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :incudine-tests))))
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:run-tests) '#:incudine-tests) args)))
    (run-tests :compiled nil)
    (run-tests :compiled t)))
