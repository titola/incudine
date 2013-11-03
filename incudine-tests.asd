;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for INCUDINE unit tests.
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

(defpackage :incudine-tests-system (:use :cl :asdf))
(in-package :incudine-tests-system)

(defsystem incudine-tests
  :depends-on (:incudine #+sbcl :sb-rt #-sbcl :rt)
  :components ((:module "tests"
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "logger")
                             (:file "pool")
                             (:file "fifo")
                             (:file "edf")
                             (:file "time")
                             (:file "bus")
                             (:file "buffer")
                             (:file "foreign-array")
                             (:file "envelope")
                             (:file "gen-partials")
                             (:file "gen-polynomial")
                             (:file "gen-random")
                             (:file "gen-window")))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :incudine-tests))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :incudine-tests))))
  (flet ((run-tests (&rest args)
           (apply (intern (string '#:run-tests) '#:incudine-tests) args)))
    (run-tests :compiled nil)
    (run-tests :compiled t)))
