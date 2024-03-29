;;;; -*- Mode: lisp -*-
;;;
;;; ASDF system definition for cudere-clm.
;;;
;;; Copyright (c) 2017 Tito Latini
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

(defsystem "cudere-clm"
  :description "Incudine version of Bill Schottstaedt's CLM"
  :version "1.0"
  :author "Tito Latini"
  :licence "GPL v2"
  :depends-on (:incudine)
  :components
  ((:module "contrib/cudere-clm"
    :serial t
    :components
    ((:static-file "COPYING")
     (:file "package")
     (:file "config")
     (:file "util")
     (:file "fft")
     (:file "cudere-clm")
     (:file "score"))
    :perform (load-op :after (o c)
               (load (or (eval (find-symbol "*CLM-INIT*" :cudere-clm))
                         (system-relative-pathname
                           "cudere-clm" "contrib/cudere-clm/clm-init.lisp"))
                     :if-does-not-exist nil))))
  :in-order-to ((test-op (test-op "cudere-clm-tests"))))
