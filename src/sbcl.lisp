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

(in-package :incudine.util)

(defvar *reduce-warnings*
  '(sb-ext:muffle-conditions sb-ext:compiler-note))

(defmacro reduce-warnings (&body body)
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((limit 1000))
    ;; Workaround for the current version of SBCL; see the FIXME
    ;; comment inside the definition of the function
    ;; INLINE-EXPANSION-OK in `sbcl/src/compiler/ir1util.lisp'
    (when (< sb-ext:*inline-expansion-limit* limit)
      (setf sb-ext:*inline-expansion-limit* limit))))

(defmacro compare-and-swap (place old new)
  `(sb-ext:compare-and-swap ,place ,old ,new))

(defmacro barrier ((kind) &body body)
  `(sb-thread:barrier (,kind) ,@body))

(defun thread-set-priority (thread priority)
  (when (bt:threadp thread)
    (incudine.external:pthread-set-priority
     (cffi:make-pointer (sb-thread::thread-os-thread thread))
     priority)
    thread))

(declaim (inline seed-from-random-state))
(defun seed-from-random-state (state)
  (aref (sb-kernel::random-state-state state) 3))

(declaim (inline seed-random-state))
(defun seed-random-state (&optional state)
  (setf *random-state* (sb-ext:seed-random-state state))
  (incudine.external::gsl-seed-random-state
   (if (numberp state)
       state
       (seed-from-random-state *random-state*)))
  (values))

(defmacro without-gcing (&body body)
  `(sb-sys:without-gcing ,@body))

(defmacro without-interrupts (&body body)
  `(sb-sys:without-interrupts ,@body))

(defmacro with-stop-for-gc-pending (&body body)
  `(when sb-kernel:*stop-for-gc-pending*
     ,@body))

;;; Used only in DOUBLE-FLOAT-EXPONENT (that it's unused)
(defmacro double-float-high-bits (x)
  `(sb-kernel:double-float-high-bits ,x))

;;; DEBUG

(defun get-bytes-consed-in (time)
  "Rough estimate of the bytes consed in TIME seconds."
  (let ((t0 #1=(sb-ext:get-bytes-consed)))
    (sleep time)
    (- #1# t0)))
