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

(in-package :incudine.vug)

(defmacro direct-conv-loop (data kernel kernel-pos-var sum-var
                            &key (start 0) end)
  (with-gensyms (index)
    `(do ((,index ,start (1+ ,index)))
         ((>= ,index ,end))
       (declare (type non-negative-fixnum ,index))
       (incf ,sum-var (* (data-ref ,data ,index)
                         (data-ref ,kernel ,kernel-pos-var)))
       (incf ,kernel-pos-var))))

(define-vug direct-convolve (in (buf buffer))
  "Direct convolution of an input with a finite impulse response
stored in a buffer."
  (with ((kernel (buffer-data buf))
         (size (buffer-size buf))
         (array-wrap (make-foreign-array size 'sample :zero-p t))
         (data (foreign-array-data array-wrap))
         (pos 0)
         (kernel-pos 0)
         (sum +sample-zero+))
    (declare (type fixnum pos kernel-pos) (type sample sum))
    (setf (data-ref data pos) in)
    (setf sum (* in (data-ref kernel 0)))
    (setf kernel-pos 1)
    (direct-conv-loop data kernel kernel-pos sum :start (1+ pos) :end size)
    (direct-conv-loop data kernel kernel-pos sum :end pos)
    (setf pos (1- (if (zerop pos) size pos)))
    sum))
