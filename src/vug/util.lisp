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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:now
     incudine:audio-in
     incudine:audio-out
     incudine:bus
     incudine.edf:at
     incudine:nrt-funcall
     incudine:buffer
     incudine:buffer-p
     incudine:buffer-data
     incudine:buffer-size
     incudine:buffer-frames
     incudine:buffer-channels
     incudine:buffer-sample-rate
     incudine:buffer-value
     incudine:buffer-mask
     incudine:buffer-lobits
     incudine:buffer-lomask
     incudine:buffer-lodiv
     incudine:size
     incudine:data-ref
     incudine:*sine-table*
     incudine:*cosine-table*
     incudine:foreign-array
     incudine:make-foreign-array
     incudine:foreign-array-data
     incudine.external:foreign-alloc-sample
     incudine.external:foreign-zero-sample
     incudine.external:foreign-copy
     incudine.external:mouse-event
     incudine.external:mouse-init
     incudine.external:mouse-loop-start
     incudine.external:mouse-stop
     incudine.external:get-mouse-status
     incudine.external:complex-to-polar
     incudine.external:polar-to-complex)
   (find-package :incudine.vug))
  #+sbcl
  (progn
    ;; Avoid the boring notes from sbcl-1.8
    (declaim (inline ash))
    (defun ash (integer count)
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (cl:ash integer count))))

(defun dummy-function (&rest rest)
  (declare (ignore rest))
  (values))

;;; Count Trailing Zeroes.
;;; Not an inlined function because it is used inside a VUG, where the
;;; argument X is possibly a foreign integer to reduce the consing
;;; (see PINK-NOISE in `vug/noise.lisp')
(defmacro ctz (x)
  (with-gensyms (num-zeros n)
    `(let ((,num-zeros 0))
       (declare (type (integer 0 64) ,num-zeros))
       (unless (zerop ,x)
         (do ((,n ,x (ash ,n -1)))
             ((not (zerop (logand ,n 1))) ,num-zeros)
           (declare (type non-negative-fixnum ,n))
           (incf ,num-zeros)))
       ,num-zeros)))

(defmacro frame-ref (frame channel)
  `(mem-ref ,frame 'sample (the non-negative-fixnum
                             (* ,channel +foreign-sample-size+))))

(defun apply-sample-coerce (form)
  (if (atom form)
      (cond ((and (numberp form) (floatp form))
             (coerce form 'sample))
            ((eq form 'pi) '(coerce pi 'sample))
            (t form))
      (cons (apply-sample-coerce (car form))
            (apply-sample-coerce (cdr form)))))
