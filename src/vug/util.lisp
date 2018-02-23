;;; Copyright (c) 2013-2018 Tito Latini
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
   '(incudine:incudine-error
     incudine:now
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
     incudine:tuning
     incudine:tuning-data
     incudine:smp-ref
     incudine:*sine-table*
     incudine:*cosine-table*
     incudine:foreign-array
     incudine:make-foreign-array
     incudine:foreign-array-data
     incudine::apply-sample-coerce
     incudine::node-controls
     incudine.util::declare-form-p
     incudine.util::separate-declaration
     incudine.util::force-sample-format
     incudine.external:foreign-alloc-sample
     incudine.external:foreign-zero-sample
     incudine.external:mouse-event
     incudine.external:mouse-init
     incudine.external:mouse-loop-start
     incudine.external:mouse-stop
     incudine.external:get-mouse-status
     incudine.external:complex-to-polar
     incudine.external:polar-to-complex)
   (find-package :incudine.vug)))

(defun dummy-function (&rest rest)
  (declare (ignore rest))
  (values))

(declaim (inline ctz))
(defun ctz (x)
  "Count Trailing Zeroes."
  (let ((num-zeros 0))
    (declare (type (integer 0 64) num-zeros))
    (unless (zerop x)
      (do ((n x (ash n -1)))
          ((not (zerop (logand n 1))) num-zeros)
        (declare (type non-negative-fixnum n))
        (incf num-zeros)))
    num-zeros))

(defmacro samples-zero (&rest sample-vars)
  `(progn ,@(mapcar (lambda (var) `(setf ,var +sample-zero+)) sample-vars)))

(defmacro vug-format-symbol (control &rest args)
  `(format-symbol :incudine.vug ,control ,@args))

(declaim (inline foreign-sample-type-p))
(defun foreign-sample-type-p (type)
  (member type '(sample positive-sample negative-sample
                 non-negative-sample non-positive-sample)))

(declaim (inline foreign-symbol-type-p))
(defun foreign-symbol-type-p (foreign-type name)
  (string= name (symbol-name (if (consp foreign-type)
                                 (car foreign-type)
                                 foreign-type))))

(declaim (inline foreign-int-p))
(defun foreign-*-p (type symbol-names)
  (and (atom type)
       (member (symbol-name type) symbol-names :test #'string=)
       t))

(declaim (inline foreign-float-type-p))
(defun foreign-float-type-p (type)
  (foreign-*-p type '("F32" "FOREIGN-FLOAT")))

(declaim (inline foreign-double-type-p))
(defun foreign-double-type-p (type)
  (foreign-*-p type '("F64" "FOREIGN-DOUBLE")))

(declaim (inline signed-or-unsigned-byte-p))
(defun signed-or-unsigned-byte-p (sym)
  (and (member sym '(signed-byte unsigned-byte)) t))

(declaim (inline foreign-int32-type-p))
(defun foreign-int32-type-p (type)
  (reduce-warnings
    (cond ((consp type)
           (when (< incudine.util::n-fixnum-bits 32)
             (and (signed-or-unsigned-byte-p (car type))
                  (= (cadr type) 32))))
          (t (or (foreign-*-p type '("I32" "INT32" "U32" "UINT32"))
                 (when (< incudine.util::n-fixnum-bits 32)
                   (signed-or-unsigned-byte-p type)))))))

(declaim (inline foreign-int64-type-p))
(defun foreign-int64-type-p (type)
  (reduce-warnings
    (cond ((consp type)
           (and (signed-or-unsigned-byte-p (car type))
                (= (cadr type) 64)))
          (t (or (foreign-*-p type '("I64" "INT64" "U64" "UINT64"))
                 (and (> incudine.util::n-fixnum-bits 32)
                      (signed-or-unsigned-byte-p type)))))))

(declaim (inline foreign-number-type-p))
(defun foreign-number-type-p (type)
  (or (foreign-sample-type-p type)
      (foreign-float-type-p type)
      (foreign-double-type-p type)
      (foreign-int32-type-p type)
      (foreign-int64-type-p type)))

(declaim (inline foreign-pointer-type-p))
(defun foreign-pointer-type-p (type)
  (foreign-*-p type '("FOREIGN-POINTER" "FRAME" "POINTER" "PTR")))

(declaim (inline foreign-type-p))
(defun foreign-type-p (type)
  (or (foreign-number-type-p type)
      (foreign-pointer-type-p type)))

(declaim (inline foreign-non-sample-type-p))
(defun foreign-non-sample-type-p (type)
  (or (foreign-float-type-p type)
      (foreign-double-type-p type)
      (foreign-int32-type-p type)
      (foreign-int64-type-p type)
      (foreign-pointer-type-p type)))
