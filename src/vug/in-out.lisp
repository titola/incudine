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

(define-vug-macro out (&rest values)
  "Mix the VALUES into the output busses."
  `(progn
     ,@(loop for value in values for ch from 0
             collect `(incf (audio-out ,ch) (sample ,value)))
     (values)))

(defmacro frame-out (ptr channels &optional (offset 0) (scale 1) scale-type)
  "Mix CHANNELS values of a foreign array of samples PTR into the
output busses.

OFFSET is the array index of the first sample to write.

The samples are scaled by SCALE (1 by default).

If SCALE-TYPE is non-NIL, it is the (unquoted) type of SCALE."
  (with-gensyms (frm scl)
    `(with ((,frm ,ptr)
            (,scl ,scale))
       (declare (type frame ,frm)
                ,@(when scale-type `((type ,scale-type ,scl))))
       (out ,@(loop for i from offset below (+ offset channels)
                    collect `(* ,scl (frame-ref ,frm ,i)))))))

(defmacro %cout (&rest values)
  (if (cdr values)
      `(cond ,@(loop for value in values
                     for chan from 0
                     collect `((= current-channel ,chan)
                               ,(if (numberp value) (sample value) value)))
             (t +sample-zero+))
      `(sample ,(car values))))

(define-vug-macro cout (&rest values)
  "Mix the nth element of VALUES into the nth output bus if n is equal
to CURRENT-CHANNEL.

Example:

    (dsp! cout-test (freq amp pos)
      (foreach-channel (cout (pan2 (sine freq amp) pos))))"
  (let ((node-value `(%cout ,@values)))
    `(progn
       (incf (incudine:audio-out current-channel) ,node-value)
       (values))))

(define-vug-macro node-out (&rest values)
  "Mix the nth element of VALUES, scaled by the node-gain, into the
nth output bus if n is equal to CURRENT-CHANNEL."
  `(progn
     (initialize (setf (incudine::node-enable-gain-p (dsp-node)) t))
     (incf (audio-out current-channel)
           (* (mem-aref (incudine::node-gain-data (dsp-node)) 'sample)
              (%cout ,@values)))
     (values)))
