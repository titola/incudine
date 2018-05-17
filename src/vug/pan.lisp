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

(define-vug stereo (input)
  "Mix the INPUT into the first two output busses."
  (out input input))

(define-vug pan2 (in pos)
  "Stereo equal power panpot with position POS between 0 (left)
and 1 (right)."
  (with-samples ((alpha (* +half-pi+ pos))
                 (left (cos alpha))
                 (right (sin alpha)))
    (cond ((= current-channel 0) (* left in))
          ((= current-channel 1) (* right in))
          (t +sample-zero+))))

(define-vug fpan2 (in pos)
  "Fast stereo equal power panpot with position POS between 0 (left)
and 1 (right)."
  (with ((tabsize (ash (buffer-size *sine-table*) -2))
         (sintab (buffer-data *sine-table*))
         (costab (buffer-data *cosine-table*))
         (index (clip (sample->fixnum (* tabsize pos)) 0 tabsize))
         (left (smp-ref costab index))
         (right (smp-ref sintab index)))
    (declare (type non-negative-fixnum tabsize index)
             (type sample left right))
    (cond ((= current-channel 0) (* left in))
          ((= current-channel 1) (* right in))
          (t +sample-zero+))))
