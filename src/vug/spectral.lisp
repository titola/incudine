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

(define-vug centroid ((abuf abuffer))
  "Compute the spectral centroid using moments."
  (with-samples ((nbins-recip (/ (sample (abuffer-nbins abuf))))
                 num denom)
    (setf num +sample-zero+ denom +sample-zero+)
    (dofft-polar (i nbins ((compute abuf)) ()
                  :result (if (zerop denom)
                              (sample 0.5)
                              (/ (* num nbins-recip) denom)))
      (incf num (* i mag0))
      (incf denom mag0))))
