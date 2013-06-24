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

(defmacro %out (&rest values)
  (if (cdr values)
      `(cond ,@(loop for value in values
                     for chan from 0
                     collect `((= current-channel ,chan)
                               ,(if (numberp value)
                                    (coerce value 'sample)
                                    value)))
             (t 0.0d0))
      `(coerce ,(car values) 'sample)))

(define-vug-macro out (&rest values)
  (let ((node-value `(%out ,@values)))
    `(progn
       ,(when incudine:*node-enable-gain-p*
          `(initialize
            (setf (incudine::node-enable-gain-p (synth-node)) t)))
       (incf (incudine:audio-out current-channel)
             ,(if incudine:*node-enable-gain-p*
                  `(* (mem-aref
                       (incudine::node-gain-data (synth-node)) 'sample)
                      ,node-value)
                  node-value)))))

(define-vug-macro node-out (&rest values)
  `(progn
     (initialize
      (setf (incudine::node-enable-gain-p (synth-node)) t))
     (incf (audio-out current-channel)
           (* (mem-aref
               (incudine::node-gain-data (synth-node)) 'sample)
              (%out ,@values)))))
