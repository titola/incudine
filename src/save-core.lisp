;;; Copyright (c) 2013-2014 Tito Latini
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

(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *core-init-function*
    (lambda ()
      (set-sample-rate (sample (or incudine.config::*sample-rate* 48000)))
      (set-sound-velocity (sample (or incudine.config::*sound-velocity* 345)))
      ;; gsl random
      (setf incudine.external::*gsl-random-generator*
            (incudine.external::make-gsl-rng))
      ;; pool
      (setf incudine.util::*foreign-sample-pool*
            (foreign-alloc :char
                           :count incudine.util::*foreign-sample-pool-size*))
      (setf incudine.util::*foreign-rt-memory-pool*
            (foreign-alloc :char
                           :count incudine.util::*foreign-rt-memory-pool-size*))
      (setf incudine.util::*foreign-nrt-memory-pool*
            (foreign-alloc :char
                           :count incudine.util::*foreign-nrt-memory-pool-size*))
      (setf incudine.util::*initialized-foreign-memory-pools* nil)
      ;; edf
      (setf incudine.edf::*heap-size*
            (if (and (numberp *rt-edf-heap-size*)
                     (not (power-of-two-p *rt-edf-heap-size*)))
                (next-power-of-two *rt-edf-heap-size*)
                1024))
      (setf incudine.edf::*heap*
            (make-array incudine.edf::*heap-size*
              :element-type 'incudine.edf::node
              :initial-contents (loop repeat incudine.edf::*heap-size*
                                   collect (incudine.edf::make-node))))
      (setf *nrt-edf-heap-size*
            (if (and (numberp *nrt-edf-heap-size*)
                     (not (power-of-two-p *nrt-edf-heap-size*)))
                (next-power-of-two *nrt-edf-heap-size*)
                65536))
      (setf *nrt-heap*
        (make-array *nrt-edf-heap-size*
                    :element-type 'incudine.edf::node
                    :initial-contents (loop repeat *nrt-edf-heap-size*
                                            collect (incudine.edf::make-node))))
      ;; bus
      (setf *bus-channels* (foreign-alloc-sample *bus-channels-size*))
      (setf *input-pointer*
            (inc-pointer *bus-channels* (* *number-of-output-bus-channels*
                                           +foreign-sample-size+)))
      (setf *output-pointer* *bus-channels*)
      (setf *bus-pointer*
            (inc-pointer *bus-channels* (* (+ %number-of-input-bus-channels
                                              *number-of-output-bus-channels*)
                                           +foreign-sample-size+)))
      (setf *output-peak-values*
            (foreign-alloc-sample *number-of-output-bus-channels*))
      ;; time
      (setf *tempo* (make-tempo *default-bpm*))
      (setf *sample-counter*
            (foreign-alloc 'sample :initial-element +sample-zero+))
      ;; graph
      (setf *node-hash* (make-node-hash *max-number-of-nodes*))
      (setf *node-root*
            (let ((group (make-node 0
                           (length (int-hash-table-items *node-hash*)))))
              (setf (node-prev group) :dummy-node
                    (node-funcons group) nil
                    (node-last group) :dummy-node)
              group))
      ;; nrt
      (setf *nrt-node-root*
            (let ((group (make-node 0 *max-number-of-nodes*)))
              (setf (node-prev group) :dummy-node
                    (node-funcons group) nil
                    (node-last group) :dummy-node)
              group))
      (setf *nrt-node-hash* (make-node-hash *max-number-of-nodes*))
      (setf *nrt-bus-channels* (foreign-alloc-sample *nrt-bus-channels-size*))
      (setf *nrt-input-pointer*
            (inc-pointer *nrt-bus-channels* %nrt-bus-pointer-offset))
      (setf *nrt-bus-pointer*
            (inc-pointer *nrt-input-pointer* %nrt-bus-pointer-offset))
      (setf *nrt-output-peak-values*
            (foreign-alloc-sample *max-number-of-channels*))
      (setf *nrt-sample-counter*
            (foreign-alloc :double :initial-element +sample-zero+))
      (setf *nrt-tempo* (make-tempo *default-bpm*))
      ;; mouse
      (setf incudine.vug::*mouse-event* (incudine.vug::alloc-mouse-event))
      ;; init
      (setf *set-readtable-p* t)
      (funcall #'init t)
      ;; buffer
      (setf *sine-table*
            (make-buffer *default-table-size*
                         :fill-function (gen:partials '(1))))
      (setf *cosine-table*
            (make-buffer *default-table-size*
                         :fill-function (gen:partials '((1 1 .25)))))
      (setf *package* (find-package :incudine.scratch)))
    "Allocation of the foreign memory and initialization when a saved
core image starts up.")

  (defvar *core-config-and-init-function*
    (lambda ()
      (incudine.config:load-incudinerc)
      (funcall *core-init-function*)))

  (defvar *core-save-function*
    (lambda ()
      ;; Stop realtime and non-realtime threads
      (rt-stop)
      (nrt-stop)
      (values))
    "Function to call before to save a core image.")

  (defvar *exit-function*
    (lambda () (rt-stop))
    "Function to call when SBCL process exits.")

  (defvar *set-core-hooks-p* t)
  (when *set-core-hooks-p*
    (pushnew *core-config-and-init-function* sb-ext:*init-hooks*)
    (pushnew *core-save-function* sb-ext:*save-hooks*)
    (pushnew *exit-function* sb-ext:*exit-hooks*)
    (setf *set-core-hooks-p* nil)))
