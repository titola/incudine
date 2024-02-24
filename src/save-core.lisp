;;; Copyright (c) 2013-2024 Tito Latini
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
  (defvar *fill-nrt-memory-pools-p* nil)
  (declaim (type boolean *fill-nrt-memory-pools-p*))

  (defvar *fill-rt-memory-pools-p* nil)
  (declaim (type boolean *fill-rt-memory-pools-p*))

  ;;; Allocation of the foreign memory and initialization when a saved
  ;;; core image starts up.
  (defvar *core-init-function*
    (lambda ()
      (when *fill-nrt-memory-pools-p*
        (loop for (pool size)
              in `((,*buffer-pool* ,+buffer-pool-initial-size+)
                   (,*envelope-pool* ,+envelope-pool-initial-size+)
                   (,*tempo-envelope-pool* ,+tempo-envelope-pool-initial-size+)
                   (,*tuning-pool* ,+tuning-pool-initial-size+)
                   (,*node-pool* ,+node-pool-initial-size+)
                   (,*foreign-array-pool* ,+foreign-array-pool-initial-size+)
                   (,*tempo-pool* ,+tempo-pool-initial-size+)
                   (,incudine.analysis::*ring-input-buffer-pool*
                    ,incudine.analysis::+ring-buffer-pool-initial-size+)
                   (,incudine.analysis::*ring-output-buffer-pool*
                    ,incudine.analysis::+ring-buffer-pool-initial-size+)
                   (,incudine.analysis::*fft-pool*
                    ,incudine.analysis::+fft-pool-initial-size+)
                   (,incudine.analysis::*ifft-pool*
                    ,incudine.analysis::+fft-pool-initial-size+)
                   (,incudine.analysis::*abuffer-pool*
                    ,incudine.analysis::+abuffer-pool-initial-size+)
                   (,incudine.analysis::*pvbuffer-pool*
                    ,incudine.analysis::+pvbuffer-pool-initial-size+))
           do (ensure-incudine-object-pool-size pool size)))
      (when *fill-rt-memory-pools-p*
        (loop for (pool size)
              in `((,*rt-buffer-pool* ,+buffer-pool-initial-size+)
                   (,*rt-envelope-pool* ,+envelope-pool-initial-size+)
                   (,*rt-tempo-envelope-pool* ,+tempo-envelope-pool-initial-size+)
                   (,*rt-tuning-pool* ,+tuning-pool-initial-size+)
                   (,*rt-foreign-array-pool* ,+foreign-array-pool-initial-size+)
                   (,*rt-tempo-pool* ,+tempo-pool-initial-size+)
                   (,incudine.analysis::*rt-ring-input-buffer-pool*
                    ,incudine.analysis::+ring-buffer-pool-initial-size+)
                   (,incudine.analysis::*rt-ring-output-buffer-pool*
                    ,incudine.analysis::+ring-buffer-pool-initial-size+)
                   (,incudine.analysis::*rt-fft-pool*
                    ,incudine.analysis::+fft-pool-initial-size+)
                   (,incudine.analysis::*rt-ifft-pool*
                    ,incudine.analysis::+fft-pool-initial-size+)
                   (,incudine.analysis::*rt-abuffer-pool*
                    ,incudine.analysis::+abuffer-pool-initial-size+)
                   (,incudine.analysis::*rt-pvbuffer-pool*
                    ,incudine.analysis::+pvbuffer-pool-initial-size+))
              do (ensure-incudine-object-pool-size pool size)))
      (set-sample-rate (sample (or incudine.config::*sample-rate* 48000)))
      (set-sound-velocity (sample (or incudine.config::*sound-velocity* 345)))
      (setf incudine.util:*null-output* (incudine.util::make-null-output-stream))
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
      (setf incudine.edf:*heap-size*
            (if (power-of-two-p *rt-edf-heap-size*)
                *rt-edf-heap-size*
                (next-power-of-two *rt-edf-heap-size*)))
      (setf incudine.edf:*heap* (incudine.edf:make-heap))
      (setf incudine.edf::*rt-heap* incudine.edf:*heap*)
      (setf *nrt-edf-heap-size*
            (if (power-of-two-p *nrt-edf-heap-size*)
                *nrt-edf-heap-size*
                (next-power-of-two *nrt-edf-heap-size*)))
      (setf *nrt-edf-heap* (incudine.edf:make-heap *nrt-edf-heap-size*))
      ;; bus
      (setf *%input-pointer* (alloc-bus-pointer 'input))
      (setf *input-pointer*
            (foreign-alloc :pointer :initial-element *%input-pointer*))
      (setf *%output-pointer* (alloc-bus-pointer 'output))
      (setf *output-pointer*
            (foreign-alloc :pointer :initial-element *%output-pointer*))
      (setf *bus-pointer* (alloc-bus-pointer 'bus))
      (setf *output-peak-values*
            (foreign-alloc-sample *number-of-output-bus-channels*))
      ;; time
      (setf *tempo* (make-tempo *default-bpm*))
      (setf *sample-counter*
            (foreign-alloc 'sample :initial-element +sample-zero+))
      (setf *rt-sample-counter* *sample-counter*)
      (setf *null-counter*
            (cffi:foreign-alloc 'sample :initial-element +sample-zero+))
      ;; graph
      (setf *node-hash* (make-node-hash *max-number-of-nodes*))
      (setf *root-node*
            (let ((group (make-node 0
                           (length (int-hash-table-items *node-hash*)))))
              (setf (node-prev group) :dummy-node
                    (node-funcons group) nil
                    (node-last group) :dummy-node)
              group))
      ;; nrt
      (setf *nrt-root-node*
            (let ((group (make-node 0 *max-number-of-nodes*)))
              (setf (node-prev group) :dummy-node
                    (node-funcons group) nil
                    (node-last group) :dummy-node)
              group))
      (setf *nrt-node-hash* (make-node-hash *max-number-of-nodes*))
      (setf *%nrt-input-pointer*
            (let ((*number-of-input-bus-channels* *max-number-of-channels*))
              (alloc-bus-pointer 'input)))
      (setf *nrt-input-pointer*
            (foreign-alloc :pointer :initial-element *%nrt-input-pointer*))
      (setf *%nrt-output-pointer*
            (let ((*number-of-output-bus-channels* *max-number-of-channels*))
              (alloc-bus-pointer 'output)))
      (setf *nrt-output-pointer*
            (foreign-alloc :pointer :initial-element *%nrt-output-pointer*))
      (setf *nrt-bus-pointer* (alloc-bus-pointer 'bus))
      (setf *nrt-output-peak-values*
            (foreign-alloc-sample *max-number-of-channels*))
      (setf *nrt-sample-counter*
            (foreign-alloc :double :initial-element +sample-zero+))
      (setf *nrt-tempo* (make-tempo *default-bpm*))
      ;; midi
      (setf *portmidi-time*
            (foreign-alloc 'sample :initial-element +sample-zero+))
      (setf incudine.vug::*midi-normalize-table*
            (incudine.vug::make-midi-normalize-table))
      (setf incudine.vug::*midi-normalize-pb-bipolar-table*
            (incudine.vug::make-midi-normalize-pb-bipolar-table))
      (setf incudine.vug::*midi-normalize-pb-table*
            (incudine.vug::make-midi-normalize-pb-table))
      (setf incudine.vug:*linear-midi-table*
            (incudine:make-buffer 128
              :initial-contents (loop for i below 128 collect (/ i 127))))
      ;; ghost buffer for foreign plugins
      (setf incudine.vug-foreign::*ghost-buffer*
            (cffi:foreign-alloc :char
              :count incudine.vug-foreign::+ghost-buffer-size+
              :initial-element 0))
      ;; mouse
      (setf incudine.vug::*mouse-event* (incudine.vug::alloc-mouse-event))
      ;; init
      (funcall #'init t)
      ;; buffer
      (setf *sine-table*
            (make-buffer *default-table-size*
                         :fill-function (gen:partials '(1))))
      (setf *cosine-table*
            (make-buffer *default-table-size*
                         :fill-function (gen:partials '((1 1 .25)))))
      (setf *package* (find-package :incudine.scratch))
      ;; tuning
      (setf *tuning-et12* (make-tuning))
      (setf *default-tuning* *tuning-et12*)))

  (defvar *core-config-and-init-function*
    (lambda ()
      (incudine.config:load-incudinerc)
      (funcall *core-init-function*)))

  ;;; Function to call before to save a core image.
  (defvar *core-save-function*
    (lambda ()
      (free-dsp-instances)
      ;; Stop realtime and non-realtime threads
      (rt-stop)
      (nrt-stop)
      (mouse-stop)
      (unless (null-pointer-p *foreign-client-name*)
        (foreign-free *foreign-client-name*)
        (setf *foreign-client-name* (null-pointer)))
      ;; Empty all the memory pools otherwise they will be static
      ;; and the finalizers don't work.
      (dolist (p (list *buffer-pool* *rt-buffer-pool* *envelope-pool*
                       *rt-envelope-pool* *tempo-envelope-pool*
                       *rt-tempo-envelope-pool* *tuning-pool* *rt-tuning-pool*
                       *foreign-array-pool* *rt-foreign-array-pool*
                       *tempo-pool* *rt-tempo-pool*
                       incudine.analysis::*ring-input-buffer-pool*
                       incudine.analysis::*rt-ring-input-buffer-pool*
                       incudine.analysis::*ring-output-buffer-pool*
                       incudine.analysis::*rt-ring-output-buffer-pool*
                       incudine.analysis::*fft-pool*
                       incudine.analysis::*rt-fft-pool*
                       incudine.analysis::*ifft-pool*
                       incudine.analysis::*rt-ifft-pool*
                       incudine.analysis::*abuffer-pool*
                       incudine.analysis::*rt-abuffer-pool*
                       incudine.analysis::*pvbuffer-pool*
                       incudine.analysis::*rt-pvbuffer-pool*))
        (incudine.util::empty-cons-pool p))
      (incudine.util::empty-cons-pool *node-pool* :cancel-finalizations t)
      (clrhash incudine.analysis::*fft-plan*)
      (when (and (find-package "SB-ACLREPL")
                 (not (find-package "LINEDIT"))
                 (boundp 'cl-user::**repl-fun-generator*))
        ;; The incudine command doesn't call SB-ACLREPL::MAKE-REPL-FUN
        ;; for the main REPL. The value of CL-USER::**REPL-FUN-GENERATOR*
        ;; is the generator defined before the installation of SB-ACLREPL.
        ;; The considerable difference is the reset of FD-STREAM-OUTPUT-COLUMN
        ;; to 0 if *STANDARD-OUTPUT* is a SYNONYM-STREAM, because it avoids
        ;; an empty line (see REPL-FUN in sbcl/src/code/toplevel.lisp; the
        ;; last checked version is sbcl-2.4.1).
        (let ((s (find-symbol "*REPL-FUN-GENERATOR*" "SB-IMPL")))
          (symbol-macrolet ((repl-fun-generator (symbol-value s)))
            (let ((new-repl-fun-generator repl-fun-generator))
              (setf repl-fun-generator
                    (lambda ()
                      (setf repl-fun-generator new-repl-fun-generator)
                      (funcall cl-user::**repl-fun-generator*)))))))
      (values)))

  ;;; Function to call when SBCL process exits.
  (defvar *exit-function*
    (lambda () (rt-stop)))

  (defvar *set-core-hooks-p* t)
  (when *set-core-hooks-p*
    (pushnew *core-config-and-init-function* sb-ext:*init-hooks*)
    (pushnew *core-save-function* sb-ext:*save-hooks*)
    (pushnew *exit-function* sb-ext:*exit-hooks*)
    (setf *set-core-hooks-p* nil)))
