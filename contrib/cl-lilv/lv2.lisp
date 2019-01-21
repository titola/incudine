;;; Copyright (c) 2019 Tito Latini
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :lv2)

;;; Worker for non-realtime plugin operations.

(defcstruct worker-state
  (interface :pointer)
  (lv2-handle :pointer)
  (data :pointer)
  (size :uint32)
  (done-p :uint32))

(declaim (inline worker-state-slot))
(defun worker-state-slot (state slot-name)
  (cffi:foreign-slot-value state '(:struct worker-state) slot-name))

(cffi:defcallback worker-respond-function :int
    ((state :pointer) (data-size :uint32) (data-ptr :pointer))
  (cffi:with-foreign-slots ((data size done-p) state (:struct worker-state))
    (setf data data-ptr size data-size done-p 1))
  +worker-success+)

(declaim (inline interface-work))
(defun interface-work (state size data)
  (cffi:foreign-funcall-pointer
    (cffi:foreign-slot-value (worker-state-slot state 'interface)
                             '(:struct worker-interface) 'work)
    () :pointer (worker-state-slot state 'lv2-handle)
    :pointer (cffi:callback worker-respond-function)
    :pointer state :uint32 size :pointer data :int))

(cffi:defcallback default-worker-func :int
    ((handle :pointer) (size :uint32) (data :pointer))
  (declare (ignore handle size data))
  (error "Missing LV2 worker schedule function")
  +worker-err-unknown+)

;; It is necessary to rebind *WORKER-SCHEDULE-FUNCTION* with the real
;; worker-schedule callback before to initialize the plugin.
(defvar *worker-schedule-function* (cffi:callback default-worker-func))

(defun make-worker-schedule ()
  (let ((ptr (cffi:foreign-alloc '(:struct worker-schedule))))
    (handler-case
        (cffi:with-foreign-slots
            ((handle schedule-work) ptr (:struct worker-schedule))
          (setf handle ptr)
          (setf schedule-work *worker-schedule-function*)
          ptr)
      (condition (c)
        (cffi:foreign-free ptr)
        (error c)))))

(defun make-worker-state (instance worker-interface)
  (let ((ptr (cffi:foreign-alloc '(:struct worker-state))))
    (handler-case
        (cffi:with-foreign-slots
            ((interface lv2-handle size done-p) ptr (:struct worker-state))
            (setf interface worker-interface)
            (setf lv2-handle instance)
            (setf size 0)
            (setf done-p 0)
            ptr)
      (condition (c)
        (cffi:foreign-free ptr)
        (error c)))))

;;; Features.

(defvar *lv2-features* nil)
(declaim (type list *lv2-features*))

(defun make-lv2-features ()
  (assert (null *lv2-features*))
  (flet ((add-feature (ptr-array array index string data-ptr)
           (push data-ptr *lv2-features*)
           (push (cffi:foreign-string-alloc string) *lv2-features*)
           (let ((ptr (cffi:mem-aptr array '(:struct feature) index)))
             (setf (cffi:mem-aref ptr :pointer 0) (first *lv2-features*))
             (setf (cffi:mem-aref ptr :pointer 1) (second *lv2-features*))
             (setf (cffi:mem-aref ptr-array :pointer index) ptr))))
    (let ((ptr nil)
          (features-ptr nil)
          (len 3))
      (handler-case
          (progn
            (setf ptr (cffi:foreign-alloc :pointer :count len))
            (setf features-ptr
                  (cffi:foreign-alloc '(:struct feature) :count len))
            (add-feature ptr features-ptr 0 "http://lv2plug.in/ns/ext/urid#map"
                         (make-urid-map))
            (add-feature ptr features-ptr 1
                         "http://lv2plug.in/ns/ext/worker#schedule"
                         (make-worker-schedule))
            (push features-ptr *lv2-features*)
            (setf (cffi:mem-aref ptr :pointer (1- len)) (cffi:null-pointer))
            ptr)
        (condition (c)
          (if ptr (cffi:foreign-free ptr))
          (if features-ptr (cffi:foreign-free features-ptr))
          (free-features)
          (error c))))))

(defun free-features ()
  (when *lv2-features*
    (loop for (uri data) on (cdr *lv2-features*) by #'cddr do
            (cffi:foreign-string-free uri)
            (cffi:foreign-free data))
    (cffi:foreign-free (car *lv2-features*)))
  (setf *lv2-features* nil))

;;; Atom Sequence

(define-constant +atom-sequence+
  (uri-to-id "http://lv2plug.in/ns/ext/atom#Sequence"))

(define-constant +atom-chunk+
  (uri-to-id "http://lv2plug.in/ns/ext/atom#Chunk"))

(define-constant +midi-event+
  (uri-to-id "http://lv2plug.in/ns/ext/midi#MidiEvent")
  :documentation
    "Numeric identifier of http://lv2plug.in/ns/ext/midi#MidiEvent.")

(define-constant +atom-sequence-buffer-size+ 131072)

(define-constant +atom-sequence-type-size+
  (cffi:foreign-type-size '(:struct atom-sequence)))

(define-constant +atom-chunk-size+
  (- +atom-sequence-buffer-size+ +atom-sequence-type-size+))

(define-constant +atom-event-type-size+
  (cffi:foreign-type-size '(:struct atom-event)))

(defstruct event
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (buffer-size +atom-sequence-buffer-size+ :type non-negative-fixnum)
  (data-index +atom-sequence-type-size+ :type non-negative-fixnum))

(defstruct (atom-sequence (:include event)
                          (:constructor %make-atom-sequence)
                          (:copier nil)))

(defstruct (atom-chunk (:include event)
                       (:constructor %make-atom-chunk)
                       (:copier nil)))

(declaim (inline align-64bit))
(defun align-64bit (x)
  (logandc2 (+ x 7) 7))

(declaim (inline reset-atom-sequence-buffer))
(defun reset-atom-sequence-buffer (ptr &optional type-p chunk-p)
  ;; Header size.
  (setf (cffi:mem-aref ptr :uint32)
        (if chunk-p
            +atom-chunk-size+
            #.(cffi:foreign-type-size '(:struct atom-sequence-body))))
  (if type-p (setf (cffi:mem-aref ptr :uint32 1)
                   (if chunk-p +atom-chunk+ +atom-sequence+)))
  ;; body.unit and body.pad zero.
  (setf (cffi:mem-aref ptr :uint64 1) 0)
  ptr)

(defun make-atom-sequence-buffer ()
  (reset-atom-sequence-buffer
    (cffi:foreign-alloc :char :count +atom-sequence-buffer-size+)
    t))

(defun make-atom-sequence (&key pointer)
  (declare (type (or cffi:foreign-pointer null) pointer))
  (%make-atom-sequence :pointer (or pointer (make-atom-sequence-buffer))))

(defun reset-atom-sequence (obj &key type-p)
  (reset-atom-sequence-buffer (atom-sequence-pointer obj) type-p)
  (setf (atom-sequence-data-index obj) +atom-sequence-type-size+)
  obj)

(defun make-atom-chunk-buffer ()
  (reset-atom-sequence-buffer
    (cffi:foreign-alloc :char :count +atom-sequence-buffer-size+)
    t t))

(defun make-atom-chunk (&key pointer)
  (declare (type (or cffi:foreign-pointer null) pointer))
  (%make-atom-chunk :pointer (or pointer (make-atom-chunk-buffer))))

(defun reset-atom-chunk (obj &key type-p)
  (reset-atom-sequence-buffer (atom-chunk-pointer obj) type-p t)
  (setf (atom-chunk-data-index obj) +atom-sequence-type-size+)
  obj)

(defun add-event (obj data &key (frames 0) (type +midi-event+) (start 0) end)
  (declare (type event obj)
           (type (or (simple-array (unsigned-byte 8) (*))
                     (unsigned-byte 32))
                 data)
           (type (unsigned-byte 64) frames)
           (type non-negative-fixnum type start)
           (type (or non-negative-fixnum null) end))
  (let* ((size (if (numberp data)
                   3 ; status byte + 2 data bytes.
                   (- (or end (length data)) start)))
         (event-size (align-64bit (+ +atom-event-type-size+ size)))
         (ptr (atom-sequence-pointer obj))
         (event-ptr (cffi:inc-pointer ptr (atom-sequence-data-index obj))))
    (declare (type non-negative-fixnum size event-size))
    (unless (> (+ (atom-sequence-data-index obj) event-size)
               (atom-sequence-buffer-size obj))
      (setf (cffi:mem-aref event-ptr :uint64) frames)
      (setf (cffi:mem-aref event-ptr :uint32 2) size)
      (setf (cffi:mem-aref event-ptr :uint32 3) type)
      (if (numberp data)
          ;; Short message encoded into four bytes.
          (setf (cffi:mem-aref event-ptr :uint32 4) data)
          (loop for i from start
                for j from (+ (atom-sequence-data-index obj)
                              +atom-event-type-size+)
                for k below size do
                  (setf (cffi:mem-aref ptr :unsigned-char j) (aref data i))))
      (incf (cffi:mem-aref ptr :uint32) event-size)
      (incf (atom-sequence-data-index obj) event-size)
      obj)))
