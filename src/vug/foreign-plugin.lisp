;;; Copyright (c) 2014 Tito Latini
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

(defpackage :incudine.vug-ext
  (:use :cl :incudine.util :incudine.vug)
  (:import-from #:alexandria #:define-constant #:make-keyword
                #:format-symbol #:ensure-symbol #:with-gensyms
                #:non-negative-fixnum)
  (:import-from #:incudine #:block-size))

(in-package :incudine.vug-ext)

(defstruct (port (:constructor %make-port))
  (name "" :type string :read-only t)
  (lisp-name "" :type string :read-only t)
  (id 0 :type non-negative-fixnum :read-only t)
  (type 0 :type non-negative-fixnum :read-only t)
  (value-type t :type (or symbol cons) :read-only t))

(defun make-port (&key (name "") (id 0) (type 0) (value-type t))
  (let ((lisp-name (lispify-name name)))
    (%make-port :name name :lisp-name lisp-name :id id :type type
                :value-type value-type)))

(defmethod print-object ((obj port) stream)
  (format stream "#<PLUGIN-PORT ~D ~A>" (port-id obj) (port-lisp-name obj)))

(defstruct (foreign-plugin (:constructor %make-foreign-plugin))
  (name "" :type string :read-only t)
  (path "" :type string :read-only t)
  (pointer (cffi:null-pointer) :type foreign-pointer :read-only t)
  (label "" :type string :read-only t)
  (id -1 :type fixnum :read-only t)
  (author "" :type string :read-only t)
  (license "" :type string :read-only t)
  (realtime-p nil :type boolean :read-only t)
  (ports #() :type simple-vector :read-only t)
  (inputs 0 :type non-negative-fixnum)
  (outputs 0 :type non-negative-fixnum)
  (sample-type t :type (or symbol cons) :read-only t)
  (instantiate-cb (cffi:null-pointer) :type foreign-pointer)
  (connect-port-cb (cffi:null-pointer) :type foreign-pointer)
  (activate-cb (cffi:null-pointer) :type foreign-pointer)
  (run-cb (cffi:null-pointer) :type foreign-pointer)
  (deactivate-cb (cffi:null-pointer) :type foreign-pointer)
  (cleanup-cb (cffi:null-pointer) :type foreign-pointer))

(defun update-io-number (plugin)
  (declare (type foreign-plugin plugin))
  (setf (foreign-plugin-inputs plugin) (port-inputs plugin))
  (setf (foreign-plugin-outputs plugin) (port-outputs plugin))
  plugin)

(defun make-foreign-plugin (&rest args)
  (update-io-number (apply #'%make-foreign-plugin args)))

(defmethod print-object ((obj foreign-plugin) stream)
  (format stream "#<~S ~S #X~8,'0X>" (type-of obj)
          (foreign-plugin-label obj)
          (cffi:pointer-address (foreign-plugin-pointer obj))))

(define-constant +input-port+     1)
(define-constant +output-port+    2)
(define-constant +control-port+   4)
(define-constant +audio-port+     8)
(define-constant +event-port+    16)
(define-constant +midi-port+     32)

(defmacro port-typep (port type)
  `(logtest (port-type ,port)
            ,(format-symbol *package* "+~A-PORT+" type)))

(defun input-port-p (port) (port-typep port input))
(defun output-port-p (port) (port-typep port output))
(defun control-port-p (port) (port-typep port control))
(defun audio-port-p (port) (port-typep port audio))
(defun event-port-p (port) (port-typep port event))
(defun midi-port-p (port) (port-typep port midi))

(defun lispify-name (name)
  "Lispify the name of a port."
  (loop for c across name
        with acc = nil and skip-p = nil do
          (case c
            ((#\space #\tab #\( #\[ #\{ #\< #\: #\. #\; #\,
                      #\- #\_ #\# #\@ #\' #\" #\`)
             ;; Replace multiple characters with '-'
             (unless skip-p
               (setf skip-p t)
               (push #\- acc)))
            ((#\) #\] #\} #\>) nil)
            (otherwise (push c acc)
                       (when skip-p
                         (setf skip-p nil))))
        finally (let ((acc (if (and acc (char= (car acc) #\-))
                               (cdr acc)
                               acc)))
                  (return (string-upcase
                            (coerce (nreverse acc) 'string))))))

(defgeneric doc-string (plugin)
  (:documentation "Documentation string for VUG."))

(defmethod doc-string ((p foreign-plugin))
  (format nil "~A.~%~A (~A/~D) by ~A."
          (foreign-plugin-name p) (type-of p)
          (foreign-plugin-label p) (foreign-plugin-id p)
          (foreign-plugin-author p)))

(declaim (inline arg-symbol))
(defun arg-symbol (port)
  (ensure-symbol (port-lisp-name port)))

(defmacro port-loop ((port-var index-var plugin) &body body)
  (with-gensyms (ports)
    `(let ((,ports (foreign-plugin-ports ,plugin)))
       (loop for ,index-var below (length ,ports)
             for ,port-var = (svref ,ports ,index-var)
             ,@body))))

(defun port-inputs (plugin)
  (port-loop (p i plugin) if (input-port-p p) sum 1))

(defun port-outputs (plugin)
  (port-loop (p i plugin) if (output-port-p p) sum 1))

(defun port-input-names (plugin)
  (port-loop (p i plugin)
    if (input-port-p p)
    collect (arg-symbol p)))

(defun port-output-names (plugin)
  (port-loop (p i plugin)
    if (output-port-p p)
    collect (arg-symbol p)))

(defun port-input-arg (port block-size)
  (let ((arg (arg-symbol port)))
    (if (or (= block-size 1) (control-port-p port))
        arg
        `(,arg cffi:foreign-pointer))))

(defun port-input-args (plugin &optional (block-size 1))
  (port-loop (p i plugin)
    if (input-port-p p) collect (port-input-arg p block-size)))

(defun io-array-p (port block-size)
  (and (> block-size 1) (audio-port-p port)))

(defun port-array-fname (port)
  (case (port-value-type port)
    ((foreign-float f32) 'make-f32-array)
    ((foreign-double f64) 'make-f64-array)
    (otherwise (error "unknown port type ~A" (port-value-type port)))))

(defmacro add-vug-declaration (var type plist array-p)
  `(push ,var (getf ,plist (if ,array-p 'cffi:foreign-pointer ,type))))

(defmacro vug-port-bindings ((declare-list-name) bindings)
  (with-gensyms (b)
    `(let* ((,declare-list-name nil)
            (,b ,bindings))
       (values ,b
               ;; Declarations.
               (loop for (type lst) on ,declare-list-name by #'cddr
                     collect (cons type (nreverse lst)))))))

(defun vug-port-input-bindings (plugin &optional (block-size 1))
  (vug-port-bindings (decl)
    (port-loop (p i plugin)
      if (input-port-p p)
      collect (let* ((name (port-lisp-name p))
                     (arg (ensure-symbol name))
                     (array-p (io-array-p p block-size)))
                (add-vug-declaration arg (port-value-type p) decl array-p)
                `(,arg ,(if array-p
                            arg
                            (vug::dsp-coercing-argument
                              arg (port-value-type p))))))))

(defun vug-port-output-bindings (plugin &optional (block-size 1))
  (vug-port-bindings (decl)
    (port-loop (p i plugin)
      if (output-port-p p)
      collect (let* ((name (port-lisp-name p))
                     (arg (ensure-symbol name))
                     (array-p (io-array-p p block-size)))
                (add-vug-declaration arg (port-value-type p) decl array-p)
                `(,arg ,(if array-p
                            `(,(port-array-fname p) ,block-size)
                            (vug::coerce-number 0 (port-value-type p))))))))

(defun vug-frame-binding (plugin block-size)
  (let ((outs (foreign-plugin-outputs plugin)))
    (when (> outs 1)
      `((outputs ,(if (= block-size 1)
                      `(make-frame ,outs)
                      `(make-pointer-array ,outs)))))))

(defun set-outputs (index out block-size &optional control-p)
  (if (= block-size 1)
      `(setf (frame-ref outputs ,index) (sample ,out))
      `(setf (cffi:mem-aref outputs :pointer ,index)
             ,(if control-p `(get-pointer ,out) out))))

(defun get-output (plugin block-size)
  (let ((names (port-output-names plugin)))
    (if (> (foreign-plugin-outputs plugin) 1)
        (if (= block-size 1)
            ;; Array of samples (frame).
            `(,@(loop for out in names for index from 0
                      collect (set-outputs index out 1))
                outputs)
            ;; Array of arrays.
            `(outputs))
        (if (= block-size 1)
            ;; Value of SAMPLE type.
            `((sample ,(car names)))
            ;; Foreign array.
            `(,(car names))))))

(defmacro with-vug-plugin ((vug-name plugin block-size) &body body)
  (multiple-value-bind (in-bindings in-decl)
      (vug-port-input-bindings plugin block-size)
    (multiple-value-bind (out-bindings out-decl)
        (vug-port-output-bindings plugin block-size)
      `(define-vug ,vug-name ,(port-input-args plugin block-size)
         ,(doc-string plugin)
         (with (,@in-bindings
                ,@out-bindings
                ,@(vug-frame-binding plugin block-size))
           (declare ,@in-decl ,@out-decl)
           ,@(when (and (> (foreign-plugin-outputs plugin) 1)
                        (> block-size 1))
               ;; Set the pointer to the foreign arrays.
               `((initialize
                  ,@(port-loop (p i plugin)
                      with index = 0
                      if (output-port-p p)
                      collect (set-outputs index (arg-symbol p) block-size
                                           (control-port-p p))
                      and do (incf index)))))
           (maybe-expand ,@(port-input-names plugin))
           ,@body
           ,@(get-output plugin block-size))))))