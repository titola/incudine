;;; Copyright (c) 2014-2019 Tito Latini
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

(in-package :incudine.vug-foreign)

(defstruct (port (:constructor %make-port))
  "Foreign plugin port type."
  (name "" :type string :read-only t)
  (lisp-name "" :type string :read-only t)
  (id 0 :type non-negative-fixnum :read-only t)
  (type 0 :type non-negative-fixnum :read-only t)
  (default nil :type (or list real))
  (value-type t :type (or symbol cons) :read-only t))

(defun make-port (&key (name "") (id 0) (type 0) default (value-type t))
  (let ((lisp-name (lispify-name name)))
    (%make-port :name name :lisp-name lisp-name :id id :type type
                :default default :value-type value-type)))

(defmethod print-object ((obj port) stream)
  (format stream "#<PLUGIN-PORT ~D ~A>" (port-id obj) (port-lisp-name obj)))

(defstruct (plugin (:constructor %make-plugin))
  "Foreign plugin type."
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
  (input-events 0 :type non-negative-fixnum)
  (output-events 0 :type non-negative-fixnum)
  (sample-type t :type (or symbol cons) :read-only t)
  (instantiate-cb (cffi:null-pointer) :type foreign-pointer)
  (connect-port-cb (cffi:null-pointer) :type foreign-pointer)
  (activate-cb (cffi:null-pointer) :type foreign-pointer)
  (run-cb (cffi:null-pointer) :type foreign-pointer)
  (deactivate-cb (cffi:null-pointer) :type foreign-pointer)
  (cleanup-cb (cffi:null-pointer) :type foreign-pointer))

(defstruct plugin-instance
  "Foreign plugin instance type."
  (label "" :type string :read-only t)
  (handle-pointer (cffi:null-pointer) :type foreign-pointer)
  (port-pointers #() :type simple-vector)
  (event-pointers nil :type (or null simple-vector)))

(declaim (inline plugin-instance-pointer))
(defun plugin-instance-pointer (obj)
  "Return the foreign plugin instance pointer."
  (plugin-instance-handle-pointer obj))

(defmethod print-object ((obj plugin-instance) stream)
  (format stream "#<~A ~S #X~8,'0X>" (type-of obj)
          (plugin-instance-label obj)
          (cffi:pointer-address (plugin-instance-pointer obj))))

(declaim (inline plugin-port-pointer))
(defun plugin-port-pointer (obj index)
  "Return the foreign pointer to the data of the plugin port INDEX."
  (aref (plugin-instance-port-pointers obj) index))

(defun audio-control-outputs (plugin)
  (- (plugin-outputs plugin) (plugin-output-events plugin)))

(defun update-io-number (plugin)
  (declare (type plugin plugin))
  (setf (plugin-inputs plugin) (port-inputs plugin))
  (setf (plugin-outputs plugin) (port-outputs plugin))
  (multiple-value-bind (in-events out-events)
      (port-events plugin)
    (setf (plugin-input-events plugin) in-events)
    (setf (plugin-output-events plugin) out-events))
  plugin)

(defun make-plugin (&rest args)
  (update-io-number (apply #'%make-plugin args)))

(defmethod print-object ((obj plugin) stream)
  (format stream "#<~A ~S #X~8,'0X>" (type-of obj)
          (plugin-label obj)
          (cffi:pointer-address (plugin-pointer obj))))

(define-constant +input-port+     1)
(define-constant +output-port+    2)
(define-constant +control-port+   4)
(define-constant +audio-port+     8)
(define-constant +event-port+    16)
(define-constant +midi-port+     32)
(define-constant +atom-port+     64)

(defmacro port-typep (port type)
  `(logtest (port-type ,port)
            ,(format-symbol *package* "+~A-PORT+" type)))

(defun input-port-p (port) (port-typep port input))
(defun output-port-p (port) (port-typep port output))
(defun control-port-p (port) (port-typep port control))
(defun audio-port-p (port) (port-typep port audio))
(defun midi-port-p (port) (port-typep port midi))
(defun atom-port-p (port) (port-typep port atom))
(defun %event-port-p (port) (port-typep port event))

(defun event-port-p (port)
  (or (atom-port-p port) (%event-port-p port) (midi-port-p port)))

(defun audio-control-input-port-p (port)
  (and (input-port-p port) (not (event-port-p port))))

(defun audio-control-output-port-p (port)
  (and (output-port-p port) (not (event-port-p port))))

;;; Lispify the name of a port.
(defun lispify-name (name)
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

(defmethod doc-string ((p plugin))
  (format nil "~A.~2%~A (~A/~D) by ~A."
          (plugin-name p) (type-of p) (plugin-label p) (plugin-id p)
          (plugin-author p)))

(declaim (inline arg-symbol))
(defun arg-symbol (port)
  (ensure-symbol (port-lisp-name port)))

(defmacro port-loop ((port-var index-var plugin) &body body)
  (with-gensyms (ports)
    `(let ((,ports (plugin-ports ,plugin)))
       (loop for ,index-var below (length ,ports)
             for ,port-var = (svref ,ports ,index-var)
             ,@body))))

(defun port-inputs (plugin)
  (port-loop (p i plugin) if (input-port-p p) sum 1))

(defun port-control-inputs (plugin)
  (port-loop (p i plugin)
    if (and (input-port-p p)
            (control-port-p p)
            (not (event-port-p p)))
    sum 1))

(defun port-outputs (plugin)
  (port-loop (p i plugin) if (output-port-p p) sum 1))

(defun port-events (plugin)
  (let ((acc (cons 0 0)))
    (port-loop (p i plugin)
      do (if (event-port-p p)
             (if (input-port-p p)
                 (incf (car acc))
                 (incf (cdr acc)))))
    (values (car acc) (cdr acc))))

(defun port-audio-control-input-names (plugin control-arguments-p)
  (port-loop (p i plugin)
    if (and (audio-control-input-port-p p)
            (or control-arguments-p (audio-port-p p)))
      collect (arg-symbol p)))

(defun port-audio-control-output-names (plugin)
  (port-loop (p i plugin)
    if (audio-control-output-port-p p)
      collect (arg-symbol p)))

(defun port-input-arg (port block-size)
  (let ((arg (arg-symbol port)))
    (if (or (= block-size 1) (control-port-p port))
        arg
        `(,arg cffi:foreign-pointer))))

(defun vug-arg-names (plugin block-size control-arguments-p)
  (port-loop (p i plugin)
    if (and (input-port-p p)
            (not (event-port-p p))
            (or control-arguments-p (audio-port-p p)))
      collect (port-input-arg p block-size)))

(defun port-input-defaults-p (plugin)
  (port-loop (p i plugin)
    if (and (input-port-p p) (port-default p))
      do (return t)))

(defun port-input-defaults (plugin control-arguments-p missing-arg-format-control)
  (when (port-input-defaults-p plugin)
    (port-loop (p i plugin)
      if (and (input-port-p p)
              (not (event-port-p p))
              (or control-arguments-p (audio-port-p p)))
        collect (or (port-default p)
                    (when missing-arg-format-control
                      `(incudine-missing-arg
                         ,(format nil missing-arg-format-control
                                  (port-name p))))))))

(defun io-array-p (port block-size)
  (and (> block-size 1) (audio-port-p port)))

(defun port-array-fname (port)
  (case (port-value-type port)
    ((foreign-float f32) 'make-f32-array)
    ((foreign-double f64) 'make-f64-array)
    (otherwise
     (incudine-error "unknown port type ~A" (port-value-type port)))))

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

(defun vug-port-input-bindings (plugin block-size control-arguments-p
                                event-buffer-size make-event-form)
  (vug-port-bindings (decl)
    (port-loop (p i plugin)
      if (and (input-port-p p)
              (or control-arguments-p (event-port-p p)))
        collect (let* ((name (port-lisp-name p))
                       (arg (ensure-symbol name))
                       (array-p (io-array-p p block-size)))
                  (add-vug-declaration arg (port-value-type p) decl array-p)
                  `(,arg
                    ,(cond (array-p arg)
                           ((event-port-p p)
                            (funcall make-event-form
                              ;; Foreign array automatically freed.
                              `(make-u32-array ,(ash event-buffer-size -2))))
                           (t
                            (vug::dsp-coercing-argument
                              arg (port-value-type p)))))))))

(defun vug-port-output-bindings (plugin block-size event-buffer-size
                                 make-event-form)
  (vug-port-bindings (decl)
    (port-loop (p i plugin)
      if (output-port-p p)
        collect (let* ((name (port-lisp-name p))
                       (arg (ensure-symbol name))
                       (array-p (io-array-p p block-size)))
                  (add-vug-declaration arg (port-value-type p) decl array-p)
                  `(,arg
                    ,(cond (array-p `(,(port-array-fname p) ,block-size))
                           ((event-port-p p)
                            (funcall make-event-form
                              ;; Foreign array automatically freed.
                              `(make-u32-array ,(ash event-buffer-size -2))
                              :output-p t))
                           (t
                            (vug::coerce-number 0 (port-value-type p)))))))))

(defun vug-frame-binding (plugin block-size)
  (let ((outs (audio-control-outputs plugin)))
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
  (let ((names (port-audio-control-output-names plugin)))
    (if (> (audio-control-outputs plugin) 1)
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

(defmacro with-vug-plugin ((vug-name plugin block-size control-arguments-p
                            &key (event-buffer-size 131072)
                            (make-event-form #'identity)
                            (reset-event-form #'identity))
                           &body body)
  (multiple-value-bind (in-bindings in-decl)
      (vug-port-input-bindings
        plugin block-size control-arguments-p event-buffer-size
        make-event-form)
    (multiple-value-bind (out-bindings out-decl)
        (vug-port-output-bindings
           plugin block-size event-buffer-size make-event-form)
      (let ((defaults (port-input-defaults
                        plugin control-arguments-p "Missing ~S argument"))
            (plugin-instance (ensure-symbol "PLUGIN-INSTANCE")))
        `(define-vug ,vug-name (,@(vug-arg-names
                                    plugin block-size control-arguments-p)
                                (,plugin-instance (or null plugin-instance)))
           ,(doc-string plugin)
           (:defaults ,@defaults nil)
           (with (,@in-bindings
                  ,@out-bindings
                  ,@(vug-frame-binding plugin block-size))
             (declare ,@in-decl ,@out-decl)
             (maybe-expand ,@(port-audio-control-input-names
                               plugin control-arguments-p))
             ,@(when (plusp (plugin-output-events plugin))
                 (port-loop (p i plugin)
                   if (and (event-port-p p) (output-port-p p))
                     collect (funcall reset-event-form (arg-symbol p)
                                      :init-time-p t :output-p t)))
             ,@body
             ,@(when (and (> (plugin-outputs plugin) 1)
                          (> block-size 1))
                 ;; Set the pointer to the foreign arrays.
                 `((initialize
                     ,@(port-loop (p i plugin)
                         with index = 0
                         if (audio-control-output-port-p p)
                           collect (set-outputs index (arg-symbol p) block-size
                                                (control-port-p p))
                           and do (incf index)))))
             ,@(when (plusp (plugin-input-events plugin))
                 `((initialize
                     (setf (plugin-instance-event-pointers ,plugin-instance)
                           (make-array ,(plugin-input-events plugin)))
                     ,@(port-loop (p i plugin)
                         with j = 0
                         if (and (event-port-p p) (input-port-p p))
                           collect
                             `(setf (svref (plugin-instance-event-pointers
                                             ,plugin-instance)
                                           ,j)
                                    ,(arg-symbol p))
                           and do (incf j)
                           and collect (funcall reset-event-form (arg-symbol p)
                                                :init-time-p t)))))
             ,@(when (plusp (plugin-output-events plugin))
                 (port-loop (p i plugin)
                   if (and (event-port-p p) (output-port-p p))
                     collect (funcall reset-event-form (arg-symbol p)
                                      :init-time-p t :output-p t)))
             ,@(when (plusp (plugin-input-events plugin))
                 (port-loop (p i plugin)
                   if (and (event-port-p p) (input-port-p p))
                     collect (funcall reset-event-form
                                      (arg-symbol p))))
             ,@(get-output plugin block-size)))))))
