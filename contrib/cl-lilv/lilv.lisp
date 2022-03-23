;;; Copyright (c) 2013-2022 Tito Latini
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

(in-package :lilv)

(defvar *world* (make-world))

(declaim (inline free-p))
(defun free-p (obj)
  (declare (type pointer-wrap))
  (cffi:null-pointer-p (pointer-wrap-pointer obj)))

(defun init-world ()
  (when (free-p *world*)
    (setf *world* (world-new))
    (world-load-all *world*)
    *world*))

(defvar *uri-atom-port* (cffi:null-pointer))
(defvar *uri-audio-port* (cffi:null-pointer))
(defvar *uri-control-port* (cffi:null-pointer))
(defvar *uri-input-port* (cffi:null-pointer))
(defvar *uri-output-port* (cffi:null-pointer))
(defvar *uri-event-port* (cffi:null-pointer))
(defvar *uri-midi-port* (cffi:null-pointer))

(defun lv2-init ()
  "Initialize the Lilv World that represents all Lilv state."
  (lilv:init-world)
  (macrolet ((set-var (var-name uri)
               `(setf ,var-name (lilv:new-uri lilv::*world* ,uri))))
    (set-var *uri-atom-port* "http://lv2plug.in/ns/ext/atom#AtomPort")
    (set-var *uri-audio-port* "http://lv2plug.in/ns/lv2core#AudioPort")
    (set-var *uri-control-port* "http://lv2plug.in/ns/lv2core#ControlPort")
    (set-var *uri-input-port* "http://lv2plug.in/ns/lv2core#InputPort")
    (set-var *uri-output-port* "http://lv2plug.in/ns/lv2core#OutputPort")
    (set-var *uri-event-port* "http://lv2plug.in/ns/ext/event#EventPort")
    (set-var *uri-midi-port* "http://lv2plug.in/ns/ext/midi#MidiEvent")
    (values)))

(defmacro with-node ((var type value) &body body)
  "Bind VAR to a newly allocated Lilv node of TYPE with dynamic extent
during BODY.

TYPE is one of :BOOL, :FLOAT, :INT, :STRING or :URI."
  (let ((func (find-symbol (format nil "NEW-~A" (string type)) "LILV")))
    (unless func
      (error 'simple-type-error
             :format-control "Unknown Lilv node type ~S"
             :format-arguments (list type)))
    `(let ((,var (,func *world* ,value)))
       (unwind-protect
            (progn ,@body)
         (lilv:node-free ,var)))))

(defmacro with-nodes (bindings &body body)
  "Create bindings to newly allocated Lilv nodes with dynamic extent
during BODY.

BINDINGS is a list of lists

    (var type value)

where VAR is the variable bound to a node of TYPE.

TYPE is one of :BOOL, :FLOAT, :INT, :STRING or :URI."
  (if bindings
      `(with-node ,(car bindings)
         (with-nodes ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defun plugin-pointer (uri)
  (declare (type string uri))
  (when (lilv:free-p lilv:*world*) (lv2-init))
  (with-node (node :uri uri)
    (lilv:plugins-get-by-uri
      (lilv:world-get-all-plugins lilv:*world*) node)))

(declaim (inline lv2:features))
(defun lv2:features ()
  "Return the NULL-terminated foreign array of LV2 Features supported by
Incudine. The foreign array is allocated during the initialization of the
Lilv World."
  (world-features *world*))

(defgeneric free (obj))

(defmethod free ((obj world))
  (unless (free-p obj)
    (world-free (world-pointer obj))
    (cffi:foreign-free (world-features obj))
    (cancel-finalization obj)
    (lv2::free-features)
    (setf (world-pointer obj) (cffi:null-pointer))
    (setf (world-features obj) (cffi:null-pointer))
    (values)))

(defmethod free ((obj instance))
  (unless (free-p obj)
    (let ((ptr (instance-pointer obj)))
      (when (instance-active-p obj)
        (instance-deactivate obj)
        (setf (instance-active-p obj) nil))
      (instance-free ptr)
      (cancel-finalization obj)
      (setf (instance-pointer obj) (cffi:null-pointer))
      (values))))

;;; Iterator similar to LILV_FOREACH(colltype, iter, collection) in lilv.h.
(defmacro iter ((var type collection &optional coll-var)
                &rest keywords-and-forms)
  (flet ((f (control type)
           (find-symbol (format nil control type) "LILV")))
    (let* ((type (string type))
           (begin (f "~A-BEGIN" type)))
      (unless begin
        (error 'simple-type-error
               :format-control "Unknown Lilv iterator for type ~S"
               :format-arguments (list type)))
      (let ((endp (f "~A-IS-END" type))
            (next (f "~A-NEXT" type))
            (coll (or coll-var (gensym))))
        `(loop :with ,coll = ,collection
               :for ,var = (,begin ,coll) :then (,next ,coll ,var)
               :until (,endp ,coll ,var)
               ,@keywords-and-forms)))))

(defmacro object-loop ((var type collection) &body body)
  (let ((get (find-symbol (format nil "~A-GET" type) "LILV")))
    (with-gensyms (i coll)
      `(iter (,i ,type ,collection ,coll)
         :for ,var = (,get ,coll ,i)
         ,@body))))

(defmacro node-loop ((var nodes) &rest keywords-and-forms)
  "Iterate over the NODES with VAR bound to each node.
LILV:NODE-LOOP supports KEYWORDS-AND-FORMS of the LOOP macro.

Example:

    (lilv:node-loop (n lilv-nodes)
      if (lilv:node-is-uri n) collect (lilv:node-as-uri n))"
  `(object-loop (,var nodes ,nodes) ,@keywords-and-forms))

(defmacro plugin-class-loop ((var plugin-classes) &rest keywords-and-forms)
  "Iterate over the PLUGIN-CLASSES with VAR bound to each class.
LILV:PLUGIN-CLASS-LOOP supports KEYWORDS-AND-FORMS of the LOOP macro."
  `(object-loop (,var plugin-classes ,plugin-classes) ,@keywords-and-forms))

(defmacro plugin-loop ((var plugins) &rest keywords-and-forms)
  "Iterate over the PLUGINS with VAR bound to each plugin.
LILV:PLUGIN-LOOP supports KEYWORDS-AND-FORMS of the LOOP macro.

Example:

    (lilv:init-world)
    (lilv:plugin-loop (p (lilv:world-get-all-plugins lilv:*world*))
      collect (lilv:node-as-uri (lilv:plugin-get-uri p)))"
  `(object-loop (,var plugins ,plugins) ,@keywords-and-forms))

(defmacro scale-point-loop ((var scale-points) &rest keywords-and-forms)
  "Iterate over the SCALE-POINTS with VAR bound to each point.
LILV:SCALE-POINT-LOOP supports KEYWORDS-AND-FORMS of the LOOP macro."
  `(object-loop (,var scale-points ,scale-points) ,@keywords-and-forms))

(defmacro ui-loop ((var uis) &rest keywords-and-forms)
  "Iterate over the UIS with VAR bound to each UI.
LILV:UI-LOOP supports KEYWORDS-AND-FORMS of the LOOP macro."
  `(object-loop (,var uis ,uis) ,@keywords-and-forms))
