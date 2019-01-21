;;; Copyright (c) 2013-2019 Tito Latini
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

(defun plugin-pointer (uri)
  (declare (type string uri))
  (when (lilv:free-p lilv:*world*) (lv2-init))
  (let ((node (lilv:new-uri lilv:*world* uri)))
    (unwind-protect
         (lilv:plugins-get-by-uri
           (lilv:world-get-all-plugins lilv:*world*) node)
      (lilv:node-free node))))

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

(defmacro node-loop ((var nodes) &rest keywords-and-forms)
  "Iterate over the NODES with VAR bound to each node and the
KEYWORDS-AND-FORMS of the LOOP macro.

Example:

    (lilv:node-loop (n lilv-nodes)
      if (lilv:node-is-uri n) collect (lilv:node-as-uri n))"
  (let ((n (gensym)))
    `(loop for ,n = (nodes-begin ,nodes) then (nodes-next ,nodes ,n)
           for ,var = (nodes-get ,nodes ,n)
           until (nodes-is-end ,nodes ,n)
          ,@keywords-and-forms)))
