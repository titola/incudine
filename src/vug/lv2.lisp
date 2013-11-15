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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(lv2-init lv2->vug)
          (find-package :incudine.vug)))

(deftype lv2-sample () 'single-float)

(defstruct lv2-plugin
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (uri (error "missing uri") :type string)
  ;; Number of the ports
  (ports 0 :type non-negative-fixnum)
  ;; Number of the input ports
  (input-ports 0 :type non-negative-fixnum)
  ;; Number of the output ports
  (output-ports 0 :type non-negative-fixnum)
  ;; List of lists (index name &optional event-p) for the inputs
  (input-args nil :type list)
  ;; List of lists (index name) for the outputs (the output events are
  ;; considered input events; they are simply pointers to LV2:EVENT-BUFFER).
  (output-args nil :type list))

(defmethod make-load-form ((obj lv2-plugin) &optional environment)
  (declare (ignore environment))
  `(get-lv2-plugin ,(lv2-plugin-uri obj)))

(defvar lilv-uri-audio-port   (cffi:null-pointer))
(defvar lilv-uri-control-port (cffi:null-pointer))
(defvar lilv-uri-input-port   (cffi:null-pointer))
(defvar lilv-uri-output-port  (cffi:null-pointer))
(defvar lilv-uri-event-port   (cffi:null-pointer))
(defvar lilv-uri-midi-port    (cffi:null-pointer))

(defun lv2-init ()
  (lilv:init-world)
  (macrolet ((set-var (var-name uri)
               `(setf ,var-name (lilv:new-uri lilv::*world* ,uri))))
    (set-var lilv-uri-audio-port   "http://lv2plug.in/ns/lv2core#AudioPort")
    (set-var lilv-uri-control-port "http://lv2plug.in/ns/lv2core#ControlPort")
    (set-var lilv-uri-input-port   "http://lv2plug.in/ns/lv2core#InputPort")
    (set-var lilv-uri-output-port  "http://lv2plug.in/ns/lv2core#OutputPort")
    (set-var lilv-uri-event-port   "http://lv2plug.in/ns/ext/event#EventPort")
    (set-var lilv-uri-midi-port    "http://lv2plug.in/ns/ext/midi#MidiEvent")
    (values)))

;;; Defined as macro to reduce the inlined functions inside the
;;; definition of a DSP
(defmacro lv2-sample (number)
  `(coerce ,number 'lv2-sample))

(declaim (inline lv2-plugin-instantiate))
(defun lv2-plugin-instantiate (plugin)
  (locally (declare #.*reduce-warnings*)
    (lilv:plugin-instantiate (lv2-plugin-pointer plugin)
                             *sample-rate*
                             (cffi:null-pointer))))

(defmacro make-lv2-instance (plugin)
  (with-gensyms (instance)
    `(with ((,instance (lv2-plugin-instantiate ,plugin)))
       ,instance)))

(defmacro update-lv2-instance (vug-varname args)
  (declare (ignore args))
  `(progn (lilv:instance-deactivate ,vug-varname)
          (lilv:instance-activate ,vug-varname)
          ,vug-varname))

(defmethod incudine:free ((obj lilv:instance))
  (lilv:free obj))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (object-to-free lv2-plugin-instantiate update-lv2-instance))

(declaim (inline lv2-run))
(defun lv2-run (fn handle sample-count)
  (cffi:foreign-funcall-pointer fn () :pointer handle :uint32 sample-count :void))

(declaim (inline lv2-port-symbol))
(defun lv2-port-symbol (plugin port)
  (intern (string-upcase
           (lilv:node-as-string
            (lilv:port-get-symbol plugin port)))))

(declaim (inline lv2-input-port-p))
(defun lv2-input-port-p (plugin port)
  (lilv:port-is-a plugin port lilv-uri-input-port))

(declaim (inline lv2-output-port-p))
(defun lv2-output-port-p (plugin port)
  (lilv:port-is-a plugin port lilv-uri-output-port))

(declaim (inline lv2-event-port-p))
(defun lv2-event-port-p (plugin port)
  (or (lilv:port-is-a plugin port lilv-uri-event-port)
      (lilv:port-is-a plugin port lilv-uri-midi-port)))

(defun analyze-lv2-ports (plugin)
  (declare #.*standard-optimize-settings*)
  (let ((inputs nil)
        (outputs nil)
        (ports (lilv:plugin-get-num-ports plugin))
        (input-ports 0)
        (output-ports 0))
    (declare (type non-negative-fixnum input-ports output-ports)
             (type list inputs outputs))
    (loop for i fixnum below ports
          for port = (lilv:plugin-get-port-by-index plugin i) do
         (cond ((lv2-input-port-p plugin port)
                (push (list* i (lv2-port-symbol plugin port)
                             (if (lv2-event-port-p plugin port) '(t)))
                      inputs)
                (incf input-ports))
               ((lv2-output-port-p plugin port)
                (cond ((lv2-event-port-p plugin port)
                       ;; The output events are considered input events
                       (push (list i (lv2-port-symbol plugin port) t) inputs)
                       (incf input-ports))
                      (t (push (list i (lv2-port-symbol plugin port)) outputs)
                         (incf output-ports))))))
    (values (the non-negative-fixnum (+ input-ports output-ports))
            input-ports output-ports
            (nreverse inputs) (nreverse outputs))))

(defun get-lv2-plugin (uri)
  (declare (type string uri))
  (if (lilv:free-p lilv:*world*) (lv2-init))
  (let ((node (lilv:new-uri lilv:*world* uri)))
    (unwind-protect
         (let ((plugin-ptr (lilv:plugins-get-by-uri
                            (lilv:world-get-all-plugins lilv:*world*)
                            node)))
           (multiple-value-bind (ports input-ports output-ports
                                 inputs outputs)
               (analyze-lv2-ports plugin-ptr)
             (make-lv2-plugin :pointer plugin-ptr :uri uri :ports ports
                              :input-ports input-ports
                              :output-ports output-ports
                              :input-args inputs :output-args outputs)))
      (lilv:node-free node))))

(defun lv2-connect-ports (instance input-param input-names
                          output-param output-names event-param)
  (flet ((index-and-var-list (param-list var-names)
           (mapcar (lambda (param name) (cons (car param) name))
                   param-list var-names)))
    (let ((input-ports (index-and-var-list input-param input-names))
          (output-ports (index-and-var-list output-param output-names)))
      (nconc
       (loop for (index . name) in (append input-ports output-ports)
             collect `(lilv:instance-connect-port
                       ,instance ,index (get-pointer ,name)))
       (loop for (index . name) in event-param
             collect `(lilv:instance-connect-port ,instance ,index ,name))))))

(defmacro lv2-get-input-names (input-param event-param filtered-input-param)
  (with-gensyms (p name)
    `(mapcar (lambda (,p)
               (cond ((third ,p)
                      (let ((,name
                             ;; Name for an argument connected to an event port
                             (format-symbol *package* "LV2-~A-EV" (second ,p))))
                        (push (cons (first ,p) ,name) ,event-param)
                        ;; The type of the input is a pointer to LV2:EVENT-BUFFER
                        `(,,name cffi:foreign-pointer)))
                     (t (push ,p ,filtered-input-param)
                      ;; The type of the input is SAMPLE
                      (second ,p))))
             ,input-param)))

(defun lv2-get-output (output-names frame-p frame)
  (if frame-p
      ;; Multiple outputs in a frame
      `(,@(loop for out in output-names
                for index from 0
                collect `(setf (frame-ref ,frame ,index)
                               (sample ,out)))
        ,frame)
      ;; Single output
      `((sample ,(car output-names)))))

;;; A simple doc-string for the generated VUG.
(defun lv2-doc-string (plugin)
  (let ((ptr (lv2-plugin-pointer plugin)))
    (format nil "~A - LV2 plugin by ~A.~%URI: ~A"
            (lilv:node-as-string (lilv:plugin-get-name ptr))
            (lilv:node-as-string (lilv:plugin-get-author-name ptr))
            (lv2-plugin-uri plugin))))

(defun %lv2->vug (uri name)
  (declare (type string uri) (type symbol name))
  (with-gensyms (instance descriptor handle run-cb frame reinit-p)
    (flet ((make-names (params)
             (mapcar (lambda (x) (gensym (symbol-name x)))
                     (delete-if #'consp params))))
      (let* ((plugin (get-lv2-plugin uri))
             (input-param (lv2-plugin-input-args plugin))
             ;; List of the input parameters connected to control/audio ports
             (filtered-input-param nil)
             (output-param (lv2-plugin-output-args plugin))
             (output-ports (lv2-plugin-output-ports plugin))
             (event-param nil)
             (arg-names (lv2-get-input-names input-param event-param
                                             filtered-input-param))
             (input-names (make-names arg-names))
             (output-names (make-names (mapcar #'second output-param)))
             ;; Use a frame if there are two or more outputs
             (frame-p (> output-ports 1)))
        `(define-vug ,name ,arg-names
           ,(lv2-doc-string plugin)
           (with (,@(mapcar (lambda (in par) `(,in (lv2-sample ,par)))
                            input-names
                            (delete-if #'consp arg-names))
                  ,@(mapcar (lambda (out) `(,out (lv2-sample 0.0)))
                            output-names)
                  ,@(if frame-p `((,frame (make-frame ,output-ports))))
                  (,instance (make-lv2-instance ,plugin))
                  (,descriptor nil)
                  (,handle nil)
                  (,run-cb nil)
                  (,reinit-p nil))
             (declare (type foreign-float ,@input-names ,@output-names))
             (initialize
              (unless ,reinit-p
                ;; Initialize only after the allocation of the instance
                (locally (declare #.*reduce-warnings*)
                  ,@(lv2-connect-ports instance (nreverse filtered-input-param)
                                       input-names output-param output-names
                                       event-param)
                  (setf ,descriptor (lilv:instance-get-descriptor ,instance))
                  (setf ,handle (lilv:instance-get-handle ,instance))
                  (setf ,run-cb (lilv:descriptor-slot-value ,descriptor
                                                            'lv2::run))
                  (lilv:instance-activate ,instance)
                  (setf ,reinit-p t))))
             ;; Expand the inputs if they are performance-time
             ,@input-names
             ;; Process one sample
             (lv2-run ,run-cb ,handle 1)
             ;; Retrieve the output(s)
             ,@(lv2-get-output output-names frame-p frame)))))))

(defmacro lv2->vug (uri name &optional debug-p)
  (if debug-p
      `(%lv2->vug ,uri ',name)
      `(macrolet ((generate (u n) (%lv2->vug u n)))
         (generate ,uri ,name))))
