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

(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'ladspa->vug (find-package :incudine.vug))
  (object-to-free ladspa-plugin-instantiate update-ladspa-instance))

(deftype ladspa-sample () 'single-float)

;;; Auxiliary structure for a foreign descriptor pointer
(defstruct (ladspa-descriptor (:constructor %make-ladspa-descriptor))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (filename (error "missing filename") :type string)
  (label (error "missing label") :type string))

(defun make-ladspa-descriptor (filename label)
  (%make-ladspa-descriptor
   :pointer (ladspa:plugin-descriptor filename label)
   :filename filename
   :label label))

(defmethod make-load-form ((obj ladspa-descriptor) &optional environment)
  (declare (ignore environment))
  `(make-ladspa-descriptor ,(ladspa-descriptor-filename obj)
                           ,(ladspa-descriptor-label obj)))

(defmethod print-object ((obj ladspa-descriptor) stream)
  (format stream "#<~S ~S ~S #X~8,'0X>"
          (type-of obj) (ladspa-descriptor-filename obj)
          (ladspa-descriptor-label obj)
          (cffi:pointer-address (ladspa-descriptor-pointer obj))))

;;; Auxiliary structure for a foreign callback pointer
(defstruct (ladspa-callback (:constructor %make-ladspa-callback))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (name (error "missing slot-name") :type symbol)
  (descriptor (error "missing ladspa descriptor pointer")
              :type ladspa-descriptor))

(defun make-ladspa-callback (name descriptor)
  (%make-ladspa-callback
   :pointer (ladspa:descriptor-slot-value (ladspa-descriptor-pointer descriptor)
                                          name)
   :name name
   :descriptor descriptor))

(defmethod make-load-form ((obj ladspa-callback) &optional environment)
  (declare (ignore environment))
  `(make-ladspa-callback ',(ladspa-callback-name obj)
                         ,(ladspa-callback-descriptor obj)))

(defmethod print-object ((obj ladspa-callback) stream)
  (format stream "#<~S ~A #X~8,'0X>"
          (type-of obj) (ladspa-callback-name obj)
          (cffi:pointer-address (ladspa-callback-pointer obj))))

(defmacro with-ladspa-callbacks (callbacks descriptor &body body)
  `(let ,(mapcar (lambda (x)
                   `(,(car x) (make-ladspa-callback ',(cadr x) ,descriptor)))
                 callbacks)
     ,@body))

;;; Defined as macro to reduce the inlined functions inside the
;;; definition of a DSP
(defmacro ladspa-sample (number)
  `(coerce ,number 'ladspa-sample))

(declaim (inline ladspa-plugin-instantiate))
(defun ladspa-plugin-instantiate (callback descriptor)
  (ladspa:instantiate (ladspa-callback-pointer callback)
                      (ladspa-descriptor-pointer descriptor)
                      (sample->fixnum *sample-rate*)))

(defmacro update-ladspa-instance (vug-varname args)
  (declare (ignore args))
  `(progn (ladspa:activate ,vug-varname) ,vug-varname))

(defmethod incudine:free ((obj ladspa:handle))
  (msg debug "Cleanup LADSPA plugin")
  (ladspa:cleanup obj))

;;; Lispify the names of the ports
(defun ladspa-port-name-to-symbol (name)
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
                  (return (intern (string-upcase
                                   (coerce (nreverse acc) 'string)))))))

(defun ladspa-port-symbol-names (descriptor)
  (let ((input-ports nil)
        (output-ports nil)
        (descr-ptr (ladspa-descriptor-pointer descriptor)))
    (loop for port-descr in (ladspa:port-descriptors descr-ptr)
          for name in (mapcar #'ladspa-port-name-to-symbol
                              (ladspa:port-names descr-ptr))
          for i from 0
          for port = (cons i name) do
         (if (ladspa:port-input-p port-descr)
             (push port input-ports)
             (push port output-ports)))
    (values (nreverse input-ports) (nreverse output-ports))))

(defun ladspa-connect-port-form (instance callback input-ports
                                 output-ports)
  (loop for (index . name) in (append input-ports output-ports)
        collect `(ladspa:connect-port (ladspa-callback-pointer ,callback)
                                      ,instance ,index
                                      (get-pointer ,name))))

(defun ladspa-get-output (output-names frame-p frame)
  (if frame-p
      ;; Multiple outputs in a frame
      `(,@(loop for out in output-names
                for index from 0
                collect `(setf (frame-ref ,frame ,index) (sample ,out)))
        ,frame)
      ;; Single output
      `((sample ,(car output-names)))))

;;; A simple doc-string for the generated VUG.
(defun ladspa-doc-string (descriptor)
  (let ((descr-ptr (ladspa-descriptor-pointer descriptor)))
    (format nil "~A.~%LADSPA plugin (~A/~D) by ~A."
            (ladspa:name descr-ptr) (ladspa:label descr-ptr)
            (ladspa:unique-id descr-ptr) (ladspa:maker descr-ptr))))

(defun %ladspa->vug (filename label vug-name)
  (flet ((make-names (params)
           (mapcar (lambda (x) (gensym (symbol-name (cdr x)))) params))
         (new-param (new-names param)
           (mapcar (lambda (n p) (cons (car p) n)) new-names param))
         (input-variables (names values)
           (mapcar (lambda (in par) `(,in (ladspa-sample ,par)))
                   names values))
         (output-variables (names)
           (mapcar (lambda (out) `(,out (ladspa-sample 0.0))) names)))
    (let ((descriptor (make-ladspa-descriptor filename label)))
      (multiple-value-bind (input-param output-param)
          (ladspa-port-symbol-names descriptor)
        (let* ((input-names (make-names input-param))
               (output-names (make-names output-param))
               (arg-names (mapcar #'cdr input-param))
               (input-param (new-param input-names input-param))
               (output-param (new-param output-names output-param))
               (frame-p (and (cdr output-param) t)))
          (with-ladspa-callbacks ((instantiate-cb ladspa:instantiate)
                                  (connect-port-cb ladspa:connect-port)
                                  (run-cb ladspa:run))
              descriptor
            (with-gensyms (frame instance reinit-p)
              `(define-vug ,vug-name ,arg-names
                 ,(ladspa-doc-string descriptor)
                 (with (,@(input-variables input-names arg-names)
                        ,@(output-variables output-names)
                        ,@(when frame-p
                            `((,frame (make-frame ,(length output-param)))))
                        (,instance (ladspa-plugin-instantiate ,instantiate-cb
                                                              ,descriptor))
                        (,reinit-p nil))
                   (declare (type foreign-float ,@input-names ,@output-names))
                   (initialize
                     (unless ,reinit-p
                       ;; Initialize only after the allocation of the instance
                       (reduce-warnings
                         ,@(ladspa-connect-port-form instance connect-port-cb
                                                     input-param output-param)
                         (ladspa:activate ,instance)
                         (setf ,reinit-p t))))
                   ;; Expand the inputs if they are performance-time
                   (maybe-expand ,@input-names)
                   ;; Process one sample
                   (ladspa:run (ladspa-callback-pointer ,run-cb) ,instance 1)
                   ;; Retrieve the output(s)
                   ,@(ladspa-get-output output-names frame-p frame))))))))))

(defmacro ladspa->vug (filename label vug-name &optional debug-p)
  (if debug-p
      `(%ladspa->vug ,filename ,label ',vug-name)
      `(macrolet ((generate (f l n) (%ladspa->vug f l n)))
         (generate ,filename ,label ,vug-name))))
