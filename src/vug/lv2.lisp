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

(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'lv2->vug (find-package :incudine.vug))
  (object-to-free incudine.vug-foreign::lv2-plugin-instantiate
                  incudine.vug-foreign::update-lv2-instance))

(in-package :incudine.vug-foreign)

(defstruct (lv2-plugin (:include plugin)
                       (:constructor %make-lv2-plugin)))

;;; MEMO: don't use a temporary lilv instance to retrieve the pointers
;;; of the callbacks because it causes a crash during the initialization
;;; of the first UGEN/DSP instance. It is safe to use the descriptor
;;; in LilvInstanceImpl struct within a UGEN/DSP.
(defun make-lv2-plugin (uri)
  (update-io-number
    (let* ((ptr (lilv:plugin-pointer uri)))
      (%make-lv2-plugin
        :name (lilv:node-as-string (lilv:plugin-get-name ptr))
        :path uri
        :pointer ptr
        :author (lilv:node-as-string (lilv:plugin-get-author-name ptr))
        :realtime-p t
        :ports (make-lv2-ports ptr)
        :sample-type 'foreign-float))))

(defun make-lv2-ports (plugin)
  (coerce
    (loop for i below (lilv:plugin-get-num-ports plugin)
          for port = (lilv:plugin-get-port-by-index plugin i)
          collect (make-port
                    :name (lilv:node-as-string
                            (lilv:port-get-symbol plugin port))
                    :id i
                    :type (lv2-port-type plugin port)
                    :value-type 'foreign-float))
    'simple-vector))

(defmethod make-load-form ((obj lv2-plugin) &optional environment)
  (declare (ignore environment))
  `(make-lv2-plugin ,(lv2-plugin-path obj)))

(defmethod print-object ((obj lv2-plugin) stream)
  (format stream "#<LV2-PLUGIN ~S>" (lv2-plugin-path obj)))

(defun lv2-port-type (plugin port)
  (flet ((portp (port-class class-value)
           (if (lilv:port-is-a plugin port port-class) class-value 0)))
    (logior (portp lilv:*uri-input-port* +input-port+)
            (portp lilv:*uri-output-port* +output-port+)
            (portp lilv:*uri-control-port* +control-port+)
            (portp lilv:*uri-audio-port* +audio-port+)
            (portp lilv:*uri-event-port* +event-port+)
            (portp lilv:*uri-midi-port* +midi-port+))))

(declaim (inline lv2-plugin-instantiate))
(defun lv2-plugin-instantiate (plugin &optional arg)
  (declare (ignore arg))
  (reduce-warnings
    (lilv:plugin-instantiate (lv2-plugin-pointer plugin) *sample-rate*
                             (cffi:null-pointer))))

(defmacro update-lv2-instance (vug-varname args)
  (declare (ignore vug-varname))
  ;; ARGS are the arguments passed to LV2-PLUGIN-INSTANTIATE, so the second
  ;; argument is REINIT-P. When REINIT-P is NIL, the ports are connected
  ;; and the plugin-instance is activated.
  `(setf ,(second args) t))

(defmethod incudine:free ((obj lilv:instance))
  (msg debug "Cleanup LV2 plugin")
  (lilv:free obj))

(defun lv2-connect-port-form (plugin descriptor handle block-size)
  `(progn
     ,@(port-loop (p i plugin)
         for name = (arg-symbol p)
         if (or (input-port-p p) (output-port-p p))
         collect `(lilv:connect-port
                    (lilv:descriptor-slot-value ,descriptor 'lv2::connect-port)
                      ,handle ,i ,(if (and (> block-size 1)
                                           (audio-port-p p))
                                      name
                                      `(get-pointer ,name))))))

(defmacro lv2-instance-?activate (instance descriptor handle activate-p)
  (with-gensyms (cb)
    `(let ((,cb (lilv:descriptor-slot-value ,descriptor
                  ',(if activate-p 'lv2::activate 'lv2::deactivate))))
       (unless (cffi:null-pointer-p ,cb)
         (cffi:foreign-funcall-pointer ,cb () :pointer ,handle :void)
         (setf (lilv::instance-active-p ,instance) ,(not activate-p))))))

(declaim (inline lv2-instance-activate))
(defun lv2-instance-activate (instance descriptor handle)
  (lv2-instance-?activate instance descriptor handle t))

(declaim (inline lv2-instance-deactivate))
(defun lv2-instance-deactivate (instance descriptor handle)
  (lv2-instance-?activate instance descriptor handle nil))

(declaim (inline lv2-run))
(defun lv2-run (descriptor handle sample-count)
  (cffi:foreign-funcall-pointer
    (lilv:descriptor-slot-value descriptor 'lv2::run) nil :pointer handle
    :uint32 sample-count :void))

(defmacro lv2-handle (instance-pointer)
  `(lilv:instance-impl-slot-value ,instance-pointer 'lv2:handle))

(defmacro lv2-descriptor (instance-pointer)
  `(lilv:instance-impl-slot-value ,instance-pointer 'lv2:descriptor))

(defmethod doc-string ((p lv2-plugin))
  (format nil "~A - LV2 plugin by ~A.~%URI: ~A"
          (lv2-plugin-name p) (lv2-plugin-author p) (lv2-plugin-path p)))

(defun %lv2->vug (uri vug-name block-size)
  (let ((plugin (make-lv2-plugin uri)))
    `(with-vug-plugin (,vug-name ,plugin ,block-size)
       (with ((reinit-p nil)
              (lv2-obj (lv2-plugin-instantiate ,plugin reinit-p))
              (lv2-ptr (lilv::instance-pointer lv2-obj))
              (lv2-handle (lv2-handle lv2-ptr))
              (lv2-descr (lv2-descriptor lv2-ptr)))
         (declare (type foreign-pointer lv2-ptr lv2-handle lv2-descr)
                  (type boolean reinit-p)
                  (preserve reinit-p))
         (initialize
          ;; REINIT-P is NIL only the first time. It becomes T in
          ;; UPDATE-LV2-INSTANCE
          (if reinit-p
              (lv2-instance-deactivate lv2-obj lv2-descr lv2-handle)
              ,(lv2-connect-port-form plugin 'lv2-descr 'lv2-handle
                                      block-size))
          (lv2-instance-activate lv2-obj lv2-descr lv2-handle))
         (lv2-run lv2-descr lv2-handle ,block-size)))))

(defmacro vug:lv2->vug (uri vug-name
                        &optional (block-size (block-size)) debug-p)
  (if debug-p
      `(%lv2->vug ,uri ',vug-name ,block-size)
      `(macrolet ((generate (u n bs) (%lv2->vug u n bs)))
         (generate ,uri ,vug-name ,block-size))))
