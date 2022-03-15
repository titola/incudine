;;; Copyright (c) 2014-2022 Tito Latini
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
  (object-to-free incudine.vug-foreign::ladspa-plugin-instantiate
                  incudine.vug-foreign::update-ladspa-instance)
  (object-to-free incudine.vug-foreign::make-ladspa-plugin-instance
                  incudine.vug-foreign::update-ladspa-plugin-state))

(in-package :incudine.vug-foreign)

(defstruct (ladspa-plugin (:include plugin)
                          (:constructor %make-ladspa-plugin)))

(defstruct (ladspa-plugin-instance (:include plugin-instance)))

(defun make-ladspa-plugin (filename label)
  (update-io-number
    (let ((descr (ladspa:plugin-descriptor filename label)))
      (unless descr
        (error 'foreign-plugin-error
               :format-control
                 "The LADSPA plugin ~S from ~S does not exist."
               :format-arguments (list label filename)))
      (%make-ladspa-plugin
        :name (ladspa:name descr)
        :path filename
        :pointer descr
        :label label
        :id (ladspa:unique-id descr)
        :author (ladspa:maker descr)
        :license (ladspa:copyright descr)
        :realtime-p (ladspa:realtime-p (ladspa:properties descr))
        :ports (make-ladspa-ports descr)
        :sample-type 'foreign-float
        :instantiate-cb (ladspa:descriptor-slot-value descr 'instantiate)
        :connect-port-cb (ladspa:descriptor-slot-value descr 'connect-port)
        :activate-cb (ladspa:descriptor-slot-value descr 'activate)
        :run-cb (ladspa:descriptor-slot-value descr 'run)
        :deactivate-cb (ladspa:descriptor-slot-value descr 'deactivate)
        :cleanup-cb (ladspa:descriptor-slot-value descr 'cleanup)))))

(defun make-ladspa-ports (descriptor)
  (let ((names (ladspa:descriptor-slot-value descriptor 'port-names)))
    (coerce
      (loop for i below (ladspa:port-count descriptor)
            collect (make-port
                      :name (cffi:mem-aref names :string i)
                      :id i
                      :type (ladspa-port-type descriptor i)
                      :default (ladspa-port-default descriptor i)
                      :value-type 'foreign-float))
      'simple-vector)))

(defmethod make-load-form ((obj ladspa-plugin) &optional environment)
  (declare (ignore environment))
  `(make-ladspa-plugin ,(ladspa-plugin-path obj) ,(ladspa-plugin-label obj)))

(defun ladspa-port-type (descriptor index)
  (let ((n (cffi:mem-aref (ladspa:descriptor-slot-value descriptor
                                                        'port-descriptors)
                          :int index)))
    (logior (if (ladspa:port-input-p n) +input-port+ 0)
            (if (ladspa:port-output-p n) +output-port+ 0)
            (if (ladspa:port-control-p n) +control-port+ 0)
            (if (ladspa:port-audio-p n) +audio-port+ 0))))

(defun ladspa-port-default (descriptor index
                            &optional (sample-rate 'incudine.util:*sample-rate*))
  (ladspa:hint-default
    (cffi:mem-aptr (ladspa:descriptor-slot-value descriptor 'port-range-hints)
                   '(:struct ladspa:port-range-hint) index)
    sample-rate))

(declaim (inline ladspa-plugin-instantiate))
(defun ladspa-plugin-instantiate (plugin &optional arg)
  (declare (ignore arg))
  ;; FLOATING-POINT-INVALID-OPERATION with some plugins written in c++
  (incudine.util::without-float-invalid-op-trap
    (ladspa:instantiate (ladspa-plugin-instantiate-cb plugin)
                        (ladspa-plugin-pointer plugin)
                        (sample->fixnum *sample-rate*))))

(defmacro update-ladspa-instance (vug-varname args)
  (declare (ignore vug-varname))
  ;; ARGS are the arguments passed to LADSPA-PLUGIN-INSTANTIATE, so
  ;; the second argument is REINIT-P. When REINIT-P is NIL, the ports
  ;; are connected and the plugin-instance is activated.
  `(setf ,(second args) t))

(defmacro update-ladspa-plugin-state (vug-varname args)
  (declare (ignore vug-varname args))
  nil)

(defmethod incudine:free ((obj ladspa:handle))
  (msg debug "Cleanup LADSPA plugin")
  (ladspa:cleanup obj))

(declaim (inline ladspa-connect-port))
(defun ladspa-connect-port (plugin instance-ptr index data-location)
  (cffi:foreign-funcall-pointer (ladspa-plugin-connect-port-cb plugin) ()
                                :pointer instance-ptr :unsigned-long index
                                :pointer data-location :void))

(defun ladspa-connect-port-form (plugin instance instance-ptr block-size)
  `(reduce-warnings
     ,@(port-loop (p i plugin)
         for name = (arg-symbol p)
         collect
           (let ((ptr (if (and (> block-size 1) (audio-port-p p))
                          name
                          `(get-pointer ,name))))
             `(progn (ladspa-connect-port ,plugin ,instance-ptr ,i ,ptr)
                     (setf (aref (plugin-instance-port-pointers ,instance) ,i)
                           ,ptr))))))

(defmacro ladspa-?activate (plugin instance instance-ptr activate-p)
  (with-gensyms (cb)
    `(let ((,cb (,(if activate-p
                      'ladspa-plugin-activate-cb
                      'ladspa-plugin-deactivate-cb)
                 ,plugin)))
       (unless (cffi:null-pointer-p ,cb)
         (cffi:foreign-funcall-pointer ,cb () :pointer ,instance-ptr :void)
         (setf (ladspa::flag-value (ladspa::handle-activated ,instance))
               ,(not activate-p))))))

(declaim (inline ladspa-activate))
(defun ladspa-activate (plugin instance instance-ptr)
  (ladspa-?activate plugin instance instance-ptr t))

(declaim (inline ladspa-deactivate))
(defun ladspa-deactivate (plugin instance instance-ptr)
  (ladspa-?activate plugin instance instance-ptr nil))

(declaim (inline ladspa-run))
(defun ladspa-run (plugin instance-ptr sample-count)
  (cffi:foreign-funcall-pointer (ladspa-plugin-run-cb plugin) ()
                                :pointer instance-ptr
                                :unsigned-long sample-count :void))

(defun %ladspa->vug (filename label vug-name block-size)
  (let ((plugin (make-ladspa-plugin filename label))
        (plugin-instance (ensure-symbol "PLUGIN-INSTANCE")))
    `(with-vug-plugin (,vug-name ,plugin ,block-size t)
       (with ((reinit-p nil)
              (ladspa-obj (ladspa-plugin-instantiate ,plugin reinit-p))
              (ladspa-ptr (ladspa::handle-ptr ladspa-obj))
              (ladspa-state
                (reduce-warnings
                  (make-ladspa-plugin-instance
                    :label ,(plugin-label plugin)
                    :handle-pointer ladspa-ptr
                    :port-pointers
                      (make-array ,(length (plugin-ports plugin)))))))
         (declare (type foreign-pointer ladspa-ptr)
                  (type boolean reinit-p)
                  (preserve reinit-p))
         (initialize
           (setf ,plugin-instance ladspa-state)
           ;; REINIT-P is NIL only the first time. It becomes T in
           ;; UPDATE-LADSPA-INSTANCE
           (if reinit-p
               (ladspa-deactivate ,plugin ladspa-obj ladspa-ptr)
               ,(ladspa-connect-port-form
                  plugin plugin-instance 'ladspa-ptr block-size))
           (ladspa-activate ,plugin ladspa-obj ladspa-ptr))
         (ladspa-run ,plugin ladspa-ptr ,block-size)))))

(defmacro vug:ladspa->vug (filename label vug-name
                           &key (block-size (block-size)) debug-p)
  "Define a new VUG and the auxiliary function named VUG-NAME to use
the LADSPA plugin with LABEL loaded from FILENAME.

FILENAME is the namestring of the plugin path, absolute or relative to
LADSPA:*LADSPA-PATH*, with or without type extension.

The control parameter PLUGIN-INSTANCE is set to the plugin instance
during the DSP initialization. It is possible to retrieve the plugin
instance through CONTROL-VALUE. This functionality works only with a
VUG or an inlined UGEN.

All the arguments of the auxiliary function are optional keywords.

If BLOCK-SIZE is not set and the incudine block size changes,
LADSPA->VUG should be called again.

If DEBUG-P is T, return the lisp form to define the VUG.

Return the new VUG structure."
  (if debug-p
      `(%ladspa->vug ,filename ,label ',vug-name ,block-size)
      `(macrolet ((generate (f l n bs) (%ladspa->vug f l n bs)))
         (generate ,filename ,label ,vug-name ,block-size))))
