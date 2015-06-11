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
  (object-to-free incudine.vug-foreign::ladspa-plugin-instantiate
                  incudine.vug-foreign::update-ladspa-instance))

(in-package :incudine.vug-foreign)

(defstruct (ladspa-plugin (:include plugin)
                          (:constructor %make-ladspa-plugin)))

(defun make-ladspa-plugin (filename label)
  (update-io-number
    (let* ((descr (ladspa:plugin-descriptor filename label)))
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

(defmethod incudine:free ((obj ladspa:handle))
  (msg debug "Cleanup LADSPA plugin")
  (ladspa:cleanup obj))

(declaim (inline ladspa-connect-port))
(defun ladspa-connect-port (plugin instance-ptr index data-location)
  (cffi:foreign-funcall-pointer (ladspa-plugin-connect-port-cb plugin) ()
                                :pointer instance-ptr :unsigned-long index
                                :pointer data-location :void))

(defun ladspa-connect-port-form (plugin instance-ptr block-size)
  `(progn
     ,@(port-loop (p i plugin)
         for name = (arg-symbol p)
         collect `(ladspa-connect-port ,plugin ,instance-ptr ,i
                                       ,(if (and (> block-size 1)
                                                 (audio-port-p p))
                                            name
                                            `(get-pointer ,name))))))

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
  (let ((plugin (make-ladspa-plugin filename label)))
    `(with-vug-plugin (,vug-name ,plugin ,block-size)
       (with ((reinit-p nil)
              (ladspa-obj (ladspa-plugin-instantiate ,plugin reinit-p))
              (ladspa-ptr (ladspa::handle-ptr ladspa-obj)))
         (declare (type foreign-pointer ladspa-ptr)
                  (type boolean reinit-p)
                  (preserve reinit-p))
         (initialize
           ;; REINIT-P is NIL only the first time. It becomes T in
           ;; UPDATE-LADSPA-INSTANCE
           (if reinit-p
               (ladspa-deactivate ,plugin ladspa-obj ladspa-ptr)
               ,(ladspa-connect-port-form plugin 'ladspa-ptr block-size))
           (ladspa-activate ,plugin ladspa-obj ladspa-ptr))
         (ladspa-run ,plugin ladspa-ptr ,block-size)))))

(defmacro vug:ladspa->vug (filename label vug-name
                           &optional (block-size (block-size)) debug-p)
  (if debug-p
      `(%ladspa->vug ,filename ,label ',vug-name ,block-size)
      `(macrolet ((generate (f l n bs) (%ladspa->vug f l n bs)))
         (generate ,filename ,label ,vug-name ,block-size))))
