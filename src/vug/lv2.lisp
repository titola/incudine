;;; Copyright (c) 2013-2022 Tito Latini
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
  (export 'lv2->vug (find-package "INCUDINE.VUG"))
  (export 'incudine::set-lv2-tuning (find-package "INCUDINE"))
  (export '(lv2::midi-message lv2::write-event) (find-package "LV2"))
  (export 'incudine.vug-foreign::plugin-lilv-instance
          (find-package "INCUDINE.VUG-FOREIGN"))
  (object-to-free incudine.vug-foreign::lv2-plugin-instantiate
                  incudine.vug-foreign::update-lv2-instance)
  (object-to-free incudine.vug-foreign::make-lv2-plugin-instance
                  incudine.vug-foreign::update-lv2-plugin-state))

(in-package :incudine.vug-foreign)

(defstruct (lv2-plugin (:include plugin)
                       (:constructor %make-lv2-plugin))
  (worker-interface-flags 0))

(defstruct (lv2-plugin-instance (:include plugin-instance))
  (worker-state (cffi:null-pointer) :type foreign-pointer)
  (lilv-instance nil :type (or null lilv:instance))
  (uri "" :type string))

(declaim (inline plugin-lilv-instance))
(defun plugin-lilv-instance (plugin-instance)
  (lv2-plugin-instance-lilv-instance plugin-instance))

(defun get-worker-interface-flags (plugin-uri)
  (let ((obj (lilv:plugin-instantiate (lilv:plugin-pointer plugin-uri)
                                      *sample-rate*
                                      (lv2:features))))
    (unwind-protect
         (let ((ext (lilv:instance-get-extension-data
                      obj
                      "http://lv2plug.in/ns/ext/worker#interface")))
           (if (cffi:null-pointer-p ext)
               0
               (loop for i below 3
                     for j = 1 then (* j 2)
                     ;; 1 = work, 2 = work-response, 4 = end-run
                     sum (if (cffi:null-pointer-p (ptr-ref ext i)) 0 j))))
      (lilv:free obj))))

(defun worker-interface-callback-p (name plugin)
  (let ((flags (lv2-plugin-worker-interface-flags plugin)))
    (logtest (case name
               (work 1)
               (work-response 2)
               (end-run 4)
               (otherwise 8))
             flags)))

(defun add-worker-state (instance)
  (let* ((obj (plugin-lilv-instance instance))
         (interface (lilv:instance-get-extension-data
                      obj "http://lv2plug.in/ns/ext/worker#interface")))
    (if (cffi:null-pointer-p interface)
        (error "Missing LV2 worker interface")
        (setf (lv2-plugin-instance-worker-state instance)
              (lv2::make-worker-state (plugin-instance-pointer instance)
                                      interface)))
    instance))

;;; Callback for non-realtime plugin operations.
(cffi:defcallback worker-schedule :int
    ((handle :pointer) (size :uint32) (data :pointer))
  (incudine:nrt-funcall
    (lambda () (lv2::interface-work handle size data)))
  lv2::+worker-success+)

(setf lv2::*worker-schedule-function* (cffi:callback worker-schedule))

(defun safe-lilv-node-as-string (ptr)
  (if (cffi:null-pointer-p ptr)
      ""
      (lilv:node-as-string ptr)))

;;; MEMO: don't use a temporary lilv instance to retrieve the pointers
;;; of the callbacks because it causes a crash during the initialization
;;; of the first UGEN/DSP instance. It is safe to use the descriptor
;;; in LilvInstanceImpl struct within a UGEN/DSP.
(defun make-lv2-plugin (uri)
  (update-io-number
    (let* ((ptr (lilv:plugin-pointer uri))
           (name (safe-lilv-node-as-string (lilv:plugin-get-name ptr))))
      (%make-lv2-plugin
        :name name
        :label name
        :path uri
        :pointer ptr
        :author (safe-lilv-node-as-string (lilv:plugin-get-author-name ptr))
        :realtime-p t
        :ports (make-lv2-ports ptr)
        :sample-type 'foreign-float
        :worker-interface-flags (get-worker-interface-flags uri)))))

(defun make-lv2-ports (plugin)
  (cffi:with-foreign-object (ptr :pointer)
    (coerce
      (loop for i below (lilv:plugin-get-num-ports plugin)
            for port = (lilv:plugin-get-port-by-index plugin i)
            for port-type = (lv2-port-type plugin port)
            collect (make-port
                      :name (lilv:node-as-string
                              (lilv:port-get-symbol plugin port))
                      :id i
                      :type port-type
                      :default (lv2-port-default plugin port ptr)
                      :value-type
                        (if (logtest port-type
                              (logior +atom-port+ +event-port+ +midi-port+))
                            'lv2::event
                            'foreign-float)))
      'simple-vector)))

(defmethod make-load-form ((obj lv2-plugin) &optional environment)
  (declare (ignore environment))
  `(make-lv2-plugin ,(lv2-plugin-path obj)))

(defmethod print-object ((obj lv2-plugin) stream)
  (print-unreadable-object (obj stream)
    (format stream "LV2-PLUGIN ~S" (lv2-plugin-path obj))))

(defun lv2-port-type (plugin port)
  (flet ((portp (port-class class-value)
           (if (lilv:port-is-a plugin port port-class) class-value 0)))
    (logior (portp lilv:*uri-atom-port* +atom-port+)
            (portp lilv:*uri-input-port* +input-port+)
            (portp lilv:*uri-output-port* +output-port+)
            (portp lilv:*uri-control-port* +control-port+)
            (portp lilv:*uri-audio-port* +audio-port+)
            (portp lilv:*uri-event-port* +event-port+)
            (portp lilv:*uri-midi-port* +midi-port+))))

(defun lv2-port-default (plugin port ptr)
  (lilv:port-get-range plugin port ptr (cffi:null-pointer) (cffi:null-pointer))
  (unwind-protect
       (and (or (lilv:node-is-float #1=(cffi:mem-ref ptr :pointer))
                (lilv:node-is-int #1#))
            (lilv:node-as-float #1#))
    (lilv:node-free #1#)))

(declaim (inline lv2-plugin-instantiate))
(defun lv2-plugin-instantiate (plugin &optional arg)
  (declare (ignore arg))
  (reduce-warnings
    (lilv:plugin-instantiate
      (lv2-plugin-pointer plugin) *sample-rate* (lv2:features))))

(defmacro update-lv2-instance (vug-varname args)
  (declare (ignore vug-varname))
  ;; ARGS are the arguments passed to LV2-PLUGIN-INSTANTIATE, so the second
  ;; argument is REINIT-P. When REINIT-P is NIL, the ports are connected
  ;; and the plugin-instance is activated.
  `(setf ,(second args) t))

(defmacro update-lv2-plugin-state (vug-varname args)
  (declare (ignore vug-varname args))
  nil)

(defmethod incudine:free ((obj lilv:instance))
  (msg debug "Cleanup LV2 plugin")
  (lilv:free obj))

(defun lv2-connect-control-ports (controls instance descriptor handle)
  (let ((plugin (lilv:plugin-pointer (lv2-plugin-instance-uri instance)))
        (j 0))
    (declare (type non-negative-fixnum j))
    (cffi:with-foreign-object (node :pointer)
      (loop for i of-type non-negative-fixnum
                  below (lilv:plugin-get-num-ports plugin)
            for port = (lilv:plugin-get-port-by-index plugin i)
            if (and (lilv:port-is-a plugin port lilv:*uri-input-port*)
                    (lilv:port-is-a plugin port lilv:*uri-control-port*))
            do (let ((ptr (cffi:mem-aptr controls :float j)))
                 (setf (cffi:mem-ref ptr :float)
                       (or (lv2-port-default plugin port node) 0.0))
                 (lilv:connect-port
                   (lilv:descriptor-slot-value descriptor 'lv2::connect-port)
                   handle i ptr)
                 (setf (aref (plugin-instance-port-pointers instance) i) ptr)
                 (incf j))))))

(defun lv2-connect-port-form (plugin instance control-arguments-p descriptor
                              handle block-size)
  `(reduce-warnings
     ,@(port-loop (p i plugin)
         for name = (arg-symbol p)
         if (or (and (input-port-p p)
                     (or control-arguments-p
                         (event-port-p p)
                         (audio-port-p p)))
                (output-port-p p))
           collect
             (let ((ptr (cond ((and (> block-size 1) (audio-port-p p)) name)
                              ((event-port-p p) `(lv2::event-pointer ,name))
                              (t `(get-pointer ,name)))))
               `(progn
                  (lilv:connect-port
                    (lilv:descriptor-slot-value ,descriptor 'lv2::connect-port)
                    ,handle ,i ,ptr)
                  (setf (aref (plugin-instance-port-pointers ,instance) ,i)
                        ,ptr))))))

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

(defun lv2-make-event-form (ptr &key output-p)
  `(,(if output-p 'lv2::make-atom-chunk 'lv2::make-atom-sequence)
     :pointer ,ptr))

(defun lv2-reset-event-form (obj &key init-time-p output-p)
  `(,(if output-p 'lv2::reset-atom-chunk 'lv2::reset-atom-sequence)
     ,obj :type-p ,init-time-p))

(defun %lv2->vug (uri vug-name block-size control-arguments-p)
  (let ((plugin (make-lv2-plugin uri))
        (plugin-instance (ensure-symbol "PLUGIN-INSTANCE")))
    `(with-vug-plugin (,vug-name ,plugin ,block-size ,control-arguments-p
                       :event-buffer-size ,lv2::+atom-sequence-buffer-size+
                       :make-event-form ,(function lv2-make-event-form)
                       :reset-event-form ,(function lv2-reset-event-form))
       (with ((reinit-p nil)
              (lv2-obj (lv2-plugin-instantiate ,plugin reinit-p))
              (lv2-ptr (lilv::instance-pointer lv2-obj))
              (lv2-handle (lv2-handle lv2-ptr))
              (lv2-descr (lv2-descriptor lv2-ptr))
              (lv2-state
                (reduce-warnings
                  (make-lv2-plugin-instance
                    :label ,(plugin-label plugin)
                    :uri ,(plugin-path plugin)
                    :handle-pointer lv2-handle
                    :lilv-instance lv2-obj
                    :port-pointers
                      (make-array
                        ,(lilv:plugin-get-num-ports (plugin-pointer plugin))))))
              ,@(unless control-arguments-p
                  `((controls (make-f32-array
                                ,(port-control-inputs plugin))))))
         (declare (type foreign-pointer lv2-ptr lv2-handle lv2-descr)
                  (type boolean reinit-p)
                  (preserve reinit-p))
         (initialize
           (setf ,plugin-instance lv2-state)
           ,@(if (plusp (lv2-plugin-worker-interface-flags plugin))
                 `((add-worker-state lv2-state)))
           ;; REINIT-P is NIL only the first time. It becomes T in
           ;; UPDATE-LV2-INSTANCE
           (if reinit-p
               (lv2-instance-deactivate lv2-obj lv2-descr lv2-handle)
               ,(lv2-connect-port-form
                  plugin plugin-instance control-arguments-p 'lv2-descr
                  'lv2-handle block-size))
           ,@(unless control-arguments-p
               `((lv2-connect-control-ports
                   controls ,plugin-instance lv2-descr lv2-handle)))
           (lv2-instance-activate lv2-obj lv2-descr lv2-handle))
         (lv2-run lv2-descr lv2-handle ,block-size)
         ,@(if (worker-interface-callback-p 'work-response plugin)
               `((lv2::interface-work-response lv2-state)))
         ,@(if (worker-interface-callback-p 'end-run plugin)
               `((lv2::interface-end-run lv2-state)))))))

(defmacro vug:lv2->vug (uri vug-name
                        &key (block-size (block-size))
                        (control-arguments-p t) debug-p)
  "Define a new VUG and the auxiliary function named VUG-NAME to use
the LV2 plugin with URI.

Initialize the Lilv World if necessary.

The control parameter PLUGIN-INSTANCE is set to the plugin instance
during the DSP initialization. It is possible to retrieve the plugin
instance through CONTROL-VALUE. This functionality works only with a
VUG or an inlined UGEN.

If CONTROL-ARGUMENTS-P is T (default), all the LV2 controls are
optional keywords of the auxiliary function. If the LV2 plugin provides
many control ports, LV2->VUG fails with a \"Control stack exhausted\"
error. In this case, set CONTROL-ARGUMENTS-P to NIL, retrieve the plugin
instance via CONTROL-VALUE and use VUG-FOREIGN:PLUGIN-PORT-POINTER out
of the DSP.

If the incudine block size changes, LV2->VUG should be called again.

If DEBUG-P is T, return the lisp form to define the VUG.

Return the new VUG structure.

Example of a LV2 effect:

    (lv2->vug \"http://plugin.org.uk/swh-plugins/amp\" swh.amp)

    (dsp! amp-test (gain)
      (out (swh.amp gain (white-noise))))

Example of a LV2 instrument:

    (set-rt-block-size 64)

    (lv2->vug SYNTH-TEST-URI synth-test)

    ;; Note: the control parameter PLUGIN could be of type T for simplicity.
    (dsp! lv2-synth-test ((plugin (or null vug-foreign:plugin-instance)))
      (:defaults nil)
      (with ((out (cffi:null-pointer)))
        (declare (type pointer out))
        ;; If the instrument has many control ports, we can use
        ;; VUG-FOREIGN:PLUGIN-PORT-POINTER out of the DSP.
        (setf out (synth-test :plugin-instance plugin))
        (foreach-frame
          ;; Plugin with a single audio output port.
          (stereo (sample (f32-ref out current-frame)))
          ;; Plugin with multiple audio output ports, for example two:
          ;; (out (sample (f32-ref (ptr-ref out 0) current-frame))
          ;;      (sample (f32-ref (ptr-ref out 1) current-frame))))))

    (rt-start)
    (lv2-synth-test :id 123)

    (defvar *synth-test* (control-value 123 'plugin))

    *synth-test*
    ;; => #<LV2-PLUGIN-INSTANCE ...>

    (defvar *midiin* (jackmidi:open))

    (make-responder *midiin*
      (lambda (st d1 d2)
        (lv2:write-event *synth-test* (lv2:midi-message st d1 d2))))

    (recv-start *midiin*)

    (defun set-lv2-control (plugin port-index value)
      ;; Change the port value in rt-thread.
      (rt-eval ()
        (setf (f32-ref (vug-foreign:plugin-port-pointer plugin port-index))
              (coerce value 'single-float)))
      value)

    ;; Read the Turtle description of plugin /path/to/SYNTH-TEST.ttl
    ;; or run the command lv2info to know the port indices.
    (set-lv2-control *synth-test* PORT-INDEX 0.1)

    (f32-ref (vug-foreign:plugin-port-pointer *synth-test* PORT-INDEX))
    ;; => 0.1

    ;; We can set more port values from nrt-thread with a single
    ;; memory barrier or CAS.
    (rt-eval ()
      (set-lv2-control ...)
      (set-lv2-control ...)
      ...)"
  (if debug-p
      `(%lv2->vug ,uri ',vug-name ,block-size ,control-arguments-p)
      `(macrolet ((generate (u n bs cap) (%lv2->vug u n bs cap)))
         (generate ,uri ,vug-name ,block-size ,control-arguments-p))))

(defun lv2:midi-message (&rest octets)
  "Return a MIDI message encoded for LV2:WRITE-EVENT."
  (apply #'midifile:data octets))

(define-compiler-macro lv2:midi-message (&rest octets)
  (if (< (length octets) 4)
      `(midifile:message ,@octets)
      `(midifile:data ,@octets)))

(defun lv2:write-event (plugin data &key (index 0) (type lv2:+midi-event+)
                        (start 0) end)
  "Send an event through an Atom port of a LV2 PLUGIN. The buffer type
of the Atom port is Atom Sequence.

DATA is a short MIDI message encoded into four bytes or a vector of
octets where START and END are the optional bounding index designators.

If the plugin has more than one input event port, INDEX is the array
index of the input event ports.

The event TYPE is a URID (an integer) obtained from LV2:URI-TO-ID.
It is a MIDI event by default."
  (declare (type plugin-instance plugin)
           (type (or (simple-array (unsigned-byte 8) (*))
                     (unsigned-byte 32))
                 data)
           (type non-negative-fixnum index type start)
           (type (or null non-negative-fixnum) end))
  (rt-eval ()
    (lv2::add-event (svref (plugin-instance-event-pointers plugin) index)
                    data :type type :start start :end end)))

(defun incudine:set-lv2-tuning
    (tuning plugin &key (event-index 0) (device-id 0) (program 0)
     single-note-tuning-p checksum-function)
  "Send a bulk tuning dump message as a LV2 MIDI message. It obviously
works if the LV2 synthesizer supports it. The new frequencies are
related to a TUNING structure.

If the plugin has more than one input event port, EVENT-INDEX is the
array index of the input event ports.

If SINGLE-NOTE-TUNING-P is non-NIL, send 128 single note tuning change
messages instead.

The optional CHECKSUM-FUNCTION requires two arguments: the foreign
buffer containing the MIDI SysEx message and the buffer size in bytes.
It is useful if the manufacturer implements a different checksum.

EVENT-INDEX, DEVICE-ID and PROGRAM default to 0."
  (flet ((send-sysex (plugin buf bufsize)
           (let ((data (cffi:make-shareable-byte-vector bufsize)))
             (dotimes (i bufsize)
               (setf (aref data i) (u8-ref buf i)))
             (lv2:write-event plugin data :index event-index))))
    (if single-note-tuning-p
        (incudine::midi-128-single-note-tuning
          tuning #'send-sysex plugin device-id program)
        (incudine::midi-bulk-tuning-dump
          tuning #'send-sysex plugin device-id program
          (or checksum-function #'incudine::midi-dump-checksum)))))

(in-package :lv2)

(defun interface-work-response (instance)
  (let ((state (vug-foreign::lv2-plugin-instance-worker-state instance)))
    (cffi:with-foreign-slots ((interface lv2-handle data size done-p)
                              state (:struct worker-state))
      (if (zerop done-p)
          lv2::+worker-success+
          (let ((res (cffi:foreign-funcall-pointer
                       (cffi:foreign-slot-value
                         interface '(:struct worker-interface) 'work-response)
                       () :pointer lv2-handle :uint32 size :pointer data
                       :int)))
            (declare (type fixnum res))
            (when (= res +worker-success+)
              (setf data (cffi:null-pointer))
              (setf size 0)
              (setf done-p 0))
            res)))))

(declaim (inline interface-end-run))
(defun interface-end-run (instance)
  (let ((state (vug-foreign::lv2-plugin-instance-worker-state instance)))
    (cffi:foreign-funcall-pointer
      (cffi:foreign-slot-value (worker-state-slot state 'interface)
                               '(:struct worker-interface) 'end-run)
      () :pointer (worker-state-slot state 'lv2-handle) :int)))
