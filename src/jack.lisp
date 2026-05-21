;;; Copyright (c) 2013-2026 Tito Latini
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

(in-package :incudine.external)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(incudine.util::defglobal
            incudine.util:*number-of-input-bus-channels*
            incudine.util:*number-of-output-bus-channels*
            alexandria:with-gensyms)))

(cffi:defcstruct rt-xrun
  (count :unsigned-int)
  (last-time sample))

(cffi:defcfun ("ja_initialize" rt-audio-init) :int
  (input-channels :unsigned-int)
  (output-channels :unsigned-int)
  (frames-per-buffer :unsigned-int)
  (client-name :pointer)
  (sample-counter-ptr :pointer))

(cffi:defcfun ("ja_start" rt-audio-start) :int)

(cffi:defcfun ("ja_stop" rt-audio-stop) :int)

(declaim (inline rt-set-io-buffers))
(cffi:defcfun ("ja_set_lisp_io" rt-set-io-buffers) :void
  (input :pointer)
  (output :pointer))

(cffi:defcfun ("ja_set_lisp_max_bufsize" rt-set-max-bufsize) :void
  (value :unsigned-int))

(declaim (inline rt-cycle-begin))
(cffi:defcfun ("ja_cycle_begin" rt-cycle-begin) :uint32)

(declaim (inline rt-continue-cycle-begin))
(cffi:defcfun ("ja_continue_cycle_begin" rt-continue-cycle-begin) :void
  (frames :uint32))

(declaim (inline rt-cycle-end))
(cffi:defcfun ("ja_cycle_end" rt-cycle-end) :void
  (frames :uint32))

(cffi:defcfun ("ja_get_cycle_start_time" rt-cycle-start-time) sample)

(cffi:defcfun "ja_get_time_offset_seconds" :double)

(cffi:defcfun ("ja_get_time_offset_frames") :uint32)

(defun rt-time-offset (&optional time-unit)
  (declare (type symbol time-unit))
  (if (and time-unit (string= (symbol-name time-unit) "FRAMES"))
      (ja-get-time-offset-frames)
      (ja-get-time-offset-seconds)))

(cffi:defcfun ("ja_client" rt-client) :pointer)

(declaim (inline rt-condition-wait))
(cffi:defcfun ("ja_condition_wait" rt-condition-wait) :void)

(declaim (inline rt-transfer-to-c-thread))
(cffi:defcfun ("ja_transfer_to_c_thread" rt-transfer-to-c-thread) :void)

(declaim (inline rt-buffer-size))
(cffi:defcfun ("ja_get_buffer_size" rt-buffer-size) :unsigned-int)

(declaim (inline rt-sample-rate))
(cffi:defcfun ("ja_get_sample_rate" rt-sample-rate) sample)

(defun rt-xruns (&optional reset-p)
  "Return the number of the occurred xruns and the time in samples of
the last xrun. If RESET-P is non-NIL, set the number of xruns to zero."
  (when reset-p
    (cffi:foreign-funcall "ja_xrun_reset" :void))
  (cffi:with-foreign-slots ((count last-time)
                            (cffi:foreign-funcall "ja_get_xruns" :pointer)
                            (:struct rt-xrun))
    (values count last-time)))

(declaim (inline rt-set-busy-state))
(cffi:defcfun ("ja_set_lisp_busy_state" rt-set-busy-state) :void
  (status :boolean))

(cffi:defcfun ("ja_set_thread_callback" set-foreign-rt-thread-callback) :boolean
  (cached-inputs-p :boolean))

(cffi:defcfun ("ja_clear_cached_inputs" rt-clear-cached-inputs) :void)

(cffi:defcfun ("ja_cache_inputs" rt-cache-inputs) :boolean)

(cffi:defcfun ("ja_has_cached_inputs" rt-cached-inputs-p) :boolean)

(cffi:defcfun ("ja_is_last_cycle" rt-last-cycle-p) :boolean)

(cffi:defcfun ("ja_inputs_from_cache_begin" rt-inputs-from-cache-begin) :boolean)

(cffi:defcfun ("ja_inputs_from_cache_end" rt-inputs-from-cache-end) :boolean)

(macrolet
  ((checking-port-name (var form result default)
     `(let ((,var (ignore-errors ,form)))
        (if (and (stringp ,var) (string/= ,var "")) ,result ,default)))
   (port-name-function (obj default-control-format)
     (with-gensyms (thing default res)
       `(let ((,thing ,obj)
              (,default (lambda (port-number)
                          (format nil ,default-control-format port-number))))
          (typecase ,thing
            (function
             (lambda (port-number)
               (checking-port-name ,res (funcall ,thing port-number) ,res
                 (funcall ,default port-number))))
            (string
             (checking-port-name ,res (format nil ,thing 1)
               (lambda (port-number) (format nil ,thing port-number))
               ,default))
            (otherwise ,default))))))
  (defglobal *audio-input-port-name-function*
      (port-name-function incudine.config::*audio-input-port-name* "in_~D"))
  (defglobal *audio-output-port-name-function*
      (port-name-function incudine.config::*audio-output-port-name* "out_~D")))

(defglobal *audio-input-port-names* (cffi:null-pointer))

(defglobal *audio-output-port-names* (cffi:null-pointer))

(defun free-port-names (ptr number-of-ports)
  (dotimes (i number-of-ports (cffi:foreign-free ptr))
    (symbol-macrolet ((p (cffi:mem-aref ptr :pointer i)))
      (unless (cffi:null-pointer-p p)
        (cffi:foreign-free p)
        (setf p (cffi:null-pointer))))))

(defun %init-audio-port-names (direction number-of-ports)
  (declare (type (member :input :output) direction)
           (type (unsigned-byte 16) number-of-ports))
  (let ((ptr (cffi:foreign-alloc :pointer :count number-of-ports
                                 :initial-element (cffi:null-pointer)))
        (get-name (if (eq direction :input)
                      *audio-input-port-name-function*
                      *audio-output-port-name-function*))
        (names nil))
    (unless (cffi:null-pointer-p ptr)
      (dotimes (i number-of-ports ptr)
        (let ((str (funcall get-name (1+ i))))
          (when (or (not (stringp str)) (string= str ""))
            (setf str
                  (format nil "~:[in~;out~]_~D" (eq direction :input) (1+ i))))
          ;; Fix duplicated names.
          (loop for s = str then (format nil "~A_~D" str (incf (cdr p)))
                for p = (assoc s names :test #'string=)
                if (and (not p) (midi-port-name-p s))
                  do (push (setf p (cons s 1)) names)
                while p
                finally (when (string/= s str) (setf str s)))
          (push (cons str 1) names)
          (let* ((bufsize (1+ (length str)))
                 (buffer (cffi:foreign-alloc :char :count bufsize)))
            (handler-case
                (cffi:foreign-funcall "ja_set_port_name"
                  :int (getf '(:input 0 :output 1) direction)
                  :unsigned-int i
                  :pointer (setf (cffi:mem-aref ptr :pointer i)
                                 (cffi:lisp-string-to-foreign str buffer bufsize))
                  :int)
              (error ()
                (cffi:foreign-free buffer)
                (setf (cffi:mem-aref ptr :pointer i) (cffi:null-pointer))))))))))

(defun init-audio-port-names (&optional input-channels output-channels)
  (macrolet ((set-port-names (direction ports new-channels old-channels)
               (with-gensyms (tmp)
                 `(when (or ,new-channels (cffi:null-pointer-p ,ports))
                    (let ((,tmp ,ports))
                      (setf ,ports
                            (%init-audio-port-names
                              ,direction (or ,new-channels ,old-channels)))
                      (free-old-ports ,tmp ,old-channels)))))
             (pref (p index) `(cffi:mem-aref ,p :pointer ,index)))
    (flet ((free-old-ports (ptr channels)
             (when (and ptr (not (cffi:null-pointer-p ptr)))
               (dotimes (i channels)
                 (let ((pn (pref ptr i)))
                   (unless (cffi:null-pointer-p pn)
                     (cffi:foreign-free pn))))
               (cffi:foreign-free ptr))))
      (set-port-names :input *audio-input-port-names*
                      input-channels *number-of-input-bus-channels*)
      (set-port-names :output *audio-output-port-names*
                      output-channels *number-of-output-bus-channels*)
      (cffi:foreign-funcall "set_port_names" :pointer *audio-input-port-names*
                            :pointer *audio-output-port-names* :void))))

(defun reset-audio-port-names ()
  "Reset the short names of the Jack audio ports.

See also the configuration variables *AUDIO-INPUT-PORT-NAME* and
*AUDIO-OUTPUT-PORT-NAME*."
  (macrolet ((free (p size)
               `(unless (cffi:null-pointer-p ,p)
                  (free-port-names ,p ,size)
                  (setf ,p (cffi:null-pointer)))))
    (free *audio-input-port-names* *number-of-input-bus-channels*)
    (free *audio-output-port-names* *number-of-output-bus-channels*)
    (init-audio-port-names)))

(defun live-audio-port-name (direction number)
  (declare (type (member :input :output) direction))
  (let ((name (cffi:foreign-funcall "ja_port_name"
                :int (getf '(:input 0 :output 1) direction)
                :unsigned-int number :string)))
    (when name
      (subseq name (1+ (position #\: name))))))

(defun audio-port-name (direction number)
  "Return the short name of the Jack audio port NUMBER (zero-based),
where DIRECTION is :INPUT or :OUTPUT. Setfable."
  (declare (type (member :input :output) direction))
  (if (eq (incudine:rt-status) :started)
      (live-audio-port-name direction number)
      (multiple-value-bind (names channels)
          (if (eq direction :input)
              (values *audio-input-port-names* *number-of-input-bus-channels*)
              (values *audio-output-port-names* *number-of-output-bus-channels*))
        (when (and (< number channels)
                   (not (cffi:null-pointer-p names)))
          (let ((p (cffi:mem-aref names :pointer number)))
            (unless (cffi:null-pointer-p p)
              (values (cffi:foreign-string-to-lisp p))))))))

(defun (setf audio-port-name) (name direction number)
  (when (used-port-name-p name)
    (incudine:incudine-error "Jack port name ~S is used" name))
  (when (string= name "")
    (incudine:incudine-error "Jack port name is of length 0"))
  (let ((res (cffi:foreign-funcall "ja_set_port_name"
               :int (getf '(:input 0 :output 1) direction)
               :unsigned-int number :string name :int)))
    (if (and (/= res 0)
             (eq (incudine:rt-status) :started))
        (incudine:incudine-error "Can't set the Jack port name ~S" name)
        (multiple-value-bind (names channels)
            (if (eq direction :input)
                (values *audio-input-port-names* *number-of-input-bus-channels*)
                (values *audio-output-port-names* *number-of-output-bus-channels*))
          (when (and (< number channels)
                     (not (cffi:null-pointer-p names)))
            (let ((bufsize (1+ (length name))))
              (symbol-macrolet ((p (cffi:mem-aref names :pointer number)))
                (unless (cffi:null-pointer-p p)
                  (cffi:foreign-free p))
                (setf p (cffi:lisp-string-to-foreign name
                          (cffi:foreign-alloc :char :count bufsize) bufsize))))
            name)))))

(defun audio-port-name-p (name)
  (flet ((find-port (direction channels)
           (if (eq (incudine:rt-status) :started)
               (loop for i below channels
                     if (equal name (live-audio-port-name direction i))
                       return t)
               (let ((names (if (eq direction :input)
                                *audio-input-port-names*
                                *audio-output-port-names*)))
                 (unless (cffi:null-pointer-p names)
                   (loop for i below channels
                         for p = (cffi:mem-aref names :pointer i)
                         if (and (not (cffi:null-pointer-p p))
                                 (string= name (cffi:foreign-string-to-lisp p)))
                           return t))))))
    (unless (string= name "")
      (or (find-port :input *number-of-input-bus-channels*)
          (find-port :output *number-of-output-bus-channels*)))))

(defun midi-port-name-p (name)
  (unless (string= name "")
    (let ((pkg (find-package "JACKMIDI")))
      (when pkg
        (and (uiop:symbol-call pkg "GET-STREAM-BY-NAME" name) t)))))

;;; Warning: it could fail if another Jack client sets a port name
;;; with the same prefix in *CLIENT-NAME* (i.e. "incudine:").
(defun used-port-name-p (name)
  (or (audio-port-name-p name) (midi-port-name-p name)))

(cffi:defcfun ("ja_get_error_msg" rt-get-error-msg) :string)

(cffi:defcfun ("ja_silent_errors" rt-silent-errors) :void
  "If SILENT-P is NIL, display JACK error messages."
  (silent-p :boolean))

(defun silent-jack-errors ()
  (rt-silent-errors t))
