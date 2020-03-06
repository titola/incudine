;;; Copyright (c) 2013-2020 Tito Latini
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
(cffi:defcfun ("ja_get_buffer_size" rt-buffer-size) :int)

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

(cffi:defcfun ("ja_get_error_msg" rt-get-error-msg) :string)

(cffi:defcfun ("ja_silent_errors" rt-silent-errors) :void
  "If SILENT-P is NIL, display JACK error messages."
  (silent-p :boolean))

(defun silent-jack-errors ()
  (rt-silent-errors t))
