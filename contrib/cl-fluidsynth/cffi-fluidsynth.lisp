;;; Copyright (c) 2015 Tito Latini
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

(in-package :fluidsynth)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library fluidsynth
    (:darwin "libfluidsynth.dylib")
    (:unix "libfluidsynth.so")
    (:cygwin "fluidsynth-0.dll")
    (t (:default "libfluidsynth")))

  (unless (cffi:foreign-library-loaded-p 'fluidsynth)
    (cffi:use-foreign-library fluidsynth)))

(defstruct wrap-pointer
  (ptr (cffi:null-pointer) :type cffi:foreign-pointer))

(defmethod print-object ((obj wrap-pointer) stream)
  (format stream "#<~S #X~8,'0X>" (type-of obj)
          (cffi:pointer-address (wrap-pointer-ptr obj))))

(declaim (inline deleted-p))
(defun deleted-p (obj)
  (cffi:null-pointer-p (wrap-pointer-ptr obj)))

(defstruct (synth (:include wrap-pointer)))

(defstruct (settings (:include wrap-pointer)))

(cffi:define-foreign-type settings-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser settings))

(cffi:define-foreign-type synth-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser synth))

(defmethod cffi:translate-to-foreign (handle (type settings-type))
  (settings-ptr handle))

(defmethod cffi:translate-to-foreign (handle (type synth-type))
  (synth-ptr handle))

(define-constant HINT-BOUNDED-BELOW  #x01)
(define-constant HINT-BOUNDED-ABOVE  #x02)
(define-constant HINT-TOGGLED        #x04)
(define-constant HINT-SAMPLE-RATE    #x08)
(define-constant HINT-LOGARITHMIC    #x10)
(define-constant HINT-INTEGER        #x20)
(define-constant HINT-FILENAME       #x01)
(define-constant HINT-OPTIONLIST     #x02)

(define-constant NO-TYPE   -1)
(define-constant NUM-TYPE   0)
(define-constant INT-TYPE   1)
(define-constant STR-TYPE   2)
(define-constant SET-TYPE   3)

(cffi:defcfun ("new_fluid_settings" new-fluid-settings) :pointer)

(cffi:defcfun ("delete_fluid_settings" delete-fluid-settings) :void
  (settings :pointer))

(defun new-settings ()
  (let* ((ptr (new-fluid-settings))
         (obj (make-settings :ptr ptr)))
    (tg:finalize obj (lambda () (delete-fluid-settings ptr)))
    obj))

(defun delete-settings (settings)
  (declare (type settings settings))
  (unless (deleted-p settings)
    (delete-fluid-settings (settings-ptr settings))
    (tg:cancel-finalization settings)
    (setf (settings-ptr settings) (cffi:null-pointer))
    t))

(cffi:defcfun ("fluid_settings_get_type" settings-get-type) :int
  (settings settings)
  (name :string))

(cffi:defcfun ("fluid_settings_get_hints" settings-get-hints) :int
  (settings settings)
  (name :string))

(cffi:defcfun ("fluid_settings_is_realtime" settings-is-realtime) :int
  (settings settings)
  (name :string))

(cffi:defcfun ("fluid_settings_setstr" settings-setstr) :int
  (settings settings)
  (name :string)
  (str :string))

(cffi:defcfun ("fluid_settings_copystr" settings-copystr) :int
  (settings settings)
  (name :string)
  (str :string)
  (len :int))

(cffi:defcfun ("fluid_settings_dupstr" settings-dupstr) :int
  (settings settings)
  (name :string)
  (str :pointer))

(cffi:defcfun ("fluid_settings_getstr" settings-getstr) :int
  (settings settings)
  (name :string)
  (str :pointer))

(cffi:defcfun ("fluid_settings_getstr_default" settings-getstr-default) :string
  (settings settings)
  (name :string))

(cffi:defcfun ("fluid_settings_str_equal" settings-str-equal) :int
  (settings settings)
  (name :string)
  (value :string))

(cffi:defcfun ("fluid_settings_setnum" settings-setnum) :int
  (settings settings)
  (name :string)
  (val :double))

(cffi:defcfun ("fluid_settings_getnum" settings-getnum) :int
  (settings settings)
  (name :string)
  (val :pointer))

(cffi:defcfun ("fluid_settings_getnum_default" settings-getnum-default) :double
  (settings settings)
  (name :string))

(cffi:defcfun ("fluid_settings_getnum_range" settings-getnum-range) :void
  (settings settings)
  (name :string)
  (min :pointer)
  (max :pointer))

(cffi:defcfun ("fluid_settings_setint" settings-setint) :int
  (settings settings)
  (name :string)
  (val :int))

(cffi:defcfun ("fluid_settings_getint" settings-getint) :int
  (settings settings)
  (name :string)
  (val :pointer))

(cffi:defcfun ("fluid_settings_getint_default" settings-getint-default) :int
  (settings settings)
  (name :string))

(cffi:defcfun ("fluid_settings_getint_range" settings-getint-range) :void
  (settings settings)
  (name :string)
  (min :pointer)
  (max :pointer))

(cffi:defcfun ("fluid_settings_foreach_option" settings-foreach-option) :void
  (settings settings)
  (name :string)
  (data :pointer)
  (func :pointer))

(cffi:defcfun ("fluid_settings_option_count" settings-option-count) :int
  (settings settings)
  (name :string))

(cffi:defcfun ("fluid_settings_option_concat" settings-option-concat) :string
  (settings settings)
  (name :string)
  (separator :string))

(cffi:defcfun ("fluid_settings_foreach" settings-foreach) :void
  (settings settings)
  (data :pointer)
  (func :pointer))

(define-constant CHANNEL-INFO-NAME-SIZE  32)

(cffi:defcstruct channel-info
  (assigned :int)
  (sfont-id :int)
  (bank :int)
  (program :int)
  (name :string)
  (reserved :pointer))

(cffi:defcfun ("new_fluid_synth" new-fluid-synth) :pointer
  (settings :pointer))

(cffi:defcfun ("delete_fluid_synth" delete-fluid-synth) :int
  (synth :pointer))

(cffi:defcfun ("fluid_synth_get_settings" get-settings) :pointer
  (synth synth))

(defun new (settings)
  (declare (type settings settings))
  (let ((synth (new-fluid-synth (settings-ptr settings))))
    (tg:finalize synth (lambda () (delete-fluid-synth synth)))
    (make-synth :ptr synth)))

(defun delete (synth)
  (declare (type synth synth))
  (unless (cffi:null-pointer-p (synth-ptr synth))
    (let ((res (delete-fluid-synth (synth-ptr synth))))
      (tg:cancel-finalization synth)
      (setf (synth-ptr synth) (cffi:null-pointer))
      (zerop res))))

;;; MIDI channel messages

(cffi:defcfun ("fluid_synth_noteon" noteon) :int
  (synth synth)
  (chan :int)
  (key :int)
  (vel :int))

(cffi:defcfun ("fluid_synth_noteoff" noteoff) :int
  (synth synth)
  (chan :int)
  (key :int))

(cffi:defcfun ("fluid_synth_cc" cc) :int
  (synth synth)
  (chan :int)
  (ctrl :int)
  (val :int))

(cffi:defcfun ("fluid_synth_get_cc" get-cc) :int
  (synth synth)
  (chan :int)
  (ctrl :int)
  (pval :pointer))

(cffi:defcfun ("fluid_synth_sysex" sysex) :int
  (synth synth)
  (data :string)
  (len :int)
  (response :string)
  (response_len :pointer)
  (handled :pointer)
  (dryrun :int))

(cffi:defcfun ("fluid_synth_pitch_bend" pitch-bend) :int
  (synth synth)
  (chan :int)
  (val :int))

(cffi:defcfun ("fluid_synth_get_pitch_bend" get-pitch-bend) :int
  (synth synth)
  (chan :int)
  (ppitch_bend :pointer))

(cffi:defcfun ("fluid_synth_pitch_wheel_sens" pitch-wheel-sens) :int
  (synth synth)
  (chan :int)
  (val :int))

(cffi:defcfun ("fluid_synth_get_pitch_wheel_sens" get-pitch-wheel-sens) :int
  (synth synth)
  (chan :int)
  (pval :pointer))

(cffi:defcfun ("fluid_synth_program_change" program-change) :int
  (synth synth)
  (chan :int)
  (program :int))

(cffi:defcfun ("fluid_synth_channel_pressure" channel-pressure) :int
  (synth synth)
  (chan :int)
  (val :int))

(cffi:defcfun ("fluid_synth_bank_select" bank-select) :int
  (synth synth)
  (chan :int)
  (bank :unsigned-int))

(cffi:defcfun ("fluid_synth_sfont_select" sfont-select) :int
  (synth synth)
  (chan :int)
  (sfont_id :unsigned-int))

(cffi:defcfun ("fluid_synth_program_select" program-select) :int
  (synth synth)
  (chan :int)
  (sfont_id :unsigned-int)
  (bank_num :unsigned-int)
  (preset_num :unsigned-int))

(cffi:defcfun ("fluid_synth_program_select_by_sfont_name"
               program-select-by-sfont-name) :int
  (synth synth)
  (chan :int)
  (sfont_name :string)
  (bank_num :unsigned-int)
  (preset_num :unsigned-int))

(cffi:defcfun ("fluid_synth_get_program" get-program) :int
  (synth synth)
  (chan :int)
  (sfont_id :pointer)
  (bank_num :pointer)
  (preset_num :pointer))

(cffi:defcfun ("fluid_synth_unset_program" unset-program) :int
  (synth synth)
  (chan :int))

(cffi:defcfun ("fluid_synth_get_channel_info" get-channel-info) :int
  (synth synth)
  (chan :int)
  (info :pointer))

(cffi:defcfun ("fluid_synth_program_reset" program-reset) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_system_reset" system-reset) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_all_notes_off" all-notes-off) :int
  (synth synth)
  (chan :int))

(cffi:defcfun ("fluid_synth_all_sounds_off" all-sounds-off) :int
  (synth synth)
  (chan :int))

(define-constant CHANNEL-TYPE-MELODIC  0)
(define-constant CHANNEL-TYPE-DRUM     1)

(cffi:defcfun ("fluid_synth_set_channel_type" set-channel-type) :int
  (synth synth)
  (chan :int)
  (type :int))

(cffi:defcfun ("fluid_synth_get_channel_preset" get-channel-preset) :pointer
  (synth synth)
  (chan :int))

(cffi:defcfun ("fluid_synth_start" start) :int
  (synth synth)
  (id :unsigned-int)
  (preset :pointer)
  (audio_chan :int)
  (midi_chan :int)
  (key :int)
  (vel :int))

(cffi:defcfun ("fluid_synth_stop" stop) :int
  (synth synth)
  (id :unsigned-int))

;;; SoundFont management

(cffi:defcfun ("fluid_synth_sfload" sfload) :int
  (synth synth)
  (filename :string)
  (reset_presets :int))

(cffi:defcfun ("fluid_synth_sfreload" sfreload) :int
  (synth synth)
  (id :unsigned-int))

(cffi:defcfun ("fluid_synth_sfunload" sfunload) :int
  (synth synth)
  (id :unsigned-int)
  (reset_presets :int))

(cffi:defcfun ("fluid_synth_add_sfont" add-sfont) :int
  (synth synth)
  (sfont :pointer))

(cffi:defcfun ("fluid_synth_remove_sfont" remove-sfont) :void
  (synth synth)
  (sfont :pointer))

(cffi:defcfun ("fluid_synth_sfcount" sfcount) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_get_sfont" get-sfont) :pointer
  (synth synth)
  (num :unsigned-int))

(cffi:defcfun ("fluid_synth_get_sfont_by_id" get-sfont-by-id) :pointer
  (synth synth)
  (id :unsigned-int))

(cffi:defcfun ("fluid_synth_get_sfont_by_name" get-sfont-by-name) :pointer
  (synth synth)
  (name :string))

(cffi:defcfun ("fluid_synth_set_bank_offset" set-bank-offset) :int
  (synth synth)
  (sfont_id :int)
  (offset :int))

(cffi:defcfun ("fluid_synth_get_bank_offset" get-bank-offset) :int
  (synth synth)
  (sfont_id :int))

;;; Reverb

(cffi:defcfun ("fluid_synth_set_reverb" set-reverb) :void
  (synth synth)
  (roomsize :double)
  (damping :double)
  (width :double)
  (level :double))

(cffi:defcfun ("fluid_synth_set_reverb_on" set-reverb-on) :void
  (synth synth)
  (on :int))

(cffi:defcfun ("fluid_synth_get_reverb_roomsize" get-reverb-roomsize) :double
  (synth synth))

(cffi:defcfun ("fluid_synth_get_reverb_damp" get-reverb-damp) :double
  (synth synth))

(cffi:defcfun ("fluid_synth_get_reverb_level" get-reverb-level) :double
  (synth synth))

(cffi:defcfun ("fluid_synth_get_reverb_width" get-reverb-width) :double
  (synth synth))

(define-constant REVERB-DEFAULT-ROOMSIZE  0.2d0)
(define-constant REVERB-DEFAULT-DAMP      0.0d0)
(define-constant REVERB-DEFAULT-WIDTH     0.5d0)
(define-constant REVERB-DEFAULT-LEVEL     0.9d0)

;;; Chorus

(define-constant CHORUS-MOD-SINE      0)
(define-constant CHORUS-MOD-TRIANGLE  1)

(cffi:defcfun ("fluid_synth_set_chorus" set-chorus) :void
  (synth synth)
  (nr :int)
  (level :double)
  (speed :double)
  (depth_ms :double)
  (type :int))

(cffi:defcfun ("fluid_synth_set_chorus_on" set-chorus-on) :void
  (synth synth)
  (on :int))

(cffi:defcfun ("fluid_synth_get_chorus_nr" get-chorus-nr) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_get_chorus_level" get-chorus-level) :double
  (synth synth))

(cffi:defcfun ("fluid_synth_get_chorus_speed_Hz" get-chorus-speed-hz) :double
  (synth synth))

(cffi:defcfun ("fluid_synth_get_chorus_depth_ms" get-chorus-depth-ms) :double
  (synth synth))

(cffi:defcfun ("fluid_synth_get_chorus_type" get-chorus-type) :int
  (synth synth))

(define-constant CHORUS-DEFAULT-N      3)
(define-constant CHORUS-DEFAULT-LEVEL  2.0d0)
(define-constant CHORUS-DEFAULT-SPEED  0.3d0)
(define-constant CHORUS-DEFAULT-DEPTH  8.0d0)
(define-constant CHORUS-DEFAULT-TYPE   CHORUS-MOD-SINE)

;;; Audio and MIDI channels

(cffi:defcfun ("fluid_synth_count_midi_channels" count-midi-channels) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_count_audio_channels" count-audio-channels) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_count_audio_groups" count-audio-groups) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_count_effects_channels" count-effects-channels) :int
  (synth synth))

;;; Synthesis parameters

(cffi:defcfun ("fluid_synth_set_sample_rate" set-sample-rate) :void
  (synth synth)
  (sample_rate :float))

(cffi:defcfun ("fluid_synth_set_gain" set-gain) :void
  (synth synth)
  (gain :float))

(cffi:defcfun ("fluid_synth_get_gain" get-gain) :float
  (synth synth))

(cffi:defcfun ("fluid_synth_set_polyphony" set-polyphony) :int
  (synth synth)
  (polyphony :int))

(cffi:defcfun ("fluid_synth_get_polyphony" get-polyphony) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_get_active_voice_count" get-active-voice-count) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_get_internal_bufsize" get-internal-bufsize) :int
  (synth synth))

(cffi:defcfun ("fluid_synth_set_interp_method" set-interp-method) :int
  (synth synth)
  (chan :int)
  (interp_method :int))

(define-constant INTERP-NONE      0)
(define-constant INTERP-LINEAR    1)
(define-constant INTERP-4THORDER  4)
(define-constant INTERP-7THORDER  7)
(define-constant INTERP-DEFAULT   INTERP-4THORDER)
(define-constant INTERP-HIGHEST   INTERP-7THORDER)

;;; Generator interface

(cffi:defcfun ("fluid_synth_set_gen" set-gen) :int
  (synth synth)
  (chan :int)
  (param :int)
  (value :float))

(cffi:defcfun ("fluid_synth_set_gen2" set-gen2) :int
  (synth synth)
  (chan :int)
  (param :int)
  (value :float)
  (absolute :int)
  (normalized :int))

(cffi:defcfun ("fluid_synth_get_gen" get-gen) :float
  (synth synth)
  (chan :int)
  (param :int))

;;; Tuning

(cffi:defcfun ("fluid_synth_create_key_tuning" create-key-tuning) :int
  (synth synth)
  (bank :int)
  (prog :int)
  (name :string)
  (pitch :pointer))

(cffi:defcfun ("fluid_synth_activate_key_tuning" activate-key-tuning) :int
  (synth synth)
  (bank :int)
  (prog :int)
  (name :string)
  (pitch :pointer)
  (apply :boolean))

(cffi:defcfun ("fluid_synth_create_octave_tuning" create-octave-tuning) :int
  (synth synth)
  (bank :int)
  (prog :int)
  (name :string)
  (pitch :pointer))

(cffi:defcfun ("fluid_synth_activate_octave_tuning" activate-octave-tuning) :int
  (synth synth)
  (bank :int)
  (prog :int)
  (name :string)
  (pitch :pointer)
  (apply :boolean))

(cffi:defcfun ("fluid_synth_tune_notes" tune-notes) :int
  (synth synth)
  (bank :int)
  (prog :int)
  (len :int)
  (keys :pointer)
  (pitch :pointer)
  (apply :boolean))

(cffi:defcfun ("fluid_synth_select_tuning" select-tuning) :int
  (synth synth)
  (chan :int)
  (bank :int)
  (prog :int))

(cffi:defcfun ("fluid_synth_activate_tuning" activate-tuning) :int
  (synth synth)
  (chan :int)
  (bank :int)
  (prog :int)
  (apply :boolean))

(cffi:defcfun ("fluid_synth_reset_tuning" reset-tuning) :int
  (synth synth)
  (chan :int))

(cffi:defcfun ("fluid_synth_deactivate_tuning" deactivate-tuning) :int
  (synth synth)
  (chan :int)
  (apply :boolean))

(cffi:defcfun ("fluid_synth_tuning_iteration_start" tuning-iteration-start) :void
  (synth synth))

(cffi:defcfun ("fluid_synth_tuning_iteration_next" tuning-iteration-next) :int
  (synth synth)
  (bank :pointer)
  (prog :pointer))

(cffi:defcfun ("fluid_synth_tuning_dump" tuning-dump) :int
  (synth synth)
  (bank :int)
  (prog :int)
  (name :string)
  (len :int)
  (pitch :pointer))

;;; Misc

(cffi:defcfun ("fluid_synth_get_cpu_load" get-cpu-load) :double
  (synth synth))

(cffi:defcfun ("fluid_synth_error" error) :string
  (synth synth))

;;; Synthesizer plugin

(declaim (inline write-s16))
(cffi:defcfun ("fluid_synth_write_s16" write-s16) :int
  (synth synth)
  (len :int)
  (lout :pointer)
  (loff :int)
  (lincr :int)
  (rout :pointer)
  (roff :int)
  (rincr :int))

(declaim (inline write-float))
(cffi:defcfun ("fluid_synth_write_float" write-float) :int
  (synth synth)
  (len :int)
  (lout :pointer)
  (loff :int)
  (lincr :int)
  (rout :pointer)
  (roff :int)
  (rincr :int))

(declaim (inline nwrite-float))
(cffi:defcfun ("fluid_synth_nwrite_float" nwrite-float) :int
  (synth synth)
  (len :int)
  (left :pointer)
  (right :pointer)
  (fx_left :pointer)
  (fx_right :pointer))

(declaim (inline process))
(cffi:defcfun ("fluid_synth_process" process) :int
  (synth synth)
  (len :int)
  (nin :int)
  (in :pointer)
  (nout :int)
  (out :pointer))

;;; Synthesizer's interface to handle SoundFont loaders

(cffi:defcfun ("fluid_synth_add_sfloader" add-sfloader) :void
  (synth synth)
  (loader :pointer))

(cffi:defcfun ("fluid_synth_alloc_voice" alloc-voice) :pointer
  (synth synth)
  (sample :pointer)
  (channum :int)
  (key :int)
  (vel :int))

(cffi:defcfun ("fluid_synth_start_voice" start-voice) :void
  (synth synth)
  (voice :pointer))

(cffi:defcfun ("fluid_synth_get_voicelist" get-voicelist) :void
  (synth synth)
  (buf :pointer)
  (bufsize :int)
  (ID :int))

(cffi:defcfun ("fluid_synth_handle_midi_event" handle-midi-event) :int
  (data :pointer)
  (event :pointer))

(cffi:defcfun ("fluid_synth_set_midi_router" set-midi-router) :void
  (synth synth)
  (router :pointer))
