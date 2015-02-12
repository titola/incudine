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

(in-package :cl-user)

(defpackage :fluidsynth
  (:use :cl)
  (:shadow #:delete #:error)
  (:import-from #:alexandria #:define-constant)
  (:export
   ;; Settings
   #:HINT-BOUNDED-BELOW
   #:HINT-BOUNDED-ABOVE
   #:HINT-TOGGLED
   #:HINT-SAMPLE-RATE
   #:HINT-LOGARITHMIC
   #:HINT-INTEGER
   #:HINT-FILENAME
   #:HINT-OPTIONLIST
   #:NO-TYPE
   #:NUM-TYPE
   #:INT-TYPE
   #:STR-TYPE
   #:SET-TYPE
   #:settings
   #:setting
   #:new-settings
   #:delete-settings
   #:settings-get-type
   #:settings-get-hints
   #:settings-is-realtime
   #:settings-setstr
   #:settings-copystr
   #:settings-dupstr
   #:settings-getstr
   #:settings-getstr-default
   #:settings-str-equal
   #:settings-setnum
   #:settings-getnum
   #:settings-getnum-default
   #:settings-getnum-range
   #:settings-setint
   #:settings-getint
   #:settings-getint-default
   #:settings-getint-range
   #:settings-foreach-option
   #:settings-option-count
   #:settings-option-concat
   #:settings-foreach
   ;; Synth
   #:CHANNEL-INFO-NAME-SIZE
   #:synth
   #:new
   #:delete
   #:get-settings
   ;; MIDI channel messages
   #:noteon
   #:noteoff
   #:cc
   #:get-cc
   #:sysex
   #:pitch-bend
   #:get-pitch-bend
   #:pitch-wheel-sens
   #:get-pitch-wheel-sens
   #:program-change
   #:channel-pressure
   #:bank-select
   #:sfont-select
   #:program-select
   #:program-select-by-sfont-name
   #:get-program
   #:unset-program
   #:get-channel-info
   #:program-reset
   #:system-reset
   #:all-notes-off
   #:all-sounds-off
   #:set-channel-type
   #:get-channel-preset
   #:start
   #:stop
   ;; SoundFont management
   #:sfont
   #:sfload
   #:sfreload
   #:sfunload
   #:add-sfont
   #:remove-sfont
   #:sfcount
   #:get-sfont
   #:get-sfont-by-id
   #:get-sfont-by-name
   #:set-bank-offset
   #:get-bank-offset
   ;; Reverb
   #:REVERB-DEFAULT-ROOMSIZE
   #:REVERB-DEFAULT-DAMP
   #:REVERB-DEFAULT-WIDTH
   #:REVERB-DEFAULT-LEVEL
   #:set-reverb
   #:set-reverb-on
   #:get-reverb-roomsize
   #:get-reverb-damp
   #:get-reverb-level
   #:get-reverb-width
   ;; Chorus
   #:CHORUS-MOD-SINE
   #:CHORUS-MOD-TRIANGLE
   #:CHORUS-DEFAULT-N
   #:CHORUS-DEFAULT-LEVEL
   #:CHORUS-DEFAULT-SPEED
   #:CHORUS-DEFAULT-DEPTH
   #:CHORUS-DEFAULT-TYPE
   #:set-chorus
   #:set-chorus-on
   #:get-chorus-nr
   #:get-chorus-level
   #:get-chorus-speed-hz
   #:get-chorus-depth-ms
   #:get-chorus-type
   ;; Audio and MIDI channels
   #:count-midi-channels
   #:count-audio-channels
   #:count-audio-group
   #:count-effects-channels
   ;; Synthesis parameters
   #:set-sample-rate
   #:set-gain
   #:get-gain
   #:set-polyphony
   #:get-polyphony
   #:get-active-voice-count
   #:get-internal-bufsize
   #:set-interp-method
   #:INTERP-NONE
   #:INTERP-LINEAR
   #:INTERP-4THORDER
   #:INTERP-7THORDER
   #:INTERP-DEFAULT
   #:INTERP-HIGHEST
   #:set-gen
   #:set-gen2
   #:get-gen
   ;; Tuning
   #:create-key-tuning
   #:activate-key-tuning
   #:create-octave-tuning
   #:activate-octave-tuning
   #:tune-notes
   #:select-tuning
   #:activate-tuning
   #:reset-tuning
   #:deactivate-tuning
   #:tuning-iteration-start
   #:tuning-iteration-next
   #:tuning-dump
   ;; Misc
   #:get-cpu-load
   #:error
   ;; Synthesizer plugin
   #:write-s16
   #:write-float
   #:nwrite-float
   #:process
   ;; Synthesizer's interface to handle SoundFont loaders
   #:add-sfloader
   #:voice
   #:alloc-voice
   #:start-voice
   #:get-voicelist
   #:handle-midi-event
   #:set-midi-router))
