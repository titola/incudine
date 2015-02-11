;;; Copyright (c) 2013-2015 Tito Latini
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

(defpackage portmidi
  (:use #:cl)
  (:nicknames #:pm)
  (:import-from #:alexandria #:non-negative-fixnum #:define-constant
                #:with-gensyms)
  (:shadow #:read #:write #:open #:close #:abort #:error #:stream
           #:input-stream-p #:output-stream-p)
  (:export
   ;; constants
   #:default-sysex-buffer-size
   #:hdrlength
   #:host-error-msg-len
   #:no-device
   #:filt-active
   #:filt-sysex
   #:filt-clock
   #:filt-play
   #:filt-tick
   #:filt-fd
   #:filt-undefined
   #:filt-reset
   #:filt-realtime
   #:filt-note
   #:filt-channel-aftertouch
   #:filt-poly-aftertouch
   #:filt-aftertouch
   #:filt-program
   #:filt-control
   #:filt-pitchbend
   #:filt-mtc
   #:filt-song-position
   #:filt-song-select
   #:filt-tune
   #:filt-systemcommon
   ;; types
   #:message
   #:timestamp
   #:stream
   #:input-stream
   #:input-stream-p
   #:input-stream-sysex-pointer
   #:input-stream-events-remain
   #:output-stream
   #:output-stream-p
   #:error
   #:device-info
   #:event-buffer
   ;; error
   #:portmidi-error
   #:allocation-error
   #:error-generic
   ;; functions
   #:initialize
   #:terminate
   #:has-host-error
   #:get-error-text
   #:get-host-error-text
   #:count-devices
   #:get-default-input-device-id
   #:get-default-output-device-id
   #:get-device-info
   #:print-devices-info
   #:open
   #:open-input
   #:open-output
   #:set-filter
   #:set-channel-mask
   #:abort
   #:close
   #:synchronize
   #:read
   #:poll
   #:write
   #:write-short
   #:write-sysex
   #:message-status
   #:message-data1
   #:message-data2
   #:decode-message
   #:decode-channel-message
   #:sysex-message-p
   #:before
   #:channel
   #:make-event-buffer
   #:free
   #:stream-pointer
   ;; macros
   #:with-input-sysex-event
   #:doevent
   #:with-event-buffer
   #:with-receiver))

(defpackage porttime
  (:use #:cl)
  (:nicknames #:pt)
  (:shadow #:error #:time #:sleep)
  (:export
   ;; types
   #:timestamp
   #:error
   ;; functions
   #:start
   #:stop
   #:started
   #:time
   #:sleep))
