;;; Copyright (c) 2013 Tito Latini
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

(defpackage sndfile
  (:use #:cl)
  (:nicknames #:sf)
  (:import-from #:alexandria #:non-negative-fixnum #:define-constant
                #:with-gensyms #:format-symbol #:make-keyword)
  (:shadow #:open #:close #:error #:format #:type #:length #:count)
  (:export
   ;; constants
   #:str-title
   #:str-copyright
   #:str-software
   #:str-artist
   #:str-comment
   #:str-date
   #:str-album
   #:str-license
   #:str-tracknumber
   #:str-genre
   #:false
   #:true
   #:sfm-read
   #:sfm-write
   #:sfm-rdwr
   #:ambisonic-none
   #:ambisonic-b-format
   #:err-no-error
   #:err-unrecognised-format
   #:err-system
   #:err-malformed-file
   #:err-unsupported-encoding
   #:channel-map-invalid
   #:channel-map-mono
   #:channel-map-left
   #:channel-map-right
   #:channel-map-center
   #:channel-map-front-left
   #:channel-map-front-right
   #:channel-map-front-center
   #:channel-map-rear-center
   #:channel-map-rear-left
   #:channel-map-rear-right
   #:channel-map-lfe
   #:channel-map-front-left-of-center
   #:channel-map-front-right-of-center
   #:channel-map-side-left
   #:channel-map-side-right
   #:channel-map-top-center
   #:channel-map-top-front-left
   #:channel-map-top-front-right
   #:channel-map-top-front-center
   #:channel-map-top-rear-left
   #:channel-map-top-rear-right
   #:channel-map-top-rear-center
   #:channel-map-ambisonic-b-w
   #:channel-map-ambisonic-b-x
   #:channel-map-ambisonic-b-y
   #:channel-map-ambisonic-b-z
   #:channel-map-max
   #:sfd-default-level
   #:sfd-custom-level
   #:sfd-no-dither
   #:sfd-white
   #:sfd-triangular-pd
   #:loop-none
   #:loop-forward
   #:loop-backward
   #:loop-alternating
   #:count-max
   ;; types
   #:sndfile
   #:format
   #:sf-count
   #:info
   #:format-info
   #:dither-info
   #:embed-file-info
   #:instr-loop
   #:instrument
   #:loop-info
   #:broadcast-info
   #:virtual-io
   ;; functions
   #:open
   #:open-fd
   #:open-virtual
   #:error
   #:strerror
   #:error-number
   #:command
   #:format-check
   #:seek
   #:set-string
   #:get-string
   #:version-string
   #:read-raw
   #:read-short
   #:read-int
   #:read-float
   #:read-double
   #:write-raw
   #:write-short
   #:write-int
   #:write-float
   #:write-double
   #:readf-short
   #:readf-int
   #:readf-float
   #:readf-double
   #:writef-short
   #:writef-int
   #:writef-float
   #:writef-double
   #:write-sync
   #:close
   #:get-lib-version
   #:get-log-info
   #:get-current-sf-info
   #:get-norm-double
   #:get-norm-float
   #:set-norm-double
   #:set-norm-float
   #:set-scale-float-int-read
   #:set-scale-int-float-write
   #:get-simple-format-count
   #:get-simple-format
   #:get-format-info
   #:get-format-major-count
   #:get-format-major
   #:get-format-subtype-count
   #:get-format-subtype
   #:calc-signal-max
   #:calc-norm-signal-max
   #:calc-max-all-channels
   #:calc-norm-max-all-channels
   #:get-signal-max
   #:get-max-all-channels
   #:set-add-peak-chunk
   #:set-add-header-pad-chunk
   #:update-header-now
   #:set-update-header-auto
   #:file-truncate
   #:set-raw-start-offset
   #:set-dither-on-write
   #:set-dither-on-read
   #:get-dither-info-count
   #:get-dither-info
   #:get-embed-file-info
   #:set-clipping
   #:get-clipping
   #:get-instrument
   #:set-instrument
   #:get-loop-info
   ;; utils
   #:sndfile-p
   #:sndfile-null-p
   #:make-info
   #:frames
   #:sample-rate
   #:channels
   #:sections
   #:seekable
   #:name
   #:extension
   #:type
   #:level
   #:offset
   #:length
   #:mode
   #:start
   #:end
   #:count
   #:gain
   #:basenote
   #:detune
   #:velocity-lo
   #:velocity-hi
   #:key-lo
   #:key-hi
   #:loop-count
   #:loops
   #:time-sig-num
   #:time-sig-den
   #:loop-mode
   #:num-beats
   #:bpm
   #:root-key
   #:future
   #:description
   #:originator
   #:originator-reference
   #:originator-date
   #:originator-time
   #:time-reference-low
   #:time-reference-high
   #:version
   #:umid
   #:reserved
   #:coding-history-size
   #:coding-history
   #:get-filelen
   #:read
   #:write
   #:tell
   #:with-open
   #:get-format
   #:info
   #:duration))
