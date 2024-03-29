;;; Copyright (c) 2013-2022 Tito Latini
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
   ;; constant
   #:FORMAT-WAV
   #:FORMAT-AIFF
   #:FORMAT-AU
   #:FORMAT-RAW
   #:FORMAT-PAF
   #:FORMAT-SVX
   #:FORMAT-NIST
   #:FORMAT-VOC
   #:FORMAT-IRCAM
   #:FORMAT-W64
   #:FORMAT-MAT4
   #:FORMAT-MAT5
   #:FORMAT-PVF
   #:FORMAT-XI
   #:FORMAT-HTK
   #:FORMAT-SDS
   #:FORMAT-AVR
   #:FORMAT-WAVEX
   #:FORMAT-SD2
   #:FORMAT-FLAC
   #:FORMAT-CAF
   #:FORMAT-WVE
   #:FORMAT-OGG
   #:FORMAT-MPC2K
   #:FORMAT-RF64
   #:FORMAT-MPEG
   #:FORMAT-PCM-S8
   #:FORMAT-PCM-16
   #:FORMAT-PCM-24
   #:FORMAT-PCM-32
   #:FORMAT-PCM-U8
   #:FORMAT-FLOAT
   #:FORMAT-DOUBLE
   #:FORMAT-ULAW
   #:FORMAT-ALAW
   #:FORMAT-IMA-ADPCM
   #:FORMAT-MS-ADPCM
   #:FORMAT-GSM610
   #:FORMAT-VOX-ADPCM
   #:FORMAT-NMS-ADPCM-16
   #:FORMAT-NMS-ADPCM-24
   #:FORMAT-NMS-ADPCM-32
   #:FORMAT-G721-32
   #:FORMAT-G723-24
   #:FORMAT-G723-40
   #:FORMAT-DWVW-12
   #:FORMAT-DWVW-16
   #:FORMAT-DWVW-24
   #:FORMAT-DWVW-N
   #:FORMAT-DPCM-8
   #:FORMAT-DPCM-16
   #:FORMAT-VORBIS
   #:FORMAT-OPUS
   #:FORMAT-ALAC-16
   #:FORMAT-ALAC-20
   #:FORMAT-ALAC-24
   #:FORMAT-ALAC-32
   #:FORMAT-MPEG-LAYER-I
   #:FORMAT-MPEG-LAYER-II
   #:FORMAT-MPEG-LAYER-III
   #:ENDIAN-FILE
   #:ENDIAN-LITTLE
   #:ENDIAN-BIG
   #:ENDIAN-CPU
   #:FORMAT-SUBMASK
   #:FORMAT-TYPEMASK
   #:FORMAT-ENDMASK
   #:STR-TITLE
   #:STR-COPYRIGHT
   #:STR-SOFTWARE
   #:STR-ARTIST
   #:STR-COMMENT
   #:STR-DATE
   #:STR-ALBUM
   #:STR-LICENSE
   #:STR-TRACKNUMBER
   #:STR-GENRE
   #:FALSE
   #:TRUE
   #:SFM-READ
   #:SFM-WRITE
   #:SFM-RDWR
   #:AMBISONIC-NONE
   #:AMBISONIC-B-FORMAT
   #:SEEK-SET
   #:SEEK-CUR
   #:SEEK-END
   #:ERR-NO-ERROR
   #:ERR-UNRECOGNISED-FORMAT
   #:ERR-SYSTEM
   #:ERR-MALFORMED-FILE
   #:ERR-UNSUPPORTED-ENCODING
   #:CHANNEL-MAP-INVALID
   #:CHANNEL-MAP-MONO
   #:CHANNEL-MAP-LEFT
   #:CHANNEL-MAP-RIGHT
   #:CHANNEL-MAP-CENTER
   #:CHANNEL-MAP-FRONT-LEFT
   #:CHANNEL-MAP-FRONT-RIGHT
   #:CHANNEL-MAP-FRONT-CENTER
   #:CHANNEL-MAP-REAR-CENTER
   #:CHANNEL-MAP-REAR-LEFT
   #:CHANNEL-MAP-REAR-RIGHT
   #:CHANNEL-MAP-LFE
   #:CHANNEL-MAP-FRONT-LEFT-OF-CENTER
   #:CHANNEL-MAP-FRONT-RIGHT-OF-CENTER
   #:CHANNEL-MAP-SIDE-LEFT
   #:CHANNEL-MAP-SIDE-RIGHT
   #:CHANNEL-MAP-TOP-CENTER
   #:CHANNEL-MAP-TOP-FRONT-LEFT
   #:CHANNEL-MAP-TOP-FRONT-RIGHT
   #:CHANNEL-MAP-TOP-FRONT-CENTER
   #:CHANNEL-MAP-TOP-REAR-LEFT
   #:CHANNEL-MAP-TOP-REAR-RIGHT
   #:CHANNEL-MAP-TOP-REAR-CENTER
   #:CHANNEL-MAP-AMBISONIC-B-W
   #:CHANNEL-MAP-AMBISONIC-B-X
   #:CHANNEL-MAP-AMBISONIC-B-Y
   #:CHANNEL-MAP-AMBISONIC-B-Z
   #:CHANNEL-MAP-MAX
   #:BITRATE-MODE-CONSTANT
   #:BITRATE-MODE-AVERAGE
   #:BITRATE-MODE-VARIABLE
   #:SFD-DEFAULT-LEVEL
   #:SFD-CUSTOM-LEVEL
   #:SFD-NO-DITHER
   #:SFD-WHITE
   #:SFD-TRIANGULAR-PDF
   #:LOOP-NONE
   #:LOOP-FORWARD
   #:LOOP-BACKWARD
   #:LOOP-ALTERNATING
   #:SFC-GET-LIB-VERSION
   #:SFC-GET-LOG-INFO
   #:SFC-GET-CURRENT-SF-INFO
   #:SFC-GET-NORM-DOUBLE
   #:SFC-GET-NORM-FLOAT
   #:SFC-SET-NORM-DOUBLE
   #:SFC-SET-NORM-FLOAT
   #:SFC-SET-SCALE-FLOAT-INT-READ
   #:SFC-SET-SCALE-INT-FLOAT-WRITE
   #:SFC-GET-SIMPLE-FORMAT-COUNT
   #:SFC-GET-SIMPLE-FORMAT
   #:SFC-GET-FORMAT-INFO
   #:SFC-GET-FORMAT-MAJOR-COUNT
   #:SFC-GET-FORMAT-MAJOR
   #:SFC-GET-FORMAT-SUBTYPE-COUNT
   #:SFC-GET-FORMAT-SUBTYPE
   #:SFC-CALC-SIGNAL-MAX
   #:SFC-CALC-NORM-SIGNAL-MAX
   #:SFC-CALC-MAX-ALL-CHANNELS
   #:SFC-CALC-NORM-MAX-ALL-CHANNELS
   #:SFC-GET-SIGNAL-MAX
   #:SFC-GET-MAX-ALL-CHANNELS
   #:SFC-SET-ADD-PEAK-CHUNK
   #:SFC-SET-ADD-HEADER-PAD-CHUNK
   #:SFC-UPDATE-HEADER-NOW
   #:SFC-SET-UPDATE-HEADER-AUTO
   #:SFC-FILE-TRUNCATE
   #:SFC-SET-RAW-START-OFFSET
   #:SFC-SET-DITHER-ON-WRITE
   #:SFC-SET-DITHER-ON-READ
   #:SFC-GET-DITHER-INFO-COUNT
   #:SFC-GET-DITHER-INFO
   #:SFC-GET-EMBED-FILE-INFO
   #:SFC-SET-CLIPPING
   #:SFC-GET-CLIPPING
   #:SFC-GET-CUE-COUNT
   #:SFC-GET-CUE
   #:SFC-SET-CUE
   #:SFC-GET-INSTRUMENT
   #:SFC-SET-INSTRUMENT
   #:SFC-GET-LOOP-INFO
   #:SFC-GET-BROADCAST-INFO
   #:SFC-SET-BROADCAST-INFO
   #:SFC-GET-CHANNEL-MAP-INFO
   #:SFC-SET-CHANNEL-MAP-INFO
   #:SFC-RAW-DATA-NEEDS-ENDSWAP
   #:SFC-WAVEX-SET-AMBISONIC
   #:SFC-WAVEX-GET-AMBISONIC
   #:SFC-RF64-AUTO-DOWNGRADE
   #:SFC-SET-VBR-ENCODING-QUALITY
   #:SFC-SET-COMPRESSION-LEVEL
   #:SFC-SET-OGG-PAGE-LATENCY-MS
   #:SFC-SET-OGG-PAGE-LATENCY
   #:SFC-GET-BITRATE-MODE
   #:SFC-SET-BITRATE-MODE
   #:SFC-SET-CART-INFO
   #:SFC-GET-CART-INFO
   #:SFC-SET-ORIGINAL-SAMPLERATE
   #:SFC-GET-ORIGINAL-SAMPLERATE
   #:SFC-TEST-IEEE-FLOAT-REPLACE
   #:SFC-SET-ADD-DITHER-ON-WRITE
   #:SFC-SET-ADD-DITHER-ON-READ
   #:COUNT-MAX
   ;; types
   #:sndfile
   #:format
   #:sf-count
   #:count
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
   #:pointer
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
   #:decode-format
   #:info
   #:duration))
