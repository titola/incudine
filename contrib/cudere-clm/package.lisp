;;; Incudine version of Common Lisp Music.
;;; Copyright (c) 2017 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (and (find-package "CLM")
             (not (find-package "CUDERE-CLM"))
             (null (find-package "CLM-ORIG")))
    (rename-package "CLM" "CLM-ORIG"))
  (pushnew :clm *features*)
  (pushnew :cudere-clm *features*))

(defpackage :cudere-clm
  (:use #:cl)
  (:nicknames :clm)
  (:import-from #:alexandria #:define-constant)
  (:export
   ;; Conditions
   #:cudere-clm-error
   ;; Constants
   #:two-pi
   #:mus-unsupported #:mus-next #:mus-aifc #:mus-riff #:mus-rf64 #:mus-bicsf
   #:mus-nist #:mus-inrs #:mus-esps #:mus-svx #:mus-voc #:mus-sndt #:mus-raw
   #:mus-smp #:mus-avr #:mus-ircam #:mus-sd1 #:mus-sppack #:mus-mus10
   #:mus-hcom #:mus-psion #:mus-maud #:mus-ieee #:mus-matlab #:mus-adc
   #:mus-midi #:mus-soundfont #:mus-gravis #:mus-comdisco #:mus-goldwave
   #:mus-srfs #:mus-midi-sample-dump #:mus-diamondware #:mus-adf #:mus-sbstudioii
   #:mus-delusion #:mus-farandole #:mus-sample-dump #:mus-ultratracker
   #:mus-yamaha-sy85 #:mus-yamaha-tx16 #:mus-digiplayer #:mus-covox #:mus-avi
   #:mus-omf #:mus-quicktime #:mus-asf #:mus-yamaha-sy99 #:mus-kurzweil-2000
   #:mus-aiff #:mus-paf #:mus-csl #:mus-file-samp #:mus-pvf #:mus-soundforge
   #:mus-twinvq #:mus-akai4 #:mus-impulsetracker #:mus-korg #:mus-nvf
   #:mus-caff #:mus-maui #:mus-sdif #:mus-ogg #:mus-flac #:mus-speex #:mus-mpeg
   #:mus-shorten #:mus-tta #:mus-wavpack
   #:mus-unknown #:mus-bshort #:mus-mulaw #:mus-byte #:mus-bfloat #:mus-bint
   #:mus-alaw #:mus-ubyte #:mus-b24int #:mus-bdouble #:mus-lshort #:mus-lint
   #:mus-lfloat #:mus-ldouble #:mus-ubshort #:mus-ulshort #:mus-l24int #:mus-bintn
   #:mus-lintn #:mus-blfoatu #:mus-lfloatu #:mus-bdoubleu #:mus-ldoubleu
   #:mus-audio-default
   #:mus-interp-none #:mus-interp-linear #:mus-interp-sinusoidal
   #:mus-interp-all-pass #:mus-interp-lagrange #:mus-interp-bezier
   #:mus-interp-hermite #:mus-linear #:mus-sinusoidal
   #:mus-chebyshev-first-kind #:mus-chebyshev-second-kind
   #:rectangular-window #:hann-window #:hanning-window #:welch-window
   #:parzen-window #:bartlett-window #:hamming-window #:blackman2-window
   #:blackman3-window #:blackman4-window #:exponential-window #:riemann-window
   #:kaiser-window #:cauchy-window #:poisson-window #:gaussian-window
   #:tukey-window #:dolph-chebyshev-window #:hann-poisson-window
   #:connes-window #:samaraki-window #:ultraspherical-window
   #:bartlett-hann-window #:bohman-window #:flat-top-window #:blackman5-window
   #:blackman6-window #:blackman7-window #:blackman8-window #:blackman9-window
   #:blackman10-window #:rv2-window #:rv3-window #:rv4-window
   ;; Variables
   #:*output* #:*reverb*
   #:*clm-srate* #:*srate* #:*clm-channels* #:*channels*
   #:*clm-file-buffer-size* #:*clm-file-name* #:*clm-header-type* #:*header-type*
   #:*clm-data-format* #:*data-format* #:*clm-tempfile-data-format*
   #:*clm-tempfile-header-type* #:*clm-verbose* #:*verbose* #:*clm-play*
   #:*clm-player* #:*clm-table-size* #:*clm-safety* #:*safety*
   #:*clm-array-print-length* #:*clm-init* #:*clm-search-list* #:*clm-notehook*
   #:*notehook* #:*clm-clipped* #:*clipped* #:*clm-src-width*
   #:*clm-delete-reverb* #:*clm-reverb-channels* #:*clm-statistics*
   #:*statistics* #:*clm-default-frequency* #:*clm-debug* #:*debug* #:*clm-ins*
   #:*clm-locsig-type* #:*interrupted* #:*offset* #:*clm-with-sound-depth*
   #:*definstrument-hook* #:*to-snd* #:*clm-ugens-package*
   #:*clm-optimize-settings* #:*clm-logger-stream*
   ;; Definitions
   #:define-clm-ugen #:definstrument #:definstrument* #:def-clm-struct
   #:def-optkey-fun #:run #:run*
   ;; Methods
   #:mus-channel #:mus-channels #:mus-close #:mus-data #:mus-file-name
   #:mus-feedback #:mus-feedforward #:mus-frequency #:mus-hop #:mus-increment
   #:mus-input? #:mus-interp-type #:mus-length #:mus-location #:mus-offset
   #:mus-order #:mus-output? #:mus-phase #:mus-ramp #:mus-reset #:mus-safety
   #:mus-scaler #:mus-xcoeff #:mus-xcoeffs #:mus-ycoeff #:mus-ycoeffs #:mus-width
   ;; Utilities
   #:hz->radians #:radians->hz
   #:seconds->samples #:samples->seconds #:times->samples
   #:mus-rand-seed #:mus-set-rand-seed
   #:clm-random #:centered-random #:mus-random
   #:sound-chans #:sound-duration #:sound-data-format #:sound-header-type
   #:sound-data-location #:sound-length #:sound-samples #:sound-frames
   #:sound-framples #:sound-srate #:sound-comment #:sound-datum-size
   #:sound-data-location #:sound-maxamp
   #:double #:double-float #:make-double-float-array #:make-double-array
   #:make-integer-array #:clear-array
   #:array-interp #:mus-interpolate
   #:partials->wave #:phase-partials->wave
   #:normalize-partials #:partials->polynomial #:polynomial
   #:file->array #:array->file
   #:frample->frample
   #:envelope->coeffs #:envelope-interp #:x-norm
   #:reduce-amplitude-quantization-noise #:inverse-integrate
   #:clm-cerror #:clm-print
   #:play #:stop-playing #:dac #:stop-dac
   ;; FFT
   #:fft #:make-fft #:with-pointer-to-fft-data
   #:rectangular->polar #:rectangular->magnitudes #:polar->rectangular
   #:make-fft-window #:apply-window #:spectrum #:convolution
   #:autocorrelate #:correlate
   ;; Common Music interface
   #:init-with-sound #:finish-with-sound #:wsdat-play
   ;; Generators
   #:make-oscil #:oscil #:oscil?
   #:make-env #:env #:env?
   #:make-table-lookup #:table-lookup #:table-lookup?
   #:make-polywave #:polywave #:polywave?
   #:make-polyshape #:polyshape #:polyshape?
   #:make-triangle-wave #:triangle-wave #:triangle-wave?
   #:make-square-wave #:square-wave #:square-wave?
   #:make-sawtooth-wave #:sawtooth-wave #:sawtooth-wave?
   #:make-pulse-train #:pulse-train #:pulse-train?
   #:make-ncos #:ncos #:ncos?
   #:make-nsin #:nsin #:nsin?
   #:make-ssb-am #:ssb-am #:ssb-am?
   #:make-wave-train #:wave-train #:wave-train?
   #:make-rand #:rand #:rand?
   #:make-rand-interp #:rand-interp #:rand-interp?
   #:make-one-pole #:one-pole #:one-pole?
   #:make-one-zero #:one-zero #:one-zero?
   #:make-two-pole #:two-pole #:two-pole?
   #:make-two-zero #:two-zero #:two-zero?
   #:make-formant #:formant #:formant?
   #:make-firmant #:firmant #:firmant?
   #:make-filter #:filter #:filter?
   #:make-fir-filter #:fir-filter #:fir-filter?
   #:make-iir-filter #:iir-filter #:iir-filter?
   #:make-delay #:delay #:delay? #:tap #:delay-tick
   #:make-comb #:comb #:comb?
   #:make-filtered-comb #:filtered-comb #:filtered-comb?
   #:make-notch #:notch #:notch?
   #:make-all-pass #:all-pass #:all-pass?
   #:make-moving-average #:moving-average #:moving-average?
   #:make-src #:src #:src?
   #:make-convolve #:convolve #:convolve? #:convolve-files
   #:make-granulate #:granulate #:granulate?
   #:make-phase-vocoder #:phase-vocoder #:phase-vocoder?
   #:phase-vocoder-amps #:phase-vocoder-freqs #:phase-vocoder-phases
   #:phase-vocoder-amp-increments #:phase-vocoder-phase-increments
   #:make-nrxysin #:nrxysin #:nrxysin?
   #:make-nrxycos #:nrxycos #:nrxycos?
   #:make-asymmetric-fm #:asymmetric-fm #:asymmetric-fm?
   ;; Sound I/O
   #:make-file->sample #:file->sample #:file->sample?
   #:make-file->frample #:file->frample #:file->frample?
   #:make-sample->file #:sample->file #:sample->file? #:continue-sample->file
   #:make-frample->file #:frample->file #:frample->file? #:continue-frample->file
   #:make-readin #:readin #:readin?
   #:make-locsig #:locsig #:locsig? #:locsig-ref #:locsig-set!
   #:locsig-reverb-ref #:locsig-reverb-set! #:move-locsig #:locsig-type
   #:make-move-sound #:move-sound #:move-sound?
   #:in-any #:ina #:inb #:out-any #:outa #:outb #:outc #:outd
   #:open-input #:open-input* #:close-input
   ;; Score
   #:with-sound #:clm-load #:sound-let #:with-offset
   #:scaled-by #:scaled-to))

(defpackage :cudere-clm.sys
  (:use #:cl #:cudere-clm #:incudine #:incudine.util)
  (:shadowing-import-from #:incudine #:play)
  (:import-from #:incudine.vug #:ugen-instance #:define-ugen
                #:ugen-perf-function #:ugen-reinit-function
                #:ugen-return-pointer #:ugen-control-pointer
                #:with #:initialize #:with-follow #:vuglet #:get-pointer
                #:make-frame #:maybe-expand #:breakpoints->local-env
                #:~ #:delay1 #:nclip)
  (:import-from #:alexandria #:non-negative-fixnum #:positive-fixnum
                #:define-constant #:with-gensyms #:format-symbol)
  (:import-from #:incudine #:incudine-simple-error #:incudine-missing-arg)
  (:import-from #:incudine.soundfile #:with-open-soundfile))

(defpackage :cudere-clm.ugens
  (:use #:cl)
  (:export
   #:oscil #:env #:table-lookup #:polywave #:polyshape
   #:triangle-wave #:square-wave #:sawtooth-wave #:pulse-train
   #:ncos #:nsin #:ssb-am #:wave-train
   #:rand #:rand-interp
   #:one-pole #:one-zero #:two-pole #:two-zero #:formant #:firmant
   #:filter #:fir-filter #:iir-filter
   #:delay #:comb #:filtered-comb #:notch #:all-pass #:moving-average
   #:readin #:src #:convolve #:granulate #:phase-vocoder
   #:nrxysin #:nrxycos #:asymmetric-fm))
