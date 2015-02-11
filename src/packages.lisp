;;; Copyright (c) 2013-2015 Tito Latini
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

(defpackage :incudine.config
  (:use :cl)
  (:import-from #:alexandria #:define-constant)
  (:export
   #:*use-foreign-sample-p* #:*max-buffer-size* #:*frames-per-buffer*
   #:*client-name* #:*max-number-of-channels* #:*number-of-input-bus-channels*
   #:*number-of-output-bus-channels* #:*number-of-bus-channels*
   #:*rt-edf-heap-size* #:*nrt-edf-heap-size* #:*rt-priority*
   #:*nrt-priority* #:*receiver-default-priority* #:*max-number-of-nodes*
   #:*default-table-size* #:*default-bpm* #:*fade-curve*
   #:*standard-optimize-settings*
   #:*foreign-sample-pool-size* #:*foreign-rt-memory-pool-size*
   #:*foreign-nrt-memory-pool-size*
   #:*sndfile-buffer-size* #:*bounce-to-disk-guard-size*
   #:*default-header-type* #:*default-data-format*
   #:load-incudinerc))

(defpackage :incudine.external
  (:use :cl)
  (:export
   #:errno-to-string #:pthread-set-priority #:sndfile-to-buffer
   #:foreign-alloc-sample #:foreign-zero-sample #:foreign-set
   #:foreign-realloc-sample
   #:init-foreign-memory-pool #:destroy-foreign-memory-pool
   #:get-foreign-used-size #:get-foreign-max-size
   #:foreign-rt-alloc-ex #:foreign-rt-free-ex #:foreign-rt-realloc-ex
   #:foreign-alloc-fft #:foreign-free-fft #:make-fft-plan #:make-ifft-plan
   #:fft-destroy-plan #:sample-complex #:sample-polar #:magnitude
   #:complex-to-polar #:polar-to-complex
   #:fft-execute #:ifft-execute
   #:apply-window #:apply-scaled-window #:apply-scaled-rectwin
   #:apply-zero-padding
   #:pconv-multiply-partitions
   #:foreign-copy #:%copy-from-ring-buffer #:%copy-to-ring-output-buffer
   #:rt-audio-init #:rt-audio-start #:rt-audio-stop #:rt-get-input
   #:rt-set-output
   #:rt-condition-wait #:rt-transfer-to-c-thread #:rt-cycle-begin #:rt-cycle-end
   #:rt-set-busy-state #:rt-buffer-size #:rt-sample-rate #:rt-set-io-buffers
   #:rt-get-error-msg #:portaudio-device-info #:portaudio-set-device
   #:mouse-event #:mouse-init #:mouse-loop-start #:mouse-stop
   #:get-mouse-status))

(defpackage :incudine.util
  (:use :cl :incudine.config)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:define-constant #:with-gensyms
                #:ensure-symbol)
  (:import-from #:cffi #:foreign-pointer #:foreign-type-size
                #:mem-ref #:mem-aref #:make-pointer #:pointer-address
                #:inc-pointer #:foreign-slot-value
                #:foreign-alloc #:foreign-free)
  (:import-from #:incudine.external #:sample-complex #:foreign-alloc-sample)
  (:export
   #:sample #:positive-sample #:negative-sample #:non-negative-sample
   #:+sample-zero+ #:non-positive-sample #:*sample-type*
   #:*use-foreign-sample-p*
   #:frame
   #:+twopi+ #:+half-pi+ #:+rtwopi+ #:+log001+ #:+sqrt2+
   #:+table-maxlen+ #:+max-lobits+ #:+phase-mask+ #:+rad2inc+ #:*cps2inc*
   #:*pi-div-sr* #:*minus-pi-div-sr* #:*twopi-div-sr*
   #:+pointer-size+ #:+foreign-sample-size+ #:+foreign-complex-size+
   #:+pointer-address-type+
   #:*sample-rate* #:*sample-duration* #:*sound-velocity* #:*r-sound-velocity*
   #:*max-number-of-channels* #:*audio-driver* #:*client-name*
   #:*number-of-input-bus-channels* #:*number-of-output-bus-channels*
   #:*number-of-bus-channels* #:*rt-edf-heap-size* #:*nrt-edf-heap-size*
   #:*max-buffer-size*
   #:*rt-thread* #:*nrt-thread* #:*fast-nrt-thread* #:*rt-priority*
   #:*nrt-priority* #:*fast-nrt-priority* #:*receiver-default-priority*
   #:*sndfile-buffer-size*
   #:*bounce-to-disk-guard-size* #:*default-header-type* #:*default-data-format*
   #:*max-number-of-nodes* #:*default-table-size* #:*default-bpm* #:*fade-curve*
   #:*allow-rt-memory-pool-p*
   #:*standard-optimize-settings*
   #:most-positive-sample #:most-negative-sample
   #:least-positive-sample #:least-negative-sample
   #:incudine-version
   #:exit
   #:next-power-of-two #:power-of-two-p
   #:apply-sample-coerce
   #:alloc-multi-channel-data #:free-multi-channel-data
   #:dochannels
   #:lin->db #:db->lin
   #:linear-interp #:cos-interp #:cubic-interp
   #:t60->pole
   #:set-sample-rate #:set-sample-duration #:set-sound-velocity
   #:sample-rate-hook #:sample-duration-hook #:sound-velocity-hook
   #:non-negative-fixnum64 #:most-positive-fixnum64
   #:limited-sample #:maybe-limited-sample
   #:channel-number #:bus-number
   #:*reduce-warnings* #:reduce-warnings
   #:*logger-stream* #:*null-output* #:*logger-force-output*
   #:msg #:nrt-msg #:logger-level #:with-local-logger
   #:logger-time #:logger-time-function #:default-logger-time-function
   #:compare-and-swap #:barrier
   #:get-bytes-consed-in
   #:thread-set-priority
   #:seed-random-state
   #:sample->fixnum #:sample->int
   #:without-interrupts #:with-gc-pending
   #:calc-lobits
   #:rt-thread-p #:rt-eval #:rt-eval-if #:allow-rt-memory-p
   #:foreign-pointer
   #:smp-ref #:i8-ref #:u8-ref #:i16-ref #:u16-ref #:i32-ref #:u32-ref
   #:i64-ref #:u64-ref #:f32-ref #:f64-ref
   #:ptr-ref
   #:with-ensure-symbol
   #:with-foreign-array #:with-samples #:with-samples*
   #:with-complex
   #:do-complex
   #:spinlock #:make-spinlock #:acquire-spinlock #:release-spinlock
   #:with-spinlock-held
   #:cons-pool #:make-cons-pool #:expand-cons-pool #:cons-pool-size
   #:cons-pool-push-cons #:cons-pool-pop-cons
   #:cons-pool-push-list #:cons-pool-pop-list
   #:nrt-global-pool-push-cons #:nrt-global-pool-pop-cons
   #:nrt-global-pool-push-list #:nrt-global-pool-pop-list
   #:rt-global-pool-push-cons #:rt-global-pool-pop-cons
   #:rt-global-pool-push-list #:rt-global-pool-pop-list
   #:make-tlist #:tlist-left #:tlist-right #:tlist-empty-p
   #:tlist-add-left #:tlist-add-right #:tlist-remove-left
   #:foreign-rt-alloc #:foreign-rt-free #:safe-foreign-rt-free
   #:foreign-rt-realloc
   #:get-foreign-sample-used-size #:get-foreign-sample-free-size
   #:get-foreign-sample-max-size #:get-rt-memory-used-size
   #:get-rt-memory-free-size #:get-rt-memory-max-size
   #:get-nrt-memory-used-size #:get-nrt-memory-free-size
   #:get-nrt-memory-max-size))

(defpackage :incudine.vug
  (:use :cl :incudine.util)
  (:nicknames :vug)
  ;; Avoid boring notes from sbcl-1.8; ASH redefined in `vug/util.lisp'
  #+sbcl (:shadow #:ash)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:non-positive-fixnum #:with-gensyms
                #:define-constant #:ensure-symbol #:format-symbol #:make-keyword
                #:maphash-values #:nreversef)
  (:import-from #:cffi #:foreign-type-size #:mem-ref #:mem-aref
                #:make-pointer #:pointer-address #:inc-pointer)
  (:export
   #:vug #:define-vug #:define-vug-macro #:vug-funcall #:destroy-vug
   #:rename-vug #:all-vug-names #:vug-macro-p #:fix-vug
   #:ugen #:define-ugen #:ugen-debug #:compile-vug #:compiled-vug-p
   #:ugen-perf-function #:ugen-reinit-function #:ugen-return-pointer
   #:ugen-control-pointer #:define-ugen-control-setter
   #:ugen-funcall #:destroy-ugen #:rename-ugen #:fix-ugen #:all-ugen-names
   #:*eval-some-specials-p* #:*specials-to-eval*
   #:with
   #:with-coerce-arguments #:with-vug-inputs #:vug-input
   #:get-pointer
   #:dsp! #:dsp-debug #:all-dsp-names #:free-dsp-instances #:destroy-dsp
   #:*update-dsp-instances*
   #:current-channel #:current-frame #:current-sample
   #:make-frame #:frame-ref #:multiple-sample-bind #:samples
   #:make-int32-array #:make-uint32-array #:make-int64-array #:make-uint64-array
   #:make-f32-array #:make-f64-array #:make-pointer-array
   #:maybe-make-i32-array #:maybe-make-u32-array #:maybe-i32-ref #:maybe-u32-ref
   #:foreign-length #:foreign-array-type-of
   #:foreach-tick #:foreach-channel #:foreach-frame
   #:counter #:downsamp #:generic-rate
   #:samphold #:interpolate
   #:tick #:external-variable #:init-only #:initialize
   #:with-follow #:without-follow
   #:maybe-expand #:update #:object-to-free
   #:out #:frame-out #:cout #:node-out
   #:dsp-node
   #:lin->lin #:lin->exp
   #:done-action #:done-self #:free-self #:free-self-when-done
   #:clip #:nclip #:wrap #:nwrap #:mirror #:nmirror
   ;; buffer
   #:make-local-buffer #:buffer-read #:buffer-write #:buffer-frame #:buffer-play
   ;; oscillator
   #:phasor #:phasor-loop #:osc #:sine #:pulse #:impulse #:buzz #:gbuzz
   #:oscrq #:oscrs #:oscrc #:oscr
   ;; envelope
   #:make-local-envelope #:make-local-linen #:make-local-perc
   #:make-local-cutoff #:make-local-asr #:make-local-adsr #:make-local-dadsr
   #:line #:x-line #:envgen #:node-segment
   ;; noise
   #:white-noise #:pink-noise #:fractal-noise #:crackle
   #:make-random-number-generator #:rand
   ;; chaos
   #:cusp #:fb-sine #:gbman #:henon #:latoocarfian #:lin-cong #:quad-map
   #:standard-map #:lorenz #:gendy
   ;; delay
   #:buf-delay-s #:buf-delay #:delay-s #:delay #:buf-vdelay #:vtap #:vdelay
   #:ff-comb #:fb-comb #:allpass #:vallpass
   #:delay-feedback #:delay1
   ;; filter
   #:~ #:pole #:pole* #:zero #:zero* #:two-pole #:two-zero
   #:cs-tone #:cs-atone
   #:one-pole #:one-zero ; (deprecated)
   #:dcblock #:lag #:lag-ud
   #:env-follower #:maf #:median #:decay #:decay-2 #:biquad #:reson
   #:resonz #:resonr #:ringz #:ringr
   #:fofilter #:lpf #:hpf #:bpf #:notch #:apf #:peak-eq #:low-shelf #:hi-shelf
   #:butter-lp #:butter-hp #:butter-bp #:butter-br
   #:nlf2
   #:moogladder #:moogff #:lpf18 #:svf
   #:direct-convolve #:part-convolve
   ;; multi-channel
   #:mono #:stereo #:pan2 #:fpan2
   ;; midi
   #:*linear-midi-table*
   #:midi-note #:midi-keynum #:midi-highest-keynum #:midi-lowest-keynum
   #:midi-note-on #:midi-note-off #:reset-midi-notes
   #:midi-velocity #:midi-cps #:midi-amp #:midi-poly-aftertouch #:midi-cc
   #:midi-program #:midi-global-aftertouch #:midi-pitch-bend
   #:midi-note-on-p #:midi-note-off-p #:midi-note-p #:midi-poly-aftertouch-p
   #:midi-cc-p #:midi-program-p #:midi-global-aftertouch-p #:midi-pitch-bend-p
   #:lin-midi-poly-aftertouch #:exp-midi-poly-aftertouch
   #:lin-midi-cc #:exp-midi-cc
   #:lin-midi-global-aftertouch #:exp-midi-global-aftertouch
   #:lin-midi-pitch-bend #:exp-midi-pitch-bend
   ;; mouse
   #:mouse-start #:mouse-status #:get-mouse-x #:get-mouse-y #:get-mouse-button
   #:mouse-x #:mouse-y #:mouse-button
   #:lin-mouse-x #:lin-mouse-y #:exp-mouse-x #:exp-mouse-y
   ;; analysis
   #:make-local-abuffer
   ;; fft
   #:make-local-fft #:make-local-ifft #:centroid #:flux #:spectral-rms
   #:rolloff #:flatness))

(defpackage :incudine.edf
  (:use :cl)
  (:import-from #:alexandria #:positive-fixnum #:non-negative-fixnum
                #:define-constant #:with-gensyms)
  (:import-from #:incudine.util #:*standard-optimize-settings*
                #:*rt-edf-heap-size* #:*rt-thread* #:sample #:+sample-zero+
                #:next-power-of-two #:power-of-two-p #:with-spinlock-held
                #:rt-thread-p)
  (:export #:+node-root+ #:*heap* #:*heap-size*
           #:node #:make-node #:heap #:make-heap
           #:at #:aat #:sched-loop #:flush-pending #:heap-empty-p #:heap-count
           #:last-time
           #:add-flush-pending-hook #:remove-flush-pending-hook
           #:reduce-heap-pool))

(defpackage :incudine.analysis
  (:use :cl)
  (:nicknames :ana)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:define-constant #:with-gensyms
                #:ensure-symbol #:format-symbol)
  (:import-from #:cffi #:mem-ref #:null-pointer #:null-pointer-p #:foreign-free)
  (:import-from #:incudine.util #:sample #:+sample-zero+ #:+twopi+ #:+half-pi+
                #:+rtwopi+ #:+log001+ #:+pointer-size+ #:+foreign-sample-size+
                #:+foreign-complex-size+ #:foreign-rt-alloc #:foreign-rt-free
                #:safe-foreign-rt-free #:foreign-pointer
                #:alloc-multi-channel-data #:free-multi-channel-data
                #:channel-number #:non-negative-fixnum64
                #:*standard-optimize-settings*
                #:*reduce-warnings* #:reduce-warnings #:sample->fixnum
                #:sample->int
                #:rt-eval #:rt-thread-p)
  (:import-from #:incudine.external #:foreign-alloc-sample #:foreign-zero-sample
                #:foreign-realloc-sample #:foreign-alloc-fft #:foreign-free-fft
                #:make-fft-plan #:make-ifft-plan #:fft-destroy-plan
                #:sample-complex #:sample-polar #:magnitude #:complex-to-polar
                #:polar-to-complex
                #:fft-execute #:ifft-execute
                #:apply-window #:apply-scaled-window #:apply-scaled-rectwin
                #:apply-zero-padding
                #:foreign-copy #:%copy-from-ring-buffer
                #:%copy-to-ring-output-buffer)
  (:export #:analysis #:analysis-p #:analysis-time #:touch-analysis
           #:window-size #:window-function #:rectangular-window
           #:abuffer #:make-abuffer #:abuffer-time #:abuffer-realpart
           #:abuffer-imagpart #:abuffer-size #:abuffer-link #:abuffer-nbins
           #:abuffer-normalized-p #:abuffer-polar #:abuffer-complex
           #:resize-abuffer #:touch-abuffer
           #:pvbuffer #:buffer->pvbuffer #:pvbuffer-data #:pvbuffer-size
           #:pvbuffer-frames #:pvbuffer-channels #:pvbuffer-fft-size
           #:pvbuffer-block-size #:pvbuffer-scale-factor
           #:fft #:fft-p #:make-fft #:*fft-default-window-function*
           #:ifft #:ifft-p #:make-ifft
           #:+fft-plan-optimal+ #:+fft-plan-best+ #:+fft-plan-fast+
           #:fft-plan #:get-fft-plan #:new-fft-plan #:remove-fft-plan
           #:fft-plan-list #:fft-input #:ifft-output #:fft-size
           #:compute-abuffer #:compute-fft #:compute-ifft
           #:dofft #:dofft-polar #:dofft-complex))

(defpackage :incudine.gen
  (:use :cl)
  (:nicknames :gen)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:with-gensyms #:make-keyword)
  (:import-from #:cffi #:mem-ref #:mem-aref #:foreign-funcall)
  (:import-from :incudine.util #:*standard-optimize-settings*
                #:*reduce-warnings*
                #:*sample-rate* #:+twopi+ #:+half-pi+
                #:+foreign-sample-size+
                #:foreign-pointer #:with-foreign-array
                #:with-samples #:with-samples* #:sample
                #:smp-ref
                #:non-negative-sample
                #:+sample-zero+ #:limited-sample #:sample->fixnum #:sample->int
                #:nrt-msg)
  (:export #:envelope
           #:partials #:gbuzz #:chebyshev-1 #:polynomial
           #:defwindow #:hanning #:hamming #:blackman #:sine-window #:bartlett
           #:rand #:all-random-distributions #:rand-args))

(defpackage :incudine
  (:use :cl :incudine.vug :incudine.util)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:non-negative-real #:positive-real
                #:positive-rational
                #:with-gensyms #:define-constant #:ensure-symbol #:format-symbol
                #:maphash-keys)
  (:import-from #:cffi #:foreign-type-size #:foreign-alloc #:foreign-free
                #:null-pointer #:null-pointer-p
                #:mem-ref #:mem-aref #:inc-pointer #:incf-pointer)
  (:import-from #:incudine.external #:pthread-set-priority #:sndfile-to-buffer
                #:foreign-alloc-sample #:foreign-zero-sample
                #:foreign-realloc-sample #:foreign-alloc-fft #:foreign-free-fft
                #:sample-complex #:sample-polar #:magnitude
                #:complex-to-polar #:polar-to-complex
                #:fft-execute #:ifft-execute
                #:apply-window #:apply-scaled-window #:apply-zero-padding
                #:foreign-copy
                #:%copy-from-ring-buffer #:%copy-to-ring-output-buffer
                #:rt-audio-init #:rt-audio-start #:rt-audio-stop
                #:rt-get-input #:rt-set-output #:rt-cycle-begin #:rt-cycle-end
                #:rt-set-io-buffers
                #:rt-condition-wait #:rt-transfer-to-c-thread
                #:rt-set-busy-state
                #:rt-buffer-size #:rt-sample-rate #:rt-get-error-msg
                #:portaudio-device-info #:portaudio-set-device)
  (:import-from #:incudine.edf #:at #:aat #:flush-pending
                #:add-flush-pending-hook #:remove-flush-pending-hook)
  (:import-from #:incudine.gen #:all-random-distributions #:rand-args)
  (:export
   #:init #:enable-sharp-t-syntax #:enable-sharp-square-bracket-syntax
   #:dsp-seq
   #:buffer #:make-buffer #:buffer-p #:size #:frames #:channels #:mask
   #:buffer-mask #:buffer-data #:fill-buffer #:smp-ref #:buffer-value
   #:buffer-size #:buffer-frames #:buffer-channels #:buffer-sample-rate
   #:buffer-value #:buffer-mask #:buffer-lobits #:buffer-lomask #:buffer-lodiv
   #:buffer-file #:buffer-load #:buffer-save #:map-buffer #:map-into-buffer
   #:buffer->list
   #:copy-buffer #:resize-buffer
   #:scale-buffer #:rescale-buffer #:normalize-buffer
   #:*default-tuning*
   #:tuning #:make-tuning #:tuning-data #:set-tuning #:set-tuning-reference
   #:tuning-notes-from-data #:tuning-cps #:tuning-cents #:tuning-ratios
   #:tuning-description #:tuning-keynum-base #:tuning-freq-base
   #:tuning-degree-index #:tuning-save
   #:load-sclfile
   #:foreign-array #:make-foreign-array #:foreign-array-data
   #:foreign-array-type
   #:sample-rate #:filename #:free #:free-p #:stop
   #:bus #:audio-in #:audio-out
   #:peak-info #:print-peak-info #:reset-peak-meters
   #:set-number-of-channels
   #:*rt-thread-start-hook* #:*rt-thread-exit-hook*
   #:rt-start #:rt-stop #:rt-status #:rt-buffer-size #:rt-sample-rate
   #:rt-loop-callback #:set-rt-block-size #:block-size
   #:portaudio-device-info #:portaudio-set-device
   #:at #:aat #:flush-pending #:flush-all-fifos
   #:tempo #:*tempo* #:make-tempo #:bpm #:bps #:now #:tempo-sync
   #:tempo-envelope #:make-tempo-envelope #:set-tempo-envelope
   #:time-at #:bps-at #:bpm-at
   #:rt-funcall #:fast-rt-funcall #:nrt-funcall #:fast-nrt-funcall
   #:*sine-table* #:*cosine-table*
   #:all-random-distributions #:rand-args
   ;; node
   #:node #:node-p #:group #:group-p #:make-group #:node-name #:node-id
   #:node-start-time #:node-uptime
   #:next-node-id #:*node-root* #:node-release-phase-p #:node-gain
   #:node-enable-gain-p #:*node-enable-gain-p* #:node-fade-time
   #:node-fade-curve #:*fade-time* #:node-fade-in #:node-fade-out
   #:pause #:unpause #:pause-p #:move #:before-p #:after-p #:head-p #:tail-p
   #:node-free-all #:stop-hook #:free-hook #:dograph #:dogroup #:dump
   #:set-control #:set-controls #:control-value #:control-getter
   #:control-setter #:control-list #:control-names
   #:reinit
   ;; responder
   #:get-receiver #:remove-receiver
   #:recv-start #:recv-stop #:recv-status #:recv-functions
   #:make-responder #:make-osc-responder #:add-responder #:remove-responder
   #:remove-all-responders #:get-responder-list
   ;; midi
   #:midiout #:midiout-sysex #:midi-tuning-sysex #:set-tuning-from-midi
   ;; envelope
   #:+seg-step-func+ #:+seg-lin-func+ #:+seg-exp-func+ #:+seg-sine-func+
   #:+seg-welch-func+ #:+seg-square-func+ #:+seg-cubic-func+
   #:envelope #:envelope-p #:make-envelope #:set-envelope #:envelope-points
   #:envelope-max-points #:envelope-level #:envelope-time #:envelope-curve
   #:envelope-data #:envelope-duration #:envelope-loop-node
   #:envelope-release-node
   #:envelope-restart-level #:envelope-at
   #:duration #:max-points #:breakpoints->env #:freq-breakpoints->env
   #:copy-envelope #:scale-envelope #:normalize-envelope #:rescale-envelope
   #:linen #:perc #:cutoff #:asr #:adsr #:dadsr
   #:make-linen #:make-perc #:make-cutoff #:make-asr #:make-adsr #:make-dadsr
   ;; nrt
   #:with-nrt #:bounce-to-disk #:bounce-to-buffer
   #:regofile->sexp #:regofile->function #:regofile->lispfile
   #:defscore-statement #:delete-score-statement))

(defpackage :incudine.voicer
  (:use :cl)
  (:nicknames :voicer)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:with-gensyms #:define-constant
                #:ensure-symbol)
  (:import-from #:incudine.util #:*standard-optimize-settings* #:reduce-warnings
                #:cons-pool #:make-cons-pool #:expand-cons-pool #:cons-pool-size
                #:cons-pool-pop-cons #:cons-pool-push-cons
                #:cons-pool-pop-list #:cons-pool-push-list
                #:nrt-global-pool-push-cons #:nrt-global-pool-pop-cons
                #:nrt-global-pool-push-list #:nrt-global-pool-pop-list
                #:make-tlist #:tlist-empty-p #:tlist-add-left
                #:tlist-add-right #:tlist-remove-left
                #:spinlock #:make-spinlock #:with-spinlock-held
                #:msg)
  (:export #:voicer #:create #:update #:node #:empty-p #:full-p
           #:steal-first-voice #:steal-last-voice #:trigger #:release
           #:polyphony #:control-value #:get-controls #:set-controls
           #:define-map #:remove-map #:remove-all-maps #:mapvoicer #:panic
           #:unsafe-trigger #:unsafe-release #:unsafe-set-polyphony
           #:unsafe-control-value #:unsafe-get-controls #:unsafe-set-controls
           #:unsafe-define-map #:unsafe-remove-map #:unsafe-mapvoicer
           #:unsafe-panic #:midi-event #:fill-freq-table #:fill-amp-table
           #:fill-freq-vector #:fill-amp-vector ; deprecated
           #:midi-bind #:scale-midi-amp
           #:keynum->cps #:velocity->amp #:set-midi-freq-function
           #:set-midi-amp-function))

(defpackage :incudine.vug-foreign
  (:use :cl :incudine.util :incudine.vug)
  (:import-from #:alexandria #:define-constant #:make-keyword
                #:format-symbol #:ensure-symbol #:with-gensyms
                #:non-negative-fixnum)
  (:import-from #:incudine #:block-size)
  (:export
   #:+input-port+ #:+output-port+ #:+control-port+ #:+audio-port+
   #:+event-port+ #:+midi-port+
   #:port #:make-port #:port-loop #:plugin #:make-plugin
   #:update-io-number #:input-port-p #:output-port-p #:control-port-p
   #:audio-port-p #:event-port-p #:midi-port-p
   #:doc-string #:with-vug-plugin))

(defpackage :incudine.osc
  (:use :cl
        #+(and sbcl (or x86 x86-64))
        #:sb-c
        #+(and sbcl (or x86 x86-64))
        #:sb-assem)
  (:nicknames #:osc)
  (:import-from :alexandria #:define-constant #:positive-fixnum
                #:non-negative-fixnum #:with-gensyms)
  (:import-from :swap-bytes #:htonl #:htonq #:ntohl #:ntohq)
  (:shadow #:open #:close #:stream #:input-stream-p #:output-stream-p)
  (:export
   #:*listen-backlog* #:*buffer-size* #:*max-values* #:*before-close-hook*
   #:stream #:input-stream #:input-stream-p #:output-stream #:output-stream-p
   #:with-stream #:open #:open-p #:close #:block-p #:broadcast #:connect
   #:reject #:connections #:host #:port #:protocol #:protocolp #:socket-fd
   #:buffer-pointer #:buffer-size #:message-pointer #:message-length
   #:message-encoding #:receive #:send #:slip-encode #:slip-decode
   #:message #:start-message #:value #:value-pointer #:midi #:address-pattern
   #:check-pattern #:index-values #:with-values #:required-values
   #:buffer-to-octets #:octets-to-buffer #:fix-size #:string-size))

(defpackage :incudine.scratch
  (:use :cl :incudine :incudine.vug :incudine.util :incudine.analysis)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:with-gensyms #:define-constant)
  (:nicknames :scratch))
