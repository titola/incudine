;;; Copyright (c) 2013-2024 Tito Latini
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
   #:*audio-driver*
   #:*use-foreign-sample-p* #:*sample-type*
   #:*max-buffer-size* #:*frames-per-buffer*
   #:*client-name* #:*max-number-of-channels* #:*number-of-input-bus-channels*
   #:*number-of-output-bus-channels* #:*number-of-bus-channels*
   #:*rt-edf-heap-size* #:*nrt-edf-heap-size* #:*rt-priority* #:*rt-cpu*
   #:*nrt-priority* #:*receiver-default-priority* #:*max-number-of-nodes*
   #:*default-table-size* #:*default-bpm* #:*fade-curve* #:*fade-time*
   #:*midi-input-timeout*
   #:*standard-optimize-settings*
   #:*foreign-sample-pool-size* #:*foreign-rt-memory-pool-size*
   #:*foreign-nrt-memory-pool-size*
   #:*sndfile-buffer-size* #:*bounce-to-disk-guard-size*
   #:*default-header-type* #:*default-data-format*
   #:load-incudinerc))

(defpackage :incudine.external
  (:use :cl)
  (:intern
   #:rt-set-io-buffers #:rt-set-max-bufsize
   #:rt-set-output #:rt-get-input #:rt-get-error-msg
   #:rt-condition-wait #:rt-set-busy-state #:rt-transfer-to-c-thread
   #:rt-audio-init #:rt-audio-start #:rt-audio-stop
   #:rt-cycle-begin #:rt-cycle-end
   #:thread-set-priority
   #:sndfile-to-buffer
   #:%copy-from-ring-buffer #:%copy-to-ring-output-buffer
   #:foreign-alloc-fft #:foreign-free-fft #:make-fft-plan #:make-ifft-plan
   #:fft-destroy-plan #:fft-execute #:ifft-execute
   #:apply-window #:apply-scaled-window #:apply-scaled-rectwin
   #:apply-zero-padding
   #:pconv-multiply-partitions)
  (:export
   #:errno-to-string
   #:foreign-alloc-sample #:foreign-zero-sample #:foreign-set
   #:init-foreign-memory-pool #:destroy-foreign-memory-pool
   #:get-foreign-used-size #:get-foreign-max-size
   #:foreign-alloc-ex #:foreign-free-ex #:foreign-realloc-ex
   #:complex-to-polar #:polar-to-complex
   #:foreign-copy #:foreign-copy-samples
   #:rt-client #:rt-buffer-size #:rt-sample-rate
   #:rt-cycle-start-time #:rt-time-offset
   #:rt-xruns #:rt-silent-errors))

(defpackage :incudine.util
  (:use :cl :incudine.config)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:define-constant #:with-gensyms
                #:ensure-symbol #:make-keyword)
  (:import-from #:cffi #:foreign-pointer #:foreign-type-size
                #:mem-ref #:mem-aref #:make-pointer #:pointer-address
                #:inc-pointer #:foreign-slot-value
                #:foreign-alloc #:foreign-free)
  (:import-from #:incudine.external #:foreign-alloc-sample)
  (:intern #:incudine-object #:incudine-object-pool #:make-incudine-object-pool
           #:incudine-object-pool-expand #:ensure-incudine-object-pool-size
           #:incudine-optimize
           #:alloc-multi-channel-data #:free-multi-channel-data
           #:apply-sample-coerce #:with-ensure-symbols)
  (:export
   #:sample #:positive-sample #:negative-sample #:non-negative-sample
   #:+sample-zero+ #:non-positive-sample
   #:*use-foreign-sample-p*
   #:frame
   #:+twopi+ #:+half-pi+ #:+rtwopi+ #:+log001+ #:+sqrt2+
   #:+table-maxlen+ #:+max-lobits+ #:+phase-mask+ #:+rad2inc+ #:*cps2inc*
   #:*pi-div-sr* #:*minus-pi-div-sr* #:*twopi-div-sr* #:*sr-div-twopi*
   #:+pointer-size+ #:+foreign-sample-size+ #:+foreign-complex-size+
   #:+pointer-address-type+
   #:*sample-rate* #:*sample-duration* #:*sound-velocity* #:*r-sound-velocity*
   #:*sample-rate-hook* #:*sound-velocity-hook* #:*block-size-hook*
   #:*max-number-of-channels* #:*client-name*
   #:*number-of-input-bus-channels* #:*number-of-output-bus-channels*
   #:*number-of-bus-channels* #:*rt-edf-heap-size* #:*nrt-edf-heap-size*
   #:*max-buffer-size*
   #:*rt-thread* #:*nrt-thread* #:*fast-nrt-thread* #:*rt-priority*
   #:*nrt-priority* #:*fast-nrt-priority* #:*receiver-default-priority*
   #:*sndfile-buffer-size*
   #:*bounce-to-disk-guard-size* #:*default-header-type* #:*default-data-format*
   #:*max-number-of-nodes* #:*default-table-size* #:*default-bpm*
   #:*fade-curve* #:*fade-time*
   #:*midi-input-timeout*
   #:*allow-rt-memory-pool-p*
   #:*standard-optimize-settings*
   #:most-positive-sample #:most-negative-sample
   #:least-positive-sample #:least-negative-sample
   #:incudine-version #:incudine-version->=
   #:exit
   #:without-interrupts #:with-pinned-objects
   #:next-power-of-two #:power-of-two-p
   #:dochannels
   #:pow
   #:hz->radians #:radians->hz
   #:linear->db #:db->linear
   #:linear-interp #:cos-interp #:cubic-interp
   #:t60->pole
   #:cheb
   #:set-sample-rate #:set-sample-duration #:set-sound-velocity
   #:non-negative-fixnum64 #:most-positive-fixnum64
   #:limited-sample #:maybe-limited-sample
   #:channel-number #:bus-number
   #:*reduce-warnings* #:reduce-warnings
   #:*logger-stream* #:*null-output* #:*logger-force-output-p*
   #:msg #:nrt-msg #:logger-level #:with-logger
   #:logger-time #:logger-time-function #:default-logger-time-function
   #:compare-and-swap #:barrier
   #:get-bytes-consed-in
   #:thread-affinity #:thread-priority
   #:seed-random-state
   #:sample->fixnum #:sample->int #:float->fixnum #:rationalize* #:parse-float
   #:sort-samples
   #:rt-thread-p #:rt-eval #:allow-rt-memory-p
   #:foreign-pointer
   #:smp-ref #:i8-ref #:u8-ref #:i16-ref #:u16-ref #:i32-ref #:u32-ref
   #:i64-ref #:u64-ref #:f32-ref #:f64-ref
   #:ptr-ref
   #:with-foreign-array #:with-samples #:with-samples*
   #:defun* #:lambda* #:defmacro* #:lambda-list-to-star-list
   #:spinlock #:make-spinlock #:spinlock-name
   #:acquire-spinlock #:try-acquire-spinlock #:release-spinlock
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
   #:foreign-realloc #:foreign-rt-realloc
   #:get-foreign-sample-used-size #:get-foreign-sample-free-size
   #:get-foreign-sample-max-size #:get-rt-memory-used-size
   #:get-rt-memory-free-size #:get-rt-memory-max-size
   #:get-nrt-memory-used-size #:get-nrt-memory-free-size
   #:get-nrt-memory-max-size
   #:mouse-start #:mouse-stop #:mouse-status
   #:get-mouse-x #:get-mouse-y #:get-mouse-button))

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
                #:incudine-object #:incudine-object-pool #:make-incudine-object-pool
                #:incudine-object-pool-expand
                #:alloc-multi-channel-data #:free-multi-channel-data
                #:channel-number #:non-negative-fixnum64
                #:*standard-optimize-settings*
                #:*reduce-warnings* #:reduce-warnings #:sample->fixnum
                #:sample->int
                #:rt-eval #:rt-thread-p)
  (:import-from #:incudine.external #:foreign-alloc-sample #:foreign-zero-sample
                #:foreign-alloc-fft #:foreign-free-fft
                #:make-fft-plan #:make-ifft-plan #:fft-destroy-plan
                #:complex-to-polar #:polar-to-complex
                #:fft-execute #:ifft-execute
                #:apply-window #:apply-scaled-window #:apply-scaled-rectwin
                #:apply-zero-padding
                #:foreign-copy #:foreign-copy-samples #:%copy-from-ring-buffer
                #:%copy-to-ring-output-buffer)
  (:intern #:fft-input-buffer #:fft-output-buffer
           #:ifft-input-buffer #:ifft-output-buffer)
  (:export #:analysis-file-error
           #:analysis #:analysis-p
           #:analysis-input-buffer #:analysis-input-buffer-size
           #:analysis-output-buffer #:analysis-output-buffer-size
           #:analysis-time #:touch-analysis #:discard-analysis
           #:window-size #:window-function #:rectangular-window #:hop-size
           #:abuffer #:make-abuffer #:abuffer-p #:abuffer-data #:abuffer-time
           #:abuffer-realpart #:abuffer-imagpart #:abuffer-size #:abuffer-link
           #:abuffer-nbins #:abuffer-normalized-p
           #:abuffer-polar #:abuffer-complex
           #:touch-abuffer #:discard-abuffer
           #:*fft-default-window-function*
           #:fft #:fft-p #:make-fft #:make-fft-from-pvbuffer
           #:fft-size #:fft-plan #:fft-window #:fft-input
           #:ifft #:ifft-p #:make-ifft #:make-ifft-from-pvbuffer
           #:ifft-size #:ifft-plan #:ifft-window #:ifft-output
           #:+fft-plan-optimal+ #:+fft-plan-best+ #:+fft-plan-fast+
           #:get-fft-plan #:new-fft-plan #:remove-fft-plan #:fft-plan-list
           #:compute-abuffer #:update-linked-object
           #:compute-fft #:compute-ifft
           #:dofft #:dofft-polar #:dofft-complex
           #:stft
           #:pvbuffer #:make-pvbuffer #:make-part-convolve-buffer
           #:pvbuffer-data #:pvbuffer-size #:pvbuffer-frames #:pvbuffer-channels
           #:pvbuffer-fft-size #:pvbuffer-block-size #:pvbuffer-sample-rate
           #:pvbuffer-scale-factor #:pvbuffer-data-type #:pvbuffer-window
           #:pvbuffer-normalized-p #:normalize-pvbuffer #:fill-pvbuffer
           #:copy-pvbuffer-data #:pvbuffer-save #:pvbuffer-load))

(defpackage :incudine.vug
  (:use :cl :incudine.util)
  (:nicknames :vug)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:non-positive-fixnum #:with-gensyms
                #:define-constant #:ensure-symbol #:format-symbol #:make-keyword
                #:maphash-values #:nreversef)
  (:import-from #:cffi #:foreign-type-size #:mem-ref #:mem-aref
                #:make-pointer #:pointer-address #:inc-pointer)
  (:import-from #:incudine.util #:incudine-object #:incudine-object-pool
                #:make-incudine-object-pool #:incudine-object-pool-expand)
  (:import-from #:incudine.analysis #:make-fft #:make-ifft
                #:fft-input #:ifft-output
                #:fft-input-buffer #:fft-output-buffer #:fft-plan
                #:ifft-input-buffer #:ifft-output-buffer #:ifft-plan
                #:abuffer #:make-abuffer #:abuffer-realpart
                #:abuffer-imagpart #:abuffer-size #:abuffer-link #:abuffer-nbins
                #:abuffer-polar #:abuffer-complex #:compute-abuffer
                #:rectangular-window #:dofft #:dofft-polar #:dofft-complex)
  (:export
   #:foreign-plugin-error
   #:vug #:vug-p #:define-vug #:vug-macro #:vug-macro-p #:define-vug-macro
   #:vug-lambda-list #:destroy-vug #:rename-vug #:all-vug-names #:fix-vug
   #:ugen #:ugen-instance #:with-ugen-instance #:with-ugen-instances
   #:define-ugen #:ugen-debug #:compile-vug #:compiled-vug-p
   #:ugen-perf-function #:ugen-reinit-function #:ugen-return-pointer
   #:ugen-control-pointer
   #:define-ugen-control-getter #:define-ugen-control-setter
   #:ugen-lambda-list #:destroy-ugen #:rename-ugen #:fix-ugen #:all-ugen-names
   #:*eval-some-specials-p* #:*specials-to-eval*
   #:with
   #:with-vug-inputs #:vug-input
   #:get-pointer
   #:dsp! #:dsp-debug #:dsp-lambda-list #:metadata #:all-dsp-names
   #:free-dsp-instances #:destroy-dsp
   #:*update-dsp-instances-p*
   #:current-channel #:current-frame #:current-sample #:current-input-sample
   #:make-frame #:frame-ref #:multiple-sample-bind #:samples #:samples-zero
   #:make-i32-array #:make-u32-array #:make-i64-array #:make-u64-array
   #:make-f32-array #:make-f64-array #:make-pointer-array
   #:maybe-make-i32-array #:maybe-make-u32-array #:maybe-i32-ref #:maybe-u32-ref
   #:foreign-length #:foreign-array-type-of
   #:foreach-tick #:foreach-channel #:foreach-frame
   #:counter #:downsamp #:snapshot #:with-control-period
   #:samphold #:interpolate
   #:tick #:external-variable #:init-only #:initialize
   #:with-follow #:without-follow
   #:maybe-expand #:update
   #:vuglet
   #:out #:frame-out #:cout #:node-out
   #:dsp-node
   #:lin->lin #:lin->exp
   #:free-self
   #:clip #:nclip #:wrap #:nwrap #:mirror #:nmirror
   ;; buffer
   #:buffer-read #:buffer-write #:buffer-frame #:buffer-play
   ;; oscillator
   #:phasor #:phasor-loop #:osc #:sine #:pulse #:impulse #:buzz #:gbuzz
   #:oscrq #:oscrs #:oscrc #:oscr
   ;; amplitude
   #:env-follower #:rms #:gain #:balance
   ;; envelope
   #:decay #:decay-2 #:line #:expon #:envelope
   ;; noise
   #:white-noise #:pink-noise #:fractal-noise #:crackle #:rand
   ;; chaos
   #:cusp #:fb-sine #:gbman #:henon #:latoocarfian #:lin-cong #:quad-map
   #:standard-map #:lorenz #:gendy
   ;; delay
   #:buf-delay-s #:buf-delay #:delay-s #:delay #:buf-vdelay #:vtap #:vdelay
   #:ff-comb #:fb-comb #:allpass-s #:allpass #:vallpass
   #:delay-feedback #:delay1
   ;; filter
   #:~ #:pole #:pole* #:zero #:zero* #:two-pole #:two-zero
   #:diff #:integrator
   #:cs-tone #:cs-atone
   #:dcblock #:lag #:lag-ud
   #:maf #:median #:biquad #:reson
   #:resonz #:resonr #:ringz #:ringr
   #:fofilter #:lpf #:hpf #:bpf #:notch #:apf #:peak-eq #:low-shelf #:hi-shelf
   #:butter-lp #:butter-hp #:butter-bp #:butter-br
   #:nlf2
   #:moogladder #:moogff #:lpf18 #:svf
   #:direct-convolve #:part-convolve
   ;; multi-channel
   #:stereo #:pan2 #:fpan2
   ;; midi
   #:*linear-midi-table*
   #:midi-keynum #:midi-highest-keynum #:midi-lowest-keynum
   #:midi-note-on #:midi-note-off #:played-midi-note #:reset-midi-notes
   #:midi-velocity #:midi-cps #:midi-amp #:midi-poly-aftertouch #:midi-cc
   #:midi-program #:midi-global-aftertouch #:midi-pitch-bend
   #:midi-note-on-p #:midi-note-off-p #:midi-note-p #:midi-poly-aftertouch-p
   #:midi-cc-p #:midi-program-p #:midi-global-aftertouch-p #:midi-pitch-bend-p
   #:lin-midi-poly-aftertouch #:exp-midi-poly-aftertouch
   #:lin-midi-cc #:exp-midi-cc
   #:lin-midi-global-aftertouch #:exp-midi-global-aftertouch
   #:lin-midi-pitch-bend #:exp-midi-pitch-bend
   ;; mouse
   #:mouse-x #:mouse-y #:mouse-button
   #:lin-mouse-x #:lin-mouse-y #:exp-mouse-x #:exp-mouse-y
   ;; fft
   #:centroid #:flux #:spectral-rms #:rolloff #:flatness))

(defpackage :incudine.edf
  (:use :cl)
  (:import-from #:alexandria #:positive-fixnum #:non-negative-fixnum
                #:non-negative-real #:define-constant #:with-gensyms)
  (:import-from #:incudine.util #:*standard-optimize-settings*
                #:*rt-edf-heap-size* #:*rt-thread* #:sample #:+sample-zero+
                #:next-power-of-two #:power-of-two-p #:with-spinlock-held
                #:rt-thread-p)
  (:export #:+root-node+ #:*heap* #:*heap-size*
           #:node #:make-node #:heap #:make-heap #:schedule-at #:unschedule-if
           #:at #:aat #:sched-loop #:flush-pending #:heap-empty-p #:heap-count
           #:with-schedule
           #:next-time #:last-time
           #:add-flush-pending-hook #:remove-flush-pending-hook
           #:reduce-heap-pool))

(defpackage :incudine.gen
  (:use :cl)
  (:nicknames :gen)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:with-gensyms #:make-keyword)
  (:import-from #:cffi #:mem-ref #:mem-aref #:foreign-funcall)
  (:import-from :incudine.util #:*standard-optimize-settings*
                #:*reduce-warnings* #:reduce-warnings #:incudine-optimize
                #:*sample-rate* #:+twopi+ #:+half-pi+
                #:+foreign-sample-size+
                #:foreign-pointer #:with-foreign-array
                #:with-samples #:with-samples* #:sample
                #:smp-ref
                #:non-negative-sample
                #:+sample-zero+ #:limited-sample #:sample->fixnum #:sample->int
                #:nrt-msg #:cheb)
  (:export #:analysis #:envelope #:fir #:hilbert
           #:partials #:gbuzz #:chebyshev-1 #:polynomial
           #:defwindow #:symmetric-loop #:symmetric-set #:bartlett #:blackman
           #:dolph-chebyshev #:gaussian #:hamming #:hanning #:kaiser #:sinc
           #:sine-window
           #:rand #:all-random-distributions #:rand-args))

(defpackage :incudine
  (:use :cl :incudine.vug :incudine.util)
  (:import-from #:incudine.util #:incudine-object #:incudine-object-pool
                #:make-incudine-object-pool #:incudine-object-pool-expand
                #:ensure-incudine-object-pool-size #:incudine-optimize
                #:apply-sample-coerce #:with-ensure-symbols)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:non-negative-real #:positive-real
                #:positive-rational
                #:with-gensyms #:define-constant #:ensure-symbol #:format-symbol
                #:make-keyword #:maphash-keys)
  (:import-from #:cffi #:foreign-type-size #:foreign-alloc #:foreign-free
                #:null-pointer #:null-pointer-p
                #:mem-ref #:mem-aref #:inc-pointer #:incf-pointer)
  (:import-from #:incudine.external #:sndfile-to-buffer
                #:foreign-alloc-sample #:foreign-zero-sample
                #:complex-to-polar #:polar-to-complex
                #:foreign-copy #:foreign-copy-samples
                #:rt-audio-init #:rt-audio-start #:rt-audio-stop
                #:rt-get-input #:rt-set-output #:rt-cycle-begin #:rt-cycle-end
                #:rt-set-io-buffers #:rt-set-max-bufsize
                #:rt-condition-wait #:rt-transfer-to-c-thread
                #:rt-time-offset #:rt-set-busy-state #:rt-silent-errors
                #:rt-buffer-size #:rt-sample-rate #:rt-xruns #:rt-get-error-msg)
  (:import-from #:incudine.edf #:at #:aat #:unschedule-if #:with-schedule
                #:flush-pending #:add-flush-pending-hook
                #:remove-flush-pending-hook)
  (:import-from #:incudine.gen #:all-random-distributions #:rand-args)
  (:intern #:network-error)
  (:export
   #:incudine-error #:incudine-simple-error #:incudine-compile-error
   #:incudine-memory-fault-error #:incudine-storage-condition
   #:incudine-network-error #:incudine-node-error #:incudine-missing-arg
   #:incudine-unknown-time-unit #:incudine-undefined-vug
   #:incudine-undefined-ugen #:incudine-undefined-dsp
   #:init #:enable-sharp-t-syntax #:enable-sharp-square-bracket-syntax
   #:deprecated-symbol-names
   #:dsp-seq
   #:buffer #:make-buffer #:buffer-p
   #:buffer-mask #:buffer-data #:fill-buffer #:smp-ref #:buffer-value
   #:buffer-size #:buffer-frames #:buffer-channels #:buffer-sample-rate
   #:buffer-value #:buffer-mask #:buffer-lobits #:buffer-lomask #:buffer-lodiv
   #:buffer-file #:buffer-load #:buffer-save #:map-buffer #:map-into-buffer
   #:buffer->array #:buffer->list #:with-buffer #:with-buffers
   #:copy-buffer #:resize-buffer
   #:scale-buffer #:rescale-buffer #:normalize-buffer #:sort-buffer
   #:*default-tuning*
   #:tuning #:make-tuning #:tuning-data #:set-tuning #:set-tuning-reference
   #:tuning-notes-from-data #:tuning-cps #:tuning-cents #:tuning-ratios
   #:tuning-description #:tuning-keynum-base #:tuning-freq-base
   #:tuning-degree-index #:minimize-tuning-ratios #:tuning-save #:copy-tuning
   #:load-sclfile
   #:pch->cps #:cps->pch #:pch->keynum #:keynum->pch
   #:free #:free-p
   #:with-cleanup #:without-cleanup
   #:incudine-finalize #:incudine-cancel-finalization
   #:dynamic-incudine-finalizer-p
   #:play #:stop #:quantize
   #:circular-shift
   #:bus #:audio-in #:audio-out
   #:peak-info #:print-peak-info #:reset-peak-meters
   #:set-number-of-channels #:set-max-buffer-size
   #:*rt-thread-start-hook* #:*rt-thread-exit-hook*
   #:rt-start #:rt-stop #:rt-status #:rt-cpu #:rt-buffer-size #:rt-sample-rate
   #:rt-xruns #:rt-loop-callback #:set-rt-block-size #:block-size
   #:rt-time-offset #:rt-silent-errors #:recover-suspended-audio-cycles-p
   #:at #:aat #:unschedule-if #:with-schedule #:flush-pending #:flush-all-fifos
   #:tempo #:*tempo* #:make-tempo #:tempo-p #:bpm #:bps #:spb #:now #:tempo-sync
   #:with-local-time
   #:tempo-envelope #:make-tempo-envelope #:tempo-envelope-p #:set-tempo-envelope
   #:copy-tempo-envelope
   #:tempo-breakpoints
   #:beats->seconds #:seconds->beats #:bps-at #:bpm-at #:spb-at
   #:timestamp
   #:rt-funcall #:fast-rt-funcall #:nrt-funcall #:fast-nrt-funcall
   #:*sine-table* #:*cosine-table*
   ;; node
   #:node #:node-p #:group #:group-p #:make-group #:node-name #:node-id
   #:node-start-time #:node-uptime #:null-node-p #:live-nodes
   #:next-node-id #:*root-node* #:node-release-phase-p #:node-gain
   #:node-enable-gain-p #:*node-enable-gain-p* #:node-fade-time
   #:node-fade-curve #:node-fade-in #:node-fade-out #:node-segment
   #:pause #:unpause #:pause-p #:move #:before-p #:after-p #:head-p #:tail-p
   #:done-p #:node-free-all #:stop-hook #:free-hook #:dograph #:dogroup #:dump
   #:set-control #:set-controls #:control-value #:control-pointer
   #:control-getter #:control-setter #:control-list #:control-names
   #:reinit
   ;; responder
   #:receiver #:receiver-stream #:remove-receiver #:remove-all-receivers
   #:all-receivers
   #:recv-start #:recv-stop #:recv-status #:recv-functions
   #:responder #:make-responder #:make-osc-responder #:add-responder
   #:remove-responder #:remove-all-responders #:all-responders
   ;; midi
   #:midiout #:midiout-sysex #:midiin-sysex-octets
   #:midi-tuning-sysex #:set-tuning-from-midi
   ;; serial
   #:open-serial-port #:serial-stream-p #:serial-flush
   ;; envelope
   #:envelope #:envelope-p #:make-envelope #:edit-envelope #:envelope-points
   #:envelope-level #:envelope-time #:envelope-curve
   #:envelope-base->curves #:set-envelope-base #:envelope-data #:envelope-duration
   #:envelope-loop-node #:envelope-release-node
   #:envelope-restart-level #:envelope-at
   #:breakpoints->env #:freq-breakpoints->env
   #:copy-envelope #:scale-envelope #:normalize-envelope #:rescale-envelope
   #:make-linen #:make-perc #:make-cutoff #:make-asr #:make-adsr #:make-dadsr
   ;; nrt
   #:with-nrt #:bounce-to-disk #:bounce-to-buffer
   #:*score-readtable*
   #:regofile->sexp #:regofile->function #:regofile->lispfile
   #:regofile->list #:regolist->file #:regostring->list #:regostring->function
   #:defscore-statement #:ignore-score-statements #:delete-score-statement))

(defpackage :incudine.voicer
  (:use :cl)
  (:nicknames :voicer)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:with-gensyms #:define-constant
                #:ensure-symbol)
  (:import-from #:incudine.util #:*standard-optimize-settings* #:reduce-warnings
                #:incudine-optimize
                #:cons-pool #:make-cons-pool #:expand-cons-pool #:cons-pool-size
                #:cons-pool-pop-cons #:cons-pool-push-cons
                #:cons-pool-pop-list #:cons-pool-push-list
                #:nrt-global-pool-push-cons #:nrt-global-pool-pop-cons
                #:nrt-global-pool-push-list #:nrt-global-pool-pop-list
                #:make-tlist #:tlist-empty-p #:tlist-add-left
                #:tlist-add-right #:tlist-remove-left
                #:spinlock #:make-spinlock #:with-spinlock-held
                #:msg)
  (:export #:voicer #:create #:update #:empty-p #:full-p
           #:polyphony #:steal-voice-mode #:trigger #:release
           #:control-value #:set-controls #:control-list #:control-names
           #:define-map #:remove-map #:remove-all-maps #:panic
           #:midi-event #:midi-bind
           #:scale-midi-amp #:fill-freq-table #:fill-amp-table))

(defpackage :incudine.vug-foreign
  (:use :cl :incudine.util :incudine.vug)
  (:nicknames :vug-foreign)
  (:import-from #:alexandria #:define-constant #:make-keyword
                #:format-symbol #:ensure-symbol #:with-gensyms
                #:non-negative-fixnum)
  (:import-from #:incudine #:block-size #:incudine-missing-arg)
  (:export
   #:+input-port+ #:+output-port+ #:+control-port+ #:+audio-port+
   #:+event-port+ #:+midi-port+
   #:port #:make-port #:port-loop #:plugin #:make-plugin
   #:plugin-instance #:plugin-instance-pointer
   #:plugin-port-pointer
   #:update-io-number #:input-port-p #:output-port-p #:control-port-p
   #:audio-port-p #:event-port-p #:midi-port-p
   #:doc-string #:with-vug-plugin))

(defpackage :incudine.osc
  (:use :cl
        #+(and sbcl (or x86 x86-64))
        #:sb-c
        #+(and sbcl (or x86 x86-64))
        #:sb-assem)
  (:import-from #:alexandria #:define-constant #:positive-fixnum
                #:non-negative-fixnum #:with-gensyms)
  (:import-from #:swap-bytes #:htonl #:htonq #:ntohl #:ntohq)
  (:import-from #:incudine #:incudine-optimize #:network-error)
  (:shadow #:open #:close #:stream #:input-stream-p #:output-stream-p)
  (:export
   #:+default-msg-flags+
   #:*listen-backlog* #:*buffer-size* #:*max-values* #:*before-close-hook*
   #:stream #:input-stream #:input-stream-p #:output-stream #:output-stream-p
   #:with-stream #:open #:open-p #:close #:block-p #:without-block #:broadcast
   #:connect #:connected-p #:reject #:close-connections #:last-recv-fd
   #:socket-send
   #:connections #:connections-fd #:host #:port #:protocol #:protocolp
   #:socket-fd #:direction #:buffer-pointer #:buffer-size #:max-values #:latency
   #:message-pointer #:message-length #:bundle-length
   #:message-encoding #:message-time
   #:receive #:send #:send-bundle
   #:slip-encode #:slip-decode
   #:bundle #:simple-bundle #:message #:start-message
   #:copy-packet
   #:value #:value-pointer
   #:midi #:address-pattern #:check-pattern #:index-values #:with-values
   #:required-values #:buffer-to-octets #:octets-to-buffer
   #:fix-size))

(defpackage :incudine.net
  (:use :cl)
  (:nicknames #:net)
  (:shadowing-import-from #:incudine.osc #:close)
  (:shadow #:open #:read #:write #:stream #:input-stream-p #:output-stream-p)
  (:import-from :alexandria #:positive-fixnum #:non-negative-fixnum)
  (:import-from #:incudine #:incudine-optimize)
  (:import-from #:incudine.osc #:+default-msg-flags+ #:*listen-backlog*
                #:*before-close-hook*
                #:with-stream #:open-p #:block-p #:without-block #:broadcast
                #:connect #:connected-p #:reject #:close-connections
                #:connections #:connections-fd #:last-recv-fd #:host #:port
                #:protocol #:protocolp #:socket-fd #:socket-send #:direction
                #:buffer-pointer #:buffer-size #:message-pointer #:message-length
                #:message-encoding #:slip-encode #:slip-decode
                #:buffer-to-octets #:octets-to-buffer)
  (:export
   #:+default-msg-flags+
   #:*listen-backlog* #:*buffer-size* #:*before-close-hook*
   #:stream #:input-stream #:input-stream-p
   #:output-stream #:output-stream-p #:with-stream #:open #:open-p #:close
   #:read #:write #:foreign-read #:foreign-write
   #:block-p #:without-block #:broadcast
   #:connect #:connected-p #:reject #:close-connections
   #:connections #:connections-fd #:last-recv-fd #:host #:port #:protocol
   #:protocolp #:socket-fd #:direction #:buffer-pointer #:buffer-size
   #:message-pointer #:message-length #:message-encoding #:slip-encode
   #:slip-decode #:socket-send #:buffer-to-octets #:octets-to-buffer
   #:buffer-to-string #:string-to-buffer))

(defpackage :incudine.soundfile
  (:use :cl)
  (:nicknames :soundfile)
  (:shadow #:stream #:input-stream-p #:output-stream-p #:open #:close
           #:read #:write #:position #:concatenate #:merge)
  (:import-from #:alexandria #:positive-fixnum #:non-negative-fixnum)
  (:import-from #:incudine #:incudine-simple-error #:incudine-missing-arg
                #:incudine-finalize #:incudine-cancel-finalization)
  (:import-from #:incudine.util #:*sndfile-buffer-size* #:*sample-rate*
                #:*default-header-type* #:*default-data-format*
                #:*standard-optimize-settings* #:incudine-optimize
                #:*reduce-warnings* #:reduce-warnings
                #:non-negative-fixnum64 #:next-power-of-two #:db->linear)
  (:export
   #:soundfile-error
   #:stream #:input-stream #:input-stream-p #:output-stream #:output-stream-p
   #:open #:open-p #:close #:with-open-soundfile #:update-header #:eof-p
   #:read-header #:read-next #:read-into-buffer #:read #:write
   #:foreign-read #:foreign-write
   #:position #:offset #:buffer-data #:buffer-size #:buffer-index #:buffer-value
   #:current-frame #:path #:sample-rate #:frames #:channels #:duration #:metadata
   #:data-location #:header-type #:data-format #:maxamp
   #:convert #:concatenate #:merge))

(defpackage :incudine.midifile
  (:use :cl)
  (:nicknames :midifile)
  (:shadow #:stream #:input-stream-p #:output-stream-p #:open #:close #:format)
  (:import-from #:alexandria #:non-negative-fixnum #:positive-fixnum
                #:non-negative-real #:positive-real)
  (:import-from #:incudine #:incudine-simple-error #:incudine-missing-arg
                #:incudine-finalize #:incudine-cancel-finalization)
  (:export #:data #:midifile-error #:midifile-parse-error
           #:invalid-running-status #:invalid-variable-length-quantity
           #:invalid-track-chunk-length
           #:stream #:input-stream #:input-stream-p
           #:output-stream #:output-stream-p
           #:open #:open-p #:close #:with-open-midifile
           #:release-cached-buffers
           #:read-header #:write-header
           #:read-event #:write-event #:write-short-event #:write-tempo-track
           #:next-track #:current-track
           #:path #:format #:number-of-tracks #:ppqn #:smpte #:tempo
           #:event-delta-time #:event-time #:event-beats #:event-seconds
           #:message #:tempo-message #:string-message #:end-of-track
           #:message-buffer #:message-length
           #:message-status #:message-data1 #:message-data2))

(defpackage :incudine.scratch
  (:use :cl :incudine :incudine.vug :incudine.util :incudine.analysis)
  (:import-from #:alexandria #:positive-fixnum #:negative-fixnum
                #:non-negative-fixnum #:with-gensyms #:define-constant)
  (:import-from #:incudine.gen #:all-random-distributions #:rand-args)
  (:import-from #:incudine.midifile #:with-open-midifile)
  (:import-from #:incudine.soundfile #:with-open-soundfile)
  (:nicknames :scratch))
