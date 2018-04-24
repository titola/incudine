;;; Copyright (c) 2013-2016 Tito Latini
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

(in-package :incudine.vug)

(define-vug %phasor (rate init end)
  (with-samples ((phase init))
    (prog1 phase
      (incf phase rate)
      (cond ((>= phase end) (decf phase end))
            ((minusp phase) (incf phase end))))))

(define-vug phasor (freq init)
  (with-samples ((rate (* freq *sample-duration*)))
    (%phasor rate init 1)))

(define-vug phasor-loop (rate start-pos loopstart loopend)
  (with-samples ((pos start-pos)
                 (old-pos pos)
                 (loopsize (- loopend loopstart))
                 (start-plus-end (+ loopend loopstart)))
    (prog1 pos
      (incf pos rate)
      (cond ((> pos loopend) (decf pos loopsize))
            ((< pos 0) (setf pos +sample-zero+))
            ((and (< pos loopstart) (>= old-pos loopstart))
             (setf pos (- start-plus-end pos))))
      (setf old-pos pos))))

(defmacro %with-osc-interp ((buffer phs frac) &body body)
  (with-gensyms (lodiv lomask)
    `(with ((,frac +sample-zero+)
            (,lodiv (buffer-lodiv ,buffer))
            (,lomask (buffer-lomask ,buffer)))
       (declare (type sample ,frac ,lodiv) (type non-negative-fixnum ,lomask))
       (setf ,frac (* ,lodiv (logand ,phs ,lomask)))
       ,@body)))

(defmacro %osc-linear-interp (buffer data phs index)
  (with-gensyms (frac mask)
    `(%with-osc-interp (,buffer ,phs ,frac)
       (with ((,mask (buffer-mask ,buffer)))
         (declare (type non-negative-fixnum ,mask))
         (linear-interp ,frac (smp-ref ,data ,index)
                        (smp-ref ,data (logand (the fixnum (1+ ,index))
                                               ,mask)))))))

(defmacro %osc-cubic-interp (buffer data phs index)
  (with-gensyms (frac index0 index2 index3 mask)
    `(%with-osc-interp (,buffer ,phs ,frac)
       (with ((,mask (buffer-mask ,buffer)))
         (declare (type non-negative-fixnum ,mask))
         (let ((,index0 (logand (the fixnum (- ,index 1)) ,mask))
               (,index2 (logand (the fixnum (+ ,index 1)) ,mask))
               (,index3 (logand (the fixnum (+ ,index 2)) ,mask)))
           (cubic-interp ,frac (smp-ref ,data ,index0)
                         (smp-ref ,data ,index)
                         (smp-ref ,data ,index2)
                         (smp-ref ,data ,index3)))))))

;;; Select the interpolation for a table lookup.
;;; The size of the table is a power of two.
(defmacro %osc-select-interpolation (interpolation buffer data phs index)
  (case interpolation
    (:linear `(%osc-linear-interp ,buffer ,data ,phs ,index))
    (:cubic `(%osc-cubic-interp ,buffer ,data ,phs ,index))
    (otherwise `(smp-ref ,data ,index))))

(defmacro %osc-phase-modulation (phs phase phase-modulation-p)
  (cond (phase-modulation-p
         (with-gensyms (phase-old %phase)
           `(with-samples ((,%phase (vug-input
                                     ,(coerce-number phase 'sample)))
                           (,phase-old +sample-zero+))
              (unless (= ,%phase ,phase-old)
                (setf ,phs
                      (logand
                        (the fixnum
                          (+ ,phs (sample->fixnum
                                    (* (- ,%phase ,phase-old) +rad2inc+))))
                        +phase-mask+))
                (setf ,phase-old ,%phase)))))
        ((and (numberp phase) (zerop phase)) '(values))
        (t `(initialize
              (setf ,phs
                    (logand (the fixnum
                              (+ ,phs (sample->fixnum (* ,phase +rad2inc+))))
                            +phase-mask+))))))

(defmacro %osc (buffer freq amp phase phase-modulation-p interpolation)
  (with-gensyms (freq-inc phs minus-lobits data index)
    `(with ((,freq-inc (sample->fixnum (* ,freq *cps2inc*)))
            (,phs 0)
            (,minus-lobits (- (buffer-lobits ,buffer)))
            (,data (buffer-data ,buffer)))
       (declare (type fixnum ,phs ,freq-inc)
                (type (integer #.(- +max-lobits+) 0) ,minus-lobits)
                (type foreign-pointer ,data))
       (%osc-phase-modulation ,phs ,phase ,phase-modulation-p)
       (let ((,index (the fixnum (ash ,phs ,minus-lobits))))
         (prog1 (* ,amp (%osc-select-interpolation ,interpolation ,buffer
                                                   ,data ,phs ,index))
           (setf ,phs (logand (the fixnum (+ ,phs ,freq-inc))
                              +phase-mask+)))))))

(defmacro impulse-oscillator (freq amp phase phase-modulation-p)
  (with-gensyms (phase-old phs freq-inc)
    `(with-samples ((,phs (sample 1))
                    (,freq-inc (* ,freq *sample-duration*)))
       (initialize (unless (zerop ,phase)
                     (decf ,phs ,phase)))
       (maybe-expand ,amp)
       (prog1 (cond ((>= ,phs 1.0)
                     (decf ,phs 1.0)
                     ,amp)
                    (t +sample-zero+))
         ,@(when phase-modulation-p
             `((with-samples (,phase-old)
                 (initialize (setf ,phase-old ,phase))
                 (unless (= ,phase ,phase-old)
                   (incf ,phs (- ,phase ,phase-old))
                   (setf ,phase-old ,phase)))))
         (incf ,phs ,freq-inc)))))

(defmacro %buzz-numerator (buffer data phs two-nh-plus-one minus-lobits mask
                           interpolation)
  (with-gensyms (phs-num index)
    (if (member interpolation '(:linear :cubic))
        `(let* ((,phs-num (* ,phs ,two-nh-plus-one))
                (,index (logand (the fixnum (ash ,phs-num ,minus-lobits))
                                ,mask)))
           (declare (type fixnum ,phs-num ,index))
           (%osc-select-interpolation ,interpolation ,buffer ,data ,phs-num
                                      ,index))
        ;; The following computation of the index is better for a lookup table
        ;; without interpolation.
        `(let ((,index (logand (the fixnum
                                 (* (the fixnum (ash ,phs ,minus-lobits))
                                    (the fixnum ,two-nh-plus-one)))
                               ,mask)))
           (smp-ref ,data ,index)))))

(defmacro %buzz (buffer freq amp num-harm phase phase-modulation-p
                 harm-change-lag interpolation)
  (with-gensyms (freq-inc phs minus-lobits mask data nh-lag old-num-harm
                 nh0 nh1 res0 res1 num0 num1 denom old-two-nh-plus-one
                 two-nh-plus-one amp0 mult0 mult1 index count cross inc-interp)
    `(with ((,freq-inc (sample->fixnum
                         ;; The angle is theta/2
                         (* 0.5 ,freq *cps2inc*)))
            (,phs 0)
            (,minus-lobits (- (buffer-lobits ,buffer)))
            (,mask (buffer-mask ,buffer))
            ;; The buffer contains a sine wave
            (,data (buffer-data ,buffer))
            (,count 0)
            (,cross 1.0d0)
            (,nh-lag (sample->fixnum (* ,harm-change-lag *sample-rate*)))
            (,inc-interp (/ (sample 1) ,nh-lag))
            (,amp0 (* ,amp 0.5))
            (,old-num-harm 0)
            (,nh0 1)
            (,nh1 1)
            (,old-two-nh-plus-one 3)
            (,two-nh-plus-one 3)
            (,mult0 (/ ,amp0 ,nh0))
            (,mult1 (/ ,amp0 ,nh1))
            (,res0 +sample-zero+)
            (,res1 +sample-zero+)
            (,num0 +sample-zero+)
            (,num1 +sample-zero+)
            (,denom +sample-zero+))
       (declare (type fixnum ,phs ,mask ,freq-inc ,nh0 ,nh1 ,nh-lag
                      ,old-num-harm ,count ,old-two-nh-plus-one
                      ,two-nh-plus-one)
                (type (integer #.(- +max-lobits+) 0) ,minus-lobits)
                (type sample ,res0 ,res1 ,num0 ,num1 ,denom ,amp0
                      ,mult0 ,mult1 ,cross ,inc-interp)
                (type foreign-pointer ,data))
       (initialize
         (setf ,nh1 (max 1 (abs ,num-harm)))
         (setf ,two-nh-plus-one (1+ (* 2 ,nh1))))
       ;; Expand here and not in the next UNLESS form if the
       ;; amplitude is modulated
       (maybe-expand ,mult0 ,mult1)
       (%osc-phase-modulation ,phs ,phase ,phase-modulation-p)
       (let ((,index (the fixnum (ash ,phs ,minus-lobits))))
         (unless (or (= ,old-num-harm ,num-harm) (plusp ,count))
           (setf ,old-num-harm ,num-harm
                 ,count ,nh-lag
                 ,cross +sample-zero+
                 ,nh0 ,nh1
                 ,nh1 (max 1 (abs ,num-harm))
                 ,old-two-nh-plus-one ,two-nh-plus-one
                 ,two-nh-plus-one (1+ (* 2 ,nh1))
                 ,mult0 (/ ,amp0 ,nh0)
                 ,mult1 (/ ,amp0 ,nh1)))
         (setf ,denom (%osc-select-interpolation ,interpolation ,buffer ,data
                                                 ,phs ,index))
         (prog1
             (cond ((or (> ,denom 1.d-5) (< ,denom -1.d-5))
                    (setf ,num1
                          (%buzz-numerator
                            ,buffer ,data ,phs ,two-nh-plus-one ,minus-lobits
                            ,mask ,interpolation))
                    (setf ,res1 (* ,mult1 (- (/ ,num1 ,denom) 1.0)))
                    (cond ((plusp ,count)
                           (decf ,count)
                           (setf ,num0
                                 (%buzz-numerator
                                   ,buffer ,data ,phs ,old-two-nh-plus-one
                                   ,minus-lobits ,mask ,interpolation))
                           (setf ,res0 (* ,mult0 (- (/ ,num0 ,denom) 1.0)))
                           (prog1 (linear-interp ,cross ,res0 ,res1)
                             (incf ,cross ,inc-interp)))
                          (t ,res1)))
                   (t (when (plusp ,count)
                        (decf ,count)
                        (incf ,cross ,inc-interp))
                      ,amp))
           (setf ,phs (logand (the fixnum (+ ,phs ,freq-inc))
                              +phase-mask+)))))))

(define-vug buzz-hq (freq amp (num-harm fixnum) phase harm-change-lag)
  (with ((count 0)
         (cross 1.0d0)
         (nh-lag (sample->fixnum (* harm-change-lag *sample-rate*)))
         (inc-interp (/ (sample 1) nh-lag))
         (old-num-harm 0)
         (nh0 1)
         (nh1 1)
         (old-two-nh-plus-one 3)
         (two-nh-plus-one 3)
         (res0 +sample-zero+)
         (res1 +sample-zero+)
         (num0 +sample-zero+)
         (num1 +sample-zero+)
         (denom +sample-zero+)
         (amp0 (* amp 0.5))
         (mult0 (/ amp0 nh0))
         (mult1 (/ amp0 nh1))
         (fdiv2 (* 0.5 freq))
         (angle +sample-zero+))
    (declare (type sample cross res0 res1 num0 num1 denom amp0 mult0 mult1
                   fdiv2 angle inc-interp)
             (type fixnum count nh0 nh1 nh-lag old-num-harm
                   old-two-nh-plus-one two-nh-plus-one))
    (initialize
      (setf nh1 (max 1 (abs num-harm)))
      (setf two-nh-plus-one (1+ (* 2 nh1))))
    ;; Expand here and not in the next UNLESS form if the amplitude
    ;; is modulated
    (maybe-expand mult0 mult1)
    (unless (or (= old-num-harm num-harm) (plusp count))
        (setf old-num-harm num-harm
              count nh-lag
              cross +sample-zero+
              nh0 nh1
              nh1 (max 1 (abs num-harm))
              old-two-nh-plus-one two-nh-plus-one
              two-nh-plus-one (1+ (* 2 nh1))
              mult0 (/ amp0 nh0)
              mult1 (/ amp0 nh1)))
    (setf angle (+ (* +twopi+ (phasor fdiv2 0)) phase))
    (setf denom (sin angle))
    (cond ((or (> denom 1.d-5) (< denom -1.d-5))
           (setf num1 (sin (* angle two-nh-plus-one)))
           (setf res1 (* mult1 (- (/ num1 denom) 1.0)))
           (cond ((plusp count)
                  (decf count)
                  (setf num0 (sin (* angle old-two-nh-plus-one)))
                  (setf res0 (* mult0 (- (/ num0 denom) 1.0)))
                  (prog1 (linear-interp cross res0 res1)
                    (incf cross inc-interp)))
                 (t res1)))
          (t (when (plusp count)
               (decf count)
               (incf cross inc-interp))
             amp))))

(defmacro gbuzz-init-index (phs minus-lobits mask &optional const)
  `(logand (the fixnum
             ,(if const
                  `(* (the fixnum (ash ,phs ,minus-lobits))
                      (the fixnum ,const))
                  `(ash ,phs ,minus-lobits)))
           ,mask))

(defmacro %gbuzz-numerator (buffer data phs c0 c1 c2 c3 mult1 mult2 mult3
                            minus-lobits mask interpolation)
  (with-gensyms (phs-num0 phs-num1 phs-num2 phs-num3 index0 index1 index2
                 index3)
    (if (member interpolation '(:linear :cubic))
        `(let* ((,phs-num0 (* ,phs ,c0))
                (,phs-num1 (* ,phs ,c1))
                (,phs-num2 (* ,phs ,c2))
                (,phs-num3 (* ,phs ,c3))
                (,index0 (gbuzz-init-index ,phs-num0 ,minus-lobits ,mask))
                (,index1 (gbuzz-init-index ,phs-num1 ,minus-lobits ,mask))
                (,index2 (gbuzz-init-index ,phs-num2 ,minus-lobits ,mask))
                (,index3 (gbuzz-init-index ,phs-num3 ,minus-lobits ,mask)))
           (declare (type fixnum ,phs-num0 ,phs-num1 ,phs-num2 ,phs-num3
                          ,index0 ,index1 ,index2 ,index3))
           (+ (- (%osc-select-interpolation ,interpolation ,buffer ,data
                                            ,phs-num0 ,index0)
                 (* ,mult1
                    (%osc-select-interpolation ,interpolation ,buffer ,data
                                               ,phs-num1 ,index1))
                 (* ,mult2
                    (%osc-select-interpolation ,interpolation ,buffer ,data
                                               ,phs-num2 ,index2)))
              (* ,mult3 (%osc-select-interpolation
                          ,interpolation ,buffer ,data ,phs-num3 ,index3))))
        `(let ((,index0 (gbuzz-init-index ,phs ,minus-lobits ,mask ,c0))
               (,index1 (gbuzz-init-index ,phs ,minus-lobits ,mask ,c1))
               (,index2 (gbuzz-init-index ,phs ,minus-lobits ,mask ,c2))
               (,index3 (gbuzz-init-index ,phs ,minus-lobits ,mask ,c3)))
           (declare (type fixnum ,index0 ,index1 ,index2 ,index3))
           (+ (- (smp-ref ,data ,index0)
                 (* ,mult1 (smp-ref ,data ,index1))
                 (* ,mult2 (smp-ref ,data ,index2)))
              (* ,mult3 (smp-ref ,data ,index3)))))))

(defmacro gbuzz-update-multipliers (c2-mult-var c3-mult-var rsum-var nh mul
                                    abs-mul)
  `(progn
     ;; No optimization with "(expt ,mul ,nh)"
     (setf ,c2-mult-var (expt (the non-negative-sample ,abs-mul) ,nh))
     (when (and (minusp ,mul) (plusp (logand ,nh 1)))
       (setf ,c2-mult-var (- ,c2-mult-var)))
     (setf ,c3-mult-var (* ,c2-mult-var ,mul))     ; mul^(nh + 1)
     (setf ,rsum-var (if (< 0.999 ,abs-mul 1.001)
                         (/ (sample 1) ,nh)
                         (/ (- 1.0 ,abs-mul) (- 1.0 (abs ,c2-mult-var)))))))

(defmacro %gbuzz (buffer freq amp num-harm lowest-harm mul phase
                  phase-modulation-p harm-change-lag interpolation)
  (with-gensyms (freq-inc phs minus-lobits mask data nh-lag old-num-harm
                 c0 c1 c2 c3 c1-mult c2-mult c3-mult old-lowest-harm old-c0
                 old-c1 old-c2 old-c3 old-c1-mult old-c2-mult old-c3-mult
                 old-mul abs-mul two-mul squared-mul-plus-one old-two-mul
                 old-squared-mul-plus-one rsum0 rsum1 nh0 nh1 res0 res1 num0
                 num1 denom0 denom1 denom-osc index count cross inc-interp out)
    `(with ((,freq-inc (sample->fixnum (* ,freq *cps2inc*)))
            (,phs 0)
            (,minus-lobits (- (buffer-lobits ,buffer)))
            (,mask (buffer-mask ,buffer))
            ;; The buffer contains a cosine wave
            (,data (buffer-data ,buffer))
            (,count 0)
            (,nh-lag (sample->fixnum (* ,harm-change-lag *sample-rate*)))
            (,inc-interp (/ (sample 1) ,nh-lag))
            (,old-num-harm 999999)
            (,nh0 1)
            (,nh1 1)
            (,c0 0)
            (,c1 0)
            (,c2 0)
            (,c3 0)
            (,old-lowest-harm 0)
            (,old-c0 0)
            (,old-c1 0)
            (,old-c2 0)
            (,old-c3 0))
       (declare (type fixnum ,phs ,mask ,freq-inc ,nh-lag ,count
                      ,old-lowest-harm ,c0 ,c1 ,c2 ,c3 ,old-c0 ,old-c1
                      ,old-c2 ,old-c3)
                (type (integer #.(- +max-lobits+) 0) ,minus-lobits)
                (type (integer 0 1000000) ,nh0 ,nh1 ,old-num-harm)
                (type sample ,inc-interp) (type foreign-pointer ,data))
       (with-samples (,c1-mult ,c2-mult ,c3-mult ,old-c1-mult ,old-c2-mult
                      ,old-c3-mult ,old-mul ,abs-mul ,old-two-mul ,two-mul
                      ,squared-mul-plus-one ,old-squared-mul-plus-one
                      ,rsum0 ,rsum1 ,res0 ,res1 ,num0 ,num1 ,denom0 ,denom1
                      ,denom-osc ,cross ,out)
         (initialize (setf ,nh1 (max 1 (abs ,num-harm))
                           ,c0 ,lowest-harm             ; lh
                           ,c1 (1- ,c0)                 ; lh - 1
                           ,c2 (+ ,c0 ,nh1)             ; lh + nh
                           ,c3 (1- ,c2)                 ; lh + nh - 1
                           ,abs-mul (abs ,mul)
                           ,two-mul (+ ,mul ,mul)
                           ,squared-mul-plus-one (+ (* ,mul ,mul) 1.0)
                           ,c1-mult ,mul)
                     (gbuzz-update-multipliers ,c2-mult ,c3-mult ,rsum1 ,nh1
                                               ,mul ,abs-mul))
         (%osc-phase-modulation ,phs ,phase ,phase-modulation-p)
         (let ((,index (the fixnum (ash ,phs ,minus-lobits))))
           ;; Expand here if MUL is modulated
           (maybe-expand ,mul)
           (unless (or (and (= ,old-num-harm ,num-harm)
                            (= ,old-lowest-harm ,lowest-harm))
                       (plusp ,count))
             (setf ,old-num-harm ,num-harm
                   ,old-lowest-harm ,lowest-harm
                   ,count ,nh-lag
                   ,cross +sample-zero+
                   ,nh0 ,nh1
                   ,nh1 (max 1 (abs ,num-harm))
                   ,old-c0 ,c0
                   ,old-c1 ,c1
                   ,old-c2 ,c2
                   ,old-c3 ,c3
                   ,old-c1-mult ,c1-mult
                   ,old-c2-mult ,c2-mult
                   ,old-c3-mult ,c3-mult
                   ,c0 ,lowest-harm
                   ,c1 (1- ,c0)
                   ,c2 (+ ,c0 ,nh1)
                   ,c3 (1- ,c2)
                   ,old-two-mul ,two-mul
                   ,old-squared-mul-plus-one ,squared-mul-plus-one
                   ,rsum0 ,rsum1)
             (when (= ,old-mul ,mul)
               (gbuzz-update-multipliers ,c2-mult ,c3-mult ,rsum1 ,nh1 ,mul
                                         ,abs-mul)))
           (unless (= ,old-mul ,mul)
             (setf ,old-mul ,mul
                   ,abs-mul (abs ,mul)
                   ,two-mul (+ ,mul ,mul)
                   ,squared-mul-plus-one (+ (* ,mul ,mul) 1.0)
                   ,c1-mult ,mul)
             (gbuzz-update-multipliers ,c2-mult ,c3-mult ,rsum1 ,nh1 ,mul
                                       ,abs-mul))
           (setf ,denom-osc (%osc-select-interpolation
                              ,interpolation ,buffer ,data ,phs ,index))
           (setf ,denom1 (- ,squared-mul-plus-one (* ,two-mul ,denom-osc)))
           (setf ,out
                 (cond ((or (> ,denom1 1.d-5) (< ,denom1 -1.d-5))
                        (setf ,num1
                              (%gbuzz-numerator
                                ,buffer ,data ,phs ,c0 ,c1 ,c2 ,c3 ,c1-mult
                                ,c2-mult ,c3-mult ,minus-lobits ,mask
                                ,interpolation))
                        (setf ,res1 (* ,rsum1 (/ ,num1 ,denom1)))
                        (* ,amp
                           (cond ((plusp ,count)
                                  (decf ,count)
                                  (setf ,num0
                                        (%gbuzz-numerator
                                          ,buffer ,data ,phs ,old-c0 ,old-c1
                                          ,old-c2 ,old-c3 ,old-c1-mult
                                          ,old-c2-mult ,old-c3-mult
                                          ,minus-lobits ,mask ,interpolation))
                                  (setf ,denom0 (- ,old-squared-mul-plus-one
                                                   (* ,old-two-mul
                                                      ,denom-osc)))
                                  (setf ,res0 (if (or (> ,denom0 1.d-5)
                                                      (< ,denom0 -1.d-5))
                                                  (* ,rsum0 (/ ,num0 ,denom0))
                                                  (sample 1)))
                                  (prog1 (linear-interp ,cross ,res0 ,res1)
                                    (incf ,cross ,inc-interp)))
                                 (t ,res1))))
                       (t (when (plusp ,count)
                            (decf ,count)
                            (incf ,cross ,inc-interp))
                          (if (minusp ,out) (- ,amp) ,amp))))
           (setf ,phs (logand (the fixnum (+ ,phs ,freq-inc)) +phase-mask+))
           ,out)))))

(define-vug gbuzz-hq (freq amp (num-harm (integer 0 1000000))
                      (lowest-harm fixnum) mul phase harm-change-lag)
  (with ((count 0)
         (nh-lag (sample->fixnum (* harm-change-lag *sample-rate*)))
         (inc-interp (/ (sample 1) nh-lag))
         (old-num-harm 999999)
         (nh0 1)
         (nh1 1)
         (c0 0)
         (c1 0)
         (c2 0)
         (c3 0)
         (old-lowest-harm 0)
         (old-c0 0)
         (old-c1 0)
         (old-c2 0)
         (old-c3 0))
    (declare (type fixnum nh-lag count old-lowest-harm c0 c1 c2 c3
                   old-c0 old-c1 old-c2 old-c3)
             (type (integer 0 1000000) nh0 nh1 old-num-harm)
             (type sample inc-interp))
    (with-samples (c1-mult c2-mult c3-mult old-c1-mult old-c2-mult old-c3-mult
                   old-mul abs-mul old-two-mul two-mul squared-mul-plus-one
                   old-squared-mul-plus-one rsum0 rsum1 res0 res1 num0 num1
                   denom0 denom1 denom-osc cross angle out)
      (initialize (setf nh1 (max 1 (abs num-harm))
                        c0 lowest-harm
                        c1 (1- c0)
                        c2 (+ c0 nh1)
                        c3 (1- c2)
                        abs-mul (abs mul)
                        c1-mult mul
                        two-mul (+ mul mul)
                        squared-mul-plus-one (+ (* mul mul) 1.0))
                  (gbuzz-update-multipliers c2-mult c3-mult rsum1 nh1 mul
                                            abs-mul))
      ;; Expand here if MUL is modulated
      (maybe-expand mul)
      (unless (or (and (= old-num-harm num-harm)
                       (= old-lowest-harm lowest-harm))
                  (plusp count))
        (setf old-num-harm num-harm
              old-lowest-harm lowest-harm
              count nh-lag
              cross +sample-zero+
              nh0 nh1
              nh1 (max 1 (abs num-harm))
              old-c0 c0
              old-c1 c1
              old-c2 c2
              old-c3 c3
              old-c1-mult c1-mult
              old-c2-mult c2-mult
              old-c3-mult c3-mult
              c0 lowest-harm
              c1 (1- c0)
              c2 (+ c0 nh1)
              c3 (1- c2)
              old-two-mul two-mul
              old-squared-mul-plus-one squared-mul-plus-one
              rsum0 rsum1)
        (when (= old-mul mul)
          (gbuzz-update-multipliers c2-mult c3-mult rsum1 nh1 mul abs-mul)))
      (unless (= old-mul mul)
        (setf old-mul mul
              abs-mul (abs mul)
              c1-mult mul
              two-mul (+ mul mul)
              squared-mul-plus-one (+ (* mul mul) 1.0))
        (gbuzz-update-multipliers c2-mult c3-mult rsum1 nh1 mul abs-mul))
      (setf angle (+ (* +twopi+ (phasor freq 0)) phase))
      (setf denom-osc (cos angle))
      (setf denom1 (- squared-mul-plus-one (* two-mul denom-osc)))
      (setf out
            (cond ((or (> denom1 1.d-5) (< denom1 -1.d-5))
                   (setf num1 (+ (- (cos (* c0 angle))
                                    (* c1-mult (cos (* c1 angle)))
                                    (* c2-mult (cos (* c2 angle))))
                                 (* c3-mult (cos (* c3 angle)))))
                   (setf res1 (* rsum1 (/ num1 denom1)))
                   (* amp
                      (cond ((plusp count)
                             (decf count)
                             (setf num0
                                   (+ (- (cos (* old-c0 angle))
                                         (* old-c1-mult (cos (* old-c1
                                                                angle)))
                                         (* old-c2-mult (cos (* old-c2
                                                                angle))))
                                      (* old-c3-mult (cos (* old-c3 angle)))))
                             (setf denom0 (- old-squared-mul-plus-one
                                             (* old-two-mul denom-osc)))
                             (setf res0 (if (or (> denom0 1.d-5)
                                                (< denom0 -1.d-5))
                                            (* rsum0 (/ num0 denom0))
                                            (sample 1)))
                             (prog1 (linear-interp cross res0 res1)
                               (incf cross inc-interp)))
                            (t res1))))
                  (t (when (plusp count)
                       (decf count)
                       (incf cross inc-interp))
                     (if (minusp out) (- amp) amp)))))))

;;; Wavetable lookup oscillator
(define-vug-macro osc (buffer &optional (freq 440.0) (amp 1.0d0)
                       (phase +sample-zero+) interpolation)
  (with-gensyms (osc)
    `(vuglet ((,osc ((buf buffer) freq amp phase)
                (%osc buf freq amp phase ,(null (constantp phase))
                      ,interpolation)))
       (,osc ,buffer ,freq ,amp ,phase))))

;;; Sine wave oscillator
(define-vug sine (freq amp phase)
  (* amp (sin (+ (* +twopi+ (phasor freq 0)) phase))))

;;; Pulse wave oscillator
(define-vug pulse (freq amp width)
  (if (< (phasor freq 0) width) amp (- amp)))

(define-vug-macro impulse (&optional (freq 0 freq-p) (amp 1) (phase 0))
  (if freq-p
      (with-gensyms (periodic-impulse)
        `(vuglet ((,periodic-impulse (freq amp phase)
                    (impulse-oscillator freq amp phase
                                        ,(null (constantp phase)))))
           (,periodic-impulse ,freq ,amp ,phase)))
      ;; Single impulse.
      `(- 1 (delay1 1))))

;;; Sinusoidal oscillator based on 2D vector rotation,
;;;  = undamped "coupled-form" resonator
;;;  = lossless 2nd-order normalized ladder filter
;;;
;;; Reference:
;;;
;;;   [1] Julius O. Smith, "Faust Oscillator Library" (oscillator.lib)
;;;
;;;   [2] https://ccrma.stanford.edu/~jos/pasp/Normalized_Scattering_Junctions.html
;;;
(define-vug oscrq (freq)
  "Sinusoidal oscillator based on 2D vector rotation (sine and cosine outputs)."
  (nlf2 (impulse) freq 1))

(define-vug oscrs (freq amp)
  "Sinusoidal oscillator based on 2D vector rotation (sine output)."
  (* amp (frame-ref (oscrq freq) 0)))

(define-vug oscrc (freq amp)
  "Sinusoidal oscillator based on 2D vector rotation (cosine output)."
  (* amp (frame-ref (oscrq freq) 1)))

(define-vug oscr (freq amp)
  "Sinusoidal oscillator based on 2D vector rotation (sine output by default)."
  (oscrs freq amp))

;;; Band limited impulse generator used in Music N languages.
;;; The output is a set of harmonically related sine partials.
(define-vug-macro buzz (freq amp num-harm &key (phase +sample-zero+)
                        (table-lookup-p t) buffer (harm-change-lag 0.001d0)
                        (interpolation :linear))
  (if table-lookup-p
      ;; Version with table lookup.
      (with-gensyms (buzz-table-lookup)
        `(vuglet ((,buzz-table-lookup (freq amp (num-harm fixnum) phase
                                       (buf buffer) harm-change-lag)
                    (%buzz buf freq amp num-harm phase
                           ,(null (constantp phase)) harm-change-lag
                           ,interpolation)))
           (,buzz-table-lookup ,freq ,amp ,num-harm ,phase
                               ,(or buffer '*sine-table*) ,harm-change-lag)))
      ;; This version uses directly the SIN function.
      `(buzz-hq ,freq ,amp ,num-harm ,phase ,harm-change-lag)))

(define-vug-macro gbuzz (freq amp num-harm lowest-harm mul
                         &key (phase +sample-zero+) (table-lookup-p t)
                         buffer (harm-change-lag 0.001d0)
                         (interpolation :linear))
  (if table-lookup-p
      ;; Version with table lookup
      (with-gensyms (gbuzz-table-lookup)
        `(vuglet ((,gbuzz-table-lookup (freq amp (num-harm fixnum)
                                        (lowest-harm fixnum) mul phase
                                        (buf buffer) harm-change-lag)
                    (%gbuzz buf freq amp num-harm lowest-harm mul phase
                            ,(null (constantp phase)) harm-change-lag
                            ,interpolation)))
           (,gbuzz-table-lookup ,freq ,amp ,num-harm ,lowest-harm ,mul
                                ,phase ,(or buffer '*cosine-table*)
                                ,harm-change-lag)))
      `(gbuzz-hq ,freq ,amp ,num-harm ,lowest-harm ,mul ,phase
                 ,harm-change-lag)))

(define-vug buffer-play ((buffer buffer) rate start-pos (loop-p boolean)
                         (done-action function))
  (prog1 (buffer-read buffer (%phasor (* rate (buffer-sample-rate buffer)
                                         *sample-duration*)
                                      start-pos
                                      (if loop-p
                                          (sample (buffer-frames buffer))
                                          most-positive-sample))
                      :wrap-p loop-p :interpolation :cubic)
    (when (done-self)
      (done-action done-action))))
