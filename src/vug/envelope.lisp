;;; Copyright (c) 2013-2017 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:+seg-lin-func+
     incudine:incudine-missing-arg
     incudine:envelope
     incudine:envelope-data
     incudine::envelope-data-size
     incudine:envelope-points
     incudine:envelope-max-points
     incudine:envelope-loop-node
     incudine:envelope-release-node
     incudine:envelope-restart-level
     incudine::segment-stack-init
     incudine::segment-stack-reposition
     incudine::%segment-update-level))
  (object-to-free incudine:make-envelope update-local-envelope))

(defmacro make-local-envelope (levels times &key curve base (loop-node -1)
                               (release-node -1) restart-level)
  (with-gensyms (%levels %times %curve %base env)
    `(with ((,%levels (locally (declare #.*reduce-warnings*)
                        ,levels))
            (,%times (locally (declare #.*reduce-warnings*)
                       ,times))
            (,%curve (locally (declare #.*reduce-warnings*)
                       ,curve))
            (,%base (locally (declare #.*reduce-warnings*)
                      ,base))
            (,env (incudine:make-envelope ,%levels ,%times
                    :curve ,%curve
                    :base ,%base
                    :loop-node ,loop-node
                    :release-node ,release-node
                    :restart-level ,restart-level
                    :real-time-p *allow-rt-memory-pool-p*)))
       ,env)))

(defmacro update-local-envelope (vug-varname args)
  (with-gensyms (size)
    `(let ((,size (max (length ,(first args))
                       (1+ (length ,(second args))))))
       (cond ((> ,size (envelope-max-points ,vug-varname))
              (incudine:free ,vug-varname)
              (setf ,vug-varname (incudine:make-envelope ,@args)))
             (t (incudine:set-envelope ,vug-varname
                                       ;; The last keyword is REAL-TIME-P,
                                       ;; absent in SET-ENVELOPE
                                       ,@(butlast args 2)))))))

(defmacro make-local-linen (attack-time sustain-time release-time
                            &key (level 1) (curve :lin) base restart-level)
  `(make-local-envelope (list 0 ,level ,level 0)
                        (list ,attack-time ,sustain-time ,release-time)
                        :curve ,curve :base ,base :restart-level ,restart-level))

(defmacro make-local-perc (attack-time release-time
                           &key (level 1) (curve -4) base restart-level)
  `(make-local-envelope (list 0 ,level 0) (list ,attack-time ,release-time)
                        :curve ,curve :base ,base :restart-level ,restart-level))

(defmacro make-local-cutoff (release-time &key (level 1) (curve :exp) base
                             restart-level)
  `(make-local-envelope (list ,level 0) (list ,release-time)
                        :curve ,curve :base ,base :release-node 0
                        :restart-level ,restart-level))

(defmacro make-local-asr (attack-time sustain-level release-time
                          &key (curve -4) base restart-level)
  `(make-local-envelope (list 0 ,sustain-level 0)
                        (list ,attack-time ,release-time)
                        :curve ,curve :base ,base :release-node 1
                        :restart-level ,restart-level))

(defmacro make-local-adsr (attack-time decay-time sustain-level release-time
                           &key (peak-level 1) (curve -4) base restart-level)
  `(make-local-envelope (list 0 ,peak-level (* ,peak-level ,sustain-level) 0)
                        (list ,attack-time ,decay-time ,release-time)
                        :curve ,curve :base ,base :release-node 2
                        :restart-level ,restart-level))

(defmacro make-local-dadsr (delay-time attack-time decay-time sustain-level
                            release-time &key (peak-level 1) (curve -4) base
                            restart-level)
  `(make-local-envelope (list 0 0 ,peak-level (* ,peak-level ,sustain-level) 0)
                        (list ,delay-time ,attack-time ,decay-time ,release-time)
                        :curve ,curve :base ,base :release-node 3
                        :restart-level ,restart-level))

(defmacro breakpoints->local-env (bp-seq &key curve base scaler offset duration
                                  (loop-node -1) (release-node -1)
                                  restart-level)
  (with-gensyms (args seq k b scl os dur)
    `(with ((,seq ,bp-seq)
            (,k ,curve)
            (,b ,base)
            (,scl ,scaler)
            (,os ,offset)
            (,dur ,duration)
            (,args (multiple-value-list
                     (incudine::breakpoints->env-arguments
                       ,seq ,k ,b ,scl ,os ,dur))))
       (make-local-envelope (first ,args) (second ,args)
                            :curve (if ,b nil (third ,args))
                            :base ,b
                            :loop-node ,loop-node
                            :release-node ,release-node
                            :restart-level ,restart-level))))

;;; Simple segments

(define-vug line (start end dur (done-action function))
  (with ((done-p nil)
         (samples (max 1 (sample->fixnum (* dur *sample-rate*))))
         (remain samples)
         (value start)
         (slope (without-follow (start end)
                  ;; Update only if DUR is changed
                  (init-only (when done-p
                               ;; Restart
                               (setf done-p nil))
                             (/ (- end value) samples)))))
    (declare (type sample value slope) (type boolean done-p)
             (type non-negative-fixnum samples remain))
    (if done-p
        value
        (cond ((<= remain 1)
               (done-action done-action)
               (setf done-p t)
               value)
              (t (decf remain)
                 (incf value slope))))))

(define-vug expon (start end dur (done-action function))
  (with ((done-p nil)
         (samples (max 1 (sample->fixnum (* dur *sample-rate*))))
         (remain samples)
         (%start (if (zerop start) 1d-5 start))
         (value %start)
         (power (without-follow (start end)
                  ;; Update only if DUR is changed
                  (init-only (when done-p
                               ;; Restart
                               (setf done-p nil))
                             (expt (the non-negative-sample (/ end value))
                                   (/ (sample samples)))))))
    (declare (type sample %start value power) (type boolean done-p)
             (type non-negative-fixnum samples remain))
    (if done-p
        value
        (cond ((<= remain 1)
               (done-action done-action)
               (setf done-p t)
               value)
              (t (decf remain)
                 (setf value (* value power)))))))

;;; Envelope Generator inspired by EnvGen of SuperCollider

(declaim (inline envelope-next-dur))
(defun envelope-next-dur (env-data index time-scale carry offset)
  (let* ((dur (+ (* (smp-ref env-data index) time-scale *sample-rate*)
                 carry offset))
         (idur (sample->fixnum dur)))
    (values (max 1 idur) (- dur idur))))

(defmacro envelope-update-dur (dur-var data index scale carry-var os)
  (with-gensyms (d c)
    `(multiple-value-bind (,d ,c)
         (envelope-next-dur ,data ,index ,scale ,carry-var ,os)
       (setf ,dur-var ,d ,carry-var ,c))))

(defmacro envelope-next-index (data-size index curr-node)
  `(the non-negative-fixnum
     (cond ((zerop ,index) 0)
           ((>= ,index ,data-size)
            ;; point to the last segment
            (setf ,index (- ,data-size 4))
            (setf ,curr-node (1- (the positive-fixnum (/ ,index 3))))
            ,index)
           (t (- ,index 3)))))

(defmacro envelope-jump-node (node-dest node-src index)
  (with-gensyms (dest)
    `(let ((,dest ,node-dest))
       (incf ,index (the non-negative-fixnum
                      (* (the non-negative-fixnum (- ,dest ,node-src)) 3)))
       (setf ,node-src ,dest))))

(declaim (inline jump-to-loop-node-p))
(defun jump-to-loop-node-p (gate curr-node loop-node release-node)
  (and (>= loop-node 0)
       (= curr-node release-node)
       (plusp gate)
       (/= curr-node loop-node)))

(declaim (inline envelope-end-of-data-p))
(defun envelope-end-of-data-p (index data-size)
  (>= index data-size))

(declaim (inline envelope-to-sustain-p))
(defun envelope-to-sustain-p (gate curr-node release-node)
  (and (= curr-node release-node) (plusp gate)))

(declaim (inline release-before-sustain-p))
(defun release-before-sustain-p (gate sustain curr-node release-node)
  (and (zerop gate)
       (null sustain)
       (plusp release-node)
       (< curr-node release-node)))

(defmacro envelope-update-sustain (var gate curr-node release-node)
  `(setf ,var (and (>= ,curr-node 0)
                   (envelope-to-sustain-p ,gate ,curr-node ,release-node))))

(defmacro envelope-sustain (sustain-var)
  `(setf ,sustain-var t))

(defmacro envelope-no-sustain (sustain-var)
  `(if ,sustain-var (setf ,sustain-var nil)))

(declaim (inline immediate-cutoff-p))
(defun immediate-cutoff-p (gate)
  (= gate -1.0))

(declaim (inline release-with-custom-duration-p))
(defun release-with-custom-duration-p (gate)
  (< gate -1.0))

(declaim (inline envelope-custom-duration))
(defun envelope-custom-duration (gate)
  (sample->fixnum (* (- -1.0 gate) *sample-rate*)))

(declaim (inline envelope-begin-p))
(defun envelope-begin-p (index dur)
  (and (zerop index) (zerop dur)))

(define-vug envelope ((env envelope) gate time-scale (done-action function)
                      (location non-negative-fixnum))
  (:defaults (incudine-missing-arg "Missing ENVELOPE struct.") 1 1 #'identity 0)
  (with-samples (last-level end (curve +seg-lin-func+) grow a2 b1 y1 y2 old-gate)
    (declare (preserve last-level end curve grow a2 b1 y1 y2))
    (with ((index 0)
           (curr-node -1)
           ;; Pointer to the frame where the first slot is LAST-LEVEL.
           ;; The variables from LAST-LEVEL to Y2 are declared PRESERVE,
           ;; therefore they are not removed during the compilation and
           ;; we get a frame with slot names.
           (consointer (reduce-warnings (list (get-pointer last-level))))
           (env-data (envelope-data env))
           (data-size (envelope-data-size env))
           (last-point (1- (the non-negative-fixnum (envelope-points env))))
           (last-dur-index (- data-size 3))
           (offset 0)
           (curr-index (envelope-next-index data-size index curr-node))
           (prev-index 0)
           (loop-node (envelope-loop-node env))
           (release-node (envelope-release-node env))
           (sustain nil)
           (done-p nil)
           (dur 0)
           (remain 0)
           (pos 0)
           (gate-trig (plusp gate))
           ;; The duration of a segment is not a multiple of 1/srate.
           ;; CARRY stores the remainder after the truncation and it is
           ;; used to compute the next duration.
           (carry 0)
           (level (cond ((prog1 (and gate-trig (<= gate old-gate))
                           (setf old-gate gate))
                         ;; Restart only when the current gate is major than
                         ;; the gate of the previous audio cycle.
                         last-level)
                        ((release-before-sustain-p gate sustain curr-node
                                                   release-node)
                         (setf carry +sample-zero+)
                         (envelope-jump-node (1- release-node) curr-node index)
                         (setf remain 0)
                         (setf end last-level))
                        ((immediate-cutoff-p gate)
                         (setf sustain nil remain 0)
                         (envelope-jump-node last-point curr-node index)
                         +sample-zero+)
                        ((release-with-custom-duration-p gate)
                         ;; Force the release stage with custom duration.
                         (setf dur (envelope-custom-duration gate)
                               ;; Anticipate one sample to avoid the repetition
                               ;; of a vertex because the last value of a segment
                               ;; is the first value of the next segment.
                               remain (1- dur)
                               curr-node (1+ curr-node))
                         (unless (= curr-node last-point)
                           (envelope-jump-node (1- last-point) curr-node index)
                           (setf index (+ index 2) ; skip dur
                                 end (smp-ref env-data index)
                                 index (+ index 1)
                                 curve (smp-ref env-data index)
                                 prev-index curr-index))
                         (setf sustain nil)
                         (segment-stack-init consointer dur)
                         last-level)
                        ((envelope-begin-p index dur)
                         (cond (gate-trig
                                (envelope-update-sustain sustain gate curr-node
                                                         release-node)
                                (setf gate-trig nil)
                                (smp-ref env-data 0))
                               ;; ENVELOPE started with GATE zero.
                               (t (setf index data-size)
                                  (samples-zero last-level end))))
                        (gate-trig
                         ;; Restart.
                         (setf gate-trig nil
                               remain 0
                               index 0
                               curr-node -1
                               curr-index (envelope-next-index data-size index
                                                               curr-node)
                               prev-index curr-index
                               done-p nil
                               sustain nil
                               ;; LEVEL is set to END during the performance.
                               end (or (envelope-restart-level env)
                                       last-level)))
                        ((zerop dur)
                         (envelope-no-sustain sustain)
                         end)
                        ((or done-p (= curr-index prev-index))
                         (envelope-no-sustain sustain)
                         last-level)
                        (t (envelope-no-sustain sustain)
                           (envelope-update-dur dur env-data index time-scale
                                                carry (- remain dur))
                           ;; One sample subtracted in the previous REMAIN value.
                           (setf remain dur
                                 end (smp-ref env-data (1+ index))
                                 index (+ index 2)
                                 curve (smp-ref env-data index))
                           (segment-stack-init consointer dur)
                           last-level))))
      (declare (type non-negative-fixnum index data-size last-point dur remain
                     pos curr-index prev-index last-dur-index offset)
               (type cons consointer)
               (type fixnum curr-node loop-node release-node)
               (type sample level carry)
               (type boolean sustain done-p gate-trig))
      (initialize (setf end level))
      (with-follow (location)
        ;; Jump location.
        (with ((dest-node 0) (end-time 0) (pos-time 0))
          (declare (type non-negative-fixnum dest-node)
                   (type sample end-time pos-time))
          (setf end-time +sample-zero+)
          (setf pos-time (* location *sample-duration*))
          (loop for i from 0 below (envelope-points env)
                for j from 1 by 3 do
                  (incf end-time (* time-scale (smp-ref env-data j)))
                  (when (> (- end-time *sample-duration*) pos-time)
                    (setf carry +sample-zero+)
                    (setf offset (truncate (/ j last-dur-index)))
                    (envelope-update-dur dur env-data j time-scale carry
                                         (- offset))
                    (setf pos location
                          curr-node i
                          index j
                          curr-index j
                          prev-index j
                          remain (1+ (sample->fixnum
                                       (* (- end-time pos-time) *sample-rate*)))
                          last-level (smp-ref env-data (if (= j 1) 0 (- j 2)))
                          end (smp-ref env-data (incf index))
                          curve (smp-ref env-data (incf index))
                          done-p nil
                          sustain nil)
                    (segment-stack-reposition consointer dur (- dur remain))
                    (setf level last-level)
                    (return))
                finally
                  ;; Last location.
                  (setf pos (1- (sample->fixnum (* end-time *sample-rate*))))
                  (envelope-jump-node (1- i) curr-node index)
                  (setf dur (envelope-next-dur env-data index time-scale 0 0)
                        remain 0
                        carry +sample-zero+
                        last-level (smp-ref env-data (1+ index))
                        index (+ index 2)
                        level last-level)
                  (unless done-p
                    (done-action done-action)
                    (setf done-p t)))))
      ;; Expand if GATE is modulated.
      (maybe-expand level)
      (cond ((or done-p sustain) (values last-level pos))
            ((<= remain 1)
             ;; End of segment.
             (cond ((envelope-end-of-data-p (incf index) data-size)
                    (done-action done-action)
                    (values (setf done-p t carry +sample-zero+ last-level end)
                            pos))
                   (t (incf curr-node)
                      (cond
                        ((jump-to-loop-node-p gate curr-node loop-node
                                              release-node)
                         (envelope-jump-node loop-node curr-node index))
                        ((envelope-to-sustain-p gate curr-node release-node)
                         (envelope-sustain sustain)))
                      ;; Compute the parameters for the next segment.
                      ;; OFFSET is 1 if it is the last segment, so CARRY
                      ;; is incremented by 0.5 to prevent rounding errors.
                      (setf offset (truncate (/ index last-dur-index)))
                      (incf carry (* offset 0.5))
                      (envelope-update-dur dur env-data index time-scale
                                           carry (- offset))
                      (setf remain (+ dur offset)
                            ;; The first value of the segment is the
                            ;; last value of the previous segment.
                            level end
                            end (smp-ref env-data (1+ index))
                            index (+ index 2)
                            curve (smp-ref env-data index)
                            prev-index curr-index)
                      (setf last-level level)
                      (segment-stack-init consointer dur)
                      (values (setf level last-level)
                              (prog1 pos (incf pos))))))
            (t (if (and (= remain 3)
                        (envelope-end-of-data-p (1+ index) data-size))
                   (setf remain 1)
                   (decf remain))
               ;; Compute the next point.
               (%segment-update-level level curve grow a2 b1 y1 y2)
               (values (setf last-level level)
                       (prog1 pos (incf pos))))))))
