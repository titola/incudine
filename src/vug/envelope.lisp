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

(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:envelope
     incudine:envelope-data
     incudine::envelope-data-size
     incudine:envelope-points
     incudine::envelope-max-points
     incudine:envelope-loop-node
     incudine:envelope-release-node
     incudine:envelope-restart-level
     incudine::segment-stack-init
     incudine::segment-stack-reposition
     incudine::%segment-update-level)))

(define-vug decay (in decay-time)
  "Exponential decay."
  (pole in (t60->pole decay-time)))

(define-vug decay-2 (in attack-time decay-time)
  "Exponential decay with the attack obtained by subtracting two DECAY's."
  (- (decay in decay-time) (decay in attack-time)))

;;; Simple segments

(define-vug line (start end duration (done-action function))
  "Linear ramp from START to END in DURATION seconds.

If the control parameter DURATION is changed, start a new ramp from
the new START, or current level, to the new END. Example:

    (dsp! ramp-test (start end duration)
      (out (* (line start end duration) (white-noise))))

    (rt-start)
    (ramp-test 0 1 3)
    (set-controls 1 :end 0 :duration 2)
    (set-controls 1 :end .5 :duration .3)
    (set-controls 1 :start 0 :end 1 :duration .5)

START, END and DURATION default to 0, 1 and 1 respectively.

The one-argument function DONE-ACTION, #'IDENTITY by default, is
called at the end of the ramp. The function argument is the DSP node."
  (:defaults 0 1 1 #'identity)
  (with ((done-p nil)
         (samples (max 1 (sample->fixnum (* duration *sample-rate*))))
         (remain samples)
         (value start)
         (slope (without-follow (start end)
                  ;; Update only if DURATION is changed
                  (init-only (when done-p
                               ;; Restart
                               (setf done-p nil))
                             (/ (- end value) samples)))))
    (declare (type sample value slope) (type boolean done-p)
             (type non-negative-fixnum samples remain))
    (cond (done-p value)
          (t (cond ((<= remain 1)
                    (funcall done-action (dsp-node))
                    (setf done-p t))
                   (t (decf remain)))
             (incf value slope)))))

(define-vug expon (start end duration (done-action function))
  "Exponential curve from START to END in DURATION seconds.

If START or END is 0, it is reset to 0.00001. The sign of END has to be
the sign of START.

If the control parameter DURATION is changed, start a new curve from the
new START, or current level, to the new END.

START, END and DURATION default to 0.00001, 1 and 1 respectively.

The one-argument function DONE-ACTION, #'IDENTITY by default, is
called at the end of the curve. The function argument is the DSP node."
  (:defaults 0 1 1 #'identity)
  (with ((done-p nil)
         (samples (max 1 (sample->fixnum (* duration *sample-rate*))))
         (remain samples)
         (%start (if (zerop start) 1d-5 start))
         (value %start)
         (power (without-follow (start end)
                  ;; Update only if DURATION is changed
                  (init-only (when done-p
                               ;; Restart
                               (setf done-p nil))
                             (expt (the non-negative-sample
                                     (/ (if (zerop end) 1d-5 end) value))
                                   (/ (sample samples)))))))
    (declare (type sample %start value power) (type boolean done-p)
             (type non-negative-fixnum samples remain))
    (cond (done-p value)
          (t (cond ((<= remain 1)
                    (funcall done-action (dsp-node))
                    (setf done-p t))
                   (t (decf remain)))
             (setf value (* value power))))))

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
  "Play back the curves of the envelope ENV.

GATE is one of:

                0   start the release phase
               -1   immediate cutoff
     less than -1   release stage with a custom duration -1 minus GATE

The envelope is re-triggered if the difference between the gate of the
current audio cycle and the gate of the previous audio cycle is a
positive value.

GATE and TIME-SCALE default to 1.

The one-argument function DONE-ACTION, #'IDENTITY by default, is
called at the end of the envelope. The function argument is the DSP node.

LOCATION, 0 by default, is the current position in samples (an integer)
of the envelope."
  (:defaults (incudine:incudine-missing-arg "Missing ENVELOPE struct.")
             1 1 #'identity 0)
  (with-samples (last-level end (curve incudine::+seg-lin-func+)
                 grow a2 b1 y1 y2 old-gate)
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
      (declare (type non-negative-fixnum dur remain pos offset)
               (type (unsigned-byte 24) index data-size last-point
                      curr-index prev-index last-dur-index)
               (type cons consointer)
               (type fixnum curr-node loop-node release-node)
               (type sample level carry)
               (type boolean sustain done-p gate-trig))
      (initialize (setf end level))
      (with-follow (location)
        ;; Jump location.
        (with ((end-time 0) (pos-time 0))
          (declare (type sample end-time pos-time))
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
                    (funcall done-action (dsp-node))
                    (setf done-p t)))))
      ;; Expand if GATE is modulated.
      (maybe-expand level)
      (cond ((or done-p sustain) (values last-level pos))
            ((<= remain 1)
             ;; End of segment.
             (cond ((envelope-end-of-data-p (incf index) data-size)
                    (funcall done-action (dsp-node))
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
                      (incf carry (* offset (sample 0.5)))
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
