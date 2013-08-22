(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:+seg-lin-func+
     incudine:envelope
     incudine:envelope-data
     incudine::envelope-data-size
     incudine:envelope-points
     incudine:envelope-max-points
     incudine:envelope-loop-node
     incudine:envelope-release-node
     incudine::%segment-init
     incudine::%segment-update-level))
  (object-to-free incudine:make-envelope update-local-envelope))

(defmacro make-local-envelope (levels times &key curve (loop-node -1)
                               (release-node -1))
  (with-gensyms (%levels %times %curve env)
    `(with ((,%levels (locally (declare #.*reduce-warnings*)
                        ,levels))
            (,%times (locally (declare #.*reduce-warnings*)
                       ,times))
            (,%curve (locally (declare #.*reduce-warnings*)
                       ,curve))
            (,env (incudine:make-envelope ,%levels ,%times :curve ,%curve
                                          :loop-node ,loop-node
                                          :release-node ,release-node
                                          :real-time-p t)))
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
                            &key (level 1) (curve :lin))
  `(make-local-envelope (list 0 ,level ,level 0)
                        (list ,attack-time ,sustain-time ,release-time)
                        :curve ,curve))

(defmacro make-local-perc (attack-time release-time
                           &key (level 1) (curve -4))
  `(make-local-envelope (list 0 ,level 0) (list ,attack-time ,release-time)
                        :curve ,curve))

(defmacro make-local-cutoff (release-time &key (level 1) (curve :exp))
  `(make-local-envelope (list ,level 0) (list ,release-time)
                        :curve ,curve :release-node 0))

(defmacro make-local-asr (attack-time sustain-level release-time
                          &key (curve -4))
  `(make-local-envelope (list 0 ,sustain-level 0)
                        (list ,attack-time ,release-time)
                        :curve ,curve :release-node 1))

(defmacro make-local-adsr (attack-time decay-time sustain-level release-time
                           &key (peak-level 1) (curve -4))
  `(make-local-envelope (list 0 ,peak-level (* ,peak-level ,sustain-level) 0)
                        (list ,attack-time ,decay-time ,release-time)
                        :curve ,curve :release-node 2))

(defmacro make-local-dadsr (delay-time attack-time decay-time sustain-level
                            release-time &key (peak-level 1) (curve -4))
  `(make-envelope (list 0 0 ,peak-level (* ,peak-level ,sustain-level) 0)
                  (list ,delay-time ,attack-time ,decay-time ,release-time)
                  :curve ,curve :release-node 3))

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
        (cond ((zerop remain)
               (done-action done-action)
               (setf done-p t)
               value)
              (t (decf remain)
                 (incf value slope))))))

(define-vug x-line (start end dur (done-action function))
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
                             (expt (the non-negative-sample
                                     (/ end value))
                                   (/ (sample samples)))))))
    (declare (type sample %start value power) (type boolean done-p)
             (type non-negative-fixnum samples remain))
    (if done-p
        value
        (cond ((zerop remain)
               (done-action done-action)
               (setf done-p t)
               value)
              (t (decf remain)
                 (setf value (* value power)))))))

;;; Envelope Generator inspired by EnvGen of SuperCollider

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro envgen-next-dur (env-data index time-scale)
    `(sample->fixnum
      (* (data-ref ,env-data ,index)
         ,time-scale *sample-rate*)))

  (defmacro envgen-next-index (data-size index curr-node)
    `(the non-negative-fixnum
       (cond ((zerop ,index) 0)
             ((>= ,index ,data-size)
              ;; point to the last segment
              (setf ,index (- ,data-size 4)
                    ,curr-node (1- (the positive-fixnum
                                     (/ ,index 3))))
              ,index)
             (t (- ,index 3)))))

  (defmacro envgen-jump-node (node-dest node-src index)
    (with-gensyms (dest)
      `(let ((,dest ,node-dest))
         (incf ,index (the non-negative-fixnum
                        (* (the non-negative-fixnum
                             (- ,dest ,node-src))
                           3)))
         (setf ,node-src ,dest))))

  (defmacro update-sustain (var gate curr-node release-node)
    `(setf ,var
           (and (plusp ,gate)
                (>= ,curr-node 0)
                (= ,curr-node ,release-node)))))

(define-vug envgen ((env envelope) gate time-scale (done-action function))
  (with ((index 0)
         (curr-node -1)
         (env-data (envelope-data env))
         (data-size (envelope-data-size env))
         (last-point (1- (the non-negative-fixnum (envelope-points env))))
         (curr-index (envgen-next-index data-size index curr-node))
         (prev-index 0)
         (loop-node (envelope-loop-node env))
         (release-node (envelope-release-node env))
         (sustain nil)
         (done-p nil)
         (dur 0)
         (remain 0)
         (tmp 0.0)
         (end 0.0)
         (grow 0.0)
         (a2 0.0)
         (b1 0.0)
         (y1 0.0)
         (y2 0.0)
         (curve +seg-lin-func+)
         (gate-trig (plusp gate))
         (level (cond ((and (zerop gate)
                            (null sustain)
                            (plusp release-node)
                            (< release-node curr-node))
                       ;; Release before sustain
                       (envgen-jump-node (1- release-node) curr-node index)
                       (setf remain 0)
                       tmp)
                      ((= gate -1.0)
                       ;; Immediate cutoff
                       (setf sustain nil remain 0)
                       (envgen-jump-node last-point curr-node index)
                       0.0)
                      ((< gate -1.0)
                       ;; Force the release stage with custom duration
                       (setf dur (sample->fixnum (* (- -1.0 gate) *sample-rate*))
                             ;; Anticipate one sample to avoid the repetition of
                             ;; a vertex because the last value of a segment is
                             ;; the first value of the next segment.
                             remain (1- dur)
                             curr-node (1+ curr-node))
                       (unless (= curr-node last-point)
                         (envgen-jump-node (1- last-point) curr-node index)
                         (setf index (+ index 2) ; skip dur
                               end (data-ref env-data index)
                               index (+ index 1)
                               curve (data-ref env-data index)
                               prev-index curr-index))
                       (setf sustain nil)
                       (%segment-init tmp end dur curve grow a2 b1 y1 y2)
                       tmp)
                      ((and (zerop index) (zerop dur))
                       (update-sustain sustain gate curr-node release-node)
                       (setf gate-trig nil)
                       (data-ref env-data 0))
                      (gate-trig
                       ;; Restart
                       (setf gate-trig nil
                             remain 0
                             index 0
                             curr-node -1
                             curr-index (envgen-next-index data-size index curr-node)
                             prev-index curr-index
                             done-p nil
                             sustain nil
                             ;; LEVEL is set to END during the performance
                             end tmp))
                      ((zerop dur)
                       (if sustain (setf sustain nil))
                       end)
                      ((or done-p (= curr-index prev-index))
                       (if sustain (setf sustain nil))
                       tmp)
                      (t (if sustain (setf sustain nil))
                         (setf dur (max 1 (+ (- (envgen-next-dur env-data index time-scale)
                                                dur)
                                             remain))
                               ;; One sample is subtracted in the previous
                               ;; value of REMAIN
                               remain dur
                               index (1+ index)
                               end (data-ref env-data index)
                               index (1+ index)
                               curve (data-ref env-data index))
                         (%segment-init tmp end dur curve grow a2 b1 y1 y2)
                         tmp))))
    (declare (type sample level end curve tmp grow a2 b1 y1 y2)
             (type non-negative-fixnum index data-size last-point dur remain
                   curr-index prev-index)
             (type fixnum curr-node loop-node release-node)
             (type boolean sustain done-p gate-trig))
    (initialize (setf end level))
    (cond ((or done-p sustain) tmp)
          (t (if (zerop remain)
                 (cond ((>= (incf index) data-size)
                        (done-action done-action)
                        (setf done-p t)
                        (setf tmp end))
                       (t (incf curr-node)
                          (cond ((and (>= loop-node 0)
                                      (= (1+ curr-node) release-node)
                                      (plusp gate)
                                      (/= curr-node loop-node))
                                 (envgen-jump-node loop-node curr-node index))
                                ((and (= curr-node release-node) (plusp gate))
                                 (setf sustain t)))
                          (setf dur (max 1 (envgen-next-dur env-data index time-scale))
                                remain (1- dur)
                                index (1+ index)
                                ;; The first value of the segment is the last value
                                ;; of the previous segment
                                level end
                                end (data-ref env-data index)
                                index (1+ index)
                                curve (data-ref env-data index)
                                prev-index curr-index)
                          (%segment-init level end dur curve grow a2 b1 y1 y2)
                          (setf tmp level)))
                 (progn (decf remain)
                        (%segment-update-level level curve grow a2 b1 y1 y2)
                        (setf tmp level)))))))
