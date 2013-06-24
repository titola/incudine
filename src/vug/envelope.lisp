(in-package :incudine.vug)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine:+seg-step-func+
     incudine:+seg-lin-func+
     incudine:+seg-exp-func+
     incudine:+seg-sine-func+
     incudine:+seg-welch-func+
     incudine:+seg-square-func+
     incudine:+seg-cubic-func+
     incudine:envelope
     incudine:envelope-data
     incudine:envelope-points
     incudine:envelope-max-points
     incudine:envelope-loop-node
     incudine:envelope-release-node
     incudine:envelope-p))
  (object-to-free incudine:make-envelope update-local-envelope))

(defmacro make-local-envelope (levels times &key curve (loop-node -1)
                               (release-node -1))
  (with-gensyms (%levels %times %curve env)
    `(with ((,%levels ,levels)
            (,%times ,times)
            (,%curve ,curve)
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
  `(make-local-envelope (locally
                            (declare #.*reduce-warnings*)
                            (list 0 ,peak-level (* ,peak-level ,sustain-level) 0))
                        (list ,attack-time ,decay-time ,release-time)
                        :curve ,curve :release-node 2))

(defmacro make-local-dadsr (delay-time attack-time decay-time sustain-level
                            release-time &key (peak-level 1) (curve -4))
  `(make-envelope (locally
                      (declare #.*reduce-warnings*)
                      (list 0 0 ,peak-level (* ,peak-level ,sustain-level) 0))
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
                                   (/ 1.0d0 samples))))))
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

;;; Envelope Generator

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro curve-case (keyform &body cases)
    (with-gensyms (curve)
      `(let ((,curve ,keyform))
         (declare (ignorable ,curve))
         (cond ,@(mapcar (lambda (x)
                           `(,(if (eq (car x) 'otherwise)
                                  t
                                  `(= ,curve ,(car x)))
                              ,@(cdr x)))
                         cases)))))

  (defmacro %segment-init (beg end dur curve grow a2 b1 y1 y2)
    (with-gensyms (w)
      `(curve-case ,curve
         (+seg-step-func+ (values))
         (+seg-lin-func+
          (setf ,grow (/ (- ,end ,beg) ,dur)))
         (+seg-exp-func+
          (setf ,grow (expt (the non-negative-sample (/ ,end ,beg))
                            (/ 1.0d0 ,dur))))
         (+seg-sine-func+
          (let ((,w (/ pi ,dur)))
            (setf ,a2 (* (+ ,end ,beg) 0.5d0)
                  ,b1 (* 2.0d0 (cos ,w))
                  ,y1 (* (- ,end ,beg) 0.5d0)
                  ,y2 (* ,y1 (sin (- (* pi 0.5d0) ,w))))))
         (+seg-welch-func+
          (let ((,w (/ (* pi 0.5d0) ,dur)))
            (setf ,b1 (* 2.0d0 (cos ,w)))
            (if (>= ,end ,beg)
                (setf ,a2 ,beg
                      ,y1 0.0d0
                      ,y2 (* (- (sin ,w)) (- ,end ,beg)))
                (setf ,a2 ,end
                      ,y1 (- ,beg ,end)
                      ,y2 (* (cos ,w) (- ,beg ,end))))))
         (+seg-square-func+
          (setf ,y1 (the non-negative-sample (sqrt ,beg))
                ,y2 (the non-negative-sample (sqrt ,end))
                ,grow (/ (- ,y2 ,y1) ,dur)))
         (+seg-cubic-func+
          (setf ,y1 (expt (the non-negative-sample ,beg) 0.3333333333333333d0)
                ,y2 (expt (the non-negative-sample ,end) 0.3333333333333333d0)
                ,grow (/ (- ,y2 ,y1) ,dur)))
         ;; custom curve
         (otherwise (if (< (abs ,curve) 0.001d0)
                        (setf ,grow (/ (- ,end ,beg) ,dur)
                              ,curve +seg-lin-func+)
                        (setf ,b1 (/ (- ,end ,beg) (- 1.0d0 (exp ,curve)))
                              ,a2 (+ ,beg ,b1)
                              ,grow (exp (/ ,curve ,dur))))))))

  (defmacro %segment-update-level (level curve grow a2 b1 y1 y2)
    (with-gensyms (y0)
      `(curve-case ,curve
         (+seg-step-func+ ,level)
         (+seg-lin-func+
          (setf ,level (+ ,level ,grow)))
         (+seg-exp-func+
          (setf ,level (* ,level ,grow)))
         (+seg-sine-func+
          (let ((,y0 (- (* ,b1 ,y1) ,y2)))
            (setf ,level (- ,a2 ,y0)
                  ,y2 ,y1 ,y1 ,y0)))
         (+seg-welch-func+
          (let ((,y0 (- (* ,b1 ,y1) ,y2)))
            (setf ,level (+ ,a2 ,y0)
                  ,y2 ,y1 ,y1 ,y0)))
         (+seg-square-func+
          (setf ,y1    (+ ,y1 ,grow)
                ,level (* ,y1 ,y1)))
         (+seg-cubic-func+
          (setf ,y1    (+ ,y1 ,grow)
                ,level (* ,y1 ,y1 ,y1)))
         ;; custom curve
         (otherwise (setf ,b1 (* ,b1 ,grow)
                          ,level (- ,a2 ,b1))))))

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

;;; Inspired by EnvGen of SuperCollider
(define-vug envgen ((env envelope) gate time-scale (done-action function))
  (with ((index 0)
         (curr-node -1)
         (env-data (envelope-data env))
         (data-size (incudine::envelope-data-size env))
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
                            (plusp release-node))
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
                             remain dur
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
                             sustain nil)
                       tmp)
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
    (cond ((or done-p sustain) level)
          (t (if (zerop remain)
                 (cond ((>= (incf index) data-size)
                        (done-action done-action)
                        (setf done-p t)
                        level)
                       (t (incf curr-node)
                          (cond ((and (>= loop-node 0)
                                      (= (1+ curr-node) release-node)
                                      (plusp gate)
                                      (/= curr-node loop-node))
                                 (envgen-jump-node loop-node curr-node index))
                                ((and (= curr-node release-node)
                                      (plusp gate))
                                 (setf sustain t)))
                          (setf dur (max 1 (envgen-next-dur env-data index time-scale))
                                remain dur
                                index (1+ index)
                                end (data-ref env-data index)
                                index (1+ index)
                                curve (data-ref env-data index)
                                prev-index curr-index)
                          (%segment-init level end dur curve grow a2 b1 y1 y2)
                          (setf tmp level)))
                 (progn (decf remain)
                        (%segment-update-level level curve grow a2 b1 y1 y2)
                        (setf tmp level)))))))

;;; Envelope segment of a node.

(defmacro with-node-segment-symbols (node symbols &body body)
  (let ((count 0)
        (instance (gensym "INSTANCE")))
    `(symbol-macrolet ((,instance (incudine::node-gain-data ,node))
                       ,@(mapcar
                          (lambda (sym)
                            (prog1 `(,sym (mem-aref ,instance 'sample ,count))
                              (incf count)))
                          symbols))
       ,@body)))

(defmacro %node-segment-init (beg end dur curve grow a2 b1 y1 y2)
  (let ((result `(%segment-init ,beg ,end ,dur ,curve ,grow
                                ,a2 ,b1 ,y1 ,y2)))
    (if (eq *sample-type* 'double-float)
        result
        (apply-sample-coerce (macroexpand-1 result)))))

(defgeneric node-segment (obj end dur &optional start curve done-action))

(defmethod node-segment ((obj incudine:node) end dur
                         &optional start curve (done-action #'identity))
  (declare #.*standard-optimize-settings*
           #.*reduce-warnings*)
  (cond ((or (incudine::null-item-p obj)
             (incudine:node-release-phase-p obj))
         obj)
        ((or incudine::*node-enable-gain-p*
             (incudine::node-enable-gain-p obj))
         (with-node-segment-symbols obj
             (level start0 end0 curve0 grow a2 b1 y1 y2)
           (when curve
             (setf curve0 (incudine::seg-function-spec->sample curve)))
           (let* ((samples (max 1 (sample->fixnum (* dur *sample-rate*))))
                  (remain samples)
                  (curve (or curve (incudine::sample->seg-function-spec curve0))))
             (declare (type non-negative-fixnum samples remain))
             (setf level (incudine::envelope-fix-zero level curve))
             (setf start0 (if start
                              (incudine::envelope-fix-zero start curve)
                              level))
             (setf end0 (incudine::envelope-fix-zero end curve))
             (when (eq done-action #'incudine:free)
               (setf (incudine:node-release-phase-p obj) t))
             (%node-segment-init start0 end0 samples curve0 grow a2 b1 y1 y2)
             (setf (incudine::node-current-function obj)
                   (lambda (chan)
                     (declare (type non-negative-fixnum chan))
                     (cond ((plusp remain)
                            (when (zerop chan)
                              (%segment-update-level level curve0 grow a2 b1 y1 y2)
                              (decf remain))
                            (funcall (incudine::node-function obj) chan))
                           (t (funcall (setf (incudine::node-current-function obj)
                                             (incudine::node-function obj))
                                       chan)
                              (funcall done-action obj)))
                     (values)))
             obj)))
        (t (funcall done-action obj))))

(defmethod node-segment ((obj integer) end dur
                         &optional start curve (done-action #'identity))
  (node-segment (incudine:node obj) end dur start curve done-action))

(defgeneric fade-in (obj &optional duration curve))

(defmethod fade-in ((obj incudine:node) &optional duration curve)
  (setf (incudine:gain obj) +sample-zero+)
  (node-segment obj (coerce 1.0 'sample) (or duration (incudine:fade-time obj))
                +sample-zero+ curve #'identity))

(defmethod fade-in ((obj integer) &optional duration curve)
  (fade-in (incudine:node obj) duration curve))

(defgeneric fade-out (obj &optional duration curve))

(defmethod fade-out ((obj incudine:node) &optional duration curve)
  (node-segment obj +sample-zero+ (or duration (incudine:fade-time obj))
                nil curve #'incudine:free))

(defmethod fade-out ((obj integer) &optional duration curve)
  (fade-out (incudine:node obj) duration curve))
