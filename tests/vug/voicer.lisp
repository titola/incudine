(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dsp! voicer-test-1 (freq amp pos gate)
    (:defaults 1000 .5 .5 1)
    (foreach-channel
      (cout (pan2 (* (envelope (make-adsr .01f0 .09f0 .8f0 .4f0) gate 1 #'free)
                     (sine freq amp))
                  pos))))
  (dsp! voicer-test-2 (freq amp pos gate)
    (:defaults 1000 .5 .5 1)
    (foreach-channel
      (cout (pan2 (* (envelope (make-adsr .01f0 .09f0 .8f0 .4f0) gate 1 #'free)
                     (butter-lp (noise-test amp) freq))
                  pos)))))

(defvar *voi1* (voicer:create 4 (voicer-test-1 440 .2d0 .5 1)))
(defvar *voi2* (voicer:create 4 (voicer-test-1 2000 :pos .2 :head 100)))
(defvar *voi3* (voicer:create 9 (voicer-test-1 440 1 .5 1)
                              :dsp-function-keyword :func))
;; Voicer without control parameter GATE, therefore TRIGGER:RELEASE
;; calls FREE NODE instead of setting GATE to 0.
(defvar *voi4* (voicer:create 9 (voicer-test-1 440 1 .5)
                              :dsp-function-keyword :func))

(define-constant +voicer-tag+ 123)

(pushnew (lambda ()
           (voicer:panic *voi1*)
           (voicer:remove-all-maps *voi1*)
           (setf (voicer:polyphony *voi1*) 4)
           (setf (voicer:steal-voice-mode *voi1*) nil))
         *test-hook*)

(defun voicer-event-1 (freq amp pos)
  (voicer:trigger *voi1* +voicer-tag+ :freq freq :amp amp :pos pos))

(defun polyphony-test-1 ()
  (setf (voicer:polyphony *voi1*) 4)
  (loop with amps = #(.1f0 .3f0 .05f0 .2f0) and space = #(.1f0 .9f0 .3f0 .7f0)
        for i from 0
        for time from 0 by .25
        for freq from 250 to 2000 by 125
        for amp = (svref amps (logand i 3))
        for pos = (svref space (logand i 3))
        do (at #[time s] #'voicer-event-1 freq amp pos)))

(deftest voicer.1
    (incudine:with-nrt (2 *sample-rate*)
      (voicer:panic *voi1*)
      (voicer:remove-all-maps *voi1*)
      (voicer:set-controls *voi1* :freq 1234 :amp .25 :pos .5)
      (setf (voicer:control-value *voi1* 'pos) .88)
      (setf (voicer:polyphony *voi1*) 100)
      ;; CONTROL-NAMES and CONTROL-LIST return a fresh list.
      (setf (nth 1 (voicer:control-names *voi1*)) 'bug)
      (setf (nth 1 (voicer:control-list *voi1*)) 12345)
      (values (voicer:polyphony *voi1*)
              (voicer:control-names *voi1*)
              (voicer:control-list *voi1*)
              (voicer:control-value *voi1* 'freq)
              (voicer:control-value *voi1* 'amp)
              (voicer:control-value *voi1* 'pos)))
  100 (FREQ AMP POS GATE) (1234 0.25 0.88 1) 1234 0.25 0.88)

(deftest voicer.2
    (incudine:with-nrt (2 *sample-rate*)
      (flet ((voicer-maps ()
               (hash-table-count (voicer::voicer-argument-maps *voi1*))))
        (voicer:remove-all-maps *voi1*)
        (voicer:define-map voicer-map-1 *voi1* (freq amp pos)
          (setf freq (* freq 2) amp (* amp 1.5) pos (- 1 pos)))
        (and (= (voicer-maps) 1)
             (zerop (progn (voicer:remove-map *voi1* 'voicer-map-1)
                           (voicer-maps))))))
  t)

(with-dsp-test (voicer.3 :channels 2
      :md5 #(245 31 91 236 31 234 232 232 210 105 241 185 136 0 21 199))
  (voicer:panic *voi1*)
  (voicer:remove-all-maps *voi1*)
  (setf (voicer:steal-voice-mode *voi1*) :first)
  (polyphony-test-1))

(with-dsp-test (voicer.4 :channels 2
      :md5 #(113 223 213 167 107 12 237 99 201 45 13 133 240 171 101 251))
  (voicer:panic *voi1*)
  (voicer:remove-all-maps *voi1*)
  (setf (voicer:steal-voice-mode *voi1*) :last)
  (polyphony-test-1))

(with-dsp-test (voicer.5 :channels 2
      :md5 #(15 88 4 90 34 98 49 205 155 50 251 70 169 98 161 232))
  (voicer:panic *voi1*)
  (voicer:remove-all-maps *voi1*)
  (setf (voicer:steal-voice-mode *voi1*) nil)
  (assert (voicer:empty-p *voi1*))
  (at #[4 s] (lambda () (assert (voicer:full-p *voi1*))))
  (polyphony-test-1))

(with-dsp-test (voicer.6 :channels 2
      :md5 #(173 146 241 1 161 44 159 183 77 33 93 152 86 255 199 104))
  (voicer:panic *voi1*)
  (voicer:remove-all-maps *voi1*)
  (setf (voicer:steal-voice-mode *voi1*) nil)
  (polyphony-test-1)
  (loop for time from 0 below 5 by .625 do
          (at #[time s] #'voicer:release *voi1* +voicer-tag+)))

(with-dsp-test (voicer.7 :channels 2
      :md5 #(153 106 242 72 203 136 40 3 255 59 67 240 177 109 201 227))
  (voicer:panic *voi1*)
  (setf (voicer:steal-voice-mode *voi1*) :first)
  (voicer:remove-all-maps *voi1*)
  (voicer:define-map voicer-map-1 *voi1* (freq amp pos)
    (setf freq (* freq 2) amp (* amp 1.5f0) pos (- 1 pos)))
  (polyphony-test-1))

(deftest voicer.8
    (incudine:with-nrt (2 *sample-rate*)
      (let ((V *voi2*)
            (res nil))
        (make-group 3210)
        (make-group 43210)
        (voicer:panic V)
        (voicer:trigger V 'test :amp 'is-not-a-voicer-control :pos .8 :head 43210)
        (dograph (n) (push (node-id n) res))
        (voicer:release V 'test)
        (dograph (n) (push (node-id n) res))
        (values
          res
          (voicer:control-names V)
          (voicer:control-list V)
          (voicer:control-value V :head))))
  (3210   43210 0
   3210 1 43210 0)
  (FREQ POS INCUDINE.VUG::HEAD)
  (2000 0.8 43210)
  43210)

(with-dsp-test (voicer.9 :channels 2
      :md5 #(1 186 27 123 45 6 178 199 169 196 199 207 129 191 71 188))
  (voicer:set-controls *voi3*
    :func #'voicer-test-1 :freq 440 :amp .2 :pos .2 :gate 1)
  (voicer:trigger *voi3* 'test)
  (at #[1 s] #'voicer:trigger *voi3* 'test :func #'voicer-test-2 :freq 200 :amp 2)
  (at #[2 s] #'voicer:trigger *voi3* 'test :amp .5
    :func (lambda (freq amp pos gate &key action)
            ;; TRIGGER:RELEASE sets GATE to 0 but it doesn't work here,
            ;; because this function is not part of a DSP instance.
            (play (lambda ()
                    (if (> gate 0) ; never 0 from VOICER:RELEASE
                        (let ((x (sample (if (> freq 200) (1- amp) amp))))
                          (incf (audio-out 0) (* (1- pos) x))
                          (incf (audio-out 1) (* pos x)))))
                  ;; The :ACTION keyword argument is required
                  ;; to set the node for the voicer.
                  :action action)))
  (at #[3 s] #'voicer:release *voi3* 'test)
  (at #[3.5 s] #'voicer:release *voi3* 'test)
  (at #[4 s] #'voicer:trigger *voi3* 'test
    :func (lambda (freq amp pos gate &key action)
            (declare (ignore freq amp pos))
            (let ((i -1))
              (declare (fixnum i))
              (play (lambda ()
                      (if (> gate 0) ; never 0 from VOICER:RELEASE
                          (incf (audio-out 0) (* (incf i) *sample-duration*))))
                    :action action))))
  ;; GATE 0 ignored by PLAY FUNCTION.
  (at #[4.5 s] (lambda () (dotimes (i 2) (voicer:release *voi3* 'test)))))

(with-dsp-test (voicer.10 :channels 2
      :md5 #(73 86 84 19 108 153 205 26 2 39 70 30 64 165 221 223))
  (voicer:set-controls *voi4*
    :func #'voicer-test-1 :freq 440 :amp .2 :pos .2)
  (voicer:trigger *voi4* 'test)
  (at #[1 s] #'voicer:trigger *voi4* 'test :func #'voicer-test-2 :freq 200 :amp 2)
  (at #[2 s] #'voicer:trigger *voi4* 'test :amp .5
    :func (lambda (freq amp pos &key action)
            (play (lambda ()
                    (let ((x (sample (if (> freq 200) (1- amp) amp))))
                      (incf (audio-out 0) (* (1- pos) x))
                      (incf (audio-out 1) (* pos x))))
                  :action action)))
  (at #[3 s] #'voicer:release *voi4* 'test)
  (at #[3.5 s] #'voicer:release *voi4* 'test)
  (at #[4 s] #'voicer:trigger *voi4* 'test
    :func (lambda (freq amp pos &key action)
            (declare (ignore freq amp pos))
            (let ((i -1))
              (declare (fixnum i))
              (play (lambda ()
                      (incf (audio-out 0) (* (incf i) *sample-duration*)))
                    :action action))))
  ;; FREE NODE from VOICER:RELEASE because GATE is not a control parameter.
  (at #[4.5 s] (lambda () (dotimes (i 2) (voicer:release *voi4* 'test)))))
