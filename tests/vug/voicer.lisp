(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dsp! voicer-test-1 (freq amp pos gate)
    (foreach-channel
      (cout (pan2 (* (envelope (make-adsr .01f0 .09f0 .8f0 .4f0) gate 1 #'free)
                     (sine freq amp))
                  pos)))))

(defvar *voi* (voicer:create 4 (voicer-test-1 440 .2d0 .5 1)))

(define-constant +voicer-tag+ 123)

(pushnew (lambda ()
           (voicer:panic *voi*)
           (voicer:remove-all-maps *voi*)
           (setf (voicer:polyphony *voi*) 4)
           (setf (voicer:steal-voice-mode *voi*) nil))
         *test-hook*)

(defun voicer-event-1 (freq amp pos)
  (voicer:trigger *voi* +voicer-tag+ :freq freq :amp amp :pos pos))

(defun polyphony-test-1 ()
  (setf (voicer:polyphony *voi*) 4)
  (loop with amps = #(.1f0 .3f0 .05f0 .2f0) and space = #(.1f0 .9f0 .3f0 .7f0)
        for i from 0
        for time from 0 by .25
        for freq from 250 to 2000 by 125
        for amp = (svref amps (logand i 3))
        for pos = (svref space (logand i 3))
        do (at #[time s] #'voicer-event-1 freq amp pos)))

(deftest voicer.1
    (incudine:with-nrt (2 *sample-rate*)
      (voicer:panic *voi*)
      (voicer:remove-all-maps *voi*)
      (voicer:set-controls *voi* :freq 1234 :amp .25 :pos .5)
      (setf (voicer:control-value *voi* 'pos) .88)
      (setf (voicer:polyphony *voi*) 100)
      ;; CONTROL-NAMES and CONTROL-LIST return a fresh list.
      (setf (nth 1 (voicer:control-names *voi*)) 'bug)
      (setf (nth 1 (voicer:control-list *voi*)) 12345)
      (values (voicer:polyphony *voi*)
              (voicer:control-names *voi*)
              (voicer:control-list *voi*)
              (voicer:control-value *voi* 'freq)
              (voicer:control-value *voi* 'amp)
              (voicer:control-value *voi* 'pos)))
  100 (FREQ AMP POS GATE) (1234 0.25 0.88 1) 1234 0.25 0.88)

(deftest voicer.2
    (incudine:with-nrt (2 *sample-rate*)
      (flet ((voicer-maps ()
               (hash-table-count (voicer::voicer-argument-maps *voi*))))
        (voicer:remove-all-maps *voi*)
        (voicer:define-map voicer-map-1 *voi* (freq amp pos)
          (setf freq (* freq 2) amp (* amp 1.5) pos (- 1 pos)))
        (and (= (voicer-maps) 1)
             (zerop (progn (voicer:remove-map *voi* 'voicer-map-1)
                           (voicer-maps))))))
  t)

(with-dsp-test (voicer.3 :channels 2
      :md5 #(245 31 91 236 31 234 232 232 210 105 241 185 136 0 21 199))
  (voicer:panic *voi*)
  (voicer:remove-all-maps *voi*)
  (setf (voicer:steal-voice-mode *voi*) :first)
  (polyphony-test-1))

(with-dsp-test (voicer.4 :channels 2
      :md5 #(113 223 213 167 107 12 237 99 201 45 13 133 240 171 101 251))
  (voicer:panic *voi*)
  (voicer:remove-all-maps *voi*)
  (setf (voicer:steal-voice-mode *voi*) :last)
  (polyphony-test-1))

(with-dsp-test (voicer.5 :channels 2
      :md5 #(15 88 4 90 34 98 49 205 155 50 251 70 169 98 161 232))
  (voicer:panic *voi*)
  (voicer:remove-all-maps *voi*)
  (setf (voicer:steal-voice-mode *voi*) nil)
  (assert (voicer:empty-p *voi*))
  (at #[4 s] (lambda () (assert (voicer:full-p *voi*))))
  (polyphony-test-1))

(with-dsp-test (voicer.6 :channels 2
      :md5 #(173 146 241 1 161 44 159 183 77 33 93 152 86 255 199 104))
  (voicer:panic *voi*)
  (voicer:remove-all-maps *voi*)
  (setf (voicer:steal-voice-mode *voi*) nil)
  (polyphony-test-1)
  (loop for time from 0 below 5 by .625 do
          (at #[time s] #'voicer:release *voi* +voicer-tag+)))

(with-dsp-test (voicer.7 :channels 2
      :md5 #(153 106 242 72 203 136 40 3 255 59 67 240 177 109 201 227))
  (voicer:panic *voi*)
  (setf (voicer:steal-voice-mode *voi*) :first)
  (voicer:remove-all-maps *voi*)
  (voicer:define-map voicer-map-1 *voi* (freq amp pos)
    (setf freq (* freq 2) amp (* amp 1.5f0) pos (- 1 pos)))
  (polyphony-test-1))
