(in-package :cudere-clm-tests)

;;; OSCIL

(deftest oscil.methods
    (incudine:with-cleanup
      (let ((os (make-oscil 123))
            (acc nil))
        (flet ((add-test (res)
                 (push (equal (list (round (mus-frequency os))
                                    (floor (* (mus-phase os) 1000))
                                    (mus-length os)
                                    (round (radians->hz (mus-increment os))))
                              res)
                       acc)))
          (add-test '(123 0 1 123))
          (loop repeat 101 do (oscil os))
          (setf (mus-frequency os) 321)
          (add-test '(321 1626 1 321))
          (setf (mus-increment os) (hz->radians 12345))
          (setf (mus-phase os) 2.468d0)
          (add-test '(12345 2468 1 12345))
          (mus-reset os)
          (push (zerop (mus-phase os)) acc)
          (oscil os 0 two-pi)
          (push (zerop (- (mus-phase os) (mus-increment os))) acc)
          (mus-reset os)
          (oscil os two-pi)
          (push (= (- (mus-phase os) (mus-increment os)) two-pi) acc)
          (every #'identity acc))))
  T)

(deftest oscil.1
    (with-sound-test ()
      (let ((os (make-oscil 440)))
        (dotimes (i *test-length*)
          (outa i (* .8 (oscil os))))))
    #(145 117 149 183 28 61 207 223 180 128 13 44 86 167 147 101))

(deftest oscil.2
    (with-sound-test ()
      (let ((os (make-oscil 0)))
        (dotimes (i *test-length*)
          (outa i (* .8 (oscil os (hz->radians
                                    (* (/ i *test-length*) 20000))))))))
  #(103 172 5 158 92 211 188 75 145 83 9 63 122 170 215 77))

(deftest oscil.3
    (with-sound-test ()
      (let ((os (make-oscil 1 (* pi .25))))
        (dotimes (i (min (floor *srate*) *test-length*))
          (outa i (oscil os)))))
  #(126 56 160 28 136 117 248 233 31 21 91 104 138 202 8 91))

;;; ENV

(deftest env.1
    (with-sound-test ()
      (let ((e (make-env '(0 0 1 1 2 0) :end (1- *test-length*))))
        (dotimes (i *test-length*) (outa i (env e)))))
  #(149 251 9 59 96 194 16 49 123 189 198 141 232 199 22 130))

(deftest env.2
    (with-sound-test ()
      (let* ((amp .8d0)
             (base 7d0)
             (e (make-env '(0 0 1 1 10 0) amp :length *test-length*
                          :base base)))
        (assert (= (mus-scaler e) amp))
        (assert (= (mus-increment e) base))
        (dotimes (i *test-length*) (outa i (env e)))))
  #(26 53 83 173 86 104 110 113 193 157 180 122 109 170 138 130))

(deftest env.3
    (with-sound-test ()
      (let* ((lst '(0 0 1 .5 2 0))
             (dur .25)
             (offset .3d0)
             (e (make-env lst :duration dur :offset offset))
             (len (floor (* dur *srate*))))
        (assert (= (mus-length e) len))
        (assert (= (mus-offset e) offset))
        (assert (equal (mus-data e) lst))
        (dotimes (i *test-length*)
          (when (>= (mus-location e) len)
            (mus-reset e))
          (outa i (env e)))))
  #(158 211 128 57 54 46 1 44 196 103 119 108 94 71 211 40))

;;; TABLE-LOOKUP

(deftest table-lookup.1
    (with-sound-test ()
      (let* ((freq 440)
             (tab (make-table-lookup freq
                    :wave (partials->wave '(1 1 2 .5 3 .25))))
             (lim (ash *test-length* -1)))
        (assert (= (mus-increment tab)
                   (double (* freq (/ (mus-length tab) *srate*)))))
        (assert (= (round (mus-frequency tab)) freq))
        (assert (= (mus-interp-type tab) mus-interp-linear))
        (dotimes (i *test-length*)
          (unless (< i lim)
            (setf lim *test-length*)
            (setf (mus-frequency tab) (* freq 2)))
          (outa i (* .8 (table-lookup tab))))))
  #(211 206 84 170 213 147 10 158 49 94 129 150 74 65 188 201))

(deftest table-lookup.2
    (with-sound-test ()
      (let* ((*clm-table-size* 2048)
             (tab (make-table-lookup 440
                   :wave (partials->wave '(1 1 3 .5 5 .25 7 .125))
                   :type mus-interp-hermite)))
        (assert (= (mus-interp-type tab) mus-interp-hermite))
        (assert (= (mus-length tab) *clm-table-size*))
        (dotimes (i *test-length*)
          (outa i (* .8 (table-lookup tab))))))
  #(158 127 152 25 62 221 8 28 40 222 170 138 74 240 123 66))

(deftest table-lookup.3
    (with-sound-test ()
      (let ((tab (make-table-lookup 0
                   :wave (partials->wave '(1 1 2 1)))))
        (dotimes (i *test-length*)
          (outa i (* .8 (table-lookup tab
                          (hz->radians (* (/ i *test-length*) 10000))))))))
  #(11 74 106 147 106 196 41 107 96 45 209 153 140 159 108 244))

;;; POLYWAVE

(deftest polywave.1
    (with-sound-test ()
      (let ((w (make-polywave 261 '(1 1 2 .5 3 .25))))
        (assert (= (mus-length w) 4))
        (assert (equal (coerce (mus-data w) 'list) '(0d0 1d0 .5d0 .25d0)))
        (dotimes (i *test-length*)
          (outa i (* .5 (polywave w))))))
  #(209 150 4 33 34 70 176 5 48 22 110 141 121 173 116 247))

(deftest polywave.2
    (with-sound-test ()
      (let ((w (make-polywave 0 '(1 1 2 1 3 1))))
        (dotimes (i *test-length*)
          (outa i (* .25 (polywave w
                          (hz->radians (* (/ i *test-length*) 10000))))))))
  #(190 239 97 100 42 31 35 15 164 26 241 223 152 82 18 116))

;;; POLYSHAPE

(deftest polyshape.1
    (with-sound-test ()
      (let ((w (make-polyshape 261 :partials '(1 1 2 .5 3 .25))))
        (assert (= (mus-length w) 4))
        (assert (equal (coerce (mus-data w) 'list) '(0d0 1d0 .5d0 .25d0)))
        (dotimes (i *test-length*)
          (outa i (* .5 (polyshape w))))))
  #(209 150 4 33 34 70 176 5 48 22 110 141 121 173 116 247))

(deftest polyshape.2
    (with-sound-test ()
      (let ((w (make-polyshape 440 :partials '(1 1 2 .5 3 .25))))
        (dotimes (i *test-length*)
          (outa i (* .25 (polyshape w (/ i *test-length*)))))))
  #(210 128 220 121 34 85 75 171 34 114 242 105 150 223 56 21))

;;; TRIANGLE-WAVE

(deftest triangle-wave.1
    (with-sound-test ()
      (let ((s (make-triangle-wave 440 .8)))
        (dotimes (i *test-length*)
          (outa i (triangle-wave s)))))
  #(36 76 37 193 208 124 223 132 188 214 0 23 204 236 126 254))

(deftest triangle-wave.2
    (with-sound-test ()
      (let ((s (make-triangle-wave 0 .8)))
        (dotimes (i *test-length*)
          (outa i (triangle-wave s
                    (hz->radians (* (/ i *test-length*) 2000)))))))
  #(27 146 114 203 245 41 71 56 101 225 206 211 23 255 16 30))

;;; SQUARE-WAVE

(deftest square-wave.1
    (with-sound-test ()
      (let* ((amp .8)
             (s (make-square-wave 440 (* amp 2))))
        (dotimes (i *test-length*)
          (outa i (- (square-wave s) amp)))))
  #(144 49 127 128 55 102 211 255 230 30 63 63 123 95 0 154))

(deftest square-wave.2
    (with-sound-test ()
      (let* ((amp .8)
             (s (make-square-wave 0 (* amp 2))))
        (dotimes (i *test-length*)
          (outa i (- (square-wave s (hz->radians (* (/ i *test-length*) 2000)))
                     amp)))))
  #(156 85 90 7 238 69 95 244 147 112 74 223 230 13 68 147))

;;; SAWTOOTH-WAVE

(deftest sawtooth-wave.1
    (with-sound-test ()
      (let ((s (make-sawtooth-wave 440 .8))
            (half (ash *test-length* -1)))
        (dotimes (i *test-length*)
          (when (= i half)
            (setf (mus-frequency s) -440))
          (outa i (sawtooth-wave s)))))
  #(188 126 82 24 128 194 217 57 20 51 54 182 39 157 152 117))

(deftest sawtooth-wave.2
    (with-sound-test ()
      (let ((s (make-sawtooth-wave 0 .8)))
        (dotimes (i *test-length*)
          (outa i (sawtooth-wave s
                    (hz->radians (* (/ i *test-length*) 2000)))))))
  #(225 90 109 20 41 83 74 191 36 244 181 206 227 187 169 37))

;;; PULSE-TRAIN

(deftest pulse-train.1
    (with-sound-test ()
      (let ((s (make-pulse-train 100 .8)))
        (dotimes (i *test-length*)
          (outa i (pulse-train s)))))
  #(127 77 144 210 173 122 58 64 59 83 11 2 49 127 187 134))

(deftest pulse-train.2
    (with-sound-test ()
      (let ((s (make-pulse-train 0 .8)))
        (dotimes (i *test-length*)
          (outa i (pulse-train s
                    (hz->radians (* (/ i *test-length*) 100)))))))
  #(17 217 157 49 94 32 193 73 168 18 89 140 26 144 5 128))

;;; NCOS

(deftest ncos.1
    (with-sound-test ()
      (let* ((cs (make-ncos 123 10))
             (l1 (ash *test-length* -1))
             (l2 (+ l1 (ash l1 -1))))
        (dotimes (i *test-length*)
          (when (= i l1)
            (setf (mus-length cs) 20))
          (when (= i l2)
            (setf (mus-frequency cs) 50)
            (setf (mus-scaler cs) 1/40))
          (outa i (ncos cs)))))
  #(84 173 13 178 195 52 72 79 108 85 168 22 153 95 247 214))

(deftest ncos.2
    (with-sound-test ()
      (let* ((cs (make-ncos 100 5)))
        (dotimes (i *test-length*)
          (outa i (ncos cs
                    (hz->radians (* (/ i *test-length*) 4000)))))))
  #(28 29 134 152 159 37 39 210 212 14 49 198 151 200 174 190))

;;; NSIN

(deftest nsin.1
    (with-sound-test ()
      (let* ((cs (make-nsin 123 10))
             (l1 (ash *test-length* -1))
             (l2 (+ l1 (ash l1 -1))))
        (dotimes (i *test-length*)
          (when (= i l1)
            (setf (mus-length cs) 20))
          (when (= i l2)
            (setf (mus-frequency cs) 50)
            (setf (mus-scaler cs) 1/40))
          (outa i (nsin cs)))))
  #(35 205 31 176 19 160 222 67 127 37 130 218 6 83 15 58))

(deftest nsin.2
    (with-sound-test ()
      (let* ((cs (make-nsin 100 5)))
        (dotimes (i *test-length*)
          (outa i (nsin cs
                    (hz->radians (* (/ i *test-length*) 4000)))))))
  #(235 30 202 104 178 161 105 99 242 95 90 36 89 171 73 17))

;;; WAVE-TRAIN

(deftest wave-train.1
    (with-sound-test ()
      (let* ((tab (make-double-array 64))
             (cs (make-ncos 400 10))
             (w (make-wave-train 220 :wave tab)))
        (setf (mus-phase cs) (* -.5 pi))
        (dotimes (i 64)
          (setf (aref tab i)
                (* (cos (- (* i 1/64 pi) (* .5 pi))) (ncos cs))))
        (dotimes (i *test-length*)
          (outa i (wave-train w)))))
  #(241 37 7 216 113 117 195 249 1 163 236 6 47 195 40 220))

(deftest wave-train.2
    (with-sound-test ()
      (let* ((tab (make-double-array 64))
             (cs (make-ncos 400 10))
             (w (make-wave-train 53 :wave tab)))
        (setf (mus-phase cs) (* -.5 pi))
        (dotimes (i 64)
          (setf (aref tab i)
                (* (cos (- (* i 1/64 pi) (* .5 pi))) (ncos cs))))
        (dotimes (i *test-length*)
          (outa i (* .5 (wave-train w
                          (hz->radians (* (/ i *test-length*) 5000))))))))
  #(194 36 86 126 197 78 79 21 135 180 22 254 206 177 86 184))

;;; RAND

(deftest rand.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((r (make-rand 100))
            (half (ash *test-length* -1)))
        (dotimes (i *test-length*)
          (when (= i half)
            (setf (mus-frequency r) 13)
            (mus-reset r))
          (outa i (rand r)))))
  #(168 168 62 173 131 103 164 196 41 78 207 71 3 54 87 83))

(deftest rand.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((r (make-rand 100 .8 :envelope '(-.5 0 1 1)))
            (half (ash *test-length* -1)))
        (dotimes (i *test-length*)
          (when (= i half)
            (setf (mus-data r) nil)
            (setf (mus-increment r) (hz->radians 8))
            (setf (mus-scaler r) 1))
          (outa i (rand r)))))
  #(74 119 65 220 59 97 34 167 170 208 6 49 16 219 71 110))

;;; RAND-INTERP

(deftest rand-interp.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((r (make-rand-interp 100))
            (half (ash *test-length* -1)))
        (dotimes (i *test-length*)
          (when (= i half)
            (setf (mus-frequency r) 13)
            (mus-reset r))
          (outa i (rand-interp r)))))
  #(252 54 120 27 139 177 39 16 255 191 125 19 221 118 145 71))

(deftest rand-interp.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((r (make-rand-interp 100 .8 :envelope '(-.5 0 1 1)))
            (half (ash *test-length* -1)))
        (dotimes (i *test-length*)
          (when (= i half)
            (setf (mus-data r) nil)
            (setf (mus-increment r) (hz->radians 8))
            (setf (mus-scaler r) 1))
          (outa i (rand-interp r)))))
  #(47 201 216 157 13 157 125 157 2 94 183 96 216 49 166 96))

;;; ONE-POLE

(deftest one-pole.1
    (with-sound-test ()
      (let ((f (make-one-pole .6 -.9))
            (cs (make-ncos 20 1000))
            (k (/ *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-ycoeff f 1) (* -.9999 i k))
          (outa i (one-pole f (ncos cs))))))
  #(174 148 186 174 184 169 108 242 60 148 231 202 113 30 64 182))

;;; ONE-ZERO

(deftest one-zero.1
    (with-sound-test ()
      (let ((f (make-one-zero .5 .5))
            (cs (make-ncos 100 200)))
        (dotimes (i *test-length*)
          (outa i (one-zero f (ncos cs))))))
  #(218 108 74 41 183 220 184 223 13 117 228 152 116 83 212 207))

;;; TWO-POLE

(deftest two-pole.1
    (with-sound-test ()
      (let ((f (make-two-pole :frequency 2000 :radius .999))
            (cs (make-ncos 10 2000)))
        (dotimes (i *test-length*)
          (outa i (* .15 (two-pole f (ncos cs)))))))
  #(184 63 120 205 241 82 59 20 239 81 116 170 104 143 130 126))

(deftest two-pole.2
    (with-sound-test ()
      (let ((f (make-two-pole :frequency 20 :radius .999))
            (cs (make-ncos 10 2000))
            (k (/ 5000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* (* 6e-5 i k)
                     (two-pole f (ncos cs)))))))
  #(133 154 102 97 129 19 146 182 72 6 25 34 149 46 124 248))

;;; TWO-ZERO

(deftest two-zero.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-two-zero :frequency 2000 :radius .999))
            (cs (make-rand (* .5 *srate*))))
        (assert (= (round (mus-frequency f)) 2000))
        (dotimes (i *test-length*)
          (outa i (* .15 (two-zero f (rand cs)))))))
  #(24 152 240 54 23 248 54 132 162 113 215 66 128 220 201 69))

(deftest two-zero.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-two-zero :frequency 20 :radius .999))
            (cs (make-rand (* .5 *srate*)))
            (k (/ 20000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* .25 (two-zero f (rand cs)))))))
  #(30 135 14 49 121 233 226 87 18 242 196 246 73 170 150 0))

;;; FORMANT

(deftest formant.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-formant 1234 .95))
            (cs (make-rand (* .5 *srate*))))
        (dotimes (i *test-length*)
          (outa i (formant f (rand cs))))))
  #(170 52 240 252 20 235 253 236 106 149 251 241 29 80 144 134))

(deftest formant.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-formant 20 .999))
            (cs (make-rand (* .5 *srate*)))
            (k (/ 20000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* 8 (formant f (rand cs)))))))
  #(128 106 99 159 31 118 15 252 148 220 66 68 110 79 102 182))

;;; FIRMANT

(deftest firmant.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-firmant 1234 .95))
            (cs (make-rand (* .5 *srate*))))
        (dotimes (i *test-length*)
          (outa i (firmant f (rand cs))))))
  #(58 214 29 116 74 222 138 19 218 125 17 47 118 24 66 50))

(deftest firmant.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-firmant 20 .999))
            (cs (make-rand (* .5 *srate*)))
            (k (/ 20000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* 8 (firmant f (rand cs)))))))
  #(76 68 58 23 16 164 226 198 184 11 138 18 107 64 50 55))

;;; CONVOLVE-FILES and READIN

(deftest convolve-files.1
    (progn
      (with-sound-test (:output *test-filter-file*)
        (outa 0 1d0)
        (outa 30000 0.5d0)
        (outa 80000 .25d0))
      (with-sound-test (:output *test-input-file*)
        (let ((os (make-oscil 440)))
          (dotimes (i 10000) (outa i (oscil os)))))
       (convolve-files *test-input-file* *test-filter-file* 1
                       *test-tmpfile-name*)
       (with-sound-test ()
         (let ((rd (make-readin *test-tmpfile-name*)))
           (dotimes (i *test-length*)
             (outa i (readin rd))))))
  #(29 152 233 110 54 120 174 233 9 27 96 185 37 239 23 125))
