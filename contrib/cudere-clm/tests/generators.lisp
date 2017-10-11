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
  #(106 252 29 148 249 14 247 24 145 143 147 46 83 121 114 153))

(deftest oscil.2
    (with-sound-test ()
      (let ((os (make-oscil 0)))
        (dotimes (i *test-length*)
          (outa i (* .8 (oscil os (hz->radians
                                    (* (/ i *test-length*) 20000))))))))
  #(232 104 214 255 251 27 25 134 220 98 177 90 194 173 164 151))

(deftest oscil.3
    (with-sound-test ()
      (let ((os (make-oscil 1 (* pi .25))))
        (dotimes (i (min (floor *srate*) *test-length*))
          (outa i (oscil os)))))
  #(37 177 232 103 52 151 227 51 55 154 237 196 15 163 123 132))

;;; ENV

(deftest env.1
    (with-sound-test ()
      (let ((e (make-env '(0 0 1 1 2 0) :end (1- *test-length*))))
        (dotimes (i *test-length*) (outa i (env e)))))
  #(245 164 30 11 187 83 119 168 91 3 247 74 146 187 66 109))

(deftest env.2
    (with-sound-test ()
      (let* ((amp .8d0)
             (base 7d0)
             (e (make-env '(0 0 1 1 10 0) amp :length *test-length*
                          :base base)))
        (assert (= (mus-scaler e) amp))
        (assert (= (mus-increment e) base))
        (dotimes (i *test-length*) (outa i (env e)))))
  #(0 22 172 137 245 117 227 55 91 173 32 30 112 206 99 217))

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
  #(159 187 152 206 44 161 235 31 9 249 59 90 192 46 87 234))

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
  #(40 58 164 206 0 192 244 2 16 54 242 204 84 90 43 35))

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
  #(156 243 171 15 170 61 182 83 106 52 129 47 20 9 213 22))

(deftest table-lookup.3
    (with-sound-test ()
      (let ((tab (make-table-lookup 0
                   :wave (partials->wave '(1 1 2 1)))))
        (dotimes (i *test-length*)
          (outa i (* .8 (table-lookup tab
                          (hz->radians (* (/ i *test-length*) 10000))))))))
  #(102 101 116 183 171 124 252 233 27 142 16 217 204 85 108 201))

;;; POLYWAVE

(deftest polywave.1
    (with-sound-test ()
      (let ((w (make-polywave 261 '(1 1 2 .5 3 .25))))
        (assert (= (mus-length w) 4))
        (assert (equal (coerce (mus-data w) 'list) '(0d0 1d0 .5d0 .25d0)))
        (dotimes (i *test-length*)
          (outa i (* .5 (polywave w))))))
  #(189 203 143 180 122 5 92 143 69 103 254 208 214 141 17 11))

(deftest polywave.2
    (with-sound-test ()
      (let ((w (make-polywave 0 '(1 1 2 1 3 1))))
        (dotimes (i *test-length*)
          (outa i (* .25 (polywave w
                          (hz->radians (* (/ i *test-length*) 10000))))))))
  #(225 162 209 196 56 87 38 234 68 30 196 242 180 233 112 169))

;;; POLYSHAPE

(deftest polyshape.1
    (with-sound-test ()
      (let ((w (make-polyshape 261 :partials '(1 1 2 .5 3 .25))))
        (assert (= (mus-length w) 4))
        (assert (equal (coerce (mus-data w) 'list) '(0d0 1d0 .5d0 .25d0)))
        (dotimes (i *test-length*)
          (outa i (* .5 (polyshape w))))))
  #(189 203 143 180 122 5 92 143 69 103 254 208 214 141 17 11))

(deftest polyshape.2
    (with-sound-test ()
      (let ((w (make-polyshape 440 :partials '(1 1 2 .5 3 .25))))
        (dotimes (i *test-length*)
          (outa i (* .25 (polyshape w (/ i *test-length*)))))))
  #(65 36 61 64 179 85 152 101 81 160 220 152 218 111 225 50))

;;; TRIANGLE-WAVE

(deftest triangle-wave.1
    (with-sound-test ()
      (let ((s (make-triangle-wave 440 .8)))
        (dotimes (i *test-length*)
          (outa i (triangle-wave s)))))
  #(124 153 65 27 94 244 236 171 202 188 27 25 196 68 8 213))

(deftest triangle-wave.2
    (with-sound-test ()
      (let ((s (make-triangle-wave 0 .8)))
        (dotimes (i *test-length*)
          (outa i (triangle-wave s
                    (hz->radians (* (/ i *test-length*) 2000)))))))
  #(196 73 148 119 197 209 175 224 49 92 172 49 254 248 39 59))

;;; SQUARE-WAVE

(deftest square-wave.1
    (with-sound-test ()
      (let* ((amp .8)
             (s (make-square-wave 440 (* amp 2))))
        (dotimes (i *test-length*)
          (outa i (- (square-wave s) amp)))))
  #(51 77 146 44 159 22 27 17 216 45 161 192 188 71 246 180))

(deftest square-wave.2
    (with-sound-test ()
      (let* ((amp .8)
             (s (make-square-wave 0 (* amp 2))))
        (dotimes (i *test-length*)
          (outa i (- (square-wave s (hz->radians (* (/ i *test-length*) 2000)))
                     amp)))))
  #(64 252 145 171 245 180 245 9 34 156 149 195 9 156 117 131))

;;; SAWTOOTH-WAVE

(deftest sawtooth-wave.1
    (with-sound-test ()
      (let ((s (make-sawtooth-wave 440 .8))
            (half (ash *test-length* -1)))
        (dotimes (i *test-length*)
          (when (= i half)
            (setf (mus-frequency s) -440))
          (outa i (sawtooth-wave s)))))
  #(70 33 21 106 252 9 115 96 6 231 107 87 198 127 227 134))

(deftest sawtooth-wave.2
    (with-sound-test ()
      (let ((s (make-sawtooth-wave 0 .8)))
        (dotimes (i *test-length*)
          (outa i (sawtooth-wave s
                    (hz->radians (* (/ i *test-length*) 2000)))))))
  #(55 74 161 173 9 160 90 11 90 121 5 5 114 74 211 57))

;;; PULSE-TRAIN

(deftest pulse-train.1
    (with-sound-test ()
      (let ((s (make-pulse-train 100 .8)))
        (dotimes (i *test-length*)
          (outa i (pulse-train s)))))
  #(22 21 180 4 232 24 226 3 60 100 4 183 85 160 117 64))

(deftest pulse-train.2
    (with-sound-test ()
      (let ((s (make-pulse-train 0 .8)))
        (dotimes (i *test-length*)
          (outa i (pulse-train s
                    (hz->radians (* (/ i *test-length*) 100)))))))
  #(236 252 137 58 121 21 2 44 198 23 21 244 118 33 116 23))

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
  #(218 189 53 159 102 157 8 96 44 9 154 217 11 174 129 218))

(deftest ncos.2
    (with-sound-test ()
      (let* ((cs (make-ncos 100 5)))
        (dotimes (i *test-length*)
          (outa i (ncos cs
                    (hz->radians (* (/ i *test-length*) 4000)))))))
  #(176 225 132 182 59 173 218 12 0 16 170 141 189 46 40 11))

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
  #(104 183 52 230 70 238 217 188 181 25 28 23 172 53 53 90))

(deftest nsin.2
    (with-sound-test ()
      (let* ((cs (make-nsin 100 5)))
        (dotimes (i *test-length*)
          (outa i (nsin cs
                    (hz->radians (* (/ i *test-length*) 4000)))))))
  #(244 62 60 74 253 102 54 192 51 95 113 102 133 12 113 135))

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
  #(185 13 3 141 52 170 248 69 179 232 238 127 132 195 8 30))

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
  #(187 99 12 144 90 138 123 90 215 21 120 5 8 122 53 15))

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
  #(178 8 138 38 173 175 231 31 131 239 72 99 215 161 107 207))

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
  #(155 202 196 205 248 78 29 169 47 207 185 220 37 246 149 181))

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
  #(83 200 110 243 186 46 92 90 252 33 177 16 24 224 0 248))

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
  #(107 104 50 10 26 181 142 57 180 69 103 11 32 5 133 42))

;;; ONE-POLE

(deftest one-pole.1
    (with-sound-test ()
      (let ((f (make-one-pole .6 -.9))
            (cs (make-ncos 20 1000))
            (k (/ *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-ycoeff f 1) (* -.9999 i k))
          (outa i (one-pole f (ncos cs))))))
  #(220 125 25 108 88 200 154 214 49 143 91 236 140 4 14 92))

;;; ONE-ZERO

(deftest one-zero.1
    (with-sound-test ()
      (let ((f (make-one-zero .5 .5))
            (cs (make-ncos 100 200)))
        (dotimes (i *test-length*)
          (outa i (one-zero f (ncos cs))))))
  #(188 247 160 1 101 94 26 211 211 184 204 67 14 38 155 115))

;;; TWO-POLE

(deftest two-pole.1
    (with-sound-test ()
      (let ((f (make-two-pole :frequency 2000 :radius .999))
            (cs (make-ncos 10 2000)))
        (dotimes (i *test-length*)
          (outa i (* .15 (two-pole f (ncos cs)))))))
  #(184 49 143 116 66 183 157 233 49 118 54 138 162 118 39 106))

(deftest two-pole.2
    (with-sound-test ()
      (let ((f (make-two-pole :frequency 20 :radius .999))
            (cs (make-ncos 10 2000))
            (k (/ 5000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* (* 6e-5 i k)
                     (two-pole f (ncos cs)))))))
  #(147 62 199 251 50 238 45 55 165 208 135 123 147 158 100 19))

;;; TWO-ZERO

(deftest two-zero.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-two-zero :frequency 2000 :radius .999))
            (cs (make-rand (* .5 *srate*))))
        (assert (= (round (mus-frequency f)) 2000))
        (dotimes (i *test-length*)
          (outa i (* .15 (two-zero f (rand cs)))))))
  #(4 15 131 249 156 32 67 124 43 21 64 8 70 42 81 150))

(deftest two-zero.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-two-zero :frequency 20 :radius .999))
            (cs (make-rand (* .5 *srate*)))
            (k (/ 20000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* .25 (two-zero f (rand cs)))))))
  #(137 23 163 116 38 110 46 66 18 232 48 24 112 141 223 13))

;;; FORMANT

(deftest formant.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-formant 1234 .95))
            (cs (make-rand (* .5 *srate*))))
        (dotimes (i *test-length*)
          (outa i (formant f (rand cs))))))
  #(72 71 196 243 70 149 134 238 212 211 243 76 135 31 252 244))

(deftest formant.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-formant 20 .999))
            (cs (make-rand (* .5 *srate*)))
            (k (/ 20000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* 8 (formant f (rand cs)))))))
  #(235 115 54 90 159 138 22 11 92 186 85 174 223 68 201 208))

;;; FIRMANT

(deftest firmant.1
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-firmant 1234 .95))
            (cs (make-rand (* .5 *srate*))))
        (dotimes (i *test-length*)
          (outa i (firmant f (rand cs))))))
  #(208 92 203 145 251 182 135 36 65 165 205 41 47 57 149 251))

(deftest firmant.2
    (with-sound-test ()
      (mus-set-rand-seed 12345)
      (let ((f (make-firmant 20 .999))
            (cs (make-rand (* .5 *srate*)))
            (k (/ 20000.0 *test-length*)))
        (dotimes (i *test-length*)
          (setf (mus-frequency f) (+ 20 (* i k)))
          (outa i (* 8 (firmant f (rand cs)))))))
  #(80 192 143 32 37 25 155 104 147 230 196 219 153 44 234 254))

;;; CONVOLVE-FILES

(deftest convolve-files.1
    (progn
      (with-sound-test (:output *test-filter-file*)
        (outa 0 1d0)
        (outa 30000 0.5d0)
        (outa 80000 .25d0))
      (with-sound-test (:output *test-input-file*)
        (let ((os (make-oscil 440)))
          (dotimes (i 10000) (outa i (oscil os)))))
      (md5sum-file
        (convolve-files *test-input-file* *test-filter-file* 1 *test-file-name*)))
  #(93 51 198 75 225 49 136 235 210 41 124 7 83 50 170 62))
