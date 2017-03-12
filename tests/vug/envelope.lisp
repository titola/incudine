(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! vug-envelope-test ((env envelope) gain gate dur)
  (out (* (db->lin gain) (envelope env gate dur #'free))))

(defvar *env-test* (make-envelope '(0 1 0) '(.5 .5)))

(with-dsp-test (vug-envelope.1
      :md5 #(32 145 63 123 200 150 230 36 247 2 47 102 178 248 244 184))
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.2
      :md5 #(70 128 93 221 105 97 30 164 13 209 118 47 105 176 131 100))
  (setf (envelope-curve *env-test* 1) :exp)
  (setf (envelope-curve *env-test* 2) :linear)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.3
      :md5 #(13 199 55 245 223 160 205 208 93 167 4 224 212 65 146 19))
  (setf (envelope-curve *env-test* 1) 4)
  (setf (envelope-curve *env-test* 2) -4)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.4
      :md5 #(201 151 64 235 148 248 65 73 8 58 36 112 111 55 72 78))
  (set-envelope-base *env-test* 7)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.5
      :md5 #(96 42 136 101 6 204 133 142 159 152 34 153 12 59 194 29))
  (set-envelope *env-test* '(0 1 0) '(.2 .8) :curve (list :cub :sqr))
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-envelope.6
      :md5 #(255 229 27 19 199 95 64 252 193 170 195 123 172 113 11 229))
  (set-envelope *env-test* '(0 1 .3 .15) '(.2 .3 .5) :base .2)
  (vug-envelope-test *env-test* -3 1 5))

(with-dsp-test (vug-linen.1
      :md5 #(110 255 215 74 77 220 70 131 239 1 98 83 29 38 244 9))
  (linen *env-test* .3 3.7 1)
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-linen.2
      :md5 #(215 171 108 65 146 85 128 39 218 16 99 241 49 86 164 95))
  (linen *env-test* .3 3.7 1 :curve :sin)
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-perc.1
      :md5 #(74 63 175 0 148 195 193 174 103 19 84 66 206 235 131 156))
  (perc *env-test* .001 1)
  (vug-envelope-test *env-test* -3 1 1))

(with-dsp-test (vug-cutoff.1
      :md5 #(138 28 147 170 26 178 34 46 162 25 41 7 233 207 61 69))
  (cutoff *env-test* 2)
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[2.8 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-cutoff.2
      :md5 #(62 248 233 33 66 97 199 247 230 112 222 148 73 164 232 137))
  (cutoff *env-test* 2 :curve 8)
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[2.8 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-asr.1
      :md5 #(154 149 220 163 107 143 243 86 2 4 61 174 143 29 38 158))
  (asr *env-test* 1 1 1.5)
  (vug-envelope-test *env-test* -3 1 1 :id 8)
  (at #[3 s] #'set-control 8 :gate 0))

(with-dsp-test (vug-adsr.1
      :md5 #(240 236 179 75 248 77 180 111 210 163 138 26 211 244 115 48))
  (adsr *env-test* .2 .09 .8 1)
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.5 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-adsr.2
      :md5 #(56 125 117 117 23 168 166 178 108 29 173 220 141 222 231 112))
  (adsr *env-test* .2 .09 .8 1 :base 12)
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.5 s] #'set-control 1 :gate 0))

(with-dsp-test (vug-dadsr.1
      :md5 #(149 120 225 228 16 147 255 87 132 159 221 201 203 175 234 28))
  (dadsr *env-test* .75 .4 .1 .7 1.2)
  (vug-envelope-test *env-test* -3 1 1 :id 1)
  (at #[3.6 s] #'set-control 1 :gate 0))
