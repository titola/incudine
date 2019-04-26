(in-package :incudine-tests)

(dsp! reson-test-1 (amp p0min p0max p1min p1max (type symbol))
  (with-samples ((in (noise-test amp))
                 (dur +vug-test-duration-sec+)
                 (p0 (expon p0min p0max dur))
                 (p1 (line p1min p1max dur)))
    (macrolet ((flt (name) `(,name in p0 p1)))
      (maybe-expand in p0 p1)
      (out (case type
             (reson (flt reson))
             (resonz (flt resonz))
             (resonr (flt resonr))
             (ringz (flt ringz))
             (ringr (flt ringr))
             (otherwise in))))))

(with-dsp-test (reson.1
      :md5 #(123 176 66 21 228 79 79 140 177 6 152 180 128 200 187 50))
  (reson-test-1 6 3456 3456 80 80 'reson))

(with-dsp-test (reson.2
      :md5 #(11 206 139 117 119 98 11 24 236 69 224 108 168 185 66 209))
  (reson-test-1 6 3456 3456 80 80 'resonz))

(with-dsp-test (reson.3
      :md5 #(125 155 142 163 192 67 172 211 21 19 84 209 85 216 10 0))
  (reson-test-1 6 3456 3456 80 80 'resonr))

(with-dsp-test (reson.4
      :md5 #(255 172 245 15 203 11 119 219 247 99 105 249 9 55 149 109))
  (reson-test-1 1/25 2345 2345 .05 .05 'ringz))

(with-dsp-test (reson.5
      :md5 #(39 215 124 157 124 156 131 84 85 220 143 111 140 20 42 107))
  (reson-test-1 1/25 2345 2345 .05 .05 'ringr))

(with-dsp-test (reson.6
      :md5 #(25 171 240 59 217 67 160 146 248 59 61 195 51 172 225 138))
  (reson-test-1 5/2 20 1000 30 90 'reson)
  (reson-test-1 1 1000 4000 5 150 'resonz)
  (reson-test-1 1 4000 16000 2 200 'resonr)
  (reson-test-1 1/120 8000 100 1e-4 .1 'ringz)
  (reson-test-1 1/100 16000 400 1e-4 .1 'ringr))
