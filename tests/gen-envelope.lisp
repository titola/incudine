(in-package :incudine-tests)

(defun gen-envelope-test-1 (env &key periodic-p normalize-p (mult 1))
  (with-cleanup
    (mapcar #'sample->fixnum
            (buffer->list
              (scale-buffer
                (make-buffer 64
                  :fill-function (gen:envelope env :periodic-p periodic-p
                                               :normalize-p normalize-p))
                mult)))))

(deftest gen-envelope.1
    (with-cleanup
      (gen-envelope-test-1 (make-envelope '(-1000 300 0) '(.5 1.2))))
  (-1000 -928 -856 -784 -712 -639 -567 -495 -423 -351 -278 -206
   -134 -62 11 83 155 227 300 293 286 279 273 266 259 253 246 239
   233 226 219 213 206 199 193 186 180 173 166 160 153 146 140 133
   126 120 113 106 100 93 86 80 73 66 60 53 46 40 33 26 20 13 6 0))

(deftest gen-envelope.2
    (with-cleanup
      (gen-envelope-test-1 (make-envelope '(-1000 300 0) '(.5 1.2))
                           :periodic-p t))
  (-1000 -928 -856 -784 -712 -639 -567 -495 -423 -351 -278 -206
   -134 -62 11 83 155 227 300 293 286 280 273 267 260 254 247 241
   234 228 221 215 208 202 195 189 182 176 169 163 156 149 143 136
   130 123 117 110 104 97 91 84 78 71 65 58 52 45 39 32 26 19 13 6))

(deftest gen-envelope.3
    (with-cleanup
      (gen-envelope-test-1 (make-envelope '(-35 8 -9 0) '(.5 .2 1.3))
                           :normalize-p t :mult 1000))
  (-1000 -919 -837 -755 -673 -591 -509 -427 -345 -263 -181 -100 -18
   64 146 228 147 66 -15 -96 -177 -258 -252 -245 -239 -233 -227
   -221 -215 -209 -203 -196 -190 -184 -178 -172 -166 -160 -154 -147
   -141 -135 -129 -123 -117 -111 -105 -98 -92 -86 -80 -74 -68 -62
   -56 -49 -43 -37 -31 -25 -19 -13 -7 0))

(deftest gen-envelope.4
    (with-cleanup
      (gen-envelope-test-1 (make-envelope '(-35 8 -9 0) '(.5 .2 1.3))
                           :periodic-p t :normalize-p t :mult 1000))
  (-1000 -924 -847 -770 -693 -617 -540 -463 -386 -309 -233 -156 -79
   -2 75 151 228 147 66 -15 -96 -177 -258 -252 -245 -239 -233 -227
   -221 -215 -209 -203 -196 -190 -184 -178 -172 -166 -160 -154 -147
   -141 -135 -129 -123 -117 -111 -105 -98 -92 -86 -80 -74 -68 -62
   -56 -49 -43 -37 -31 -25 -19 -13 -7))
