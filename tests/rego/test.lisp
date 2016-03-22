(in-package :incudine-tests)

(with-dsp-test (rego.1 :channels 2
      :md5 #(245 220 65 244 25 76 31 230 109 144 248 153 16 170 160 134))
  (test-regofile "t1.rego"))

(with-dsp-test (rego.2 :channels 2
      :md5 #(245 220 65 244 25 76 31 230 109 144 248 153 16 170 160 134))
  (test-regofile "t2.rego"))

(with-dsp-test (rego.3
      :md5 #(151 116 137 222 25 122 7 35 219 181 202 137 145 62 243 213))
  (test-regofile "loop-1.rego"))

(with-dsp-test (rego.4 :channels 2
      :md5 #(193 32 71 149 211 96 68 209 150 170 210 249 231 75 179 112))
  (test-regofile "jump-1.rego"))

(with-dsp-test (sco.1 :channels 2
      :md5 #(250 199 97 144 27 34 63 91 8 171 162 207 206 79 147 224))
  (test-regofile "test-1.sco"))

(with-dsp-test (sco.2 :channels 2
      :md5 #(250 199 97 144 27 34 63 91 8 171 162 207 206 79 147 224))
  (test-regofile "test-2.sco"))

(with-dsp-test (include-rego-loop-error :channels 2
      :md5 #(224 78 104 237 105 222 73 188 82 233 83 242 152 133 181 19))
  ;; Error caused by a recursive inclusion.
  (test-regofile "include-loop-1.rego"))

(with-dsp-test (include-rego.1 :channels 2
      :md5 #(27 162 219 116 104 206 115 33 162 240 2 110 95 12 177 84))
  (test-regofile "include-1.rego"))

(with-dsp-test (include-rego.2
      :md5 #(135 85 80 12 247 185 67 6 96 133 75 171 136 8 254 246))
  ;; Include four times the same rego file.
  (test-regofile "include-2.rego"))

(with-dsp-test (include-rego.3 :channels 2
      :md5 #(54 197 232 30 39 69 182 30 66 29 33 106 203 142 205 132))
  ;; Lisp tag shadowed in the included rego file.
  (test-regofile "include-3.rego"))

(with-dsp-test (org-mode.1
      :md5 #(216 196 146 164 88 88 197 20 138 165 207 215 200 7 22 251))
  (test-regofile "org-mode.rego"))
