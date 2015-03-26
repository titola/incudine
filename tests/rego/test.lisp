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
      :md5 #(148 166 170 150 45 178 81 164 159 42 67 247 148 180 200 115))
  (test-regofile "jump-1.rego"))

(with-dsp-test (sco.1 :channels 2
      :md5 #(71 193 179 146 156 78 122 250 116 201 235 154 167 101 159 248))
  (test-regofile "test-1.sco"))

(with-dsp-test (include-rego-loop-error :channels 2
      :md5 #(224 78 104 237 105 222 73 188 82 233 83 242 152 133 181 19))
  ;; Error caused by a recursive inclusion.
  (test-regofile "include-loop-1.rego"))

(with-dsp-test (include-rego.1 :channels 2
      :md5 #(102 13 193 63 76 150 80 124 6 231 184 161 255 29 142 147))
  (test-regofile "include-1.rego"))

(with-dsp-test (include-rego.2
      :md5 #(135 85 80 12 247 185 67 6 96 133 75 171 136 8 254 246))
  ;; Include four times the same rego file.
  (test-regofile "include-2.rego"))

(with-dsp-test (include-rego.3 :channels 2
      :md5 #(215 28 243 91 9 95 147 192 203 232 199 70 157 178 29 32))
  ;; Lisp tag shadowed in the included rego file.
  (test-regofile "include-3.rego"))
