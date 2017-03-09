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
      :md5 #(184 165 198 163 9 177 92 90 162 153 132 206 42 123 98 176))
  (test-regofile "jump-1.rego"))

(with-dsp-test (sco.1 :channels 2
      :md5 #(234 178 158 73 51 52 178 79 206 228 55 8 55 238 133 87))
  (test-regofile "test-1.sco"))

(with-dsp-test (sco.2 :channels 2
      :md5 #(234 178 158 73 51 52 178 79 206 228 55 8 55 238 133 87))
  (test-regofile "test-2.sco"))

(with-dsp-test (include-rego-loop-error :channels 2
      :md5 #(224 78 104 237 105 222 73 188 82 233 83 242 152 133 181 19))
  ;; Error caused by a recursive inclusion.
  (test-regofile "include-loop-1.rego"))

(with-dsp-test (include-rego.1 :channels 2
      :md5 #(3 94 27 193 123 204 160 70 109 22 49 51 216 191 21 204))
  (test-regofile "include-1.rego"))

(with-dsp-test (include-rego.2
      :md5 #(135 85 80 12 247 185 67 6 96 133 75 171 136 8 254 246))
  ;; Include four times the same rego file.
  (test-regofile "include-2.rego"))

(with-dsp-test (include-rego.3 :channels 2
      :md5 #(237 122 246 121 255 221 7 237 173 52 12 39 41 110 52 122))
  ;; Lisp tag shadowed in the included rego file.
  (test-regofile "include-3.rego"))

(with-dsp-test (org-mode.1
      :md5 #(216 196 146 164 88 88 197 20 138 165 207 215 200 7 22 251))
  (test-regofile "org-mode.rego"))

(with-dsp-test (paral.1
      :md5 #(132 22 147 114 139 210 67 6 172 195 244 193 123 100 193 191))
  (test-regofile "paral-1.rego"))

(with-dsp-test (paral.2
      :md5 #(225 98 8 15 231 24 109 114 111 196 54 172 233 204 51 11))
  (test-regofile "paral-2.rego"))
