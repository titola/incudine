(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! svf-test-1 (fcut res drive (index fixnum))
  (with ((frm (svf (buzz 100 .75 200) fcut res drive)))
    (declare (type frame frm))
    (out (smp-ref frm index))))

(dsp! svf-test-2 (f0 f1 dur res drive (index fixnum))
  (with ((fcut (expon f0 f1 dur #'identity))
         (frm (svf (buzz 100 .35 200) fcut res drive)))
    (declare (type sample fcut) (type frame frm))
    (out (smp-ref frm index))))

(dsp! svf-test-3 ((index fixnum))
  (with ((frm (svf (buzz 100 .3 200) (+ 4100 (sine 1/2 4000 0))
                   (+ .5 (sine 1 .49 0)) 0)))
    (declare (type frame frm))
    (stereo (smp-ref frm index))))

(with-dsp-test (svf.1
      :md5 #+64-bit #(198 16 88 205 6 212 194 69 128 56 112 238 148 92 10 130)
           #-64-bit #(239 28 103 51 254 245 75 218 34 62 228 36 181 61 40 85))
  (svf-test-1 2000 0 0 0 :id 1)                    ; lp
  (at #[1 s] #'set-control 1 :index 1)             ; hp
  (at #[2 s] #'set-control 1 :index 2)             ; bp
  (at #[3 s] #'set-control 1 :index 3)             ; notch
  (at #[4 s] #'set-controls 1 :res .8 :index 4))   ; peaking

(with-dsp-test (svf.2
      :md5 #+64-bit #(113 62 255 39 167 63 81 21 203 35 127 231 21 189 181 227)
           #-64-bit #(198 221 141 213 234 65 62 142 111 238 204 151 104 126 107 32))
  (svf-test-2 100 8000 1 .8 0 0 :id 1)
  (at #[1 s] #'set-controls 1 :f1 100 :dur 1 :index 1)
  (at #[1.5 s] #'set-control 1 :drive .09)
  (at #[2 s] #'set-controls 1 :f1 8000 :dur 1 :drive 0 :index 2)
  (at #[2.5 s] #'set-control 1 :drive .09)
  (at #[3 s] #'set-controls 1 :f1 100 :dur 1 :res 0 :drive 0 :index 3)
  (at #[3.5 s] #'set-control 1 :drive .09)
  (at #[4 s] #'set-controls 1 :f1 8000 :dur 1 :res .9 :drive 0 :index 4)
  (at #[4.5 s] #'set-control 1 :drive .09))

(with-dsp-test (svf.3 :channels 2
      :md5 #+64-bit #(240 84 42 34 146 72 121 59 26 17 137 116 72 34 16 144)
           #-64-bit #(101 118 237 225 90 24 197 40 9 217 20 69 239 143 46 98))
  (svf-test-3 0 :id 1)
  (at #[1 s] #'set-control 1 :index 1)
  (at #[2 s] #'set-control 1 :index 2)
  (at #[3 s] #'set-control 1 :index 3)
  (at #[4 s] #'set-control 1 :index 4))
