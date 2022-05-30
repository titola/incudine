(in-package :incudine-tests)

(dsp! non-local-exit-1 ()
  (block one
    (out 1)
    (block two
      (out 2)
      (return-from two)
      (out 4))
    (out 8)
    (return-from one)
    (out 16)))

(dsp! non-local-exit-2 ()
  (block one
    (out 1)
    (block two
      (out 2)
      (return-from one)
      (out 4))
    (out 8)
    (return-from one)
    (out 16)))

(dsp! non-local-exit-3 ()
  (block one
    (out 1)
    (flet ((two ()
             (out 2)
             (return-from two)
             (out 4)))
      (two)
      (out 8)
      (return-from one)
      (out 16))))

(dsp! non-local-exit-4 ()
  (block one
    (out 1)
    (flet ((two ()
             (out 2)
             (return-from one)
             (out 4)))
      (two)
      (out 8)
      (return-from one)
      (out 16))))

;;; (dsp! block-scope-test ()
;;;   (block nil
;;;     ;; It fails because RANDOM is not performance-time, therefore
;;;     ;; the VUG parameter control doesn't change during the performance,
;;;     ;; and RETURN is moved out of the block scope (performance-time
;;;     ;; in this case).
;;;     ;;   (out (sine (if (= 0 (random 10)) (return) (sample 1000))))
;;;
;;;     ;; It is correct because the parameter control is performance-time.
;;;     (out (sine (if (= 0 (tick (random 10))) (return) (sample 1000))))
;;;
;;;     ;; Some alternatives:
;;;     ;;   (out (sine (tick (if (= 0 (random 10)) (return) (sample 1000)))))
;;;     ;;   (out (sine (if (= 0 (random 10)) (tick (return)) (sample 1000))))
;;;     ))

(with-dsp-test (non-local-exit.1
      :md5 #(236 74 185 110 41 165 251 197 126 19 248 226 13 135 54 79))
  (non-local-exit-1))

(with-dsp-test (non-local-exit.2
      :md5 #(13 96 180 70 245 252 7 228 80 17 181 33 3 49 209 45))
  (non-local-exit-2))

(with-dsp-test (non-local-exit.3
      :md5 #(236 74 185 110 41 165 251 197 126 19 248 226 13 135 54 79))
  (non-local-exit-3))

(with-dsp-test (non-local-exit.4
      :md5 #(13 96 180 70 245 252 7 228 80 17 181 33 3 49 209 45))
  (non-local-exit-4))
