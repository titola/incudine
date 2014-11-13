(in-package :incudine-tests)

(compile-vug 'vug-test-1 'sample)

;;; Test name, return-type, arguments, types and control-flags.
(deftest ugen.1
    (let ((u (ugen 'vug-test-1)))
      (if u
          (values t (incudine.vug::ugen-name u)
                  (incudine.vug::ugen-return-type u)
                  (incudine.vug::ugen-args u)
                  (incudine.vug::ugen-arg-types u)
                  (incudine.vug::ugen-control-flags u))))
  t vug-test-1 sample (freq amp phase) (sample sample sample) (3 1 1))

(compile-vug 'vug-test-2 'sample)

;;; Test arguments and types.
(deftest ugen.2
    (let ((u (ugen 'vug-test-2)))
      (if u
          (values t (incudine.vug::ugen-args u)
                  (incudine.vug::ugen-arg-types u))))
  t (buf rate offset loop-p done-action)
  (buffer sample sample boolean function))

;;; Test RENAME-UGEN and DESTROY-UGEN.
(deftest ugen.3
    (progn
      (define-ugen ugen-tmp sample (amp) (white-noise amp))
      (let ((n1 (incudine.vug::ugen-name (ugen 'ugen-tmp))))
        (rename-ugen 'ugen-tmp 'tmp-ugen)
        (let ((n2 (incudine.vug::ugen-name (ugen 'tmp-ugen))))
          (destroy-ugen 'tmp-ugen)
          (values n1 n2 (null (ugen 'ugen-tmp)) (null (ugen 'tmp-ugen))
                  (null (vug 'ugen-tmp)) (null (vug 'tmp-ugen))))))
  ugen-tmp tmp-ugen t t nil t)
