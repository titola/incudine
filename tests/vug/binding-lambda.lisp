(in-package :incudine-tests)

(deftest binding-lambda.1
    (progn
      (define-ugen binding-lambda-1 list ()
        (with-samples ((a 0.123)
                       ;; VUG-VARIABLE value to cache in FLET.
                       (b (funcall (lambda () (+ a a))))
                       (c 0.321))
          (list a b c)))
      (let* ((u (funcall (binding-lambda-1)))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  (0.123d0 0.246d0 0.321d0))

(deftest binding-lambda.2
    (progn
      (define-ugen binding-lambda-2 list ()
        (with ((a 123)
               ;; VUG-VARIABLE value to cache in FLET.
               (b (funcall (lambda () (+ a a))))
               (c 321))
          (list a b c)))
      (let* ((u (funcall (binding-lambda-2)))
             (res (funcall (ugen-perf-function u))))
        (free u)
        res))
  (123 246 321))
