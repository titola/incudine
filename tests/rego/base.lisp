(in-package :incudine-tests)

(dsp! rego-test-1 (freq amp)
  (out (sine freq amp 0)))

(dsp! rego-test-2 (freq amp pos)
  (foreach-channel (cout (pan2 (sine freq amp 0) pos))))

(dsp! rego-test-3 (freq amp dur)
  (stereo (* (envelope (make-local-perc .1 .9) 1 dur #'free)
             (sine freq amp 0))))

(defmacro test-regofile (path)
  `(funcall
     (let ((*package* (find-package "INCUDINE-TESTS")))
       (regofile->function
         (merge-pathnames ,path #.(load-time-value
                                   (or *compile-file-pathname*
                                       *load-pathname*)))))))

(defmacro regofile->list-test (name path list)
  `(deftest ,name
       (mapcar (lambda (l)
                 (mapcar (lambda (x)
                           (if (typep x 'float) (two-decimals x) x))
                         l))
               (let ((*package* (find-package "INCUDINE-TESTS")))
                 (regofile->list
                   (merge-pathnames ,path #.(load-time-value
                                             (or *compile-file-pathname*
                                                 *load-pathname*))))))
     ,list))

(defscore-statement i1 (time dur freq amp)
  (list time 'rego-test-3 freq amp `(dur ,dur)))
