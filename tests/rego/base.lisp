(defpackage #:incudine-tests/score-package-1 (:use #:cl))
(defpackage #:incudine-tests/score-package-2 (:use #:cl))

(in-package :incudine-tests)

(dsp! rego-test-1 (freq amp)
  (out (sine freq amp 0)))

(dsp! rego-test-2 (freq amp pos)
  (foreach-channel (cout (pan2 (sine freq amp 0) pos))))

(dsp! rego-test-3 (freq amp dur)
  (stereo (* (envelope (make-perc .1f0 .9f0) 1 dur #'free)
             (sine freq amp 0))))

(defmacro test-regofile (path &optional compile-p)
  `(funcall
     (let ((*package* (find-package "INCUDINE-TESTS")))
       (regofile->function
         (merge-pathnames ,path #.(load-time-value
                                   (or *compile-file-pathname*
                                       *load-pathname*)))
         nil ,compile-p))))

(defmacro test-regostring (from-path &optional compile-p)
  `(funcall
     (let ((*package* (find-package "INCUDINE-TESTS"))
           (*default-pathname-defaults*
             #.(load-time-value (or *compile-file-pathname* *load-pathname*))))
       (regostring->function
         (alexandria:read-file-into-string (merge-pathnames ,from-path))
         nil ,compile-p))))

(defun rego-test-filter (list)
  (mapcar (lambda (l)
            (mapcar (lambda (x)
                      (if (typep x 'float) (two-decimals x) x))
                    l))
          list))

(defmacro regofile->list-test (name path list
                               &optional (reduce-float-precision-p t))
  (with-gensyms (lst)
    `(deftest ,name
         (let* ((*package* (find-package "INCUDINE-TESTS"))
                (*read-default-float-format* 'single-float)
                (,lst (regofile->list
                        (merge-pathnames ,path #.(load-time-value
                                                   (or *compile-file-pathname*
                                                       *load-pathname*))))))
           ,(if reduce-float-precision-p
                `(rego-test-filter ,lst)
                lst))
       ,list)))

(defmacro regostring->list-test (name from-path list
                                 &optional (reduce-float-precision-p t))
  (with-gensyms (lst)
    `(deftest ,name
         (let* ((*package* (find-package "INCUDINE-TESTS"))
                (*read-default-float-format* 'single-float)
                (*default-pathname-defaults*
                  #.(load-time-value (or *compile-file-pathname*
                                         *load-pathname*)))
                (,lst (regostring->list
                        (alexandria:read-file-into-string
                          (merge-pathnames ,from-path)))))
           ,(if reduce-float-precision-p
                `(rego-test-filter ,lst)
                lst))
       ,list)))

(defscore-statement i1 (time dur freq amp)
  (list time 'rego-test-3 freq amp `(dur ,dur)))

(defscore-statement "InstrName1" (x) (list 0 'instr-1 x))
(defscore-statement |InstrName2| (x) (list 0 'instr-2 x))
(defscore-statement instrname3 (x) (list 0 'instr-3 x))
