(defsystem "cudere-clm-tests"
  :depends-on (:cudere-clm #+sbcl :sb-rt #+sbcl :sb-md5
                           #-sbcl :rt    #-sbcl :md5)
  :components
  ((:module "tests"
    :components
    ((:file "package")
     (:file "util" :depends-on ("package"))
     (:file "generators" :depends-on ("util")))))
  :perform (test-op (o c) (symbol-call :cudere-clm-tests :do-tests)))
