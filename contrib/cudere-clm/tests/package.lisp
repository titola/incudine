(defpackage :cudere-clm-tests
  (:use :cl :cudere-clm #+sbcl :sb-rt #-sbcl :rtest)
  (:import-from #+sbcl :sb-md5 #-sbcl :md5 #:md5sum-file))
