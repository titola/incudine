(in-package :cudere-clm-tests)

(defvar *test-file-name* "/tmp/test.wav")
(defvar *test-filter-file* "/tmp/filter.wav")
(defvar *test-input-file* "/tmp/input.wav")
(defvar *test-srate* 48000)
(defvar *test-length* (* *test-srate* 5))
(defvar *test-header-type* mus-riff)
(defvar *test-data-format* mus-ldouble)

(defmacro with-sound-test ((&rest args) &body body)
  (let* ((keys (loop for (k v) on args by #'cddr collect k))
         (opts (loop for (k v) on (list :output *test-file-name*
                                        :srate *test-srate*
                                        :header-type *test-header-type*
                                        :data-format *test-data-format*
                                        :statistics t :play nil)
                               by #'cddr
                     unless (member k keys)
                       collect k and collect v)))
    `(md5sum-file (with-sound (,@args ,@opts) ,@body))))
