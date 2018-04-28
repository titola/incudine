(in-package :cudere-clm-tests)

(defun test-file-pathname (file)
  (namestring (asdf:compile-file-pathname* file :output-file file)))

(defvar *test-file-name* (test-file-pathname "test.wav"))
(defvar *test-filter-file* (test-file-pathname "filter.wav"))
(defvar *test-input-file* (test-file-pathname "input.wav"))
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

(defun delete-test-files ()
  (dolist (f (list *test-file-name* *test-filter-file* *test-input-file*))
    (when (probe-file f) (delete-file f))))
