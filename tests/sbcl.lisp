(in-package :incudine-tests)

(defmacro md5sum-sequence (sequence &key (start 0) end)
  `(sb-md5:md5sum-sequence ,sequence :start ,start :end ,end))
