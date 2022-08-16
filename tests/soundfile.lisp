(in-package :incudine-tests)

(defvar *data-raw-file*
  (merge-pathnames "tests/data.raw" (asdf:system-source-directory :incudine)))

(deftest soundfile-raw.1
    (soundfile:with-open-soundfile
        (sf *data-raw-file* :header-type "raw" :data-format "double")
      (loop repeat 10 collect (floor (soundfile:read-next sf))))
  (9 8 7 6 5 4 3 2 1 0))

(deftest soundfile-raw.2
    (soundfile:with-open-soundfile
        (sf *data-raw-file* :header-type "raw" :data-format "double"
         :data-location 8)
      (loop repeat 9 collect (floor (soundfile:read-next sf))))
  (8 7 6 5 4 3 2 1 0))

(deftest soundfile-raw.3
    (soundfile:with-open-soundfile
        (sf *data-raw-file* :header-type "raw" :data-format "pcm-s8")
      (setf (soundfile:position sf) 80)
      (loop repeat 8 collect (round (* 10 (soundfile:read-next sf)))))
  (-3 -2 -1 0 1 2 3 4))

(deftest soundfile-raw.4
    (soundfile:with-open-soundfile
        (sf *data-raw-file* :header-type "raw" :data-format "pcm-s8"
         :data-location 4)
      (setf (soundfile:position sf) 80)
      (loop repeat 4 collect (round (* 10 (soundfile:read-next sf)))))
  (1 2 3 4))
