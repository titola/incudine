(in-package :incudine)

(defun serial-stream-p (obj)
  (declare (ignore obj))
  nil)

(defun serial-flush (stream &key (direction :io))
  (declare (ignore stream direction))
  nil)
