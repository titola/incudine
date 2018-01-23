(in-package :incudine-tests)

(defun gen-fir-from-env-1 (order srate env)
  (with-cleanup
    (let ((b (make-buffer order
               :fill-function (gen:fir env :sample-rate srate))))
      (mapcar (lambda (x) (floor (* x 1000))) (buffer->list b)))))

(defun gen-fir-from-buffer-1 (order vals)
  (with-cleanup
    (let ((b (make-buffer order :initial-contents vals)))
      (fill-buffer b (gen:fir nil))
      (mapcar (lambda (x) (floor (* x 1000))) (buffer->list b)))))

(deftest gen-fir.1
    (equal (gen-fir-from-env-1 12 48000 '(999 0 1000 1 2000 1 2001 0))
           (gen-fir-from-buffer-1 12 '(0 0 1 1 0 0 0 0 0 0 0 0)))
  T)

(deftest gen-fir.2
    (equal (gen-fir-from-env-1 15 1 '(0 0 .1 1.00001 .2 .5 .3 .8 .4 0))
           (gen-fir-from-buffer-1 15 '(0 1/3 2/3 1 5/6 2/3 .5 .6 .7 .8 .4 0 0 0 0)))
  T)
