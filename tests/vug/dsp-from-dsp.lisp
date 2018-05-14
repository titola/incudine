(in-package :incudine-tests)

(dsp! dsp-from-dsp-1 () (out .2))

(dsp! dsp-from-dsp-2 ()
  (with ((node nil)
         (t0 (* *sample-rate* 2))
         (t1 (* t0 2))
         (uptime +sample-zero+)
         (to-free t))
    (declare (type (or node null) node) (type sample t0 t1 uptime)
             (boolean to-free))
    (initialize
      (setf node (dsp-from-dsp-1)))
    (setf uptime (node-uptime (dsp-node)))
    (cond ((and to-free (> uptime t0))
           (free node)
           (setf to-free nil))
          ((> uptime t1)
           (free-self)))
    (out .5)))

(with-dsp-test (dsp-from-dsp.1
      :md5 #(184 76 127 213 109 176 24 40 33 112 185 17 246 117 240 254))
  (dsp-from-dsp-2))
