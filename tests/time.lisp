(in-package :incudine-tests)

(deftest tempo.1
    (let ((tempo (make-tempo 120)))
      (values (two-decimals (bpm tempo))
              (two-decimals (bps tempo))))
  120.0 0.5)

(deftest tempo.2
    (let ((tempo (make-tempo 135)))
      (setf (bpm tempo) 120)
      (let ((bpm1 (two-decimals (bpm tempo)))
            (bps1 (two-decimals (bps tempo))))
        (setf (bps tempo) 1)
        (values bpm1 bps1
                (two-decimals (bpm tempo))
                (two-decimals (bps tempo)))))
  120.0 0.5 60.0 1.0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *old-readtable* *readtable*)
  (setf *readtable* incudine::*incudine-readtable*))

(deftest time-units
    (let ((*sample-rate* (sample 96000))
          (*tempo* (make-tempo 95))
          (tempo (make-tempo 135)))
      (prog1 (mapcar #'sample->int
                     (list #[3 b] #[1/2 beat] #[4 beats tempo]
                           #[3 samples] #[12345 samps]
                           #[1 s] #[1/2 sec] #[13 seconds]
                           #[2 m] #[3 meters] #[5 meters 338]
                           #[1 min] #[3 minute]
                           #[100 ms] #[334 msec]
                           #[1 h] #[24 hours]
                           #[1 d] #[7 days]
                           #[1 w] #[4 weeks]))
        (setf *readtable* *old-readtable*)))
  (181894 30315 170666 3 12345 96000 48000 1248000 556 834 1420
   5760000 17280000 9600 32064 345600000 8294400000 8294400000
   58060800000 58060800000 232243200000))
