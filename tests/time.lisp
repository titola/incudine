(in-package :incudine-tests)

(deftest tempo.1
    (with-cleanup
      (let ((tempo (make-tempo 120)))
        (values (two-decimals (bpm tempo))
                (two-decimals (bps tempo))
                (two-decimals (spb tempo)))))
  120.0 2.0 0.5)

(deftest tempo.2
    (with-cleanup
      (let ((tempo (make-tempo 135)))
        (setf (bpm tempo) 120)
        (let ((bpm1 (two-decimals (bpm tempo)))
              (bps1 (two-decimals (bps tempo)))
              (spb1 (two-decimals (spb tempo))))
          (setf (bps tempo) 2.6)
          (values bpm1 bps1 spb1
                  (two-decimals (bpm tempo))
                  (two-decimals (bps tempo))
                  (two-decimals (spb tempo))))))
  120.0 2.0 0.5 156.0 2.6 0.38)

(deftest with-cleanup-tempo.1
    (free-p (with-cleanup (make-tempo 180)))
  T)

(enable-sharp-square-bracket-syntax)

(deftest time-units
    (with-cleanup
      (let ((*sample-rate* (sample 96000))
            (*tempo* (make-tempo 95))
            (tempo (make-tempo 135)))
        (mapcar #'sample->int
                (list #[3 b] #[1/2 beat] #[4 beats tempo]
                      #[3 samples] #[12345 samps]
                      #[1 s] #[1/2 sec] #[13 seconds]
                      #[2 m] #[3 meters] #[5 meters 338]
                      #[1 min] #[3 minute]
                      #[100 ms] #[334 msec]
                      #[1 h] #[24 hours]
                      #[1 d] #[7 days]
                      #[1 w] #[4 weeks]))))
  (181894 30315 170666 3 12345 96000 48000 1248000 556 834 1420
   5760000 17280000 9600 32064 345600000 8294400000 8294400000
   58060800000 58060800000 232243200000))

(deftest with-schedule-time.1
    (let ((times nil))
      (with-schedule
        (incf (now))
        (push (now) times)
        (with-local-time ()
          (incf (now))
          (push (now) times)
          (with-local-time (:start 124d0)
            (decf (now))
            (push (now) times)))
        (push (now) times))
      (reverse times))
  (0.0d0 1.0d0 123d0 0.0d0))

(deftest with-schedule-time.2
    (let ((times nil))
      (labels ((rec (n)
                 (with-schedule
                   (when (> n 0)
                     (assert (eq incudine.edf:*heap* incudine::*nrt-edf-heap*))
                     (push (sample->fixnum (now)) times)
                     (at (+ (now) #[1/4 s]) #'rec (1- n))))))
        (with-logger (*null-output*)
          (bounce-to-buffer (*buffer-test-c1* :sample-rate +sample-rate-test+)
            (rec 100)))
        times))
  (240000 228000 216000 204000 192000 180000 168000 156000 144000 132000
   120000 108000 96000 84000 72000 60000 48000 36000 24000 12000 0))
