(in-package :incudine-tests)

(deftest tuning.1
    (let* ((tun (make-tuning))
           (res1 (list (tuning-description tun) (tuning-cents tun)
                       (tuning-ratios tun) (tuning-keynum-base tun)
                       (sample->fixnum (tuning-freq-base tun))
                       (tuning-degree-index tun)))
           (res2 (mapcar (lambda (k)
                           (sample->fixnum (* (tuning-cps tun k) 100)))
                         (loop for i from 48 to 72 collect i))))
      (free tun)
      (values res1 res2))
  ("12-tone equal temperament"
   #(0.0 100.0 200.0 300.0 400.0 500.0 600.0 700.0 800.0 900.0
     1000.0 1100.0 1200.0)
   #(1 3118/2943 5252/4679 5399/4540 5429/4309 3249/2434 4756/3363
     7523/5021 4813/3032 6041/3592 7831/4395 2943/1559 2)
   69 440 9)
  (13081 13859 14683 15556 16481 17461 18499 19599 20765 21999
   23308 24694 26162 27718 29366 31112 32962 34922 36999 39199
   41530 44000 46616 49388 52325))

(deftest tuning.2
    (let* ((tun (make-tuning
                  :notes '(16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5 15/8 2/1)
                  :keynum-base 60
                  :freq-base 261.625565
                  :degree-index 0))
           (res1 (list (tuning-cents tun) (tuning-ratios tun)))
           (res2 (mapcar (lambda (k)
                           (sample->fixnum (* (tuning-cps tun k) 100)))
                         (append (loop for i below 12 collect i)
                                 (loop for i from 116 below 128 collect i)))))
      (free tun)
      (values res1 res2))
  (#(0.0 111.731285 203.90999 315.6413 386.3137 498.045 582.5122
     701.95496 813.6863 884.3587 1017.59625 1088.2687 1200.0)
   #(1 16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5 15/8 2))
  (817 872 919 981 1021 1090 1144 1226 1308 1362 1471 1532
   669761 697668 753481 784876 837201 893015 941852 1004642
   1046502 1116269 1172082 1255802))

(deftest tuning.3
    (let* ((tun (make-tuning
                  :notes '(16/15 203.90999 6/5 386.3137 4/3 582.5122 3/2
                           813.6863 5/3 1017.59625 15/8 1200.0)
                  :keynum-base 60
                  :freq-base 261.625565
                  :degree-index 0))
           (res1 (list (tuning-cents tun) (tuning-ratios tun)))
           (res2 (mapcar (lambda (k)
                           (sample->fixnum (* (tuning-cps tun k) 100)))
                         (append (loop for i below 12 collect i)
                                 (loop for i from 116 below 128 collect i)))))
      (free tun)
      (values res1 res2))
  (#(0.0 111.731285 203.90999 315.6413 386.3137 498.045 582.5122
     701.95496 813.6863 884.3587 1017.59625 1088.2687 1200.0)
   #(1 16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5 15/8 2))
  (817 872 919 981 1021 1090 1144 1226 1308 1362 1471 1532
   669761 697668 753481 784876 837201 893015 941852 1004642
   1046502 1116269 1172082 1255802))

(deftest tuning.4
    (let ((tun (make-tuning :notes '(16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5
                                     15/8 2/1))))
      (set-tuning tun '(9/8 5/4 4/3 3/2 5/3 15/8 2/1))
      (flet ((tun-test (tun)
               (mapcar (lambda (k)
                         (sample->fixnum (* (tuning-cps tun k) 100)))
                       (append (loop for i below 12 collect i)
                               (loop for i from 48 below 72 collect i)))))
        (let ((res1 (list (tuning-cents tun) (tuning-ratios tun)))
              (res2 (tun-test tun)))
          (set-tuning-reference tun 60 261.625565 0)
          (setf (tuning-description tun) "SET-TUNING-REFERENCE test.")
          (let ((res3 (tuning-description tun))
                (res4 (tun-test tun)))
            (free tun)
            (values res1 res2 res3 res4)))))
  (#(0.0 203.90999 386.3137 498.045 701.95496 884.3587 1088.2687 1200.0)
   #(1 9/8 5/4 4/3 3/2 5/3 15/8 2))
  (45 51 57 64 68 77 85 91 103 114 128 137
   5500 5866 6600 7333 8250 8800 9900 11000 11733 13200
   14666 16500 17600 19800 22000 23466 26400 29333 33000
   35200 39600 44000 46933 52800)
  "SET-TUNING-REFERENCE test."
  (68 76 85 95 102 114 127 136 153 170 191 204
   8175 8720 9810 10901 12263 13081 14716 16351 17441 19621
   21802 24527 26162 29432 32703 34883 39243 43604 49054
   52325 58865 65406 69766 78487))

(deftest tuning.5
    (let ((tun (make-tuning)))
      (loop for freq in '(264 281.6 297 316.80002 330 352 369.6
                          396 422.4 440 475.19998 495 528)
            for keynum from 60
            do (setf (tuning-cps tun keynum) freq))
      (tuning-notes-from-data tun 60 72 "TUNING-NOTES-FROM-DATA test.")
      (let ((res1 (list (tuning-description tun) (tuning-cents tun)
                        (tuning-ratios tun)))
            (res2 (mapcar (lambda (k)
                            (sample->fixnum (* (tuning-cps tun k) 100)))
                          (append (loop for i below 12 collect i)
                                  (loop for i from 116 below 128 collect i)))))
        (free tun)
        (values res1 res2)))
  ("TUNING-NOTES-FROM-DATA test."
   #(0.0 111.731285 203.90999 315.6413 386.3137 498.045 582.5122
     701.95496 813.6863 884.3587 1017.59625 1088.2687 1200.0)
   #(1 16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5 15/8 2))
  (825 880 928 990 1031 1100 1155 1237 1320 1375 1485 1546
   675840 704000 760320 792000 844800 901120 950400 1013760
   1056000 1126400 1182720 1267200))

(deftest tuning.6
    (let ((tun (make-tuning
                 :notes '(293377/275041 57547/51153 83539/69616 107001/85601
                          132365/99274 69703/49788 433894/289263 155171/96982
                          276853/166112 1438049/798916 561752/299601 2))))
      (flet ((ratios () (cdr (coerce (tuning-ratios tun) 'list))))
        (let ((res1 (ratios)))
          (minimize-tuning-ratios tun)
          (let ((res2 (ratios)))
            (free tun)
            (values res1 res2)))))
  (293377/275041 57547/51153 83539/69616 107001/85601 132365/99274 69703/49788
   433894/289263 155171/96982 276853/166112 1438049/798916 561752/299601 2)
  (16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5 15/8 2))
