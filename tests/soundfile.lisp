(in-package :incudine-tests)

(macrolet ((test-files (&rest args)
             `(progn
                ,@(loop for p in args collect
                    `(defvar ,(car p)
                       (merge-pathnames ,(cdr p)
                         (asdf:system-source-directory :incudine)))))))
 (test-files
  (*data-raw-file*   . "tests/data.raw")
  (*test-pcm16*      . "tests/test-pcm16.wav")
  (*test-pcm16-flac* . "tests/test-pcm16.flac")
  (*test-pcm24*      . "tests/test-pcm24.wav")
  (*test-pcm24-flac* . "tests/test-pcm24.flac")))

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

(deftest soundfile-raw.5
    (soundfile:with-open-soundfile
        (sf *data-raw-file* :header-type "raw" :data-format "double"
         :read-from-memory-p t)
      (values
        (loop for x = (floor (soundfile:read-next sf))
              until (soundfile:eof-p sf) collect x)
        (list (setf (soundfile:position sf) 123456789)
              (soundfile:position sf)
              (soundfile:eof-p sf))
        (list (setf (soundfile:position sf) 2)
              (floor (soundfile:read-next sf))
              (soundfile:eof-p sf))
        (multiple-value-bind (ptr size) (soundfile:file-data sf)
          (loop for i below size collect (u8-ref ptr i)))))
  (9 8 7 6 5 4 3 2 1 0)
  (11 11 T)
  (2 7 NIL)
  (#x00 #x00 #x00 #x00 #x00 #x00 #x22 #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #x20 #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #x1C #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #x18 #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #x14 #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #x10 #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #x08 #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x40
   #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #x3F
   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
   #xDA #xE7 #xF3 #x00 #x0D #x19 #x26 #x33))

(deftest soundfile-raw.6
    (soundfile:with-open-soundfile
        (sf *data-raw-file* :header-type "raw" :data-format "double"
         :buffer-size 5 :max-buffer-size 10)
      (flet ((buffer-content ()
               (soundfile:read-into-buffer sf)
               (loop for i below (soundfile:buffer-size sf)
                     collect (floor (soundfile:buffer-value sf i)))))
        (values (buffer-content)
                (progn (setf (soundfile:buffer-size sf) 10)
                       (buffer-content))
                (progn (setf (soundfile:buffer-size sf) 100)
                       (buffer-content))
                (loop repeat 10 collect (floor (soundfile:read-next sf)))
                (soundfile:buffer-index sf)
                (progn (setf (soundfile:position sf) 0)
                       (setf (soundfile:buffer-size sf) 5)
                       (dotimes (i 6 (soundfile:buffer-index sf))
                         (soundfile:read-next sf))))))
  (9 8 7 6 5)
  (9 8 7 6 5 4 3 2 1 0)
  (9 8 7 6 5 4 3 2 1 0)
  (9 8 7 6 5 4 3 2 1 0)
  10
  1)

(deftest soundfile-read-from-memory.1
    (let (acc)
      (flet ((memtest (file)
               (soundfile:with-open-soundfile (sf file :read-from-memory-p t)
                 (multiple-value-bind (ptr size) (soundfile:file-data sf)
                   (push (loop for i below (ash size -1) by 512
                               collect (i16-ref ptr i))
                         acc)))))
        (memtest *test-pcm16*)
        (memtest *test-pcm16-flac*)
        (values (apply 'equal acc) (first acc))))
  T
  (0 1608 3212 4808 6393 7962 9512 11039 12539 14010 15446 16846
   18204 19519 20787 22005 23170 24279 25329 26319 27245 28105
   28898 29621 30273 30852 31356 31785 32137 32412 32609 32728
   32767 32728 32609 32412 32137 31785 31356 30852 30273 29621
   28898 28105 27245 26319 25329 24279 23170 22005 20787 19519
   18204 16846 15446 14010 12539 11039 9512 7962 6393 4808 3212
   1608 0 -1608 -3212 -4808 -6393 -7962 -9512 -11039 -12539 -14010
   -15446 -16846 -18204 -19519 -20787 -22005 -23170 -24279 -25329
   -26319 -27245 -28105 -28898 -29621 -30273 -30852 -31356 -31785
   -32137 -32412 -32609 -32728 -32767 -32728 -32609 -32412 -32137
   -31785 -31356 -30852 -30273 -29621 -28898 -28105 -27245 -26319
   -25329 -24279 -23170 -22005 -20787 -19519 -18204 -16846 -15446
   -14010 -12539 -11039 -9512 -7962 -6393 -4808 -3212 -1608))

(deftest soundfile-read-from-memory.2
    (let (acc frames-test)
      (flet ((memtest (file)
               (soundfile:with-open-soundfile (sf file :read-from-memory-p t)
                 (push (/ (second (multiple-value-list (soundfile:file-data sf)))
                          3 #|bytes|#)
                       frames-test)
                 (push (loop for i from 0 by 512
                             until (soundfile:eof-p sf)
                             collect (floor (* 1000000000 (soundfile:read sf i)))
                             finally (push (soundfile:current-frame sf) frames-test))
                       acc))))
        (memtest *test-pcm24*)
        (memtest *test-pcm24-flac*)
        (values (apply 'equal acc) (apply '= frames-test) (first acc))))
  T T
  (0 49067616 98017096 146730422 195090293 242980122 290284633
   336889863 382683396 427555084 471396684 514102697 555570125
   595699191 634393215 671558856 707106709 740951061 773010373
   803207397 831469535 857728481 881921172 903989195 923879384
   941543936 956940174 970031142 980785131 989176392 995184659
   998795390 999999880 998795390 995184659 989176392 980785131
   970031142 956940174 941543936 923879384 903989195 881921172
   857728481 831469535 803207397 773010373 740951061 707106709
   671558856 634393215 595699191 555570125 514102697 471396684
   427555084 382683396 336889863 290284633 242980122 195090293
   146730422 98017096 49067616 0 -49067617 -98017097 -146730423
   -195090294 -242980123 -290284634 -336889864 -382683397
   -427555085 -471396685 -514102698 -555570126 -595699192
   -634393216 -671558857 -707106710 -740951062 -773010374
   -803207398 -831469536 -857728482 -881921173 -903989196
   -923879385 -941543937 -956940175 -970031143 -980785132
   -989176393 -995184660 -998795391 -999999881 -998795391
   -995184660 -989176393 -980785132 -970031143 -956940175
   -941543937 -923879385 -903989196 -881921173 -857728482
   -831469536 -803207398 -773010374 -740951062 -707106710
   -671558857 -634393216 -595699192 -555570126 -514102698
   -471396685 -427555085 -382683397 -336889864 -290284634
   -242980123 -195090294 -146730423 -98017097 -49067617 0))
