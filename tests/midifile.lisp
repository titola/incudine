(in-package :incudine-tests)

(defvar *test-midi-file*
  (merge-pathnames "test.mid"
    #.(load-time-value (or *compile-file-pathname* *load-pathname*))))

(defun number-of-midi-events (path)
  (midifile:with-open-midifile (mf path)
    (loop for n from 0 while (midifile:read-event mf) finally (return n))))

(defmacro with-output-midifile-test ((var) &body body)
  `(midifile:with-open-midifile (,var *test-midi-file* :direction :output
                                 :if-exists :supersede)
     ,@body))

(deftest midifile.1
    (let ((test-output-stream nil))
      (with-output-midifile-test (mf)
        (setf test-output-stream (midifile:output-stream-p mf))
        ;; Write 16 empty tracks.
        (dotimes (i 15) (midifile:next-track mf)))
      (let ((bytes (coerce (alexandria:read-file-into-byte-vector
                             *test-midi-file*)
                           'list))
            (format (midifile:format *test-midi-file*))
            (ntracks (midifile:number-of-tracks *test-midi-file*))
            (ppqn (midifile:ppqn *test-midi-file*))
            (tempo (midifile:tempo *test-midi-file*))
            (nevents (number-of-midi-events *test-midi-file*)))
        (values (every (lambda (x) (equal x '(#xff #x2f 0)))
                       (midifile:with-open-midifile (mf *test-midi-file*)
                         (loop for st = (midifile:read-event mf)
                               while st
                               collect (list st (midifile:message-data1 mf)
                                             (midifile:message-data2 mf)))))
                test-output-stream format ntracks ppqn tempo nevents bytes)))
  T T 1 16 480 120.0 16
  #.(loop for i below 16
          when (= i 0)
            ;; header-chunk
            append '(77 84 104 100 0 0 0 6 0 1 0 16 1 224)
          append '(77 84 114 107 0 0 0 4 0 255 47 0)))

(defun midifile-count-events (&optional tracks)
  (midifile:with-open-midifile (mf *test-midi-file*)
    (let ((n 0))
      (if (consp tracks)
          (loop for trk = (midifile:current-track mf)
                while (midifile:read-event mf)
                when (member trk tracks :test #'=)
                  do (incf n))
          (loop while (midifile:read-event mf) do (incf n)))
      n)))

(deftest midifile.2
    (let ((nevent-list '(2 3 5 8 13 21)))
      (with-output-midifile-test (mf)
        (dolist (nevents nevent-list)
          (dotimes (i (1- nevents))
            (midifile:write-short-event mf i
              (midifile:message #xb0 7 (random 128)) 3))
          (midifile:next-track mf)))
      (values (= (midifile-count-events) (1+ (reduce #'+ nevent-list)))
              (= (midifile-count-events '(1 3 5)) 32)
              (= (midifile-count-events '(0 2 4)) 20)))
  T T T)

(deftest midifile.3
    (progn
      (with-output-midifile-test (mf)
        (dotimes (ch 2)
          (dotimes (i 20)
            (midifile:write-short-event mf (* i .1)
                                        (midifile:message (logior #xb0 ch) 7 i)
                                        3))
          (when (= ch 0)
            ;; Test EOT 2.1 beats after the last event.
            (midifile:end-of-track mf 4)
            (midifile:next-track mf))))
      (values (midifile:with-open-midifile (mf *test-midi-file*)
                (loop while (midifile:read-event mf)
                      collect (list (midifile:event-time mf)
                                    (midifile:event-delta-time mf)
                                    (midifile:event-beats mf)
                                    (midifile:message-status mf)
                                    (midifile:message-data1 mf)
                                    (midifile:message-data2 mf))))
              (coerce (alexandria:read-file-into-byte-vector *test-midi-file*)
                      'list)))
  ((0 0 0.0d0 176 7 0) (48 48 0.1d0 176 7 1) (96 48 0.2d0 176 7 2)
   (144 48 0.3d0 176 7 3) (192 48 0.4d0 176 7 4) (240 48 0.5d0 176 7 5)
   (288 48 0.6d0 176 7 6) (336 48 0.7d0 176 7 7) (384 48 0.8d0 176 7 8)
   (432 48 0.9d0 176 7 9) (480 48 1.0d0 176 7 10) (528 48 1.1d0 176 7 11)
   (576 48 1.2d0 176 7 12) (624 48 1.3d0 176 7 13) (672 48 1.4d0 176 7 14)
   (720 48 1.5d0 176 7 15) (768 48 1.6d0 176 7 16) (816 48 1.7d0 176 7 17)
   (864 48 1.8d0 176 7 18) (912 48 1.9d0 176 7 19)
   ;; End of Track 0; absolute time is zero because MIDIFILE:READ-EVENT calls
   ;; MIDIFILE:NEXT-TRACK after a MIDI EOT Meta event, so the track-time is zero.
   (0 1008 0.0d0 255 47 0)
   (0 0 0.0d0 177 7 0) (48 48 0.1d0 177 7 1) (96 48 0.2d0 177 7 2)
   (144 48 0.3d0 177 7 3) (192 48 0.4d0 177 7 4) (240 48 0.5d0 177 7 5)
   (288 48 0.6d0 177 7 6) (336 48 0.7d0 177 7 7) (384 48 0.8d0 177 7 8)
   (432 48 0.9d0 177 7 9) (480 48 1.0d0 177 7 10) (528 48 1.1d0 177 7 11)
   (576 48 1.2d0 177 7 12) (624 48 1.3d0 177 7 13) (672 48 1.4d0 177 7 14)
   (720 48 1.5d0 177 7 15) (768 48 1.6d0 177 7 16) (816 48 1.7d0 177 7 17)
   (864 48 1.8d0 177 7 18) (912 48 1.9d0 177 7 19)
   ;; End of Track 1, the last, so the absolute time is not reset to zero.
   (912 0 1.9d0 255 47 0))
  (77 84 104 100 0 0 0 6 0 1 0 2 1 224
   ;; Track 0
   77 84 114 107 0 0 0 66
   ;; Running status is in effect, delta-time 480/10
   0 176 7 0 48 7 1 48 7 2 48 7 3 48 7 4 48 7 5 48 7 6 48 7 7 48 7 8
   48 7 9 48 7 10 48 7 11 48 7 12 48 7 13 48 7 14 48 7 15 48 7 16
   48 7 17 48 7 18 48 7 19 135 112 255 47 0
   ;; Track 1
   77 84 114 107 0 0 0 65
   0 177 7 0 48 7 1 48 7 2 48 7 3 48 7 4 48 7 5 48 7 6 48 7 7 48 7 8
   48 7 9 48 7 10 48 7 11 48 7 12 48 7 13 48 7 14 48 7 15 48 7 16 48
   7 17 48 7 18 48 7 19 0 255 47 0))

(defun midifile-tempo-map-test ()
  (multiple-value-bind (bpms delta-times) (midifile:tempo *test-midi-file*)
    (values bpms delta-times
            (midifile:with-open-midifile (mf *test-midi-file*)
              (loop for st = (midifile:read-event mf)
                    while st
                    collect (list (midifile:event-time mf)
                                  (/ (floor (* (midifile:event-seconds mf) 100))
                                     100.0)
                                  st
                                  (coerce (subseq (midifile:message-buffer mf)
                                                  0 (midifile:message-length mf))
                                          'list)))))))

(deftest midifile-tempo-map.1
    (progn
      (with-output-midifile-test (mf)
        (loop for bpm in '(96 145 120) for time in '(0 8 20)
              do (midifile:write-event mf time (midifile:tempo-message bpm)))
        (midifile:next-track mf)
        (loop for i from 0 by 10
              for time below 25 by 3
              do (midifile:write-short-event mf time
                                             (midifile:message #xb0 7 i) 3)))
      (midifile-tempo-map-test))
  (96.0 145.0 120.0)
  (8.0 12.0)
   ;; Tempo track.
  ((0 0.0 255 (255 81 3 9 137 104))
   (3840 5.0  255 (255 81 3 6 80 97))
   (9600 9.96 255 (255 81 3 7 161 32))
   (0 0.0 255 (255 47 0))
   ;; Track 1
   (    0  0.0  176 (176 7 0))
   ( 1440  1.87 176 (176 7 10))
   ( 2880  3.75 176 (176 7 20))
   ( 4320  5.41 176 (176 7 30))
   ( 5760  6.65 176 (176 7 40))
   ( 7200  7.89 176 (176 7 50))
   ( 8640  9.13 176 (176 7 60))
   (10080 10.46 176 (176 7 70))
   (11520 11.96 176 (176 7 80))
   (11520 11.96 255 (255 47 0))))

(deftest midifile-tempo-map.2
    (let ((tenv (incudine:make-tempo-envelope '(96 145 120) '(8 12)
                                              :curve :step)))
      (unwind-protect
           (progn
             (with-output-midifile-test (mf)
               ;; Track 0
               (midifile:write-tempo-track mf tenv)
               ;; Track 1
               (loop for i from 0 by 10
                     for time below 25 by 3
                     do (midifile:write-short-event mf time
                                                    (midifile:message #xb0 7 i)
                                                    3)))
             (midifile-tempo-map-test))
        (incudine:free tenv)))
  (96.0 145.0 120.0)
  (8.0 12.0)
   ;; Tempo track.
  ((0 0.0 255 (255 81 3 9 137 104))
   (3840 5.0  255 (255 81 3 6 80 97))
   (9600 9.96 255 (255 81 3 7 161 32))
   (0 0.0 255 (255 47 0))
   ;; Track 1
   (    0  0.0  176 (176 7 0))
   ( 1440  1.87 176 (176 7 10))
   ( 2880  3.75 176 (176 7 20))
   ( 4320  5.41 176 (176 7 30))
   ( 5760  6.65 176 (176 7 40))
   ( 7200  7.89 176 (176 7 50))
   ( 8640  9.13 176 (176 7 60))
   (10080 10.46 176 (176 7 70))
   (11520 11.96 176 (176 7 80))
   (11520 11.96 255 (255 47 0))))

;; MIDIFILE:STRING-MESSAGE test, MIDIFILE:RELEASE-CACHED-BUFFERS test
;; and delete the temporary file.
(deftest midifile-last-test
    (let ((str "End of MIDI File tests"))
      (with-output-midifile-test (mf)
        (midifile:write-event mf 0 (midifile:string-message 1 str)))
      (let ((bytes (alexandria:read-file-into-byte-vector *test-midi-file*)))
        (delete-file *test-midi-file*)
        (midifile:release-cached-buffers)
        (values #+sbcl
                (incudine.util::octets-to-string
                  bytes :start 26 :end (+ 26 (length str)))
                (coerce bytes 'list))))
  #+sbcl "End of MIDI File tests"
  (77 84 104 100 0 0 0 6 0 0 0 1 1 224 77 84 114 107 0 0 0 30 0 255
   1 22 69 110 100 32 111 102 32 77 73 68 73 32 70 105 108 101 32
   116 101 115 116 115 0 255 47 0))
