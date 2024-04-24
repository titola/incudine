(in-package :incudine-tests)

(defun profile-function-test (x) x)

(dsp! profile-test-1 () (out 0))
(dsp! profile-test-2 () (out 0))
(dsp! profile-test-3 () (out 0))

(dsp! profile-test-4 (dur)
  (out (envelope (reduce-warnings (make-envelope (list 1/4 0) (list dur)))
                 :done-action #'free)))

(defun intern-profiled-function-names ()
  (sort (mapcar (lambda (s) (intern (string s) "INCUDINE-TESTS")) (profile))
        'string< :key 'symbol-name))

(deftest profile-test.1
    (progn
      (unprofile)
      (profile profile-test-1 profile-test-2 profile-function-test profile-test-3)
      (with-logger (*null-output*)
        (bounce-to-buffer (*buffer-test-c1* :frames 12345)
          (profile-test-1)
          (profile-test-2 :id 7)
          (profile-test-3 :id 999)
          (at 10000 #'profile-test-1 :id 610)
          (at 11111 #'profile-test-1 :id 101)
          (at 11111 #'profile-test-2 :id 102)
          (at 11111 #'profile-test-3 :id 103)))
      (profile-function-test 123)
      (profile-report)
      (values (intern-profiled-function-names)
              (progn (unprofile profile-test-2 profile-test-1)
                     (intern-profiled-function-names))
              (progn (profile-reset)
                     (intern-profiled-function-names))
              (progn (unprofile profile-test-3 profile-function-test)
                     (intern-profiled-function-names))))
  (INIT-PROFILE-TEST-1
   INIT-PROFILE-TEST-2
   INIT-PROFILE-TEST-3
   PROFILE-FUNCTION-TEST
   PROFILE-TEST-1.1/NODE-1
   PROFILE-TEST-1.2/NODE-610
   PROFILE-TEST-1.3/NODE-101
   PROFILE-TEST-2.1/NODE-7
   PROFILE-TEST-2.2/NODE-102
   PROFILE-TEST-3.1/NODE-999
   PROFILE-TEST-3.2/NODE-103)
  (INIT-PROFILE-TEST-3
   PROFILE-FUNCTION-TEST
   PROFILE-TEST-3.1/NODE-999
   PROFILE-TEST-3.2/NODE-103)
  (INIT-PROFILE-TEST-3
   PROFILE-FUNCTION-TEST)
  NIL)

(deftest profile-test.2
  (progn
    (unprofile)
    (profile profile-test-1)
    (let (res)
      (with-logger (*null-output*)
        (bounce-to-buffer (*buffer-test-c1* :frames 1)
          ;; The wrapper extends action.
          (profile-test-1 :id 7 :action (lambda (n) (setf res (node-id n))))))
      (values res (intern-profiled-function-names) (unprofile))))
  7 (INIT-PROFILE-TEST-1 PROFILE-TEST-1.1/NODE-7) NIL)

(with-dsp-test (profile-test.3
      :md5 #(118 212 150 153 99 77 24 11 203 16 35 255 110 172 91 252))
  (profile-test-4 1 :action #'pause :id 123)
  (profile-test-4 2 :action #'pause :id 124)
  (profile-test-4 99999999 :id 125)
  (at *sample-rate* #'profile-node 123 9999999)
  (at (* 2 *sample-rate*) #'profile-node 124 1500)
  ;; Profiled without gaps.
  (at (* 3 *sample-rate*) #'profile-node 125 10000))

;;; PROFILE-NODE with a group node to get the same result of profile-test.3.
(with-dsp-test (profile-test.4
      :md5 #(118 212 150 153 99 77 24 11 203 16 35 255 110 172 91 252))
  (make-group 1000 :action #'pause)
  (profile-node 1000 99999999)
  (profile-test-4 99999999 :id 125)
  (at *sample-rate* #'profile-test-4 1 :id 123 :head 1000)
  (at (* 2 *sample-rate*) #'profile-test-4 2 :id 124 :head 1000)
  ;; The group node 1000 is internally unpaused, but the paused nodes
  ;; of that group are ignored.
  (at (+ (* 2 *sample-rate*) 1500) #'pause 124))
