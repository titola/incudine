(in-package :incudine-tests)

;; GET-DSP-NODE is necessary to test the special variable *DSP-NODE*,
;; otherwise a direct call to DSP-NODE is internally transformed.
(defun get-dsp-node () (dsp-node))

(dsp! dsp-node-test-1 ()
  (with ((p t))
    (initialize
      (assert (and (dsp-node) (eq (dsp-node) (get-dsp-node)))))
    ;; Performance time.
    (when p
      (setf p nil)
      (assert (and (dsp-node) (eq (dsp-node) (get-dsp-node)))))
    (out 1)))

(deftest dsp-node.1
    (with-logger (*null-output*)
      (bounce-to-buffer (*buffer-test-c1* :frames 4)
        (at 0 #'dsp-node-test-1)
        (at 1 #'free 0)
        (at 2 #'dsp-node-test-1)
        (at 3 #'free 0))
      (values
        (loop for i below 4 collect (buffer-value *buffer-test-c1* i))
        ;; Bug fixed: side effect from the reused DSP instance.
        (dsp-node)))
  (1d0 0d0 1d0 0d0)
  NIL)

(deftest dsp-node.2
    (with-logger (*null-output*)
      (bounce-to-buffer (*buffer-test-c1* :frames 10)
        (at 0 #'dsp-node-test-1 :id 1)
        (at 1 #'dsp-node-test-1 :id 2)
        (at 2 #'dsp-node-test-1 :id 3)
        (at 3 #'free 2)
        (at 4 #'play (lambda ()) :id 2)
        (at 5 #'dsp-node-test-1 :id 4))
      (loop for i below 10 collect (buffer-value *buffer-test-c1* i)))
  (1d0 2d0 3d0 2d0 2d0 3d0 3d0 3d0 3d0 3d0))
