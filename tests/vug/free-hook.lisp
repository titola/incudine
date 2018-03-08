(in-package :incudine-tests)

(define-ugen ugen-free-hook-test-1 null ((fn function))
  (initialize
    (when (dsp-node)
      (push fn (free-hook (dsp-node))))))

(dsp! dsp-free-hook-test-1 ((fn function))
  (initialize (push fn (free-hook (dsp-node)))))

(dsp! dsp-free-hook-test-2 ((fn function))
  (ugen-free-hook-test-1 fn))

(defun dsp-free-hook-test (dsp-fn)
  (let ((hook nil)
        (fn (lambda (n) (msg info "FREE-HOOK test for node ~A" n))))
    (with-local-logger (*logger-stream* :info)
      (bounce-to-buffer (*buffer-test-c1*)
        (funcall dsp-fn fn :id 123)
        (setf hook (copy-list (free-hook 123)))))
    (values (= (count fn hook) 1)
            (= (length hook) 2))))

(deftest dsp-free-hook.1
    (dsp-free-hook-test 'dsp-free-hook-test-1)
  T T)

(deftest dsp-free-hook.2
    (dsp-free-hook-test 'dsp-free-hook-test-2)
  T T)
