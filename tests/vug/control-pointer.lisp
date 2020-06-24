(in-package :incudine-tests)

(enable-sharp-square-bracket-syntax)

(dsp! dsp-control-1 ((node (or fixnum node)) (control-name symbol)
                     initial-value value duration)
  (with ((ptr (cffi:null-pointer))
         (func nil)
         (this (dsp-node)))
    (declare (type pointer ptr)
             (type (or function null) func)
             (type node this))
    (initialize
      (push (lambda (n) n (free this)) (free-hook node))
      (setf (values ptr func) (control-pointer node control-name)))
    (setf (smp-ref ptr) (line initial-value value duration))
    (if func (funcall (the function func)))
    ;; Oscillation to monitor this instance after FREE NODE.
    (out (sine 8000 .2))))

(dsp! controlled-1 (freq amp) (out (sine freq amp)))

(with-dsp-test (control-pointer.1
      :md5 #(145 31 168 81 179 232 47 97 172 231 43 244 24 252 220 179))
  (controlled-1 440 .3 :id 1
    :action (lambda (n) (dsp-control-1 n 'freq 440 880 1 :id 2 :before n)))
  (at #[1.5 s] #'set-controls 2 :value 100 :duration 2)
  (at #[3.8 s] #'set-controls 2 :value 440 :duration .25)
  ;; It implies `(free 2)' called from the free-hook of node 1.
  (at #[4.3 s] #'free 1))
