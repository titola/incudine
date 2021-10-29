(in-package :snd)

(defmacro with-interrupts (&body body)
  `(sb-sys:with-interrupts ,@body))

(defmacro run-program (program-name args &rest rest)
  (let ((name (gensym)))
    `(let ((,name ,program-name))
       (sb-ext:run-program ,name ,args ,@rest
                           :search (null (pathname-name (probe-file ,name)))))))

(defun process-stream ()
  (sb-ext:process-pty *snd*))

(defun process-alive-p ()
  (and *snd* (sb-ext:process-alive-p *snd*)))

(defun process-exited-p ()
  (eq (sb-ext:process-status *snd*) :exited))

(defun process-exit-code ()
  (sb-ext:process-exit-code *snd*))
