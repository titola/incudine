(in-package :incudine)

(define-condition incudine-deprecation-warning (simple-warning) ())

(defvar *deprecated* nil)

(defun deprecated-symbol-names (&key obsolete-p)
  "Return a list of lists

    (deprecated-symbol :from date)

If OBSOLETE-P is T, the deprecated symbol is obsolete starting from
the specified date.

A deprecated symbol is obsolete after one year."
  (flet ((date (l) (getf (cdr l) :from)))
    (sort (if obsolete-p
              (mapcar (lambda (x)
                        (list (car x) :from (+ (date x) 10000)))
                      *deprecated*)
              (copy-tree *deprecated*))
          #'< :key #'date)))

(defmacro deprecated-function ((name args &rest body)
                               &key run-time-message-p date package
                               (replacement (car body)))
  (with-gensyms (form rest)
    `(progn
       (export ',name ,(or package *package*))
       (pushnew (list ',name :from ,date) *deprecated*)
       (defun ,name ,args
         ,@(if run-time-message-p
               `((incudine::deprecated-msg ',name ',replacement)))
         ,@body)
       (define-compiler-macro ,name (&whole ,form &rest ,rest)
         (declare (ignore ,rest))
         (warn (make-condition 'incudine-deprecation-warning
                 :format-control "~S is deprecated,~%use ~S instead."
                 :format-arguments (list ',name ',replacement)))
         ,form))))

(defmacro deprecated-macro ((name args &rest body)
                            &key date package (replacement (car body)))
  (multiple-value-bind (decl form)
      (incudine.util::separate-declaration body)
    `(progn
       (export ',name ,(or package *package*))
       (pushnew (list ',name :from ,date) *deprecated*)
       (defmacro ,name ,args ,@decl
         (warn (make-condition 'incudine-deprecation-warning
           :format-control "~S is deprecated,~%use ~S instead."
           :format-arguments (list ',name ',replacement)))
         ,@form))))

(defun deprecated-msg (old new)
  (msg warn "~S is deprecated,~%~6Tuse ~S instead." old new))

;;;---------------------[ Deprecated symbols ]----------------------

(in-package :incudine.util)

(incudine::deprecated-function
   (db->lin (value) (db->linear value))
   :replacement incudine.util:db->linear
   :date 20181209
   :package "INCUDINE.UTIL")

(incudine::deprecated-function
   (lin->db (value) (linear->db value))
   :replacement incudine.util:linear->db
   :date 20181209
   :package "INCUDINE.UTIL")

(in-package :incudine.vug)

(incudine::deprecated-macro
   (done-self () `(done-p))
   :replacement incudine:done-p
   :date 20180512
   :package "INCUDINE.VUG")

(in-package :incudine.analysis)

(incudine::deprecated-function
   (buffer->pvbuffer (buf partsize &key (start 0) frames)
     (make-part-convolve-buffer buf partsize :start start :frames frames))
   :replacement incudine.analysis:make-part-convolve-buffer
   :date 20190222
   :package "INCUDINE.ANALYSIS"
   :run-time-message-p t)
