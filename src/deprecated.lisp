(in-package :incudine)

(define-condition incudine-deprecation-warning (simple-warning) ())
(define-condition incudine-deprecation-error (incudine-simple-error) ())

(defun deprecation-error (name replacement obsolete-p)
  (multiple-value-bind (func type)
      (if obsolete-p
          (values #'error 'incudine-deprecation-error)
          (values #'warn 'incudine-deprecation-warning))
    (funcall func type
      :format-control "~S is ~:[deprecated~;obsolete~],~%use ~S instead."
      :format-arguments (list name obsolete-p replacement))))

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
                               (replacement (car body)) obsolete-p)
  (with-gensyms (form rest)
    `(progn
       (export ',name ,(or package *package*))
       (pushnew (list ',name :from ,date) *deprecated*)
       (defun ,name ,args
         ,@(cond (obsolete-p
                  `((with-simple-restart (continue "Use symbol anyway.")
                      (incudine::deprecation-error ',name ',replacement t))))
                 (run-time-message-p
                  `((incudine::deprecated-msg ',name ',replacement))))
         ,@body)
       (define-compiler-macro ,name (&whole ,form &rest ,rest)
         (declare (ignore ,rest))
         (incudine::deprecation-error ',name ',replacement ,obsolete-p)
         ,form))))

(defmacro deprecated-macro ((name args &rest body)
                            &key date package (replacement (car body))
                            obsolete-p)
  (multiple-value-bind (decl form)
      (incudine.util::separate-declaration body)
    `(progn
       (export ',name ,(or package *package*))
       (pushnew (list ',name :from ,date) *deprecated*)
       (defmacro ,name ,args ,@decl
         (incudine::deprecation-error ',name ',replacement ,obsolete-p)
         ,@form))))

(defun deprecated-msg (old new)
  (msg warn "~S is deprecated,~%~6Tuse ~S instead." old new))

;;;---------------------[ Deprecated symbols ]----------------------

(in-package :incudine)

(incudine::deprecated-function
   (time-at (tempo-env beats &optional (offset 0))
     (beats->seconds tempo-env beats offset))
   :replacement incudine:beats->seconds
   :date 20190718
   :package "INCUDINE")

(in-package :incudine.util)

(incudine::deprecated-function
   (db->lin (value) (db->linear value))
   :replacement incudine.util:db->linear
   :date 20181209
   :obsolete-p t
   :package "INCUDINE.UTIL")

(incudine::deprecated-function
   (lin->db (value) (linear->db value))
   :replacement incudine.util:linear->db
   :date 20181209
   :obsolete-p t
   :package "INCUDINE.UTIL")

(incudine::deprecated-macro
   (with-local-logger (&whole whole
                       (&optional stream level (time-unit nil time-unit-p)
                        time-format-function) &body body)
     (declare (ignore stream level time-unit time-unit-p time-format-function body))
     `(with-logger ,@(cdr whole)))
   :replacement incudine.util:with-logger
   :date 20200613
   :package "INCUDINE.UTIL")

(in-package :incudine.analysis)

(incudine::deprecated-function
   (buffer->pvbuffer (buf partsize &key (start 0) frames)
     (make-part-convolve-buffer buf partsize :start start :frames frames))
   :replacement incudine.analysis:make-part-convolve-buffer
   :date 20190222
   :obsolete-p t
   :package "INCUDINE.ANALYSIS"
   :run-time-message-p t)
