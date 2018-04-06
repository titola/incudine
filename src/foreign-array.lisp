;;; Copyright (c) 2013-2018 Tito Latini
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :incudine)

(defstruct (foreign-array (:include incudine-object)
                          (:constructor %make-foreign-array)
                          (:copier nil))
  (data nil :type (or foreign-pointer null))
  (type 'sample)
  (size 0 :type non-negative-fixnum)
  (free-func #'foreign-free :type function)
  (real-time-p nil :type boolean))

(define-constant +foreign-array-pool-initial-size+ 2000)

(defvar *foreign-array-pool*
  (make-incudine-object-pool +foreign-array-pool-initial-size+
                             #'%make-foreign-array nil))
(declaim (type incudine-object-pool *foreign-array-pool*))

(defvar *rt-foreign-array-pool*
  (make-incudine-object-pool +foreign-array-pool-initial-size+
                             #'%make-foreign-array t))
(declaim (type incudine-object-pool *rt-foreign-array-pool*))

(declaim (inline fill-foreign-array))
(defun fill-foreign-array (obj data size type free-func real-time-p)
  (setf (foreign-array-data obj) data
        (foreign-array-size obj) size
        (foreign-array-type obj) type
        (foreign-array-free-func obj) free-func
        (foreign-array-real-time-p obj) real-time-p)
  obj)

(declaim (inline safe-foreign-rt-free))
(defun safe-foreign-rt-free (ptr)
  ;; We use a realtime memory allocator without lock, and the memory
  ;; is (de)allocated in realtime from a single rt-thread.
  (rt-eval () (foreign-rt-free ptr)))

(defun make-rt-foreign-array (dimension element-type zero-p
                              initial-element initial-contents
                              &optional (dynamic-finalizer-p t))
  (let ((data (foreign-rt-alloc element-type :count dimension
                                :zero-p zero-p
                                :initial-element initial-element
                                :initial-contents initial-contents)))
    (incudine-finalize
      (fill-foreign-array
        (incudine.util::alloc-rt-object *rt-foreign-array-pool*)
        data dimension element-type #'safe-foreign-rt-free t)
      (lambda ()
        (safe-foreign-rt-free data)
        (incudine-object-pool-expand *rt-foreign-array-pool* 1))
      dynamic-finalizer-p)))

(defun make-nrt-foreign-array-data (dimension element-type zero-p
                                    initial-element initial-contents)
  (cond (zero-p
         (incudine.external:foreign-set
          (foreign-alloc element-type :count dimension)
          0 (* (foreign-type-size element-type) dimension)))
        (initial-element
         (foreign-alloc element-type :count dimension
                        :initial-element initial-element))
        (initial-contents
         (foreign-alloc element-type :count dimension
                        :initial-contents initial-contents))
        (t (foreign-alloc element-type :count dimension))))

(defun make-nrt-foreign-array (dimension element-type zero-p
                               initial-element initial-contents
                               &optional (dynamic-finalizer-p t))
  (let ((data (make-nrt-foreign-array-data dimension element-type zero-p
                                           initial-element
                                           initial-contents)))
    (incudine-finalize
      (fill-foreign-array
        (incudine.util::alloc-object *foreign-array-pool*)
        data dimension element-type #'foreign-free nil)
      (lambda ()
        (foreign-free data)
        (incudine-object-pool-expand *foreign-array-pool* 1))
      dynamic-finalizer-p)))

(declaim (inline %%make-foreign-array))
(defun %%make-foreign-array (dimension element-type zero-p
                             initial-element initial-contents
                             &optional (dynamic-finalizer-p t))
  (if (allow-rt-memory-p)
      (make-rt-foreign-array dimension element-type zero-p
                             initial-element initial-contents
                             dynamic-finalizer-p)
      (make-nrt-foreign-array dimension element-type zero-p
                              initial-element initial-contents
                              dynamic-finalizer-p)))

(defun make-foreign-array (dimension element-type &key zero-p
                           initial-element initial-contents)
  (%%make-foreign-array
    dimension element-type zero-p initial-element initial-contents))

(defmethod free-p ((obj foreign-array))
  (null (foreign-array-data obj)))

(defmethod free ((obj foreign-array))
  (unless (free-p obj)
    (funcall (foreign-array-free-func obj) (foreign-array-data obj))
    (incudine-cancel-finalization obj)
    (setf (foreign-array-data obj) nil)
    (setf (foreign-array-size obj) 0)
    (if (foreign-array-real-time-p obj)
        (incudine.util::free-rt-object obj *rt-foreign-array-pool*)
        (incudine.util::free-object obj *foreign-array-pool*))
    (nrt-msg debug "Free ~A" (type-of obj)))
  (values))

(in-package :incudine.util)

(define-constant +fast-nrt-foreign-array-max-size+ 64)

(defmacro with-foreign-array ((var type &optional (count 1)) &body body)
  (with-gensyms (array-wrap array-wrap-p)
    `(let* ((,array-wrap-p (or (rt-thread-p)
                               (> ,count +fast-nrt-foreign-array-max-size+)))
            (,array-wrap (if ,array-wrap-p
                             (incudine::%%make-foreign-array
                               ,count ,type nil nil nil nil)))
            (,var (if ,array-wrap-p
                      (incudine:foreign-array-data ,array-wrap)
                      (incudine.util::foreign-nrt-alloc ,type :count ,count))))
       (declare (ignorable ,array-wrap))
       (unwind-protect (progn ,@body)
         (reduce-warnings
           (if ,array-wrap-p
               (incudine:free ,array-wrap)
               (incudine.util::foreign-nrt-free ,var)))))))

(defmacro %with-samples ((bindings set let) &body body)
  (with-gensyms (c-array)
    (if *use-foreign-sample-p*
        (let* ((bindings (loop for x in bindings
                               collect (if (consp x) x (list x 0d0))))
               (vars (mapcar (lambda (x) (declare (ignore x)) (gensym))
                             bindings))
               (paral-p (eq let 'let*))
               (count 0))
          `(with-foreign-array (,c-array 'sample ,(length bindings))
             (,let ,(loop for (x y) in bindings
                          for v on vars
                          collect (list (car v) (if paral-p `(sample ,y) y))
                          when (and paral-p (cdr v))
                            collect (list x (car v)))
               ,@(when paral-p
                   `((declare (ignorable ,@(butlast (mapcar #'car bindings))))))
               (symbol-macrolet
                   ,(mapcar (lambda (x)
                              (prog1 `(,(if (consp x) (car x) x)
                                       (smp-ref ,c-array ,count))
                                (incf count)))
                            bindings)
                 (,set ,@(loop for x in bindings
                               for v in vars
                               append `(,(car x)
                                        ,(if paral-p v `(sample ,v)))))
                 ,@body))))
        `(,let (,@(mapcar (lambda (x)
                            (if (consp x)
                                `(,(car x) (sample ,(cadr x)))
                                `(,x ,+sample-zero+)))
                          bindings))
           (declare (type sample
                          ,@(mapcar (lambda (x) (if (consp x) (car x) x))
                                    bindings)))
           ,@body))))

;;; The expansion within the definition of a VUG is different
;;; (see %WITH-SAMPLES in `vug/vug.lisp')
(defmacro with-samples (bindings &body body)
  `(%with-samples (,bindings psetf let) ,@body))

(defmacro with-samples* (bindings &body body)
  `(%with-samples (,bindings setf let*) ,@body))
