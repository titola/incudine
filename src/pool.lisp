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

(in-package :incudine.util)

;;; CONS-POOL

(defstruct (cons-pool
             (:constructor make-cons-pool (&key data (size (length data))
                                           expand-function grow))
             (:copier nil))
  "Cons pool type."
  (data (incudine:incudine-missing-arg "CONS-POOL without initial contents.")
        :type list)
  (size 0 :type non-negative-fixnum)
  (expand-function #'default-expand-cons-pool :type function)
  (grow 128 :type non-negative-fixnum))

(setf
 (documentation 'make-cons-pool 'function)
 "Create and return a new CONS-POOL structure initialized with the
list DATA of length SIZE.

The function EXPAND-FUNCTION is used to add new conses to the pool.

The pool is incremented by GROW if more conses are required."
 (documentation 'cons-pool-size 'function)
 "Return the cons pool size.")

(defmethod print-object ((obj cons-pool) stream)
  (format stream "#<CONS-POOL ~D>" (cons-pool-size obj)))

(defmacro expand-cons-pool (pool delta new-form)
  "Add DELTA cons cells to the POOL.

NEW-FORM is evaluated to set the CAR of a CONS."
  (with-gensyms (lst p d i exp-size)
    `(let ((,p ,pool)
           (,d ,delta))
       (declare (type cons-pool ,p) (type positive-fixnum ,d))
       (labels ((expand (,lst ,i)
                  (declare (type non-negative-fixnum ,i)
                           (type list ,lst))
                  (if (zerop ,i)
                      ,lst
                      (expand (cons ,new-form ,lst) (1- ,i)))))
         (with-struct-slots ((data size grow) ,p cons-pool)
           (let ((,exp-size (max ,d grow)))
             (incf size ,exp-size)
             (setf data (expand data ,exp-size))))))))

(defun default-expand-cons-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta nil))

(defmacro cons-pool-push-cons (pool cons)
  "Return the CONS cell to the POOL."
  (with-gensyms (p x)
    `(let ((,p ,pool)
           (,x ,cons))
       (declare (type cons ,x) (type cons-pool ,p))
       (with-struct-slots ((data size) ,p cons-pool)
         (incf size)
         (setf (cdr ,x) data data ,x)))))

(defmacro cons-pool-pop-cons (pool)
  "Get a cons cell from POOL."
  (with-gensyms (p result)
    `(let ((,p ,pool))
       (declare (type cons-pool ,p))
       (with-struct-slots ((data size expand-function) ,p cons-pool)
         (when (zerop size)
           (funcall expand-function ,p))
         (let ((,result data))
           (decf size)
           (setf data (cdr data)
                 (cdr ,result) nil)
           ,result)))))

(defmacro cons-pool-push-list (pool list)
  "Return the LIST to the POOL."
  (with-gensyms (lst p i l)
    `(let ((,p ,pool)
           (,lst ,list))
       (declare (type list ,lst) (type cons-pool ,p))
       (do ((,i 1 (1+ ,i))
            (,l ,lst (cdr ,l)))
           ((null (cdr ,l))
            (with-struct-slots ((data size) ,p cons-pool)
              (incf size ,i)
              (setf (cdr ,l) data data ,lst)))
         (declare (type positive-fixnum ,i) (type list ,l))))))

(defmacro cons-pool-pop-list (pool list-size)
  "Get a list of length LIST-SIZE from POOL."
  (with-gensyms (lsize p i lst)
    `(let ((,lsize ,list-size)
           (,p ,pool))
       (declare (type positive-fixnum ,lsize)
                (type cons-pool ,p))
       (with-struct-slots ((data size) ,p cons-pool)
         (when (< size ,lsize)
           (funcall (cons-pool-expand-function ,p) ,p ,lsize))
         (do ((,i 1 (1+ ,i))
              (,lst data (cdr ,lst)))
             ((= ,i ,lsize)
              (prog1 data
                (decf size ,lsize)
                (setf data (cdr ,lst))
                (setf (cdr ,lst) nil)))
           (declare (type positive-fixnum ,i) (type list ,lst)))))))

(defun empty-cons-pool (pool &key cancel-finalizations)
  (when cancel-finalizations
    (mapc #'cancel-finalization (cons-pool-data pool)))
  (setf (cons-pool-size pool) 0)
  (setf (cons-pool-data pool) nil))

;;; NRT GLOBAL CONS-POOL (used in nrt-thread)

(defvar *nrt-global-pool*
  (make-cons-pool
    :data (make-list 2048)
    :size 2048
    :grow 2048))
(declaim (type cons-pool *nrt-global-pool*))

(defvar *nrt-global-pool-spinlock* (make-spinlock "NRT-GLOBAL-POOL"))
(declaim (type spinlock *nrt-global-pool-spinlock*))

(defun nrt-global-pool-push-cons (cons)
  "Return the CONS cell to the global pool.

This function has to be called from any non-real-time thread."
  (with-spinlock-held (*nrt-global-pool-spinlock*)
    (cons-pool-push-cons *nrt-global-pool* cons)))

(defun nrt-global-pool-pop-cons ()
  "Return a preallocated cons cell.

This function has to be called from any non-real-time thread."
  (with-spinlock-held (*nrt-global-pool-spinlock*)
    (cons-pool-pop-cons *nrt-global-pool*)))

(defun nrt-global-pool-push-list (lst)
  "Return the LIST to the global pool.

This function has to be called from any non-real-time thread."
  (with-spinlock-held (*nrt-global-pool-spinlock*)
    (cons-pool-push-list *nrt-global-pool* lst)))

(defun nrt-global-pool-pop-list (list-size)
  "Return a preallocated list of length LIST-SIZE.

This function has to be called from any non-real-time thread."
  (with-spinlock-held (*nrt-global-pool-spinlock*)
    (cons-pool-pop-list *nrt-global-pool* list-size)))

;;; RT GLOBAL CONS-POOL (used in rt-thread)

(defvar *rt-global-pool*
  (make-cons-pool
    :data (make-list 2048)
    :size 2048
    :grow 2048))
(declaim (type cons-pool *rt-global-pool*))

(declaim (inline rt-global-pool-push-cons))
(defun rt-global-pool-push-cons (cons)
  "Return the CONS cell to the global pool.

This function has to be called from the real-time thread."
  (cons-pool-push-cons *rt-global-pool* cons))

(declaim (inline rt-global-pool-pop-cons))
(defun rt-global-pool-pop-cons ()
  "Return a preallocated cons cell.

This function has to be called from the real-time thread."
  (cons-pool-pop-cons *rt-global-pool*))

(declaim (inline rt-global-pool-push-list))
(defun rt-global-pool-push-list (lst)
  "Return the LIST to the global pool.

This function has to be called from the real-time thread."
  (cons-pool-push-list *rt-global-pool* lst))

(declaim (inline rt-global-pool-pop-list))
(defun rt-global-pool-pop-list (list-size)
  "Return a preallocated list of length LIST-SIZE.

This function has to be called from the real-time thread."
  (cons-pool-pop-list *rt-global-pool* list-size))

;;; TLIST

(declaim (inline set-cons))
(defun set-cons (cons new-car new-cdr)
  (declare (type cons cons))
  (setf (car cons) new-car
        (cdr cons) new-cdr))

(defun make-tlist (pool)
  "Get a cons cell from POOL and return an empty tlist."
  (declare (type cons-pool pool))
  (let ((entry (cons-pool-pop-cons pool)))
    (set-cons entry nil nil)
    entry))

(declaim (inline tlist-left))
(defun tlist-left (tl)
  "Return the left element of the tlist TL."
  (caar tl))

(declaim (inline tlist-right))
(defun tlist-right (tl)
  "Return the right element of the tlist TL."
  (cadr tl))

(declaim (inline tlist-empty-p))
(defun tlist-empty-p (tl)
  "Returns T if TL is the empty tlist; otherwise, returns NIL."
  (null (car tl)))

(declaim (inline tlist-add-left))
(defun tlist-add-left (tl obj pool)
  "Get a cons cell from the cons POOL and add OBJ to the left of
the tlist TL."
  (declare (type cons tl) (type cons-pool pool))
  (let ((entry (cons-pool-pop-cons pool)))
    (set-cons entry obj (car tl))
    (if (tlist-empty-p tl)
        (setf (cdr tl) entry))
    (setf (car tl) entry)))

(declaim (inline tlist-add-right))
(defun tlist-add-right (tl obj pool)
  "Get a cons cell from the cons POOL and add OBJ to the right of
the tlist TL."
  (declare (type cons tl) (type cons-pool pool))
  (let ((entry (cons-pool-pop-cons pool)))
    (set-cons entry obj nil)
    (if (tlist-empty-p tl)
        (setf (car tl) entry)
        (setf (cddr tl) entry))
    (setf (cdr tl) entry)))

(declaim (inline tlist-remove-left))
(defun tlist-remove-left (tl pool)
  "Remove the left element of the tlist TL and return it.

Return the cons cell to the cons POOL."
  (declare (type cons tl) (type cons-pool pool))
  (unless (tlist-empty-p tl)
    (let ((entry (car tl)))
      (setf (car tl) (cdar tl))
      (cons-pool-push-cons pool entry)
      (if (tlist-empty-p tl)
          (setf (cdr tl) nil))
      (car entry))))

;;; FOREIGN MEMORY POOL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *foreign-sample-pool-size* (* 64 1024 1024))

  (defvar *foreign-rt-memory-pool-size* (* 64 1024 1024))

  (defvar *foreign-nrt-memory-pool-size* (* 64 1024 1024))

  (defvar *foreign-sample-pool*
    (foreign-alloc :char :count *foreign-sample-pool-size*))

  (defvar *foreign-rt-memory-pool*
    (foreign-alloc :char :count *foreign-rt-memory-pool-size*))

  ;;; This pool is used in non-realtime for short temporary arrays.
  (defvar *foreign-nrt-memory-pool*
    (foreign-alloc :char :count *foreign-nrt-memory-pool-size*))

  (defvar *initialized-foreign-memory-pools* nil)

  ;;; Lock used only to free the memory if the rt-thread is absent
  ;;; (memory allocated in realtime but the rt-thread has been terminated)
  (defvar *rt-memory-lock* (bt:make-lock "RT-MEMORY"))
  (defvar *rt-memory-sample-lock* (bt:make-lock "RT-MEMORY-SAMPLE"))

  ;;; Lock to alloc/free the memory in non-realtime.
  (defvar *nrt-memory-lock* (make-spinlock "NRT-MEMORY"))

  (defun init-foreign-memory-pools ()
    (unless *initialized-foreign-memory-pools*
      (incudine.external:init-foreign-memory-pool
       *foreign-sample-pool-size* *foreign-sample-pool*)
      (incudine.external:init-foreign-memory-pool
       *foreign-rt-memory-pool-size* *foreign-rt-memory-pool*)
      (incudine.external:init-foreign-memory-pool
       *foreign-nrt-memory-pool-size* *foreign-nrt-memory-pool*)
      (setf *initialized-foreign-memory-pools* t)))

  (defvar *add-init-foreign-memory-pools-p* t)
  (when *add-init-foreign-memory-pools-p*
    (setf *add-init-foreign-memory-pools-p* nil)
    (init-foreign-memory-pools)
    (push (lambda () (init-foreign-memory-pools))
          incudine::*initialize-hook*)))

;;; Realtime version of CFFI:FOREIGN-ALLOC.
;;; The NULL-TERMINATED-P keyword is removed.
(defun foreign-rt-alloc (type &key zero-p initial-element initial-contents
                         (count 1 count-p))
  "Allocate enough memory to hold COUNT objects of type TYPE. If
ZERO-P is T, the memory is initialized with zeros. If INITIAL-ELEMENT
is supplied, each element of the newly allocated memory is initialized
with its value. If INITIAL-CONTENTS is supplied, each of its elements
will be used to initialize the contents of the newly allocated memory.

This function has to be called from the real-time thread."
  (let (contents-length)
    (when initial-contents
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    (let* ((size (* (foreign-type-size type) count))
           (ptr (incudine.external:foreign-alloc-ex
                  size *foreign-rt-memory-pool*)))
      (cond (zero-p (incudine.external:foreign-set ptr 0 size))
            (initial-contents
             (dotimes (i contents-length)
               (setf (mem-aref ptr type i) (elt initial-contents i))))
            (initial-element
             (dotimes (i count)
               (setf (mem-aref ptr type i) initial-element))))
      ptr)))

(define-compiler-macro foreign-rt-alloc (&whole form type &rest args
                                         &key (count 1 count-p)
                                         &allow-other-keys)
  (if (or (and count-p (<= (length args) 2)) (null args))
      (cond
        ((and (constantp type) (constantp count))
         `(incudine.external:foreign-alloc-ex
            ,(* (eval count) (foreign-type-size (eval type)))
            *foreign-rt-memory-pool*))
        ((constantp type)
         `(incudine.external:foreign-alloc-ex
            (* ,count ,(foreign-type-size (eval type)))
            *foreign-rt-memory-pool*))
        (t form))
      form))

(declaim (inline %foreign-rt-free))
(defun %foreign-rt-free (ptr)
  (incudine.external:foreign-free-ex ptr *foreign-rt-memory-pool*))

(defun foreign-rt-free (ptr)
  "Free PTR previously allocated by FOREIGN-RT-ALLOC.

This function has to be called from the real-time thread."
  (if *rt-thread*
      (%foreign-rt-free ptr)
      (bt:with-lock-held (*rt-memory-lock*)
        (%foreign-rt-free ptr))))

(defun foreign-rt-realloc (ptr type &key zero-p initial-element initial-contents
                           (count 1 count-p))
  "Change the size of the memory block pointed to by ptr to hold COUNT
objects of type TYPE. If ZERO-P is T, the memory is initialized with zeros.
If INITIAL-ELEMENT is supplied, each element of the newly reallocated
memory is initialized with its value. If INITIAL-CONTENTS is supplied,
each of its elements will be used to initialize the contents of the newly
reallocated memory.

This function has to be called from the real-time thread."
  (let (contents-length)
    (when initial-contents
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    (let* ((size (* (foreign-type-size type) count))
           (ptr (incudine.external:foreign-realloc-ex
                   ptr size *foreign-rt-memory-pool*)))
      (cond (zero-p (incudine.external:foreign-set ptr 0 size))
            (initial-contents
             (dotimes (i contents-length)
               (setf (mem-aref ptr type i) (elt initial-contents i))))
            (initial-element
             (dotimes (i count)
               (setf (mem-aref ptr type i) initial-element))))
      ptr)))

(define-compiler-macro foreign-rt-realloc (&whole form ptr type &rest args
                                           &key (count 1 count-p)
                                           &allow-other-keys)
  (if (or (and count-p (<= (length args) 2)) (null args))
      (cond
        ((and (constantp type) (constantp count))
         `(incudine.external:foreign-realloc-ex
            ,ptr ,(* (eval count) (cffi:foreign-type-size (eval type)))
            *foreign-rt-memory-pool*))
        ((constantp type)
         `(incudine.external:foreign-realloc-ex
            ,ptr (* ,count ,(cffi:foreign-type-size (eval type)))
            *foreign-rt-memory-pool*))
        (t form))
      form))

(declaim (inline foreign-rt-alloc-sample))
(defun foreign-rt-alloc-sample (size &optional zerop)
  (let* ((dsize (* size +foreign-sample-size+))
         (ptr (incudine.external:foreign-alloc-ex
                 dsize *foreign-sample-pool*)))
    (when zerop (incudine.external:foreign-set ptr 0 dsize))
    ptr))

(declaim (inline %foreign-rt-free-sample))
(defun %foreign-rt-free-sample (ptr)
  (incudine.external:foreign-free-ex ptr *foreign-sample-pool*))

(declaim (inline foreign-rt-free-sample))
(defun foreign-rt-free-sample (ptr)
  (if *rt-thread*
      (%foreign-rt-free-sample ptr)
      (bt:with-lock-held (*rt-memory-sample-lock*)
        (%foreign-rt-free-sample ptr))))

(declaim (inline foreign-rt-realloc-sample))
(defun foreign-rt-realloc-sample (ptr size &optional zerop)
  (let* ((dsize (* size +foreign-sample-size+))
         (ptr (incudine.external:foreign-realloc-ex
                 ptr dsize *foreign-sample-pool*)))
    (when zerop (incudine.external:foreign-set ptr 0 dsize))
    ptr))

;;; Based on CFFI:FOREIGN-ALLOC.
(defun foreign-realloc (ptr type &key zero-p initial-element initial-contents
                        (count 1 count-p))
  "Change the size of the memory block pointed to by ptr to hold COUNT
objects of type TYPE. If ZERO-P is T, the memory is initialized with zeros.
If INITIAL-ELEMENT is supplied, each element of the newly reallocated
memory is initialized with its value. If INITIAL-CONTENTS is supplied,
each of its elements will be used to initialize the contents of the newly
reallocated memory."
  (let (contents-length)
    (when initial-contents
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    (let* ((size (* count (cffi:foreign-type-size type)))
           (ptr (incudine.external::%foreign-realloc ptr size)))
      (cond (zero-p
             (incudine.external:foreign-set ptr 0 size))
            (initial-contents
             (dotimes (i contents-length)
               (setf (cffi:mem-aref ptr type i) (elt initial-contents i))))
            (initial-element
             (dotimes (i count)
               (setf (cffi:mem-aref ptr type i) initial-element))))
      ptr)))

(define-compiler-macro foreign-realloc (&whole form ptr type &rest args
                                        &key (count 1 count-p) &allow-other-keys)
  (if (or (and count-p (<= (length args) 2)) (null args))
      (cond
        ((and (constantp type) (constantp count))
         `(incudine.external::%foreign-realloc
            ,ptr ,(* (eval count) (cffi:foreign-type-size (eval type)))))
        ((constantp type)
         `(incudine.external::%foreign-realloc
            ,ptr (* ,count ,(cffi:foreign-type-size (eval type)))))
        (t form))
      form))

;;; Based on CFFI:FOREIGN-ALLOC to use TLSF Memory Storage allocator.
;;; The NULL-TERMINATED-P keyword is removed.
(defun foreign-nrt-alloc (type &key zero-p initial-element initial-contents
                          (count 1 count-p))
  "Allocate enough memory to hold COUNT objects of type TYPE. If
ZERO-P is T, the memory is initialized with zeros. If INITIAL-ELEMENT
is supplied, each element of the newly allocated memory is initialized
with its value. If INITIAL-CONTENTS is supplied, each of its elements
will be used to initialize the contents of the newly allocated memory."
  (let (contents-length)
    (when initial-contents
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    (let* ((size (* (foreign-type-size type) count))
           (ptr (with-spinlock-held (*nrt-memory-lock*)
                  (incudine.external:foreign-alloc-ex
                   size *foreign-nrt-memory-pool*))))
      (cond (zero-p (incudine.external:foreign-set ptr 0 size))
            (initial-contents
             (dotimes (i contents-length)
               (setf (mem-aref ptr type i) (elt initial-contents i))))
            (initial-element
             (dotimes (i count)
               (setf (mem-aref ptr type i) initial-element))))
      ptr)))

(defun foreign-nrt-free (ptr)
  (with-spinlock-held (*nrt-memory-lock*)
    (incudine.external:foreign-free-ex ptr *foreign-nrt-memory-pool*)))

(declaim (inline get-foreign-sample-used-size))
(defun get-foreign-sample-used-size ()
  "Return the number of bytes allocated from the foreign memory pool
used to allocate arrays of samples in DSP and UGEN instances from the
real-time thread."
  (incudine.external:get-foreign-used-size *foreign-sample-pool*))

(declaim (inline get-foreign-sample-free-size))
(defun get-foreign-sample-free-size ()
  "Return the number of bytes that can be allocated from the foreign
memory pool used to allocate arrays of samples in DSP and UGEN instances
from the real-time thread."
  (- *foreign-sample-pool-size* (get-foreign-sample-used-size)))

(declaim (inline get-foreign-sample-max-size))
(defun get-foreign-sample-max-size ()
  "Return the maximum size of the foreign memory pool used to allocate
arrays of samples in DSP and UGEN instances from the real-time thread."
  (incudine.external:get-foreign-max-size *foreign-sample-pool*))

(declaim (inline get-rt-memory-used-size))
(defun get-rt-memory-used-size ()
  "Return the number of bytes allocated from the foreign memory pool
used from the real-time thread."
  (incudine.external:get-foreign-used-size *foreign-rt-memory-pool*))

(declaim (inline get-rt-memory-free-size))
(defun get-rt-memory-free-size ()
  "Return the the number of bytes that can be allocated from the
foreign memory pool used from the real-time thread."
  (- *foreign-rt-memory-pool-size* (get-rt-memory-used-size)))

(declaim (inline get-rt-memory-max-size))
(defun get-rt-memory-max-size ()
  "Return the maximum size of the foreign memory pool used from the
real-time thread."
  (incudine.external:get-foreign-max-size *foreign-rt-memory-pool*))

(declaim (inline get-nrt-memory-used-size))
(defun get-nrt-memory-used-size ()
  "Return the number of bytes allocated from the foreign memory pool
used to allocate temporary foreign arrays from any non-real-time thread."
  (incudine.external:get-foreign-used-size *foreign-nrt-memory-pool*))

(declaim (inline get-nrt-memory-free-size))
(defun get-nrt-memory-free-size ()
  "Return the number of bytes that can be allocated from the foreign
memory pool used to allocate temporary foreign arrays from any
non-real-time thread."
  (- *foreign-nrt-memory-pool-size* (get-nrt-memory-used-size)))

(declaim (inline get-nrt-memory-max-size))
(defun get-nrt-memory-max-size ()
  "Return the maximum size of the foreign memory pool used to allocate
temporary foreign arrays from any non-real-time thread."
  (incudine.external:get-foreign-max-size *foreign-nrt-memory-pool*))
