;;; Copyright (c) 2014-2017 Tito Latini
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

(in-package :incudine.vug)

(defstruct (ugen-instance (:constructor %make-ugen-instance)
                          (:copier nil))
  (name nil :type symbol)
  (return-pointer nil :type (or foreign-pointer null))
  ;; Sequence #[c0-ptr-or-nil c0-func-or-nil c1-ptr-or-nil c1-func-or-nil ...]
  (controls nil :type (or simple-vector null))
  (init-function #'dummy-function :type function)
  (perf-function #'dummy-function :type function)
  (free-function #'dummy-function :type function))

(defmacro make-ugen-instance (&key name return-pointer controls init-function
                              perf-function free-function)
  (with-gensyms (free-fn obj)
    `(let* ((,free-fn ,free-function)
            (,obj (%make-ugen-instance
                    :name ,name
                    :return-pointer ,return-pointer
                    :controls ,controls
                    :free-function ,free-fn
                    :init-function (or ,init-function #'dummy-function)
                    :perf-function (or ,perf-function #'dummy-function))))
       (when ,free-fn (incudine:incudine-finalize ,obj ,free-fn))
       ,obj)))

(defmethod print-object ((obj ugen) stream)
  (format stream "#<UGEN ~A>" (ugen-name obj)))

(defmethod print-object ((obj ugen-instance) stream)
  (format stream "#<~A ~A>" (class-name (class-of obj))
          (or (ugen-instance-name obj) "(free)")))

(declaim (inline index-of-ugen-control))
(defun index-of-ugen-control (ugen control-name)
  (position control-name (ugen-args ugen) :key #'symbol-name :test #'string=))

(declaim (inline ugen-control-flag))
(defun ugen-control-flag (ugen index)
  (nth index (ugen-control-flags ugen)))

(declaim (inline ugen-arg-type))
(defun ugen-arg-type (ugen index)
  (nth index (ugen-arg-types ugen)))

(declaim (inline ugen-reinit-function))
(defun ugen-reinit-function (ugen-instance)
  "Return the function to reinitialize UGEN-INSTANCE."
  (declare (type ugen-instance ugen-instance))
  (ugen-instance-init-function ugen-instance))

(declaim (inline ugen-perf-function))
(defun ugen-perf-function (ugen-instance)
  "Return UGEN-INSTANCE's performance function."
  (declare (type ugen-instance ugen-instance))
  (ugen-instance-perf-function ugen-instance))

(declaim (inline ugen-return-pointer))
(defun ugen-return-pointer (ugen-instance)
  "Return the foreign pointer to the memory used to store the
UGEN-INSTANCE's result when the type of the result is foreign."
  (declare (type ugen-instance ugen-instance))
  (ugen-instance-return-pointer ugen-instance))

(defun ugen-control-pointer (ugen-instance control-name)
  "Return the foreign pointer to the memory used to store the value of
the UGEN-INSTANCE's control CONTROL-NAME and the function of no
arguments to update the dependencies if it exists."
  (let* ((u (ugen (ugen-instance-name ugen-instance)))
         (index (index-of-ugen-control u control-name)))
    (when index
      (let* ((pos (* 2 index))
             (ptr (svref (ugen-instance-controls ugen-instance) pos)))
        (when ptr
          (values ptr (svref (ugen-instance-controls ugen-instance)
                             (1+ pos))))))))

(defun ugen-control-new-value (value ctrl-type value-type value-type-p)
  (if value-type-p
      (if (subtypep value-type ctrl-type)
          value
          `(coerce ,value ',ctrl-type))
      (if (subtypep ctrl-type 'sample)
          `(coerce ,value ',ctrl-type)
          value)))

(defmacro define-ugen-control-setter (ugen-name control-name
                                      &optional setter-name (value-type nil
                                                             value-type-p))
  (let* ((u (ugen ugen-name))
         (index (index-of-ugen-control u control-name)))
    (when index
      (let* ((flag (ugen-control-flag u index))
             (type (ugen-arg-type u index))
             (ptr-p (ctrl-foreign-object-p flag))
             (id (* index 2))
             (name (or setter-name
                       (format-symbol *package* "SET-~A-~A"
                                      ugen-name control-name))))
        (with-gensyms (ctrl)
          (with-ensure-symbols (ugen-instance value)
            (let* ((type (if ptr-p
                             (if (subtypep type 'sample) type :pointer)
                             type))
                   (new-value (ugen-control-new-value value type value-type
                                                      value-type-p)))
              `(progn
                 ,@(if (and ptr-p
                            value-type-p
                            (or (subtypep value-type 'sample)
                                (subtypep value-type 'foreign-pointer)))
                       `((declaim (inline ,name))))
                 (defun ,name (,ugen-instance ,value)
                   (declare (type ugen-instance ,ugen-instance)
                            ,@(if value-type-p `((type ,value-type ,value))))
                   (let ((,ctrl (ugen-instance-controls ,ugen-instance)))
                     ,@(if ptr-p `((setf (mem-ref (svref ,ctrl ,id) ',type)
                                         ,new-value)))
                     ,@(if (ctrl-update-function-p flag)
                           `((funcall (the function (svref ,ctrl ,(1+ id)))
                                      ,@(unless ptr-p `(,new-value)))))
                     (values)))
                 (values (compile ',name))))))))))

(defmacro ugen-funcall (name &rest arguments)
  `(funcall (ugen-callback (ugen ,name)) ,@arguments))

(defmacro ugen-inline-funcall (name &rest arguments)
  `(funcall (ugen-inline-callback (ugen ,name)) ,@arguments))

(defmethod incudine:free-p ((obj ugen-instance))
  (every (lambda (fn) (eq fn #'dummy-function))
         (list (ugen-instance-init-function obj)
               (ugen-instance-perf-function obj)
               (ugen-instance-free-function obj))))

(defmethod incudine:free ((obj ugen-instance))
  (unless (incudine:free-p obj)
    (funcall (ugen-instance-free-function obj))
    (incudine:incudine-cancel-finalization obj)
    (setf (ugen-instance-init-function obj) #'dummy-function
          (ugen-instance-perf-function obj) #'dummy-function
          (ugen-instance-free-function obj) #'dummy-function
          (ugen-instance-name obj) nil)
    (nrt-msg debug "Free instance of ~A" (type-of obj)))
  (values))

(defun rename-ugen (old-name new-name)
  (declare (type symbol old-name new-name))
  (cond ((dsp new-name)
         (msg error "~A was defined to be a DSP." new-name))
        ((vug new-name)
         (msg error "~A was defined to be a VUG." new-name))
        (t
         (let ((ugen (ugen old-name)))
           (cond (ugen
                  (remhash old-name *ugens*)
                  (setf (gethash new-name *ugens*) ugen)
                  (setf (ugen-name ugen) new-name)
                  (setf (symbol-function new-name) (symbol-function old-name))
                  (fmakunbound old-name)
                  new-name)
                 (t (msg error "~A is not a legal UGEN name." old-name)))))))

;;; No panic if we accidentally redefine a function related to a UGEN.
(defun fix-ugen (name)
  (let ((ugen (ugen name)))
    (when (and ugen
               (or (not (fboundp name))
                   (not (eq (ugen-callback ugen)
                            (symbol-function name)))))
      (setf (symbol-function name) (ugen-callback ugen))
      t)))

(declaim (inline all-ugen-names))
(defun all-ugen-names ()
  (sort (loop for name being the hash-keys in *ugens*
              when (find-symbol (symbol-name name))
              collect name)
        #'string-lessp :key #'symbol-name))

(declaim (inline compiled-vug-p))
(defun compiled-vug-p (name)
  (let ((vug (vug name))
        (ug (ugen name)))
    (and vug ug (eq (vug-callback vug) (ugen-inline-callback ug)))))

(declaim (inline store-ugen-return-pointer))
(defun store-ugen-return-pointer ()
  (when *ugen-return-value*
    `(get-pointer ,*ugen-return-value*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (object-to-free get-ugen-instance reinit-ugen))

(declaim (inline get-ugen-instance))
(defun get-ugen-instance (name &rest args)
  (let* ((ugen (ugen name))
         (fn (reduce-warnings
               (apply (the function (ugen-callback ugen)) args))))
    (declare (type ugen ugen) (type function fn))
    (funcall fn)))

(defmacro reinit-ugen (vug-varname args)
  `(funcall (ugen-instance-init-function ,vug-varname) ,@(cdr args)))

(defmacro generate-ugen-code (name arguments arg-names bindings ugen node
                              ugen-instance-constructor obj)
  (with-gensyms (vug-body smpvec-size f32vec-size f64vec-size i32vec-size
                 i64vec-size ptrvec-size)
    `(let* ((*vug-variables* (make-vug-variables))
            (*variables-to-preserve* nil)
            (*ugen-return-value* nil)
            (*initialization-code* (make-initialization-code-stack))
            (,vug-body (format-vug-code ,(dsp-vug-block arguments obj)))
            (,smpvec-size (add-sample-variables))
            (,f32vec-size (add-float-variables))
            (,f64vec-size (add-double-variables))
            (,i32vec-size (add-int32-variables))
            (,i64vec-size (add-int64-variables))
            (,ptrvec-size (add-pointer-variables)))
       (debug-deleted-variables)
       (debug-foreign-bytes ,smpvec-size (+ ,f32vec-size ,i32vec-size)
                            (+ ,f64vec-size ,i64vec-size) ,ptrvec-size)
       (with-gensyms (smpvecw smpvec init-node f32vecw f32vec f64vecw f64vec
                      i32vecw i32vec i64vecw i64vec ptrvecw ptrvec)
         `(lambda ()
            (declare #.*standard-optimize-settings*)
            (with-foreign-arrays ((,smpvec ,smpvecw 'sample ,,smpvec-size)
                                  (,f32vec ,f32vecw :float ,,f32vec-size)
                                  (,f64vec ,f64vecw :double ,,f64vec-size)
                                  (,i32vec ,i32vecw :int32 ,,i32vec-size)
                                  (,i64vec ,i64vecw :int64 ,,i64vec-size)
                                  (,ptrvec ,ptrvecw :pointer ,,ptrvec-size))
              (with-sample-variables (,(vug-variables-foreign-sample-names)
                                      ,smpvec 'sample)
                (with-foreign-variables
                    ((,(vug-foreign-varnames float) ,f32vec :float)
                     (,(vug-foreign-varnames double) ,f64vec :double)
                     (,(vug-foreign-varnames int32) ,i32vec :int32)
                     (,(vug-foreign-varnames int64) ,i64vec :int64)
                     (,(vug-foreign-varnames pointer) ,ptrvec :pointer))
                  ,(%expand-variables
                     (reorder-initialization-code)
                     `(let ((%dsp-node% ,(or ,node
                                             ;; The UGEN is outside a DSP.
                                             '(incudine::make-temp-node))))
                        (declare (type (or incudine:node null) %dsp-node%))
                        ,@(initialization-code)
                        (,(or ',ugen-instance-constructor 'make-ugen-instance)
                          :name ,',name
                          :return-pointer ,(store-ugen-return-pointer)
                          :controls ,(set-parameters-form ,ugen)
                          :init-function
                            (lambda (,@',arg-names &optional ,init-node)
                              (declare (type (or incudine:node null) ,init-node)
                                       #.*reduce-warnings*)
                              (reset-foreign-arrays
                                ,smpvec ,,smpvec-size ,+foreign-sample-size+
                                ,f32vec ,,f32vec-size 4
                                ,f64vec ,,f64vec-size 8
                                ,i32vec ,,i32vec-size 4
                                ,i64vec ,,i64vec-size 8
                                ,ptrvec ,,ptrvec-size ,+pointer-size+)
                              (if ,init-node (setf %dsp-node% ,init-node))
                              (with-init-frames
                                (let* ,',bindings
                                  ,(reinit-bindings-form)
                                  ,@(initialization-code)))
                              (values))
                          :free-function
                            ,(to-free-form smpvecw ,smpvec-size
                                           f32vecw ,f32vec-size
                                           f64vecw ,f64vec-size
                                           i32vecw ,i32vec-size
                                           i64vecw ,i64vec-size
                                           ptrvecw ,ptrvec-size
                                           '%dsp-node%
                                           ;; Free the node only if the UGEN
                                           ;; is outside a DSP.
                                           (if ,node 0 1))
                          :perf-function
                            (lambda (&optional (current-channel 0))
                              (declare (type channel-number current-channel)
                                       (ignorable current-channel))
                              (let ((current-frame 0)
                                    (current-sample 0))
                                (declare (non-negative-fixnum current-frame
                                                              current-sample)
                                         (ignorable current-frame
                                                    current-sample))
                                ,@,vug-body)))))))))))))

(declaim (inline control-flag))
(defun control-flag (foreign-obj-p dependencies-p)
  (logior (if foreign-obj-p
              +ctrl-foreign-object+
              +ctrl-update-function+)
          (if dependencies-p +ctrl-update-function+ 0)))

(defun set-parameters-form (ugen)
  (let ((param-list (vug-variables-to-update *vug-variables*))
        (stack nil)
        (flags nil))
    (prog1
      (when param-list
        `(vector
           ,@(dolist (p param-list stack)
               (update-vug-parameter-aux-varname p)
               (let ((v (vug-parameter-aux-varname p))
                     (dep (control-dependence p)))
                 (when (equal dep '((progn))) (setf dep nil))
                 (push (control-flag (foreign-object-p p) dep) flags)
                 (cond ((foreign-object-p p)
                        (push (when dep `(lambda () ,@dep (values))) stack)
                        (push `(get-pointer ,v) stack))
                       (t
                        (push `(lambda (x) (setf ,v x) ,@dep (values)) stack)
                        (push nil stack)))))))
      (when ugen
        (setf (ugen-control-flags ugen) flags)))))

(defun get-bindings (names types)
  (loop for x in names for y in types collect `(,x ,y)))

(defmacro maybe-store-return-value (return-type &body body)
  (if (foreign-type-p return-type)
      `(with ((ret (store-ugen-return-value (progn ,@body))))
         (declare (preserve ret) (type ,return-type ret))
         ret
         (values))
      `(progn ,@body)))

(defmacro %compile-vug (name bindings return-type args arg-names
                        ugen-instance-constructor ugen)
  (with-gensyms (get-function fn node)
    `(macrolet ((,get-function (,@arg-names ,node)
                  `(prog1
                     ,(generate-ugen-code ',name ,args ,arg-names
                                          ,bindings ,ugen ,node
                                          ,ugen-instance-constructor
                                          (maybe-store-return-value
                                            ,return-type
                                            (vug-funcall ',name
                                                         ,@arg-names)))
                     (nrt-msg info "new alloc for UGEN ~A" ',',name))))
       (let ((,fn (lambda (,@arg-names &optional ,node)
                    (declare (type (or incudine:node null) ,node))
                    ,(documentation (ugen-inline-callback ugen) 'function)
                    (let* (,@bindings
                           (%dsp-node% ,node))
                      (declare (ignorable %dsp-node%))
                      (,get-function ,@arg-names ,node)))))
         (setf (ugen-callback ,ugen) ,fn)
         (setf (ugen-return-type ,ugen) ',return-type)
         (setf (symbol-function ',name) ,fn)
         (setf (gethash ',name *ugens*) ,ugen)
         ,ugen))))

(defun compile-vug (name-or-vug return-type &optional force-p
                    ugen-instance-constructor)
  (declare (type (or symbol vug) name-or-vug))
  (multiple-value-bind (vug name)
      (if (symbolp name-or-vug)
          (values (vug name-or-vug) name-or-vug)
          (values name-or-vug (vug-name name-or-vug)))
    (cond ((null vug)
           (msg error "~A is not a legal VUG name." name))
          ((and (compiled-vug-p name) (not force-p))
           (symbol-function name))
          ((vug-macro-p vug)
           (msg error "~A is a VUG-MACRO." name))
          (t
           (let* ((arg-names (vug-args vug))
                  (types (vug-arg-types vug))
                  (args (get-bindings arg-names types))
                  (arg-bindings (dsp-coercing-arguments args))
                  (fn (vug-callback vug)))
             (let ((ugen (make-ugen :name name
                                    :return-type return-type
                                    :args arg-names
                                    :arg-types types
                                    :inline-callback fn)))
               (incudine.util::cudo-compile
                 `(%compile-vug ,name ,arg-bindings ,return-type ,args
                                ,arg-names ,ugen-instance-constructor
                                ,ugen))))))))

(defun get-ugen-specs (def)
  (multiple-value-bind (form doc)
      (if (stringp (car def))
          (values (cdr def) (car def))
          def)
    (do ((specs nil (cons (car l) specs))
         (l form (cdr l)))
        ((not (keywordp (caar l))) (values specs l doc)))))

(defun get-ugen-spec (name specs)
  (second (assoc name specs)))

(defmacro define-ugen (name return-type lambda-list &body body)
  (multiple-value-bind (specs form doc) (get-ugen-specs body)
    (let ((instance-constructor (get-ugen-spec :constructor specs)))
      `(compile-vug (define-vug ,name ,lambda-list ,doc ,@form) ',return-type
                    nil ',instance-constructor))))

;;; Return a function to show the code generated by DEFINE-UGEN.
;;; The arguments of the function are the arguments of the UGEN
;;; plus one optional STREAM.
(defmacro ugen-debug (name return-type lambda-list &body body)
  (multiple-value-bind (specs form doc) (get-ugen-specs body)
    (declare (ignore doc))
    (let ((instance-constructor (get-ugen-spec :constructor specs)))
      (multiple-value-bind (args types) (arg-names-and-types lambda-list)
        `(%%codegen-debug ,name ,lambda-list generate-ugen-code
             (,(dsp-coercing-arguments (get-bindings args types)) nil nil
              ,instance-constructor)
           (maybe-store-return-value ,return-type ,@form))))))

(defmacro with-ugen-instance ((var ugen-name &rest args) &body body)
  `(let ((,var (funcall (,ugen-name ,@args))))
     (unwind-protect
          (progn ,@body)
       (incudine:free ,var))))

(defmacro with-ugen-instances (bindings &body body)
  (if bindings
      `(with-ugen-instance ,(car bindings)
         (with-ugen-instances ,(cdr bindings)
           ,@body))
      `(progn ,@body)))
