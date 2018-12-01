;;; Copyright (c) 2014-2018 Tito Latini
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
  "Return the UGEN-INSTANCE performance function.

If the return type of UGEN-INSTANCE is SAMPLE or a foreign pointer,
the performance function returns no values. In this case, the function
UGEN-RETURN-POINTER has to be called to get the foreign pointer to the
returned value."
  (declare (type ugen-instance ugen-instance))
  (ugen-instance-perf-function ugen-instance))

(declaim (inline ugen-return-pointer))
(defun ugen-return-pointer (ugen-instance)
  "If the return type of UGEN-INSTANCE is SAMPLE or a foreign pointer,
return the foreign pointer to the returned value. Otherwise, return NIL."
  (declare (type ugen-instance ugen-instance))
  (ugen-instance-return-pointer ugen-instance))

(defun ugen-control-pointer (ugen-instance control-name-or-index)
  "If the UGEN-INSTANCE control CONTROL-NAME-OR-INDEX is represented
by a foreign object (i.e. a control of SAMPLE type), the first returned
value is a foreign pointer to the control value, otherwise it is a
function of no arguments called to get the control value.

The second returned value is the function of no arguments called to
update the dependencies if it exists."
  (flet ((ctrl-ref (u i)
           (let ((pos (* 2 i)))
             (values (svref (ugen-instance-controls u) pos)
                     (svref (ugen-instance-controls u) (1+ pos))))))
    (if (numberp control-name-or-index)
        (ctrl-ref ugen-instance control-name-or-index)
        (let* ((u (ugen (ugen-instance-name ugen-instance)))
               (index (index-of-ugen-control u control-name-or-index)))
          (when index
            (ctrl-ref ugen-instance index))))))

(defun ugen-control-new-value (value ctrl-type value-type value-type-p)
  (if value-type-p
      (if (subtypep value-type ctrl-type)
          value
          `(coerce ,value ',ctrl-type))
      (if (subtypep ctrl-type 'sample)
          `(coerce ,value ',ctrl-type)
          value)))

(defmacro* define-ugen-control-getter
    ((ugen-name (incudine:incudine-missing-arg "UGEN-NAME"))
     (control-name (incudine:incudine-missing-arg "CONTROL-NAME"))
     getter-name arg-name ugen-instance-type (inline-p t) method-p)
  "Define a UGEN control getter for the control CONTROL-NAME.

The GETTER-NAME defaults to [ugen-name]-[control-name].

The argument name ARG-NAME for the UGEN instance defaults to UGEN-INSTANCE.

UGEN-INSTANCE-TYPE defaults to UGEN-INSTANCE.

If INLINE-P is T (default if METHOD-P is NIL), the getter is declared inline.

If METHOD-P is T, the getter is defined as a method on a generic function."
  (let* ((u (ugen ugen-name))
         (index (index-of-ugen-control u control-name)))
    (when index
      (let* ((flag (ugen-control-flag u index))
             (utype (ugen-arg-type u index))
             (ptr-p (ctrl-foreign-object-p flag))
             (id (* index 2))
             (name (or getter-name
                       (format-symbol *package* "~A-~A" ugen-name
                                      control-name)))
             (ugen-instance (ensure-symbol (if arg-name
                                               (symbol-name arg-name)
                                               "UGEN-INSTANCE")))
             (ugen-instance-type (or ugen-instance-type 'ugen-instance))
             (type (if ptr-p
                       (if (subtypep utype 'sample) utype :pointer)
                       utype))
             (ctrl (gensym "CTRL")))
        `(progn
           ,@(when (and inline-p (not method-p))
               `((declaim (inline ,name))))
           (,@(if method-p
                  `(defmethod ,name ((,ugen-instance ,ugen-instance-type)))
                  `(defun ,name (,ugen-instance)
                     (declare (type ,ugen-instance-type ,ugen-instance))))
             (let ((,ctrl (ugen-instance-controls ,ugen-instance)))
               ,(if ptr-p
                    `(mem-ref (svref ,ctrl ,id) ',type)
                    `(reduce-warnings
                       (the ,type (funcall (svref ,ctrl ,id)))))))
           (values (compile ',name)))))))

(defmacro* define-ugen-control-setter
    ((ugen-name (incudine:incudine-missing-arg "UGEN-NAME"))
     (control-name (incudine:incudine-missing-arg "CONTROL-NAME"))
     setter-name value-type value-name arg-name ugen-instance-type
     (inline-p t) method-p)
  "Define a UGEN control setter for the control CONTROL-NAME.

The SETTER-NAME defaults to set-[ugen-name]-[control-name].

If VALUE-TYPE is non-NIL, it is the new value type.

VALUE-NAME is the argument name for the new value and defaults to VALUE.

The argument name ARG-NAME for the UGEN instance defaults to UGEN-INSTANCE.

UGEN-INSTANCE-TYPE defaults to UGEN-INSTANCE.

If INLINE-P is T (default if METHOD-P is NIL), the setter is declared inline.

If METHOD-P is T, the setter is defined as a method on a generic function."
  (let* ((u (ugen ugen-name))
         (index (index-of-ugen-control u control-name)))
    (when index
      (let* ((flag (ugen-control-flag u index))
             (utype (ugen-arg-type u index))
             (ptr-p (ctrl-foreign-object-p flag))
             (id (* index 2))
             (name (if setter-name
                       (if method-p `(setf ,setter-name) setter-name)
                       (format-symbol *package* "SET-~A-~A"
                                      ugen-name control-name)))
             (ugen-instance (ensure-symbol (if arg-name
                                               (symbol-name arg-name)
                                               "UGEN-INSTANCE")))
             (value (or value-name (ensure-symbol "VALUE")))
             (ugen-instance-type (or ugen-instance-type 'ugen-instance))
             (type (if ptr-p
                       (if (subtypep utype 'sample) utype :pointer)
                       utype))
             (new-value (if (eq type :pointer)
                            value
                            (ugen-control-new-value
                              value type value-type value-type)))
             (ctrl (gensym "CTRL")))
        `(progn
           ,@(when (or (and inline-p (not method-p))
                       (and ptr-p
                            value-type
                            (or (subtypep value-type 'sample)
                                (subtypep value-type 'foreign-pointer))))
               `((declaim (inline ,name))))
           (,@(if method-p
                  `(defmethod ,name
                       (,(if value-type `(,value ,value-type) value)
                        (,ugen-instance ,ugen-instance-type)))
                  `(defun ,name (,ugen-instance ,value)
                     (declare (type ,ugen-instance-type ,ugen-instance)
                              ,@(when value-type
                                  `((type ,value-type ,value))))))
             (let ((,ctrl (ugen-instance-controls ,ugen-instance)))
               ,@(if ptr-p `((setf (mem-ref (svref ,ctrl ,id) ',type)
                                   ,new-value)))
               ,@(if (ctrl-update-function-p flag)
                     `((funcall (the function (svref ,ctrl ,(1+ id)))
                                ,@(unless ptr-p `(,new-value)))))
               ,value))
           (values (compile ',name)))))))

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
  "Rename the UGEN named OLD-NAME to NEW-NAME."
  (declare (type symbol old-name new-name))
  (cond ((dsp new-name)
         (incudine-error "~A was defined to be a DSP." new-name))
        ((vug new-name)
         (incudine-error "~A was defined to be a VUG." new-name))
        (t
         (let ((ugen (ugen old-name)))
           (cond (ugen
                  (remhash old-name *ugens*)
                  (setf (gethash new-name *ugens*) ugen)
                  (setf (ugen-name ugen) new-name)
                  (setf (symbol-function new-name) (symbol-function old-name))
                  (fmakunbound old-name)
                  new-name)
                 (t (incudine-error "~A is not a legal UGEN name." old-name)))))))

(defun fix-ugen (name)
  "The function named NAME is forced to be the auxiliary function of
the UGEN with the same name. Useful if that function is accidentally
redefined."
  (let ((ugen (ugen name)))
    (when (and ugen
               (or (not (fboundp name))
                   (not (eq (ugen-callback ugen)
                            (symbol-function name)))))
      (setf (symbol-function name) (ugen-callback ugen))
      t)))

(defun all-ugen-names (&optional inaccessible-p)
  "Return the name list of the defined UGEN's.

If INACCESSIBLE-P is T, the list also includes the symbols unexported
from the other packages."
  (%all-vug-names *ugens* inaccessible-p))

(defun compiled-vug-p (obj)
  "Whether OBJ is a compiled VUG or the name of a compiled VUG."
  (declare (type (or symbol vug) obj))
  (let* ((vug (if (vug-p obj) obj (vug obj)))
         (ug (ugen (vug-name vug))))
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

(defun fill-ugen-instance (obj return-pointer controls
                           init-fn free-fn perf-fn)
  (setf (ugen-instance-return-pointer obj) return-pointer
        (ugen-instance-controls obj) controls
        (ugen-instance-init-function obj) init-fn
        (ugen-instance-free-function obj) free-fn
        (ugen-instance-perf-function obj) perf-fn))

(defmacro with-ugen-preamble ((instance-var free-fn-var to-free-var
                               name node instance-constructor)
                              &body body)
  `(let ((%dsp-node% (or ,node
                         ;; The UGEN is outside a DSP.
                         (incudine::make-temp-node)))
         (,instance-var (,(or instance-constructor 'make-ugen-instance)
                          :name ,name))
         (,free-fn-var nil))
     (declare (type incudine:node %dsp-node%)
              (type ugen-instance ,instance-var)
              (type (or function null) ,free-fn-var))
     (let ((incudine::*to-free* nil)
           (,to-free-var nil))
       (declare (type list ,to-free-var))
       ,@body)
     (when ,free-fn-var
       (incudine:incudine-finalize ,instance-var ,free-fn-var))))

(defmacro generate-ugen-code (name arguments arg-names optimize bindings ugen
                              node ugen-instance-constructor obj)
  (with-gensyms (vug-body smpvec-size f32vec-size f64vec-size i32vec-size
                 i64vec-size ptrvec-size)
    `(let* ((*vug-variables* (make-vug-variables))
            (*variables-to-preserve* nil)
            (*no-follow-parameter-list* nil)
            (*ugen-return-value* nil)
            (*initialization-code* (make-initialization-code-stack))
            (,vug-body (format-vug-code ,(dsp-vug-block nil arguments obj)))
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
                      i32vecw i32vec i64vecw i64vec ptrvecw ptrvec free-fn u
                      to-free)
         `(lambda ()
            (declare ,,optimize)
            (with-ugen-preamble (,u ,free-fn ,to-free  ',',name
                                 ,,node ,',ugen-instance-constructor)
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
                       `(progn
                          ,@(initialization-code)
                          (setf ,to-free incudine::*to-free*)
                          (fill-ugen-instance ,u
                            ,(store-ugen-return-pointer)
                            ,(set-parameters-form ,ugen)
                            ;; Init function.
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
                              (when ,init-node
                                (setf %dsp-node% ,init-node))
                              (with-init-frames
                                (free-incudine-objects ,to-free)
                                (let* ((incudine::*to-free* nil)
                                       ,@',bindings)
                                  ,(reinit-bindings-form)
                                  ,@(initialization-code)
                                  (setf ,to-free incudine::*to-free*)))
                              (values))
                            ;; Free function.
                            (setf ,free-fn
                                  ,(to-free-form to-free
                                     ;; Free the node only if the UGEN
                                     ;; is outside a DSP.
                                     `(unless ,,node %dsp-node%)
                                     smpvecw ,smpvec-size
                                     f32vecw ,f32vec-size
                                     f64vecw ,f64vec-size
                                     i32vecw ,i32vec-size
                                     i64vecw ,i64vec-size
                                     ptrvecw ,ptrvec-size))
                            ;; Performance function.
                            (lambda (&optional (current-channel 0))
                              (declare (type channel-number current-channel)
                                       (ignorable current-channel))
                              (let ((current-frame 0)
                                    (current-input-sample 0)
                                    (current-sample 0))
                                (declare (non-negative-fixnum current-frame
                                          current-input-sample current-sample)
                                         (ignorable current-frame
                                                    current-input-sample
                                                    current-sample))
                                ,@,vug-body))))))))))))))

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
                        (push `(lambda () ,v) stack)))))))
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

(defmacro %compile-vug (name bindings return-type args arg-names optimize
                        optional-keys ugen-instance-constructor ugen)
  (with-gensyms (get-function fn node)
    `(macrolet ((,get-function (,@arg-names ,node)
                  `(prog1
                     ,(generate-ugen-code ,name ,args ,arg-names ,optimize
                        ,bindings ,ugen ,node ,ugen-instance-constructor
                        (maybe-store-return-value
                          ,return-type (vug-funcall ',name ,@arg-names)))
                     (nrt-msg info "new alloc for UGEN ~A" ',',name))))
       (let ((,fn (,@(if optional-keys
                         `(lambda* (,@optional-keys ugen-node))
                         `(lambda (,@arg-names &optional ugen-node)))
                    (declare (type (or incudine:node null) ugen-node))
                    ,(documentation (ugen-inline-callback ugen) 'function)
                    (let* (,@bindings
                           (%dsp-node% ugen-node))
                      (declare (ignorable %dsp-node%))
                      (,get-function ,@arg-names ugen-node)))))
         (setf (ugen-callback ,ugen) ,fn)
         (setf (ugen-return-type ,ugen) ',return-type)
         (setf (symbol-function ',name) ,fn)
         (setf (gethash ',name *ugens*) ,ugen)
         ,ugen))))

(incudine.util:defun* compile-vug (name-or-vug return-type force-p
                                   ugen-instance-constructor optimize)
  "Define a new UGEN and the auxiliary function named NAME by
compiling an existing VUG.

NAME-OR-VUG is a VUG structure or a VUG name.

The UGEN output is of type RETURN-TYPE.

If FORCE-P is T, force the compilation.

If UGEN-INSTANCE-CONSTRUCTOR is non-NIL, it is the constructor used to
create the UGEN instances.

If OPTIMIZE is non-NIL, it is the quoted list of optimization
qualities for the declaration OPTIMIZE."
  (declare (type (or symbol vug) name-or-vug))
  (multiple-value-bind (vug name)
      (if (symbolp name-or-vug)
          (values (vug name-or-vug) name-or-vug)
          (values name-or-vug (vug-name name-or-vug)))
    (cond ((null vug)
           (incudine-error "~A is not a legal VUG name." name))
          ((and (compiled-vug-p name) (not force-p))
           (symbol-function name))
          ((vug-macro-p vug)
           (incudine-error "~A is a VUG-MACRO." name))
          (t
           (let* ((arg-names (vug-args vug))
                  (types (vug-arg-types vug))
                  (args (get-bindings arg-names types))
                  (arg-bindings (dsp-coercing-arguments args))
                  (defaults (vug-defaults vug))
                  (fn (vug-callback vug))
                  (ugen (make-ugen :name name
                                   :return-type (or return-type t)
                                   :args arg-names
                                   :arg-types types
                                   :defaults defaults
                                   :inline-callback fn)))
             (incudine.util::cudo-compile
              `(%compile-vug
                 ,name ,arg-bindings ,return-type ,args ,arg-names
                 ,(if optimize
                      (if (or (atom optimize) (eq (car optimize) 'quote))
                          optimize
                          (optimize-settings
                            `((:optimize ,@optimize)) :ugen-spec-p t))
                      '*standard-optimize-settings*)
                 ,(mapcar #'list arg-names defaults)
                 ,ugen-instance-constructor ,ugen)))))))

(defun get-ugen-specs (def)
  (multiple-value-bind (form doc)
      (if (stringp (car def))
          (values (cdr def) (car def))
          def)
    (do ((specs nil (cons (car l) specs))
         (l form (cdr l)))
        ((not (vug-spec-p l)) (values doc specs l)))))

(defun get-ugen-spec (name specs)
  (cdr (assoc name specs)))

(defun format-ugen-accessor-spec-name (spec)
  (let* ((pl (cdr spec))
         (name (and (not (getf pl :method-p))
                    (getf pl :name)))
         (setter nil)
         (spec (copy-list spec)))
    (when name
      (setf setter (format-symbol *package* "SET-~A" name))
      (setf (getf (cdr spec) :name) setter))
    (values spec name setter)))

(defun extend-ugen-specs (specs)
  (let ((readers) (writers) (setfable))
    (flet ((add-setfable-functions (spec)
             (multiple-value-bind (sp name setter)
                 (format-ugen-accessor-spec-name spec)
               (when name (push (list name setter) setfable))
               sp)))
      (dolist (sp specs)
        (case (first sp)
          (:readers (push (rest sp) readers))
          (:writers (push (rest sp) writers))
          (:accessors
           (push (copy-list (rest sp)) readers)
           (push (mapcar #'add-setfable-functions (rest sp)) writers))))
      (list (cons :readers (apply #'append (nreverse readers)))
            (cons :writers (apply #'append (nreverse writers)))
            (cons :setfable (nreverse setfable))))))

(defun define-ugen-control-get/setters (ugen-name instance-type specs)
  (flet ((def (fname spec args)
           `(,fname ,ugen-name ,(car spec)
             ,@(loop for a in args
                     if (consp a)
                       append (mapcar
                                (lambda (x)
                                  (let ((val (getf (cdr spec) x)))
                                    (or (and (eq x :inline-p)
                                             (null val)
                                             (not (getf (cdr spec) :method-p)))
                                        val)))
                                a)
                     else
                       collect a))))
    (let ((specs (extend-ugen-specs specs)))
      `(,@(loop for sp in (get-ugen-spec :readers specs)
                collect (def 'define-ugen-control-getter sp
                         `((:name :arg-name) ,instance-type
                           (:inline-p :method-p))))
        ,@(loop for sp in (get-ugen-spec :writers specs)
                collect (def 'define-ugen-control-setter sp
                         `((:name :value-type :value-name :arg-name)
                           ,instance-type (:inline-p :method-p))))
        ,@(loop for args in (get-ugen-spec :setfable specs)
                collect `(defsetf ,@args))))))

;;; SBCL VOP style for optional UGEN SPEC's.
(defmacro define-ugen (name return-type arglist &body body)
  "Define a new VUG, the UGEN obtained by compiling that VUG and the
auxiliary function named NAME.

If the UGEN is declared inline within the definition of a VUG, UGEN
or DSP, it is replaced with the parent VUG.

The UGEN output is of type RETURN-TYPE.

Each element of the ARGLIST is a list

    (argument-name argument-type)

or a symbol ARGUMENT-NAME if the control parameter is of type SAMPLE.

If the auxiliary function NAME is used within the body of DEFINE-VUG,
DEFINE-VUG-MACRO, DEFINE-UGEN or DSP!, the behaviour is analogous to
the auxiliary function of a VUG. Otherwise, it returns the function
of no arguments to allocate new UGEN instances. In the last case, it
accepts an UGEN node as optional argument.

If the first forms in BODY are lists beginning with a keyword, they
are UGEN SPEC's. The keyword indicates the interpretation of the
other forms in the specification:

    :DEFAULTS default-values
        Default values for UGEN parameter controls.

    :INSTANCE-TYPE Name
        Type of the UGEN instance (default: UGEN-INSTANCE).

    :CONSTRUCTOR Name
        Contructor function used to create a UGEN instance
        (default: MAKE-[instance-type]).

    :READERS {(Control-Name {Key Value}*)}*
        Specifications of the control getters.
        The valid keywords are:

        :NAME         Getter name (default: [ugen-name]-[control-name])
        :ARG-NAME     Argument name for the UGEN instance (default: UGEN-INSTANCE)
        :INLINE-P     T to declare the function inline
                      (default: T if METHOD-P NIL)
        :METHOD-P     T to define the getter with DEFMETHOD (default: nil)

    :WRITERS {(Control-Name {Key Value}*)}*
        Specifications of the control setters.
        The valid keywords are:

        :NAME         Setter name (default: SET-[ugen-name]-[control-name])
        :ARG-NAME     Argument name for the UGEN instance (default: UGEN-INSTANCE)
        :VALUE-NAME   Argument name for the new value (default: VALUE)
        :VALUE-TYPE   New value type
        :INLINE-P     T to declare the function inline
                      (default: T if METHOD-P NIL)
        :METHOD-P     T to define the setter with DEFMETHOD (default: nil)

    :ACCESSORS {(Control-Name {Key Value}*)}*
        Specifications of the control getters and setters.
        The valid keywords are:

        :NAME :ARG-NAME :VALUE-NAME :VALUE-TYPE :INLINE-P :METHOD-P

        If :METHOD-P is NIL and :NAME is specified, the function NAME
        is SETF-able:

            (defun name ...)
            (defun set-name ...)
            (defsetf name set-name)

        If :METHOD-P is T, define the methods:

            (defmethod name ...)
            (defmethod (setf name) ...)

    :OPTIMIZE {Quality | (Quality Value)}*
        Optimization qualities for the declaration OPTIMIZE.

If the specification :DEFAULTS is defined, all the arguments of the
auxiliary function are optional keywords.

Return the new UGEN structure."
  (multiple-value-bind (doc specs form) (get-ugen-specs body)
    (let* ((instance-type (or (car (get-ugen-spec :instance-type specs))
                              'ugen-instance))
           (instance-constructor (or (car (get-ugen-spec :constructor specs))
                                     (format-symbol
                                       (symbol-package instance-type)
                                       "MAKE-~A" instance-type))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (compile-vug (define-vug ,name ,arglist ,@(and doc `(,doc))
                                  ,@specs ,@form)
                      ',return-type nil ',instance-constructor
                      (optimize-settings ',specs :ugen-spec-p t))
         ,@(define-ugen-control-get/setters name instance-type specs)))))

(defmacro ugen-debug (name return-type arglist &body body)
  "Return a function to show the code generated by DEFINE-UGEN.

See DEFINE-UGEN for the macro function arguments.

The returned function requires the UGEN arguments plus one optional
argument to specify the output stream."
  (multiple-value-bind (doc specs form) (get-ugen-specs body)
    (declare (ignore doc))
    (let ((instance-constructor (car (get-ugen-spec :constructor specs)))
          (defaults (get-ugen-spec :defaults specs))
          (optimize (optimize-settings specs :ugen-spec-p t)))
      (check-default-args arglist defaults 'ugen)
      (multiple-value-bind (args types) (arg-names-and-types arglist)
        `(%%codegen-debug
            ,name ,arglist ,defaults ,optimize generate-ugen-code
             (,(dsp-coercing-arguments (get-bindings args types)) nil nil
              ,instance-constructor)
           (maybe-store-return-value ,return-type (tick ,@form)))))))

(defmacro with-ugen-instance ((var ugen-name &rest args) &body body)
  "Bind VAR to a newly allocated UGEN instance with arguments ARGS and
dynamic extent during BODY."
  `(let ((,var (funcall (,ugen-name ,@args))))
     (declare (type ugen-instance ,var))
     (incudine::maybe-unwind-protect (progn ,@body) (incudine:free ,var))))

(defmacro with-ugen-instances (bindings &body body)
  "Create bindings to newly allocated UGEN instances with dynamic extent
during BODY.

BINDINGS is a list of lists

    (var ugen-name &rest args)

where VAR is the variable bound to a UGEN instance named UGEN-NAME
with arguments ARGS."
  (let ((vars (mapcar #'car bindings)))
    `(let ,(mapcar (lambda (x) `(,(car x) (funcall ,(cdr x)))) bindings)
       ,(and vars `(declare (type ugen-instance ,@vars)))
       (incudine::maybe-unwind-protect (progn ,@body)
         ,@(mapcar (lambda (x) `(incudine:free ,x)) vars)))))
