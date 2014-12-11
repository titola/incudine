;;; Copyright (c) 2013-2014 Tito Latini
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

(declaim (special *vug-variables* *variables-to-preserve* *inlined-ugens*
                  *ugen-return-value*))

(defvar *vugs* (make-hash-table))
(declaim (type hash-table *vugs*))

(defvar *ugens* (make-hash-table))
(declaim (type hash-table *ugens*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *constructors-for-objects-to-free* nil)
  (declaim (type list *constructors-for-objects-to-free*))

  (defvar *objects-to-free* (make-hash-table))
  (declaim (type hash-table *objects-to-free*))

  (defmacro object-to-free (name reinit-fname)
    `(progn
       (pushnew ',name *constructors-for-objects-to-free*)
       (setf (gethash ',name *objects-to-free*)
             ',reinit-fname)))

  (object-to-free make-array update-lisp-array)
  (object-to-free make-foreign-array update-foreign-array)

  (defvar *numeric-operations*
    '(* + - / 1+ 1- conjugate gcd lcm
      abs acos acosh asin asinh atan atanh cis cos cosh exp expt isqrt log phase
      signum sin sinh sqrt tan tanh
      /= < <= = > >= evenp max min minusp oddp plusp zerop
      ceiling complex decode-float denominator fceiling ffloor float
      float-digits float-precision float-radix float-sign floor fround ftruncate
      imagpart integer-decode-float mod numerator rational rationalize realpart
      rem round scale-float truncate ash boole boole-1 boole-2 boole-and
      boole-andc1 boole-andc2 boole-c1 boole-c2 boole-clr boole-eqv boole-ior
      boole-nand boole-nor boole-orc1 boole-orc2 boole-set boole-xor
      integer-length logand logandc1 logandc2 logbitp logcount logeqv logior
      lognand lognor lognot logorc1 logorc2 logtest logxor byte byte-position
      byte-size deposit-field dpb ldb ldb-test mask-field))

  (defvar *misc-non-destructive-operations*
    '(coerce locally progn))

  (defvar *incudine-non-destructive-operations*
    '(sample next-power-of-two power-of-two-p lin->db db->lin linear-interp
      cos-interp cubic-interp t60->pole sample->fixnum sample->int calc-lobits))

  (defvar *known-non-destructive-functions*
    (let ((ht (make-hash-table)))
      (dolist (flist (list *numeric-operations*
                           *misc-non-destructive-operations*
                           *incudine-non-destructive-operations*)
               ht)
        (dolist (op flist) (setf (gethash op ht) nil)))))

  (defun known-non-destructive-function-p (fname)
    (multiple-value-bind (ign present-p)
        (gethash fname *known-non-destructive-functions*)
      (declare (ignore ign))
      present-p)))

(defvar *default-specials-to-eval*
  '(*sample-rate* *sample-duration* *cps2inc* *pi-div-sr* *minus-pi-div-sr*
    *twopi-div-sr* *sound-velocity* *r-sound-velocity*))
(declaim (type list *default-specials-to-eval*))

(defvar *specials-to-eval* *default-specials-to-eval*)
(declaim (type list *specials-to-eval*))

(defvar *eval-some-specials-p* nil)
(declaim (type boolean *eval-some-specials-p*))

(declaim (inline special-var-to-eval-p))
(defun special-var-to-eval-p (varname)
  (and (symbolp varname)
       (incudine.util::var-globally-special-p varname)
       (member varname *specials-to-eval*)))

;;; Virtual Unit Generator
(defstruct (vug (:copier nil))
  (name nil :type symbol)
  (callback nil :type (or function null))
  (args nil :type list)
  (arg-types nil :type list)
  (macro-p nil :type boolean :read-only t))

(defstruct (vug-object (:copier nil))
  (name nil)
  (type t)
  (block-p nil :type boolean))

;;; Parameter to control the DSP
(defstruct (vug-parameter (:include vug-object)
                          (:constructor %make-vug-parameter)
                          (:copier nil))
  (value nil)
  (aux-varname nil)
  (vars-to-update nil :type list))

(defstruct (vug-symbol (:include vug-object) (:copier nil)))

(defstruct (vug-variable (:include vug-object)
                         (:constructor %make-vug-variable)
                         (:copier nil))
  (value nil)
  (ref-count 0 :type non-negative-fixnum)
  (check-value-p t :type boolean)
  (to-set-p t :type boolean)
  (skip-init-set-p nil :type boolean)
  (conditional-expansion nil :type symbol)
  (input-p nil :type boolean)
  (init-time-p t :type boolean)
  (performance-time-p nil :type boolean)
  (variables-to-recheck nil :type list))

(defstruct (vug-variables (:copier nil))
  (bindings nil :type list)
  (parameter-list nil :type list)
  (to-update nil :type list)
  (to-free nil :type list)
  (to-expand-multiple-times nil :type list)
  (bindings-to-cache nil :type list)
  ;; List of the variables with at least a setter form in the
  ;; init-time bindings. It is used to determine if an init-time
  ;; variable is unused after the first binding.
  (init-time-setter nil :type list)
  ;; Unused variables
  (deleted (make-hash-table :test 'eq) :type hash-table)
  ;; List of the variables with type SAMPLE
  (foreign-sample nil :type list)
  ;; List of the variables with type single and double float
  (foreign-float nil :type list)
  (foreign-double nil :type list)
  ;; List of the variables with unboxed type 32-bit integer
  ;; (used only on a 32-bit system)
  (foreign-int32 nil :type list)
  ;; List of the variables with unboxed type 64-bit integer
  (foreign-int64 nil :type list)
  ;; List of the variables with type pointer
  (foreign-pointer nil :type list))

(defstruct (vug-function (:include vug-object)
                         (:copier nil))
  (inputs nil :type list))

(defstruct (vug-declaration (:include vug-object)
                            (:constructor %make-vug-declaration)
                            (:copier nil))
  (spec nil))

(defmethod print-object ((obj vug) stream)
  (format stream "#<~A ~A>"
          (if (vug-macro-p obj) 'vug-macro 'vug)
          (vug-name obj)))

(defmethod print-object ((obj vug-object) stream)
  (format stream "#<~A ~A>" (type-of obj) (vug-object-name obj)))

(declaim (inline init-time-p))
(defun init-time-p (obj)
  (if (vug-variable-p obj) (vug-variable-init-time-p obj) t))

(declaim (inline performance-time-p))
(defun performance-time-p (obj)
  (and (vug-variable-p obj) (vug-variable-performance-time-p obj)))

(declaim (inline no-performance-time-p))
(defun no-performance-time-p (obj)
  (and (vug-variable-p obj) (not (vug-variable-performance-time-p obj))))

(declaim (inline no-vug-variable-to-set))
(defun no-vug-variable-to-set (var init-pass-p)
  (if init-pass-p
      (setf (vug-variable-skip-init-set-p var) t)
      (setf (vug-variable-to-set-p var) nil)))

(defun vug-variable-value-zero-p (var)
  (let ((val (vug-variable-value var)))
    (or (and (numberp val) (zerop val))
        (and (vug-symbol-p val)
             (eq (vug-symbol-name val) '+sample-zero+)))))

(declaim (inline vug-variable-to-expand-multiple-times-p))
(defun vug-variable-to-expand-multiple-times-p (var)
  (member var (vug-variables-to-expand-multiple-times *vug-variables*)))

(declaim (inline vug))
(defun vug (name)
  (declare (type symbol name))
  (values (gethash name *vugs*)))

(declaim (inline ugen))
(defun ugen (name)
  (declare (type symbol name))
  (values (gethash name *ugens*)))

(declaim (inline ugen-variable-p))
(defun ugen-variable-p (var)
  (vug-type-p var 'ugen-instance))

(defmacro vug-funcall (vug-name &rest arguments)
  `(funcall (vug-callback (vug ,vug-name)) ,@arguments))

(declaim (inline vug-object-name-string))
(defun vug-object-name-string (obj)
  (symbol-name (vug-object-name obj)))

(declaim (inline make-vug-parameter))
(defun make-vug-parameter (name value type)
  (let ((par (%make-vug-parameter :name name :value value :type type)))
    (push par (vug-variables-parameter-list *vug-variables*))
    par))

(declaim (inline vug-input-p))
(defun vug-input-p (obj)
  (and (vug-function-p obj)
       (eq (vug-function-name obj) 'vug-input)))

(declaim (inline get-ugen-instance-p))
(defun get-ugen-instance-p (obj)
  (and (vug-function-p obj)
       (eq (vug-function-name obj) 'get-ugen-instance)))

(declaim (inline unquote-vug-symbol-name))
(defun unquote-vug-symbol-name (obj)
  (second (vug-object-name obj)))

(declaim (inline store-ugen-return-value-p))
(defun store-ugen-return-value-p (obj)
  (and (boundp '*ugen-return-value*)
       (vug-function-p obj)
       (eq (vug-function-name obj) 'store-ugen-return-value)))

(declaim (inline store-ugen-return-varname))
(defun store-ugen-return-varname (name)
  (setf *ugen-return-value* name))

(defun make-vug-variable (name value &optional type)
  (multiple-value-bind (input-p value ref-count)
      (cond ((vug-input-p value)
             (values t (car (vug-function-inputs value)) 0))
            ((store-ugen-return-value-p value)
             (store-ugen-return-varname name)
             (values nil (car (vug-function-inputs value)) 1))
            (t (values nil value 0)))
    (let ((obj (%make-vug-variable :name name :value value :type type
                                   :ref-count ref-count :input-p input-p))
          (to-recheck-p (or input-p (get-ugen-instance-p value))))
      (when (boundp '*vug-variables*)
        (if (performance-time-code-p obj value to-recheck-p)
            (setf (vug-variable-init-time-p obj) nil)
            (resolve-variable-to-update value obj))
        (when (object-to-free-p value)
          (pushnew obj (vug-variables-to-free *vug-variables*)))
        (push obj (vug-variables-bindings *vug-variables*)))
      obj)))

(declaim (inline vug-name-p))
(defun vug-name-p (vug name)
  (eq (vug-object-name vug) name))

(declaim (inline vug-type-p))
(defun vug-type-p (vug type)
  (eq (vug-object-type vug) type))

(declaim (inline init-time-vug-function-p))
(defun init-time-vug-function-p (obj)
  (member (vug-object-name obj) '(init-only lambda)))

(declaim (inline performance-time-function-p))
(defun performance-time-function-p (fname)
  (member fname '(now audio-in audio-out bus)))

(declaim (inline performance-time-vug-function-p))
(defun performance-time-vug-function-p (obj)
  (performance-time-function-p (vug-object-name obj)))

(defun performance-time-code-p (var obj var-to-recheck-p)
  (labels ((ptime-p (obj)
             (cond ((vug-object-p obj)
                    (cond ((vug-object-block-p obj) t)
                          ((vug-function-p obj)
                           (unless (init-time-vug-function-p obj)
                             (or (performance-time-vug-function-p obj)
                                 (ptime-p (vug-function-inputs obj)))))
                          ((vug-variable-p obj)
                           (cond ((vug-variable-performance-time-p obj) t)
                                 ((vug-variable-init-time-p obj)
                                  (if (or var-to-recheck-p
                                          (vug-variable-input-p obj))
                                      (pushnew var
                                        (vug-variable-variables-to-recheck obj)))
                                  (ptime-p (vug-variable-value obj)))
                                 (t t)))))
                   ((consp obj)
                    (or (ptime-p (car obj)) (ptime-p (cdr obj)))))))
    (ptime-p obj)))

(defun resolve-variable-to-update (obj var)
  (cond ((vug-function-p obj)
         (resolve-variable-to-update (vug-function-inputs obj) var))
        ((vug-variable-p obj)
         (resolve-variable-to-update (vug-variable-value obj) var))
        ((vug-parameter-p obj)
         (unless (member var (vug-parameter-vars-to-update obj))
           (variable-to-update var obj)))
        ((consp obj)
         (resolve-variable-to-update (car obj) var)
         (resolve-variable-to-update (cdr obj) var))))

(declaim (inline variable-to-update))
(defun variable-to-update (var param)
  (push var (vug-parameter-vars-to-update param))
  (pushnew param (vug-variables-to-update *vug-variables*)))

(declaim (inline vug-variable-to-preserve-p))
(defun vug-variable-to-preserve-p (var)
  (and (boundp '*variables-to-preserve*)
       (member var *variables-to-preserve*)))

(defun preserve-vug-variable (obj)
  (declare (type vug-variable obj))
  (when (boundp '*variables-to-preserve*)
    (pushnew obj *variables-to-preserve*)))

(defun coerce-number (obj output-type-spec)
  (if (and (numberp obj)
           (not (or (foreign-int32-type-p output-type-spec)
                    (foreign-int64-type-p output-type-spec))))
      (cond ((foreign-float-type-p output-type-spec)
             (coerce obj 'single-float))
            ((foreign-double-type-p output-type-spec)
             (coerce obj 'double-float))
            ((subtypep output-type-spec 'sample)
             (force-sample-format obj))
            (t (coerce obj output-type-spec)))
      obj))

(defun maybe-coerce-value (obj type)
  (when (vug-variable-p obj)
    (let ((value (vug-variable-value obj)))
      (when (numberp value)
        (let ((new (coerce-number value type)))
          (unless (eq (type-of value) (type-of new))
            (setf (vug-variable-value obj) new)))))))

(defun make-vug-declaration (spec)
  (declare (type list spec))
  (let ((obj (%make-vug-declaration :name (car spec) :spec spec)))
    (dolist (decl spec)
      (case (car decl)
        (type (let ((type (second decl)))
                (dolist (i (cddr decl))
                  (when (vug-object-p i)
                    (setf (vug-object-type i) type)
                    (maybe-coerce-value i type)
                    (when (and (ugen-variable-p i)
                               (not (vug-variable-init-time-p i)))
                      (resolve-variable-to-update (vug-variable-value i) i))))))
        (preserve
         (dolist (i (cdr decl))
           (when (vug-variable-p i)
             (preserve-vug-variable i))))
        (performance-time
         (dolist (i (cdr decl))
           (if (vug-variable-p i)
               (setf (vug-variable-performance-time-p i) t)
               (msg error
                    "performance-time declaration, ~A is not a VUG variable"
                    i))))))
    obj))

(declaim (inline vug-block-p))
(defun vug-block-p (object)
  (let ((vug (vug object)))
    (and vug (not (macro-function (vug-name vug))))))

(declaim (inline ugen-block-p))
(defun ugen-block-p (object)
  (and (ugen object) t))

(declaim (inline function-call-p))
(defun function-call-p (lst)
  (and (symbolp (car lst)) (fboundp (car lst))))

(declaim (inline init-binding-form-p))
(defun init-binding-form-p (lst)
  (eq (car lst) 'with))

(declaim (inline binding-form-p))
(defun binding-form-p (lst)
  (member (car lst) '(let let* symbol-macrolet compiler-let)))

(declaim (inline setter-form-p))
(defun setter-form-p (obj)
  (member obj '(setf setq incf decf psetf psetq)))

(declaim (inline quote-symbol-p))
(defun quote-symbol-p (lst)
  (and (eq (first lst) 'quote) (atom (second lst))))

(declaim (inline quote-function-p))
(defun quote-function-p (lst)
  (eq (car lst) 'function))

(declaim (inline local-buffer-p))
(defun local-buffer-p (obj)
  (and (vug-function-p obj) (vug-name-p obj 'incudine:make-buffer)))

(declaim (inline object-to-free-p))
(defun object-to-free-p (obj)
  (and (vug-function-p obj)
       (member (vug-object-name obj) *constructors-for-objects-to-free*)))

(declaim (inline parse-bindings))
(defun parse-bindings (lst flist mlist floop-info)
  (mapcar (lambda (x)
            (list (car x)
                  `(make-vug-variable
                     (gensym (string ',(car x)))
                     (remove-wrapped-parens
                       ,(let ((value (cadr x)))
                          (parse-vug-def (if (vug-parameter-p value)
                                             (vug-parameter-value value)
                                             value)
                                         nil flist mlist floop-info))))))
          lst))

(declaim (inline parse-block-form))
(defun parse-block-form (form flist mlist floop-info)
  `(make-vug-function :name ',(car form)
     :inputs (list ',(cadr form)
                   ,@(parse-vug-def (cddr form) nil flist mlist floop-info))))

(defun parse-lambda-body (form flist mlist floop-info)
  (multiple-value-bind (decl rest) (separate-declaration form)
    `(,@(mapcar (lambda (x)
                  `(make-vug-function :name 'declare :inputs ',(cdr x)))
                decl)
      ,@(parse-vug-def rest nil flist mlist floop-info))))

(defun parse-let-form (form flist mlist floop-info)
  (let ((args (cadr form)))
    `(let ,(mapcar (lambda (x) `(,(car x) ',(car x))) args)
       (declare (type symbol ,@(mapcar #'car args)))
       (make-vug-function :name ',(car form)
         :inputs (list (list ,@(mapcar (lambda (x)
                                         `(list ,(car x)
                                                ,(parse-vug-def (cadr x) nil
                                                                flist mlist
                                                                floop-info)))
                                       args))
                       ,@(parse-lambda-body (cddr form) flist mlist
                                            floop-info))))))

(defun parse-init-bindings (form flist mlist floop-info)
  `(,(car form)
    ,(parse-bindings (cadr form) flist mlist floop-info)
    ,@(multiple-value-bind (decl rest) (separate-declaration (cddr form))
        `(,@(parse-vug-def decl)
          (make-vug-function :name 'progn
            :inputs (list ,@(parse-vug-def rest nil flist mlist
                                           floop-info)))))))

(defun parse-lambda-form (form flist mlist floop-info)
  (let ((args (cadr form)))
    `(let ,(lambda-bindings args)
       (make-vug-function :name ',(car form)
         :inputs (list ',args
                       (list ,@(parse-lambda-body (cddr form)
                                                  flist mlist floop-info)))))))

(defmacro inlined-ugen-p (ugen-name)
  `(getf *inlined-ugens* ,ugen-name))

(defun parse-declare-form (form)
  (case (car form)
    ((type ftype) `(list 'type ',(second form) ,@(cddr form)))
    ((inline notinline)
     (let* ((ugens (remove-if-not #'ugen (cdr form)))
            (funcs (set-difference (cdr form) ugens)))
       (dolist (u ugens)
         (setf (inlined-ugen-p u) (eq (car form) 'inline)))
       (when funcs
         `(list ',(car form) ,@(cdr funcs)))))
    ((dynamic-extent ignore optimize special ignorable)
     `(list ',(car form) ,@(cdr form)))
    (otherwise (cond ((string= (symbol-name (car form)) "PERFORMANCE-TIME")
                      `(list 'performance-time ,@(cdr form)))
                     ((string= (symbol-name (car form)) "PRESERVE")
                      `(list 'preserve ,@(cdr form)))
                     (t `(list 'type ',(car form) ,@(cdr form)))))))

(defmacro %with-local-args (args &body body)
  `(let ,(mapcar (lambda (x) `(,x ',x)) args)
     (declare (ignorable ,@args))
     ,@body))

(defun parse-flet-form (form flist mlist floop-info)
  (declare (type list form flist mlist))
  (let (acc)
    `(make-vug-function :name ',(car form)
       :inputs (list
                 (list ,@(mapcar (lambda (def)
                                   (push (car def) acc)
                                   `(list ',(car def)
                                          ',(cadr def)
                                          (%with-local-args ,(cadr def)
                                            ,@(parse-vug-def (cddr def)
                                                             nil flist mlist
                                                             floop-info))))
                                 (cadr form)))
                 ,@(parse-vug-def (cddr form) nil (nconc acc flist) mlist
                                  floop-info)))))

(defun parse-labels-form (form flist mlist floop-info)
  (declare (type list form flist mlist))
  (let ((acc (copy-list flist))
        (definitions (cadr form)))
    (dolist (l definitions) (push (car l) acc))
    `(make-vug-function :name ',(car form)
       :inputs (list (list ,@(mapcar (lambda (def)
                                       `(list ',(car def)
                                              ',(cadr def)
                                              (%with-local-args ,(cadr def)
                                                ,@(parse-vug-def (cddr def) nil
                                                                 acc mlist
                                                                 floop-info))))
                                     definitions))
                     ,@(parse-vug-def (cddr form) nil acc mlist floop-info)))))

(defun parse-macrolet-form (form flist mlist floop-info)
  (declare (type list form flist mlist))
  (let ((acc (copy-list mlist))
        (definitions (cadr form)))
    (dolist (l definitions) (push l acc))
    `(make-vug-function :name 'progn
       :inputs (list ,@(parse-vug-def (cddr form) nil flist acc floop-info)))))

(declaim (inline parse-locally-form))
(defun parse-locally-form (form flist mlist floop-info)
  `(make-vug-function :name ',(car form)
     :inputs ,(multiple-value-bind (decl rest)
                (separate-declaration (cdr form))
                `(list ,@(mapcar (lambda (x)
                                   `(make-vug-function :name 'declare
                                                       :inputs ',(cdr x)))
                                 decl)
                       ,@(parse-vug-def rest nil flist mlist floop-info)))))

(declaim (inline parse-tagbody-form))
(defun parse-tagbody-form (form flist mlist floop-info)
  `(make-vug-function :name ',(car form)
     :inputs (list ,@(mapcar (lambda (x)
                               (if (atom x)
                                   (list 'quote x)
                                   (parse-vug-def x nil flist mlist
                                                  floop-info)))
                             (cdr form)))))

(declaim (inline parse-go-form))
(defun parse-go-form (form)
  `(make-vug-function :name ',(car form) :inputs '(,(cadr form))))

(declaim (inline parse-tick-form))
(defun parse-tick-form (form flist mlist floop-info)
  (if (atom (cadr form))
      `(make-vug-symbol :name ',(cadr form) :block-p t)
      `(make-vug-function
         :name 'progn
         :inputs (list ,@(parse-vug-def (cdr form) t flist mlist floop-info))
         :block-p t)))

(declaim (inline lambda-bindings))
(defun lambda-bindings (args)
  (loop for i in args
        unless (member i '(&optional &key &rest))
        collect `(,i ',i)))

(defmacro return-ugen-foreign-value (ugen-name type)
  `(cffi:mem-ref (ugen-instance-return-pointer ,ugen-name)
                 ',(cond ((foreign-float-type-p type) :float)
                         ((foreign-double-type-p type) :double)
                         ((foreign-int32-type-p type) :int32)
                         ((foreign-int64-type-p type) :int64)
                         ((foreign-pointer-type-p type) :pointer)
                         ((subtypep type 'sample) 'sample))))

(defmacro ugen-run (ugen-var type)
  (with-gensyms (ugen)
    `(let ((,ugen ,ugen-var))
       (declare (type ugen-instance ,ugen))
       (funcall (ugen-instance-perf-function ,ugen) current-channel)
       ,@(when (foreign-type-p type)
           (list `(return-ugen-foreign-value ,ugen ,type))))))

(defun parse-ugen (def flist mlist floop-info)
  (let ((type (ugen-return-type (ugen (car def)))))
    (parse-vug-def
      ;; We can use the arguments of GET-UGEN-INSTANCE during the
      ;; code-walking. The value of the VUG-VARIABLE bound to a UGEN
      ;; is a VUG-FUNCTION where the arguments are respectively the
      ;; name and the arguments of the UGEN.
      `(with ((ugen (get-ugen-instance ',(car def) ,@(cdr def) (dsp-node)))
              (ret (ugen-run ugen ,type)))
         (declare (type ugen-instance ugen) (type ,type ret))
         ret)
      nil flist mlist floop-info)))

(defmacro foreach-frame-loop (in-ptr out-ptr now-var &body body)
  (declare (ignore in-ptr out-ptr))
  `(loop for current-sample of-type non-negative-fixnum
                            below incudine::*block-samples*
                            by *number-of-output-bus-channels*
         for current-frame of-type non-negative-fixnum from 0
         do (progn ,@body ,@(if now-var `((incf ,now-var))))))

(defun parse-foreach-frame-form (def flist mlist)
  (with-gensyms (in-ptr out-ptr now ch frame)
    (parse-vug-def
     `(with ((,in-ptr (incudine::input-pointer))
             (,out-ptr (incudine::output-pointer))
             (,now (smp-ref incudine::*sample-counter* 0)))
        (declare (type pointer ,in-ptr ,out-ptr) (type sample ,now))
        (set-local-io-pointer ,in-ptr (incudine::input-pointer))
        (set-local-io-pointer ,out-ptr (incudine::output-pointer))
        (set-local-now ,now (smp-ref incudine::*sample-counter* 0))
        ;; The follow macros work outside the nested VUGs that are updated
        ;; during the walking in UPDATE-VARIABLES-INIT-TIME-SETTER and
        ;; REDUCE-VUG-VARIABLES.
        (macrolet ((audio-in (,ch &optional ,frame)
                     (declare (ignore ,frame))
                     `(smp-ref ,',in-ptr
                               (the non-negative-fixnum
                                    (+ ,,ch current-sample))))
                   (audio-out (,ch &optional ,frame)
                     (declare (ignore ,frame))
                     `(smp-ref ,',out-ptr
                               (the non-negative-fixnum
                                    (+ ,,ch current-sample))))
                   (now () ',now))
          (foreach-frame-loop ,in-ptr ,out-ptr ,now ,@(cdr def))))
     nil flist mlist (list in-ptr out-ptr now))))

;;; The follow functions are only "tags":

(defun tick (&rest forms) forms)

(defun external-variable (obj) (identity obj))

(defun update (obj) (identity obj))

(defun init-only (&rest forms) forms)

(defun initialize (&rest forms) forms)

(defmacro without-follow (parameters &body body) parameters body)

(defun vug-input (arg) arg)

(defun store-ugen-return-value (form) form)

(defmacro foreach-frame (&body body)
  `(progn ,@body))

(defmacro set-local-io-pointer (ptr-var value)
  `(setf ,ptr-var ,value))

(defmacro set-local-now (now-var value)
  `(setf ,now-var ,value))

;;; End of the "tags".

;;; MAYBE-EXPAND is used inside the definition of a VUG, when the
;;; expansion of one or more performance-time variables is to inhibit
;;; after a particular point of the code (i.e. loop or condition). It
;;; avoids the obscure isolated vug-variables in the body of a VUG.
(defmacro maybe-expand (&body body) `(progn ,@body))

(defun parse-vug-def (def &optional cdr-p flist mlist floop-info)
  (declare (type boolean cdr-p) (type list flist mlist))
  (cond ((null def) nil)
        ((consp def)
         (let ((name (car def)))
           (cond ((consp name)
                  (cons (parse-vug-def name nil flist mlist floop-info)
                        (parse-vug-def (cdr def) t flist mlist floop-info)))
                 ((null cdr-p)
                  (cond ((init-binding-form-p def)
                         (parse-init-bindings def flist mlist floop-info))
                        ((declare-form-p def)
                         (let ((decl (remove-if #'null
                                                (mapcar #'parse-declare-form
                                                        (cdr def)))))
                           (when decl
                             `(make-vug-declaration (list ,@decl)))))
                        ((eq name 'locally)
                         (parse-locally-form def flist mlist floop-info))
                        ((member name flist) ; local function
                         `(make-vug-function :name ',name
                            :inputs (list ,@(parse-vug-def
                                              (cdr def) t flist mlist
                                              floop-info))))
                        ((member name mlist :key #'car) ; local macro
                         (destructuring-bind (lambda-list &rest body)
                             (cdr (assoc name mlist))
                           (parse-vug-def
                             (eval `(destructuring-bind ,lambda-list ',(cdr def)
                                      ;; Remove a possible doc string before
                                      ;; a declaration.
                                      ,@(do ((l body (cdr l)))
                                            ((not (stringp (car l))) l))))
                            nil flist mlist floop-info)))
                        ((function-call-p def)
                         (cond ((binding-form-p def)
                                (parse-let-form def flist mlist floop-info))
                               ((ugen-block-p name)
                                (if (inlined-ugen-p name)
                                    `(ugen-inline-funcall ',name
                                       ,@(parse-vug-def
                                           (cdr def) t flist mlist floop-info))
                                    (parse-ugen def flist mlist floop-info)))
                               ((eq name 'ugen-run)
                                `(make-vug-function :name ',name
                                   :inputs (list ,(second def) ',(third def))
                                   :block-p t))
                               ((vug-block-p name)
                                (let ((form
                                       `(vug-funcall ',name
                                          ,@(parse-vug-def (cdr def) t flist
                                                           mlist floop-info))))
                                  (if floop-info
                                      `(make-vug-function
                                         :name 'filter-foreach-frame-form
                                         :inputs (list ,@floop-info ,form))
                                      form)))
                               ((member name '(vug-funcall ugen-funcall))
                                `(,name ,(cadr def)
                                        ,@(parse-vug-def
                                            (cddr def) t flist mlist
                                            floop-info)))
                               ((eq name 'foreach-frame)
                                (if floop-info
                                    ;; All the nested FOREACH-FRAME loops are
                                    ;; merged with the first.
                                    `(make-vug-function :name 'progn
                                       :inputs (list ,@(parse-vug-def (cdr def)
                                                         nil flist mlist
                                                         floop-info)))
                                    (parse-foreach-frame-form def flist mlist)))
                               ((eq name 'tick)
                                (parse-tick-form def flist mlist floop-info))
                               ((eq name 'get-pointer)
                                ;; Inhibit the expansion of GET-POINTER macro
                                `(make-vug-function :name 'get-pointer
                                   :inputs (list ,@(parse-vug-def
                                                     (cdr def) t flist mlist
                                                     floop-info))))
                               ((eq name 'external-variable)
                                `(make-vug-symbol :name ',(cadr def)))
                               ((eq name 'without-follow)
                                `(make-vug-function :name ',name
                                   :inputs (list (list ,@(cadr def))
                                                 ,@(parse-vug-def (cddr def)))))
                               ((quote-function-p def)
                                `(make-vug-function :name 'function
                                   :inputs (list ',(cadr def))))
                               ((eq name 'lambda)
                                (parse-lambda-form def flist mlist floop-info))
                               ((eq name 'flet)
                                (parse-flet-form def flist mlist floop-info))
                               ((eq name 'labels)
                                (parse-labels-form def flist mlist floop-info))
                               ((eq name 'macrolet)
                                (parse-macrolet-form def flist mlist
                                                     floop-info))
                               ((eq name 'foreach-frame-loop)
                                `(make-vug-function :name 'foreach-frame-loop
                                   :inputs (list ,@(parse-vug-def (cdr def) nil
                                                                  flist mlist
                                                                  floop-info))))
                               ((member name '(set-local-io-pointer
                                               set-local-now))
                                `(make-vug-function :name ',name
                                   :inputs (list ,(second def) ',(third def))))
                               ((and (macro-function name)
                                     ;; Avoid the expansion of the setter forms.
                                     ;; This information is used during the code
                                     ;; generation.
                                     (not (setter-form-p name)))
                                (let ((expansion
                                       (macroexpand-1
                                        (if (member name '(with-samples
                                                           with-samples*))
                                            ;; The bindings of the initialization
                                            ;; are sequential, so there is not
                                            ;; difference between WITH-SAMPLES
                                            ;; and WITH-SAMPLES* inside a
                                            ;; definition of a VUG
                                            `(%with-samples ,@(cdr def))
                                            def))))
                                  (if (vug name)
                                      `(mark-vug-block
                                         ,(parse-vug-def expansion nil flist
                                                         mlist floop-info))
                                      (parse-vug-def expansion nil flist mlist
                                                     floop-info))))
                               ((quote-symbol-p def)
                                `(make-vug-symbol :name '',(second def)))
                               ((member name
                                        '(block return-from the catch throw))
                                (parse-block-form def flist mlist floop-info))
                               ((eq name 'tagbody)
                                (parse-tagbody-form def flist mlist floop-info))
                               ((eq name 'go) (parse-go-form def))
                               ((eq name 'quote)
                                `(make-vug-function :name 'quote
                                                    :inputs ',(cdr def)))
                               #+(and sbcl x86)
                               ((and (member name '(sin cos tan))
                                     (not (and (consp (cadr def))
                                               (eq (caadr def) 'the))))
                                ;; X86 uses FSIN, FCOS and FPTAN
                                `(make-vug-function :name ',name
                                   :inputs (list ,(parse-vug-def
                                                    `(the limited-sample
                                                          ,@(cdr def))
                                                    nil flist mlist
                                                    floop-info))))
                               (t `(make-vug-function :name ',name
                                     :inputs (list
                                               ,@(parse-vug-def
                                                   (cdr def) t flist mlist
                                                   floop-info))))))
                        (t (cons name (parse-vug-def (cdr def) t flist mlist
                                                     floop-info)))))
                 (t (cons (parse-vug-def name nil flist mlist floop-info)
                          (parse-vug-def (cdr def) t flist mlist
                                         floop-info))))))
        ((eq def 'pi) (sample pi))
        ((and (symbolp def) (or (boundp def) (eq def '%dsp-node%)))
         `(make-vug-symbol :name ',def))
        ((and (symbolp def)
              (member def '(current-channel current-frame current-sample)))
         `(make-vug-symbol :name ',def :block-p t))
        (t def)))

(defun remove-wrapped-parens (obj)
  (if (consp obj)
      (if (cdr obj) obj (remove-wrapped-parens (car obj)))
      obj))

(defun remove-lisp-declaration (obj)
  (if (consp obj)
      (if (vug-declaration-p (car obj))
          (remove-lisp-declaration (cdr obj))
          (cons (remove-lisp-declaration (car obj))
                (remove-lisp-declaration (cdr obj))))
      obj))

(defun recheck-variables (var)
  (declare (type vug-variable var))
  (let ((vardep (pop (vug-variable-variables-to-recheck var))))
    (when vardep
      (when #1=(vug-variable-init-time-p vardep)
        (setf #1# nil)
        (when (vug-variable-deleted-p vardep)
          (undelete-vug-variable vardep)
          (msg debug "undelete ~A after recheck" vardep))
        (msg debug "~A is performance-time after recheck" vardep))
      (recheck-variables var)
      (recheck-variables vardep))))

(declaim (inline set-variable-performance-time))
(defun set-variable-performance-time (var)
  (declare (type vug-variable var))
  (unless #1=(vug-variable-performance-time-p var) (setf #1# t))
  (recheck-variables var)
  var)

(defun update-setter-form (obj)
  (declare (type vug-function obj))
  (loop for (var value) on (vug-function-inputs obj) by #'cddr do
          (if (vug-variable-p var)
              (set-variable-performance-time var)
              (update-vug-variables var))
          (update-vug-variables value)))

(declaim (inline force-vug-expansion-p))
(defun force-vug-expansion-p (obj)
  (declare (type vug-function obj))
  (eq (vug-function-name obj) 'update))

(declaim (inline add-variable-to-expand-multiple-times))
(defun add-variable-to-expand-multiple-times (var)
  (when (boundp '*vug-variables*)
    (pushnew var (vug-variables-to-expand-multiple-times *vug-variables*))))

(defun update-variables-to-expand-multiple-times (obj)
  (cond ((vug-function-p obj)
         (cond ((force-vug-expansion-p obj)
                (let ((var (car (vug-function-inputs obj))))
                  (add-variable-to-expand-multiple-times var)
                  (update-variables-to-expand-multiple-times
                    (vug-variable-value var))))
               (t (update-variables-to-expand-multiple-times
                    (vug-function-inputs obj)))))
        ((vug-variable-p obj)
         (update-variables-to-expand-multiple-times (vug-variable-value obj)))
        ((consp obj)
         (update-variables-to-expand-multiple-times (car obj))
         (update-variables-to-expand-multiple-times (cdr obj)))))

;;; Transform AUDIO-IN or AUDIO-OUT in SMP-REF used with
;;; the pointer defined in the expansion of FOREACH-FRAME loop.
(defun update-audio-io (obj floop-info)
  (destructuring-bind (in-ptr out-ptr now) floop-info
    (declare (ignore now))
    (let ((fname (vug-function-name obj)))
      (setf (vug-function-name obj) 'smp-ref)
      (setf (vug-function-inputs obj)
            (list (if (eq fname 'audio-in) in-ptr out-ptr)
                  (make-vug-function :name 'the
                    :inputs (list 'non-negative-fixnum
                                  (make-vug-function :name '+
                                    :inputs (list* 'current-sample
                                                   (vug-function-inputs
                                                     obj)))))))
      obj)))

;; Transform `(now)' in `(progn now-var)', where NOW-VAR is the
;; foreign variable defined in the expansion of FOREACH-FRAME loop.
(defun update-now-function (obj floop-info)
  (setf (vug-function-name obj) 'progn)
  (setf (vug-function-inputs obj) (list (third floop-info)))
  obj)

;; Transform a nested FOREACH-FRAME loop in PROGN.
(defmacro update-foreach-frame-loop (obj update-func &rest args)
  (with-gensyms (in-ptr out-ptr now rest)
    `(destructuring-bind (,in-ptr ,out-ptr ,now &rest ,rest)
         (vug-function-inputs ,obj)
       (setf (vug-function-name ,obj) 'progn)
       (setf (vug-function-inputs ,obj) ,rest)
       (,update-func ,rest ,@args (list ,in-ptr ,out-ptr ,now)))))

(defun update-vug-variables (obj)
  (cond ((and (vug-function-p obj)
              (not (eq (vug-object-name obj) 'initialize)))
         (cond ((force-vug-expansion-p obj)
                ;; An updated variable is performance-time
                (let ((var (car (vug-function-inputs obj))))
                  (add-variable-to-expand-multiple-times var)
                  (set-variable-performance-time var)
                  (update-vug-variables (vug-variable-value var))))
               ((setter-form-p (vug-object-name obj))
                ;; A variable in a setter form in the body of the VUG
                ;; becomes performance-time
                (update-setter-form obj))
               (t (update-vug-variables (vug-function-inputs obj)))))
        ((vug-variable-p obj)
         (update-variables-to-expand-multiple-times (vug-variable-value obj)))
        ((consp obj)
         (update-vug-variables (car obj))
         (update-vug-variables (cdr obj))))
  obj)

(defun preserve-pointer-to-foreign-variable (obj)
  (cond ((vug-variable-p obj)
         (when (vug-variable-deleted-p obj)
           (undelete-vug-variable obj)
           (msg debug "undelete ~A (pointer required)" obj))
         (preserve-vug-variable obj))
        (t (msg error "cannot get the pointer to ~A" obj))))

(defun update-variables-init-time-setter ()
  (labels ((update-setter (obj &optional floop-info)
             (loop for (var value) on (vug-function-inputs obj) by #'cddr do
                     (if (vug-variable-p var)
                         (unless (vug-variable-performance-time-p var)
                           (pushnew var (vug-variables-init-time-setter
                                          *vug-variables*)))
                         (update var floop-info))
                     (update value floop-info)))
           (update (obj &optional floop-info)
             (cond ((vug-function-p obj)
                    (case (vug-object-name obj)
                      ((setf setq incf decf psetf psetq)
                       (update-setter obj floop-info)
                       (update (vug-function-inputs obj) floop-info))
                      (filter-foreach-frame-form
                       (update-foreach-frame-loop obj update))
                      ((audio-in audio-out)
                       (update (vug-function-inputs obj) floop-info)
                       (when floop-info (update-audio-io obj floop-info)))
                      (now
                       (when floop-info (update-now-function obj floop-info)))
                      (get-pointer
                       (let ((obj (car (vug-function-inputs obj))))
                         (preserve-pointer-to-foreign-variable obj)))
                      (otherwise (update (vug-function-inputs obj) floop-info))))
                   ((vug-variable-p obj)
                    (update (vug-variable-value obj) floop-info))
                   ((consp obj)
                    (update (car obj) floop-info)
                    (update (cdr obj) floop-info)))))
    (loop for var in (vug-variables-bindings *vug-variables*)
          do (update (vug-variable-value var)))
    *vug-variables*))

(declaim (inline vug-variable-with-init-time-setter-p))
(defun vug-variable-with-init-time-setter-p (var)
  (declare (type vug-variable var))
  (find var (vug-variables-init-time-setter *vug-variables*) :test 'eq))

(declaim (inline reducible-vug-variable-p))
(defun reducible-vug-variable-p (var)
  (declare (type vug-variable var))
  (and (vug-variable-init-time-p var)
       (not (vug-variable-to-preserve-p var))
       (not (vug-variable-performance-time-p var))
       (not (vug-variable-with-init-time-setter-p var))))

(declaim (inline constant-vug-variable-value-p))
(defun constant-vug-variable-value-p (value)
  (and (not (vug-object-p value)) (constantp value)))

(defun reducible-vug-function-p (obj)
  (labels ((reducible-p (x)
             (cond ((vug-function-p x)
                    (when (known-non-destructive-function-p
                            (vug-function-name x))
                      (every #'reducible-p (vug-function-inputs x))))
                   ((numberp x) t)
                   ((and *eval-some-specials-p* (vug-symbol-p x)
                         (special-var-to-eval-p (vug-object-name x)))
                    t)
                   ((vug-variable-p x)
                    (multiple-value-bind (cached cached-p)
                        (vug-variable-replacement x)
                      (and cached-p (constant-vug-variable-value-p cached))))
                   ((vug-object-p x) nil)
                   ((constantp x) t))))
    (and (vug-function-p obj) (reducible-p obj))))

(defun reduce-vug-function (obj)
  (labels ((f-reduce (x)
             (cond ((vug-function-p x)
                    `(,(vug-function-name x)
                       ,@(mapcar #'f-reduce (vug-function-inputs x))))
                   ((vug-variable-p x) (vug-variable-replacement x))
                   ((vug-symbol-p x) (vug-symbol-name x))
                   ((atom x) x))))
    (eval (f-reduce obj))))

(defmacro replace-vug-variable (var value &optional delete-deps-p)
  `(progn
     (setf (gethash ,var (vug-variables-deleted *vug-variables*)) ,value)
     ,(when delete-deps-p `(delete-vug-variable-dependencies ,var))))

(defmacro msg-debug-delete-variable (var init-or-performance)
  `(msg debug "delete ~A ~A of type ~A" ,init-or-performance ,var
        (vug-variable-type ,var)))

(defun variable-binding-to-cache-p (var)
  (declare (type vug-variable var))
  (labels ((to-cache-p (obj)
             (cond ((vug-function-p obj)
                    (if (eq (vug-function-name obj) 'lambda)
                        t
                        (to-cache-p (vug-function-inputs obj))))
                   ((consp obj)
                    (or (to-cache-p (car obj)) (to-cache-p (cdr obj)))))))
    (let ((value (vug-variable-value var)))
      ;; When OBJECT-TO-FREE-P is T, the right side of the binding is different.
      (unless (object-to-free-p value)
        (to-cache-p value)))))

(defun find-bindings-to-cache (variables)
  (let ((to-cache (remove-if-not #'variable-binding-to-cache-p variables)))
    (when to-cache
      (setf (vug-variables-bindings-to-cache *vug-variables*) to-cache))))

(defun delete-vug-variable-dependencies (var)
  (declare (type vug-variable var))
  (when *vug-variables*
    (dolist (par (vug-variables-to-update *vug-variables*))
      (setf #1=(vug-parameter-vars-to-update par) (delete var #1#)))))

(declaim (inline vug-variable-replacement))
(defun vug-variable-replacement (var)
  (declare (type vug-variable var))
  (gethash var (vug-variables-deleted *vug-variables*)))

(declaim (inline vug-variable-deleted-p))
(defun vug-variable-deleted-p (var)
  (multiple-value-bind (cached cached-p) (vug-variable-replacement var)
    (declare (ignore cached))
    cached-p))

(declaim (inline undelete-vug-variable))
(defun undelete-vug-variable (var)
  (declare (type vug-variable var))
  (remhash var (vug-variables-deleted *vug-variables*)))

(defmacro reduce-foreach-frame-loop (obj info replaced-vars initialize-body-p)
  (with-gensyms (in out now old-in old-out old-now rest old-vars)
    `(cond (,info
            ;; Merge the nested FOREACH-FRAME loop and use the
            ;; variables of the first loop.
            (destructuring-bind (,in ,out ,now) ,info
              (destructuring-bind (,old-in ,old-out ,old-now &rest ,rest)
                  (vug-function-inputs ,obj)
                (declare (ignore ,rest))
                (let ((,old-vars (list ,old-in ,old-out ,old-now)))
                  (push ,old-vars ,replaced-vars)
                  (mapc (lambda (old new)
                          (replace-vug-variable old new)
                          (msg-debug-delete-variable old "performance-time"))
                        ,old-vars (list ,in ,out ,now)))))
            (setf (vug-function-name ,obj) 'progn)
            (setf (vug-function-inputs ,obj)
                  (cdddr (vug-function-inputs ,obj)))
            (msg debug "merge nested FOREACH-FRAME loop")
            (reduce-vars (vug-function-inputs ,obj) ,initialize-body-p ,info))
           (t (reduce-vars
               ;; Skip the variables of the loop (useless and it's avoid
               ;; the increment of VUG-VARIABLE-REF-COUNT slot).
               (cdddr (vug-function-inputs ,obj)) ,initialize-body-p ,info)))))

(defun update-foreach-frame-loop-vars (replaced-vars)
  (dolist (vars replaced-vars)
    (mapc (lambda (var)
            (let ((count (vug-variable-ref-count var)))
              (when (plusp count)
                (incf (vug-variable-ref-count (vug-variable-replacement var))
                      count))))
          vars)))

(defun reduce-vug-variables (obj)
  (let ((floop-replaced-vars))
    (labels ((reduce-vars (x initialize-body-p floop-info)
               (cond
                 ((vug-variable-p x)
                  (incf (vug-variable-ref-count x))
                  (multiple-value-bind (cached cached-p)
                      (vug-variable-replacement x)
                    (declare (ignore cached))
                    (let ((value (vug-variable-value x)))
                      (cond
                        ((and (reducible-vug-variable-p x)
                              (constant-vug-variable-value-p value))
                         (unless cached-p
                           (when (vug-variable-p value)
                             (incf (vug-variable-ref-count value)))
                           (replace-vug-variable x value)
                           (msg-debug-delete-variable x "init-time")))
                        ((vug-variable-performance-time-p x)
                         (when (vug-variable-check-value-p x)
                           (setf (vug-variable-check-value-p x) nil)
                           (reduce-vars value initialize-body-p floop-info)))
                        (t
                         (when (vug-variable-check-value-p x)
                           (setf (vug-variable-check-value-p x) nil)
                           (reduce-vars value initialize-body-p floop-info))
                         (when (and (not cached-p)
                                    (no-performance-time-p value)
                                    (not (vug-variable-to-preserve-p x))
                                    (not (vug-variable-with-init-time-setter-p x)))
                           (multiple-value-bind (cached cached-p)
                               (vug-variable-replacement value)
                             (when (not cached-p)
                               (incf (vug-variable-ref-count value)))
                             (replace-vug-variable x (if cached-p cached value) t)
                             (msg-debug-delete-variable x "init-time"))))))))
                 ((vug-function-p x)
                  (let ((fname (vug-function-name x)))
                    (cond
                      ((eq fname 'filter-foreach-frame-form)
                       (update-foreach-frame-loop x reduce-vars
                                                  initialize-body-p))
                      ((member fname '(audio-in audio-out))
                       (when floop-info (update-audio-io x floop-info))
                       (reduce-vars (vug-function-inputs x)
                                    initialize-body-p floop-info))
                      ((eq fname 'now)
                       (when floop-info (update-now-function x floop-info))
                       (reduce-vars (vug-function-inputs x)
                                    initialize-body-p floop-info))
                      ((member fname '(set-local-io-pointer set-local-now))
                       ;; Avoid to increment VUG-VARIABLE-REF-COUNT.
                       nil)
                      ((eq fname 'foreach-frame-loop)
                       (reduce-foreach-frame-loop x floop-info
                                                  floop-replaced-vars
                                                  initialize-body-p))
                      ((eq fname 'get-pointer)
                       (let ((obj (car (vug-function-inputs x))))
                         (preserve-pointer-to-foreign-variable obj)))
                      (t (when (and initialize-body-p (setter-form-p fname))
                           ;; Preserve the init-time variables updated
                           ;; inside the body of INITIALIZE
                           (loop for binding on (vug-function-inputs x)
                                             by #'cddr
                                 for obj = (car binding)
                                 when (and (vug-variable-p obj)
                                           (vug-variable-init-time-p obj)
                                           (not (performance-time-p obj)))
                                 do (pushnew obj (vug-variables-init-time-setter
                                                   *vug-variables*))
                                    (when (vug-variable-deleted-p obj)
                                      (undelete-vug-variable obj)
                                      (msg debug "undelete ~A within setter form"
                                           obj))))
                         (reduce-vars (vug-function-inputs x)
                                      (or initialize-body-p
                                          (eq fname 'initialize))
                                      floop-info)))))
                 ((consp x)
                  (reduce-vars (car x) initialize-body-p floop-info)
                  (reduce-vars (cdr x) initialize-body-p floop-info)))))
      (update-variables-init-time-setter)
      (reduce-vars obj nil nil)
      (update-foreach-frame-loop-vars floop-replaced-vars)
      obj)))

(defun debug-deleted-variables ()
  (msg debug "deleted ~D unused variables~%~4T~A"
       (hash-table-count #1=(vug-variables-deleted *vug-variables*))
       (loop for var being the hash-keys in #1#
             collect (vug-object-name var))))

(defmacro vug-block (&body body)
  `(mark-vug-block
     (update-vug-variables
       (fix-sequence-of-forms
         (remove-wrapped-parens
           (remove-lisp-declaration
             (list ,@(let ((*inlined-ugens* nil))
                       (parse-vug-def body)))))))))

(declaim (inline fix-sequence-of-forms))
(defun fix-sequence-of-forms (obj)
  (if (and (consp obj) (cdr obj))
      (make-vug-function :name 'progn :inputs obj)
      obj))

(declaim (inline mark-vug-block))
(defun mark-vug-block (obj)
  (declare (type (or vug-object null)))
  (when obj
    (setf (vug-object-block-p obj) t)
    obj))

(defmacro with-coerce-arguments (bindings &body body)
  `(let ,(mapcar (lambda (x)
                   (if (consp x)
                       `(,(car x) (coerce-number ,(car x) ',(cadr x)))
                       `(,x (coerce-number ,x 'sample)))) ; default
                 bindings)
     ,@body))

(defmacro with-argument-bindings ((args types &optional vug-input-p)
                                  &body body)
  `(with ,(mapcar (lambda (x) `(,x ,(if vug-input-p `(vug-input ,x) x)))
                  args)
     ,@(mapcar (lambda (arg type) `(declare (type ,type ,arg)))
               args types)
     ,@body))

(declaim (inline arg-names-and-types))
(defun arg-names-and-types (lambda-list)
  (let ((names) (types))
    (dolist (arg lambda-list)
      (cond ((consp arg)
             (push (car arg) names)
             (push (cadr arg) types))
            (t (push arg names)
               (push 'sample types))))
    (values (nreverse names) (nreverse types))))

(defun add-vug (name args arg-types callback &optional macro-p)
  (let ((obj (make-vug :name name :callback callback :args args
                       :arg-types arg-types :macro-p macro-p)))
    (when (ugen name)
      (destroy-ugen name)
      (nrt-msg debug "destroy UGEN ~A" name))
    (setf (gethash name *vugs*) obj)))

(defmacro define-vug (name lambda-list &body body)
  (if (dsp name)
      (msg error "~A was defined to be a DSP." name)
      (with-gensyms (fn init)
        (multiple-value-bind (args types) (arg-names-and-types lambda-list)
          (multiple-value-bind (doc config vug-body) (extract-vug-config body)
            `(let ((,fn (lambda ,args ,doc
                          (flet ((,fn ,args
                                   (with-coerce-arguments ,lambda-list
                                     (vug-block
                                       (with-argument-bindings (,args ,types t)
                                         ,@vug-body)))))
                            (let ((,init (getf ,config :pre-hook)))
                              (when ,init (funcall ,init))
                              (,fn ,@args))))))
               (setf (symbol-function ',name) ,fn)
               (add-vug ',name ',args ',types ,fn)))))))

(defmacro define-vug-macro (name lambda-list &body body)
  (if (dsp name)
      (msg error "~A was defined to be a DSP." name)
      `(progn
         (defmacro ,name ,lambda-list ,@body)
         (add-vug ',name ',lambda-list nil (macro-function ',name) t))))

(defun rename-vug (old-name new-name)
  (declare (type symbol old-name new-name))
  (if (dsp new-name)
      (msg error "~A was defined to be a DSP." new-name)
      (let ((vug (vug old-name)))
        (cond (vug
               (remhash old-name *vugs*)
               (setf (gethash new-name *vugs*) vug)
               (setf (vug-name vug) new-name)
               (if (macro-function old-name)
                   (setf (macro-function new-name) (macro-function old-name))
                   (setf (symbol-function new-name) (symbol-function old-name)))
               (fmakunbound old-name)
               new-name)
              (t (msg error "~A is not a legal VUG name." old-name))))))

;;; No panic if we accidentally redefine a function related to a VUG.
(defun fix-vug (name)
  (let ((vug (vug name)))
    (when vug
      (cond ((vug-macro-p vug)
             (when (not (eq (vug-callback vug) (macro-function name)))
               (setf (macro-function name) (vug-callback vug))
               t))
            ((and (not (ugen name))
                  (or (not (fboundp name))
                      (not (eq (vug-callback vug) (symbol-function name)))))
             (setf (symbol-function name) (vug-callback vug))
             t)))))

(declaim (inline extract-vug-config))
(defun extract-vug-config (code)
  (declare (type list code))
  (let ((doc (when (stringp (car code))
               (car code))))
    (do ((l (if doc (cdr code) code) (cddr l))
         (acc nil))
        ((or (null l) (not (keywordp (car l))))
         (values doc (when acc `(list ,@acc)) l))
      (push (second l) acc)
      (push (first l) acc))))

(declaim (inline destroy-vug))
(defun destroy-vug (name)
  (when (vug name)
    (remhash name *vugs*)
    (fmakunbound name)
    (values)))

(defun destroy-ugen (name)
  (when (ugen name)
    (remhash name *ugens*)
    (let ((vug (vug name)))
      (when vug
        (setf (symbol-function name) (vug-callback vug)))))
  (values))

(declaim (inline all-vug-names))
(defun all-vug-names ()
  (sort (loop for name being the hash-keys in *vugs*
              when (find-symbol (symbol-name name))
              collect name)
        #'string-lessp :key #'symbol-name))

(defmacro with (bindings &body body)
  `(let* ,bindings ,@body))

(defmacro %with-samples (bindings &body body)
  `(with ,(mapcar (lambda (x)
                    (if (consp x)
                        (let ((value (cadr x)))
                          `(,(car x)
                            ,(if (and (numberp value)
                                      (not (typep value 'sample)))
                                 (force-sample-format value)
                                 value)))
                        `(,x ,+sample-zero+)))
                  bindings)
     ,@(when bindings
         `((declare (type sample
                          ,@(mapcar (lambda (x) (if (consp x) (car x) x))
                                    bindings)))))
     ,@body))

;;; Used only inside the definition of a VUG-MACRO to specify the
;;; inputs of the VUG.
;;; BINDINGS is a list ((vug-varname1 value1) (vug-varname2 value2) ...)
(defmacro with-vug-inputs (bindings &body body)
  `(with ,(mapcar (lambda (x) `(,(car x) (vug-input ,(cadr x))))
                  bindings)
     ,@body))
