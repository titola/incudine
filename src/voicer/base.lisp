;;; Copyright (c) 2013-2025 Tito Latini
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

(in-package :incudine.voicer)

(defstruct (node (:copier nil))
  value
  tag
  (to-release nil :type boolean)
  (next nil :type (or node null))
  (prev nil :type (or node null)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream)
    (format stream "VOICER-NODE ~A" (node-tag obj))))

(define-constant +node-pool-size+ incudine.util:*max-number-of-nodes*)

(define-constant +node-pool-grow+ (floor (* +node-pool-size+ 0.1)))

(define-constant +voicer-pool-size+ 1000)

(define-constant +voicer-pool-grow+ (floor (* +voicer-pool-size+ 0.1)))

(incudine.util::defglobal *voicer-pool-spinlock* (make-spinlock "VOICER-POOL"))
(declaim (type spinlock *voicer-pool-spinlock*))

(defun expand-voicer-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta nil))

(defun expand-node-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta (make-node)))

(defvar *node-pool*
  (make-cons-pool
    :data (loop repeat +node-pool-size+ collect (make-node))
    :size +node-pool-size+
    :expand-function #'expand-node-pool
    :grow +node-pool-grow+))
(declaim (type cons-pool *node-pool*))

(defvar *cons-pool*
  (make-cons-pool
    :data (make-list +voicer-pool-size+)
    :size +voicer-pool-size+
    :expand-function #'expand-voicer-pool
    :grow +voicer-pool-grow+))
(declaim (type cons-pool *cons-pool*))

(defstruct (voicer (:constructor %make-voicer)
                   (:copier nil))
  "Voicer type."
  (node-pool nil :type cons-pool)
  (generic-pool nil :type cons-pool)
  (objects nil :type list)
  (object-hash (make-hash-table) :type hash-table)
  (polyphony 1 :type non-negative-fixnum)
  (available-nodes 0 :type non-negative-fixnum)
  (count 0 :type non-negative-fixnum)
  (spinlock (make-spinlock "Voicer") :type spinlock)
  (trigger-function #'identity :type function)
  (release-function #'release-function-default :type function)
  (object-free-function #'incudine:free :type function)
  (steal-function nil :type (or function null))
  (arguments (make-hash-table :test 'equal) :type hash-table)
  (argument-maps (make-hash-table) :type hash-table))

(defun make-voicer (polyphony &optional steal-function)
  (with-spinlock-held (*voicer-pool-spinlock*)
    (let* (;; Pessimistic approssimation:
           ;;   1 object + 2 cons cells for every tag
           (gpool-size (* polyphony 3))
           (obj (%make-voicer
                  :polyphony polyphony
                  :node-pool (make-cons-pool
                               :data (cons-pool-pop-list *node-pool* polyphony)
                               :size polyphony
                               :expand-function #'expand-local-node-pool
                               :grow 1)
                  :generic-pool (make-cons-pool
                                  :data (cons-pool-pop-list *cons-pool* gpool-size)
                                  :size gpool-size
                                  :expand-function #'expand-local-pool
                                  :grow 12)
                  :objects (cons-pool-pop-cons *cons-pool*)
                  :available-nodes polyphony)))
      (when steal-function
        (setf (voicer-steal-function obj) steal-function))
      obj)))

(declaim (inline empty-p))
(defun empty-p (voicer)
  "Return T if all the voices of the given VOICER are available."
  (declare (type voicer voicer))
  (zerop (voicer-count voicer)))

(declaim (inline full-p))
(defun full-p (voicer)
  "Return T if all the voices of the given VOICER are allocated."
  (declare (type voicer voicer))
  (>= (voicer-count voicer) (voicer-polyphony voicer)))

(defun release-function-default (voicer tag object-free-p)
  (declare (type voicer voicer) (type boolean object-free-p))
  (if (empty-p voicer)
      voicer
      (let* ((object-entries (gethash tag (voicer-object-hash voicer)))
             (object-tlist (car object-entries))
             (pending-tlist (cdr object-entries))
             (pool (voicer-generic-pool voicer)))
        (declare (type cons object-entries object-tlist pending-tlist)
                 #.*standard-optimize-settings*)
        (if object-free-p
            (let ((obj (tlist-remove-left object-tlist pool)))
              (when obj
                (tlist-add-right pending-tlist obj pool)
                (cond ((node-value obj)
                       (reduce-warnings
                         (funcall (voicer-object-free-function voicer)
                                  (node-value obj)))
                       (setf (node-value obj) nil))
                      (t (setf (node-to-release obj) t)))))
            (remove-node voicer (tlist-remove-left
                                  (if (tlist-empty-p pending-tlist)
                                      object-tlist
                                      pending-tlist)
                                  pool))))))

(defmacro with-lock ((voicer) &body body)
  `(with-spinlock-held ((voicer-spinlock ,voicer)) ,@body))

(defmacro without-rt-lock ((voicer &optional thread-type) &body body)
  (with-gensyms (func)
    `(let ((,func (lambda () (with-lock (,voicer) ,@body))))
       (if (incudine.util:rt-thread-p)
           (,(if (eq thread-type :fast-nrt)
                 'incudine:fast-nrt-funcall
                 'incudine:nrt-funcall)
            ,func)
           (funcall ,func)))))

(declaim (inline polyphony))
(defun polyphony (voicer)
  "Return the maximum number of voices allocable by the given VOICER.
Setfable."
  (declare (type voicer voicer))
  (voicer-polyphony voicer))

(defun set-polyphony (voicer value)
  (declare (type voicer voicer) (type non-negative-fixnum value))
  (without-rt-lock (voicer)
    (when (> value (voicer-available-nodes voicer))
      (let ((delta (- value (voicer-available-nodes voicer)))
            (node-pool (voicer-node-pool voicer))
            (generic-pool (voicer-generic-pool voicer)))
        (funcall (incudine.util::cons-pool-expand-function node-pool)
                 node-pool delta)
        (funcall (incudine.util::cons-pool-expand-function generic-pool)
                 generic-pool (* delta 3))
        (setf (voicer-available-nodes voicer) value)))
    (setf (voicer-polyphony voicer) value)))

(defsetf polyphony set-polyphony)

(defmethod print-object ((obj voicer) stream)
  (print-unreadable-object (obj stream)
    (format stream "VOICER :POLYPHONY ~D :COUNT ~D"
            (polyphony obj) (voicer-count obj))))

(defmethod incudine:free ((obj voicer))
  (let ((npool-size (cons-pool-size (voicer-node-pool obj)))
        (gpool-size (cons-pool-size (voicer-generic-pool obj)))
        (objects (voicer-objects obj)))
    (with-spinlock-held (*voicer-pool-spinlock*)
      ;; Retrieve the cons cells of the local pools.
      (when (plusp npool-size)
        (cons-pool-push-list
          *node-pool*
          (cons-pool-pop-list (voicer-node-pool obj) npool-size)))
      (when (plusp gpool-size)
        (let ((gpool-data (cons-pool-pop-list
                            (voicer-generic-pool obj) gpool-size)))
          (loop for l on gpool-data do (setf (car l) nil))
          (cons-pool-push-list *cons-pool* gpool-data)))
      ;; Retrieve the cons cell used for the first and the last object.
      (when (consp objects)
        (setf (car objects) nil
              (cdr objects) nil)
        (cons-pool-push-cons *cons-pool* objects)
        (setf (voicer-objects obj) nil))
      ;; Free the hash-table of the objects and retrieve the cons cells.
      (alexandria:maphash-values
        (lambda (v)
          (cons-pool-push-list *cons-pool* (car v))
          (setf (car v) nil)
          (cons-pool-push-list *cons-pool* v))
        (voicer-object-hash obj))
      (clrhash (voicer-object-hash obj))
      (clrhash (voicer-arguments obj))
      (clrhash (voicer-argument-maps obj))
      (setf (voicer-polyphony obj) 0
            (voicer-available-nodes obj) 0)
      (values))))

(declaim (inline first-node))
(defun first-node (voicer)
  (declare (type voicer voicer))
  (car (voicer-objects voicer)))

(declaim (inline set-first-node))
(defun set-first-node (voicer node)
  (declare (type voicer voicer))
  (setf (car (voicer-objects voicer)) node))

(defsetf first-node set-first-node)

(declaim (inline last-node))
(defun last-node (voicer)
  (declare (type voicer voicer))
  (cdr (voicer-objects voicer)))

(declaim (inline set-last-node))
(defun set-last-node (voicer node)
  (declare (type voicer voicer))
  (setf (cdr (voicer-objects voicer)) node))

(defsetf last-node set-last-node)

(declaim (inline counter-inc))
(defun counter-inc (voicer)
  (declare (type voicer voicer))
  (incf (voicer-count voicer)))

(declaim (inline counter-dec))
(defun counter-dec (voicer)
  (declare (type voicer voicer))
  (if (plusp #1=(voicer-count voicer))
      (decf (the positive-fixnum #1#))
      0))

(defun free-voice-node (voicer node position)
  (declare (type voicer voicer) (type (or node null) node)
           (type (member :first :last) position))
  (when node
    (let* ((object-entries (gethash (node-tag node) (voicer-object-hash voicer)))
           (object-tlist (car object-entries))
           (pending-tlist (cdr object-entries))
           (pool (voicer-generic-pool voicer)))
      (if (or (eq position :first)
              (eq node (incudine.util:tlist-left object-tlist)))
          (tlist-remove-left object-tlist pool)
          (setf (cdr object-tlist) (last (car object-tlist) 2)
                (cddr object-tlist) nil))
      (tlist-add-right pending-tlist node pool)
      (setf (node-to-release node) t)
      (funcall (voicer-object-free-function voicer) (node-value node))
      (setf (node-value node) nil)))
  voicer)

(defun find-live-node (voicer first-func next-func)
  (labels ((rec (node)
             (declare (type (or node null) node))
             (when node
               (if (node-to-release node)
                   (rec (funcall next-func node))
                   node))))
    (rec (funcall first-func voicer))))

(defun steal-first-voice (voicer)
  (free-voice-node
    voicer (find-live-node voicer #'first-node #'node-next) :first))

(defun steal-last-voice (voicer)
  (free-voice-node
    voicer (find-live-node voicer #'last-node #'node-prev) :last))

(defun select-steal-function (mode)
  (case mode
    (:first #'steal-first-voice)
    (:last #'steal-last-voice)))

(defun steal-voice-mode (voicer)
  "Voice to steal when all the voices of the VOICER are allocated.
Should be one of :FIRST, :LAST or NIL. Setfable."
  (let ((func (voicer-steal-function voicer)))
    (and func (if (eq func #'steal-last-voice) :last :first))))

(defun set-steal-voice-mode (voicer mode)
  (declare (type voicer voicer) (type (member :first :last nil) mode))
  (without-rt-lock (voicer)
    (setf (voicer-steal-function voicer) (select-steal-function mode))
    mode))

(defsetf steal-voice-mode set-steal-voice-mode)

(defun expand-local-node-pool (pool &optional (delta 0))
  (let* ((grow (if (plusp delta)
                   delta
                   (incudine.util::cons-pool-grow pool)))
         (lst (with-spinlock-held (*voicer-pool-spinlock*)
                (cons-pool-pop-list *node-pool* grow))))
    (cons-pool-push-list pool lst)
    pool))

(defun expand-local-pool (pool &optional (delta 0))
  (let* ((grow (if (plusp delta)
                   delta
                   (incudine.util::cons-pool-grow pool)))
         (lst (with-spinlock-held (*voicer-pool-spinlock*)
                (cons-pool-pop-list *cons-pool* grow))))
    (cons-pool-push-list pool lst)
    pool))

(defun node-pool-pop (voicer)
  #+(or cmu sbcl) (declare (values node))
  (let* ((entry (cons-pool-pop-cons (voicer-node-pool voicer)))
         (value (car entry)))
    (cons-pool-push-cons (voicer-generic-pool voicer) entry)
    value))

(defun node-pool-push (voicer obj)
  (declare (type node obj))
  (let ((entry (cons-pool-pop-cons (voicer-generic-pool voicer))))
    (setf (car entry) obj)
    (cons-pool-push-cons (voicer-node-pool voicer) entry)))

(defun add-node (voicer tag new)
  (declare #.*standard-optimize-settings* (type voicer voicer))
  (let ((last (last-node voicer)))
    (declare (type (or node null) last))
    (setf (last-node voicer) new)
    (if (empty-p voicer)
        (setf (first-node voicer) new)
        (setf (node-next last) new))
    (let ((object-tlist (car (gethash tag (voicer-object-hash voicer))))
          (pool (voicer-generic-pool voicer)))
      (declare (type list object-tlist))
      (if object-tlist
          (tlist-add-right object-tlist new pool)
          (let ((object-tlist (make-tlist pool))
                (entry (cons-pool-pop-cons pool)))
            (setf (car entry) object-tlist
                  ;; There is a pool of nodes, so it is necessary to
                  ;; use a separate pending-tlist for the objects in
                  ;; release phase, otherwise these nodes are possibly
                  ;; freed and reused with unwanted side effects.
                  (cdr entry) (make-tlist pool))
            (tlist-add-left object-tlist new pool)
            (setf (gethash tag (voicer-object-hash voicer)) entry)))
      (counter-inc voicer)
      new)))

(defun remove-node (voicer obj)
  (declare (type voicer voicer) (type node obj)
           #.*standard-optimize-settings*)
  (if (node-prev obj)
      (setf (node-next (node-prev obj)) (node-next obj))
      (setf (first-node voicer) (node-next obj)))
  (if (node-next obj)
      (setf (node-prev (node-next obj)) (node-prev obj))
      (setf (last-node voicer) (node-prev obj)))
  (node-pool-push voicer obj)
  (counter-dec voicer)
  voicer)

(declaim (inline set-node))
(defun set-node (node value tag prev next)
  (declare (type node node) (type (or node null) prev next))
  (setf (node-value node) value
        (node-to-release node) nil
        (node-tag node) tag
        (node-prev node) prev
        (node-next node) next))

(defun unsafe-trigger (voicer tag)
  (declare (type voicer voicer))
  (incudine-optimize
    (when (full-p voicer)
      (if (voicer-steal-function voicer)
          (funcall (the function (voicer-steal-function voicer)) voicer)
          (return-from unsafe-trigger nil)))
    (loop for fn being the hash-values in (voicer-argument-maps voicer)
          do (funcall (the function (car fn))))
    (let ((new-node (node-pool-pop voicer)))
      (set-node new-node nil tag
                (unless (eq new-node (last-node voicer))
                  (last-node voicer))
                nil)
      (funcall (voicer-trigger-function voicer) voicer tag new-node)
      (add-node voicer tag new-node))))

(defun trigger (voicer tag)
  "Allocate a voice of the given VOICER with identifier TAG."
  (declare (type voicer voicer))
  (without-rt-lock (voicer :fast-nrt) (unsafe-trigger voicer tag)))

(declaim (inline unsafe-release))
(defun unsafe-release (voicer tag &optional (object-free-p t))
  (incudine-optimize
    (funcall (voicer-release-function voicer) voicer tag object-free-p)))

(defun %release (voicer tag &optional (object-free-p t) free-function)
  (declare (type voicer voicer) (type boolean object-free-p))
  (without-rt-lock (voicer :fast-nrt)
    (unsafe-release voicer tag object-free-p)
    (when free-function (funcall free-function))))

(declaim (inline release))
(defun release (voicer tag)
  "Release a voice of the given VOICER with identifier TAG."
  (%release voicer tag))

(defmacro init-voicer-arguments (arguments hash)
  (with-gensyms (value)
    `(progn
       (clrhash ,hash)
       ,@(mapcar (lambda (arg)
                   `(setf (gethash ,(symbol-name arg) ,hash)
                          (cons (lambda (,value) (setf ,arg ,value))
                                (lambda () ,arg))))
                 arguments)
       (setf (gethash "%CONTROL-LIST%" ,hash)
             (cons nil (lambda () (list ,@arguments))))
       (setf (gethash "%CONTROL-NAMES%" ,hash)
             (cons nil (lambda () (copy-list ',arguments)))))))

(defun remove-unused-maps (mapping-hash args)
  (maphash (lambda (key value)
             (if (set-difference (cdr value) args)
                 (remhash key mapping-hash)))
           mapping-hash))

(defmacro %set-default-trigger-function (voicer func-name args)
  (with-gensyms (free-hook tag node v vnode pool)
    `(let ((,pool (voicer-generic-pool ,voicer)))
       (setf (voicer-trigger-function ,voicer)
             (lambda (,v ,tag ,vnode)
               (let ((,free-hook (cons-pool-pop-cons ,pool)))
                 (setf (car ,free-hook)
                       (lambda (,node)
                         (declare (ignore ,node))
                         (%release ,v ,tag nil
                                   (lambda ()
                                     (cons-pool-push-cons ,pool ,free-hook)))))
                 (,func-name ,@args
                    :free-hook ,free-hook
                    :action (lambda (,node)
                              (incudine:fast-nrt-funcall
                                (lambda ()
                                  (with-lock (,v)
                                    (cond ((node-to-release ,vnode)
                                           ;; Very short event.
                                           (incudine:fast-rt-funcall
                                             (lambda ()
                                               (funcall
                                                (voicer-object-free-function ,v)
                                                (incudine::node-id ,node)))))
                                          (t (setf (node-value ,vnode)
                                                   (incudine::node-id
                                                     ,node)))))))))))))))

(defun select-object-free-function (dsp-arguments)
  (if (member "GATE" dsp-arguments :key #'symbol-name :test #'string-equal)
      (lambda (id) (if id (incudine:set-control id :gate 0)))
      (lambda (id) (when id (incudine:free id)))))

(defmacro %create-voicer (old-voicer form &optional polyphony steal-function)
  (with-gensyms (voicer)
    (let* ((func-name (car form))
           (dsp-prop (gethash func-name incudine.vug::*dsps*)))
      (if dsp-prop
          (let ((dsp-args (incudine.vug::dsp-properties-arguments dsp-prop)))
            `(let (,@(mapcar (lambda (arg value) `(,arg ,value))
                             dsp-args (cdr form))
                   (,voicer ,(or old-voicer
                                 `(make-voicer ,polyphony ,steal-function))))
               (setf (voicer-object-free-function ,voicer)
                     (select-object-free-function ',dsp-args))
               (init-voicer-arguments ,dsp-args (voicer-arguments ,voicer))
               (%set-default-trigger-function ,voicer ,func-name ,dsp-args)
               ,@(when old-voicer
                   `((unless (null ',dsp-args)
                       (remove-unused-maps
                         (voicer-argument-maps ,voicer) ',dsp-args))))
               ,voicer))
          (incudine:incudine-error "Unknown DSP")))))

(defmacro create (polyphony form &key steal-voice)
  "Create and return a new VOICER structure usable for voice management.

POLYPHONY is the maximum number of allocable voices.

FORM is the template of the DSP related to the voicer:

    (dsp-function-name &rest dsp-function-arguments)

If STEAL-VOICE is :FIRST or :LAST, release the first or the last
allocated voice when there aren't available voices.

Example:

    (defvar *voi* (voicer:create 20 (superbass 110 .2 1)))

    where 110, 0.2 and 1 are the default control parameters
    of the DSP instances."
  (declare (type positive-fixnum polyphony) (type cons form)
           (type (member :first :last nil) steal-voice))
  `(%create-voicer nil ,form ,polyphony (select-steal-function ,steal-voice)))

(defmacro update (voicer form)
  "Update the VOICER instance with a new template FORM:

        (dsp-function-name &rest dsp-function-arguments)"
  `(%create-voicer ,voicer ,form))

(declaim (inline unsafe-control-value))
(defun unsafe-control-value (voicer control-name)
  (declare (type voicer voicer) (type symbol control-name))
  (let ((entry (gethash (symbol-name control-name) (voicer-arguments voicer))))
    (declare #.*standard-optimize-settings*)
    (if entry
        (values (funcall (the function (cdr entry))) t)
        (values nil nil))))

(declaim (inline unsafe-set-control))
(defun unsafe-set-control (voicer control-name value)
  (declare (type voicer voicer) (type symbol control-name))
  (let ((entry (gethash (symbol-name control-name) (voicer-arguments voicer))))
    (declare #.*standard-optimize-settings*)
    (when entry (funcall (the function (car entry)) value))))

(defsetf unsafe-control-value unsafe-set-control)

(defun control-value (voicer control-name)
  "Return the value of the control parameter CONTROL-NAME related to
the given VOICER. Setfable."
  (declare (type voicer voicer) (type symbol control-name))
  (without-rt-lock (voicer) (unsafe-control-value voicer control-name)))

(defun set-control (voicer control-name value)
  (declare (type voicer voicer) (type symbol control-name))
  (without-rt-lock (voicer :fast-nrt)
    (unsafe-set-control voicer control-name value)))

(defsetf control-value set-control)

(defun control-list (voicer)
  "Return the list of the values of the control parameters related to
the given VOICER."
  (control-value voicer '%control-list%))

(defun control-names (voicer)
  "Return the list of control names related to the given VOICER."
  (control-value voicer '%control-names%))

(defun %unsafe-set-controls (voicer arguments)
  (do ((pl arguments (cdr pl)))
      ((null pl))
    (declare (type list pl))
    (let ((control-name (car pl)))
      (declare (type symbol control-name))
      (setf pl (cdr pl))
      (unsafe-set-control voicer control-name (car pl)))))

(declaim (inline unsafe-set-controls))
(defun unsafe-set-controls (voicer &rest arguments)
  (%unsafe-set-controls voicer arguments))

(defun set-controls (voicer &rest arguments)
  "Set the control parameters of the given VOICER.

ARGUMENTS is an even number of arguments that are alternating control
parameter names and values."
  (without-rt-lock (voicer :fast-nrt) (%unsafe-set-controls voicer arguments)))

(defmacro with-controls (controls voicer &body body)
  `(symbol-macrolet
       ,(mapcar (lambda (arg-entry)
                  (let ((var-name (if (symbolp arg-entry)
                                      arg-entry
                                      (car arg-entry)))
                        (arg-name (if (symbolp arg-entry)
                                      arg-entry
                                      (cadr arg-entry))))
                    `(,var-name (unsafe-control-value ,voicer ',arg-name))))
                controls)
     ,@body))

(defmacro define-map (name voicer controls &body function-body)
  "Define a mapping function named NAME to call immediately after
VOICER:TRIGGER, before the new event.

CONTROLS is the list of the control parameters used within
FUNCTION-BODY.

Example:

    (voicer:define-map map-test my-voicer (freq amp)
      (setf freq (+ 100 (random 2000))
            amp (random (if (> freq 800) .1 .3))))"
  (with-gensyms (v)
    `(let ((,v ,voicer))
       (without-rt-lock (,v)
         (with-controls ,controls ,v
           (setf (gethash ',name (voicer-argument-maps ,v))
                 (cons (lambda () ,@function-body (values)) ',controls))))
       ,v)))

(defun remove-map (voicer name)
  "Remove the mapping function definition named NAME related to the VOICER."
  (without-rt-lock (voicer)
    (remhash name (voicer-argument-maps voicer))))

(defun remove-all-maps (voicer)
  "Remove all the mapping function definitions related to the VOICER."
  (without-rt-lock (voicer)
    (clrhash (voicer-argument-maps voicer))))

(defun unsafe-mapvoicer (function voicer)
  (declare (type function function) (type voicer voicer))
  (labels ((rec (obj)
             (when obj
               (funcall function obj)
               (rec (node-next obj)))))
    (declare #.*standard-optimize-settings*)
    (rec (first-node voicer))
    voicer))

(defun panic (voicer)
  "Force the release of the voices allocated by the given VOICER."
  (without-rt-lock (voicer)
    (unsafe-mapvoicer
      (lambda (obj)
        (funcall (voicer-object-free-function voicer) (node-value obj)))
      voicer)))
