;;; Copyright (c) 2013-2017 Tito Latini
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
  (format stream "#<VOICER-NODE ~A>" (node-tag obj)))

(define-constant +node-pool-size+ incudine.util:*max-number-of-nodes*)

(define-constant +node-pool-grow+ (floor (* +node-pool-size+ 0.1)))

(define-constant +voicer-pool-size+ 1000)

(define-constant +voicer-pool-grow+ (floor (* +voicer-pool-size+ 0.1)))

(defvar *voicer-pool-spinlock* (make-spinlock "VOICER-POOL"))
(declaim (type spinlock *voicer-pool-spinlock*))

(declaim (inline expand-voicer-pool))
(defun expand-voicer-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta nil))

(declaim (inline expand-node-pool))
(defun expand-node-pool (pool &optional (delta 1))
  (expand-cons-pool pool delta (make-node)))

(defvar *node-pool*
  (make-cons-pool :data (loop repeat +node-pool-size+ collect (make-node))
                  :size +node-pool-size+
                  :expand-func #'expand-node-pool
                  :grow +node-pool-grow+))
(declaim (type cons-pool *node-pool*))

(defvar *voicer-pool*
  (make-cons-pool :data (make-list +voicer-pool-size+)
                  :size +voicer-pool-size+
                  :expand-func #'expand-voicer-pool
                  :grow +voicer-pool-grow+))
(declaim (type cons-pool *voicer-pool*))

(defstruct (voicer (:constructor %make-voicer)
                   (:copier nil))
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
  (arguments (make-hash-table) :type hash-table)
  (argument-maps (make-hash-table) :type hash-table))

(defun make-voicer (polyphony)
  (with-spinlock-held (*voicer-pool-spinlock*)
    (let* ((npool (cons-pool-pop-list *node-pool* polyphony))
           ;; Pessimistic approssimation:
           ;;   1 object + 2 cons cells for every tag
           (gpool-size (* polyphony 3))
           (gpool (cons-pool-pop-list *voicer-pool* gpool-size)))
      (%make-voicer :polyphony polyphony
                    :node-pool (make-cons-pool
                                :data npool
                                :size polyphony
                                :expand-func #'expand-local-node-pool
                                :grow 1)
                    :generic-pool (make-cons-pool
                                   :data gpool
                                   :size gpool-size
                                   :expand-func #'expand-local-pool
                                   :grow 12)
                    :objects (cons-pool-pop-cons *voicer-pool*)
                    :available-nodes polyphony))))

(defun release-function-default (voicer tag object-free-p)
  (declare #.*standard-optimize-settings*
           (type voicer voicer) (type boolean object-free-p))
  (let* ((object-entries (gethash tag (voicer-object-hash voicer)))
         (object-tlist (car object-entries))
         (pending-tlist (cdr object-entries))
         (pool (voicer-generic-pool voicer)))
    (declare (type cons object-entries object-tlist pending-tlist))
    (cond ((and (tlist-empty-p object-tlist)
                (tlist-empty-p pending-tlist))
           voicer)
          (object-free-p
           (let ((obj (tlist-remove-left object-tlist pool)))
             (when obj
               (tlist-add-right pending-tlist obj pool)
               (cond ((node-value obj)
                      (reduce-warnings
                        (funcall (voicer-object-free-function voicer)
                                 (node-value obj)))
                      (setf (node-value obj) nil))
                     (t (setf (node-to-release obj) t))))))
          (t (remove-node voicer
                          (tlist-remove-left (if (tlist-empty-p pending-tlist)
                                                 object-tlist
                                                 pending-tlist)
                                             pool))))))

(defun unsafe-set-polyphony (voicer value)
  (declare (type voicer voicer) (type non-negative-fixnum value))
  (when (> value (voicer-available-nodes voicer))
    (let ((delta (- value (voicer-available-nodes voicer)))
          (node-pool (voicer-node-pool voicer))
          (generic-pool (voicer-generic-pool voicer)))
      (funcall (incudine.util::cons-pool-expand-func node-pool) node-pool delta)
      (funcall (incudine.util::cons-pool-expand-func generic-pool) generic-pool
               (* delta 3))
      (setf (voicer-available-nodes voicer) value)))
  (setf (voicer-polyphony voicer) value))

(defmacro with-safe-change ((voicer) &body body)
  `(with-spinlock-held ((voicer-spinlock ,voicer))
     ,@body))

(declaim (inline polyphony))
(defun polyphony (voicer)
  (declare (type voicer voicer))
  (voicer-polyphony voicer))

(declaim (inline set-polyphony))
(defun set-polyphony (voicer value)
  (with-safe-change (voicer) (unsafe-set-polyphony voicer value)))

(defsetf polyphony set-polyphony)

(defmethod print-object ((obj voicer) stream)
  (format stream "#<VOICER :POLYPHONY ~D :COUNT ~D>"
          (polyphony obj) (voicer-count obj)))

(defmethod incudine:free ((obj voicer))
  (let ((npool-size (cons-pool-size (voicer-node-pool obj)))
        (gpool-size (cons-pool-size (voicer-generic-pool obj)))
        (objects (voicer-objects obj)))
    (with-spinlock-held (*voicer-pool-spinlock*)
      ;; Retrieve the cons cells of the local pools
      (when (plusp npool-size)
        (cons-pool-push-list *node-pool*
          (cons-pool-pop-list (voicer-node-pool obj) npool-size)))
      (when (plusp gpool-size)
        (let ((gpool-data (cons-pool-pop-list (voicer-generic-pool obj)
                                              gpool-size)))
          (loop for l on gpool-data do (setf (car l) nil))
          (cons-pool-push-list *voicer-pool* gpool-data)))
      ;; Retrieve the cons cell used for the first and the last object
      (when (consp objects)
        (setf (car objects) nil
              (cdr objects) nil)
        (cons-pool-push-cons *voicer-pool* objects)
        (setf (voicer-objects obj) nil))
      ;; Free the hash-table of the objects and retrieve the cons cells
      (alexandria:maphash-values (lambda (v)
                                   (cons-pool-push-list *voicer-pool* (car v))
                                   (setf (car v) nil)
                                   (cons-pool-push-list *voicer-pool* v))
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

(declaim (inline empty-p))
(defun empty-p (voicer)
  (declare (type voicer voicer))
  (zerop (voicer-count voicer)))

(declaim (inline full-p))
(defun full-p (voicer)
  (declare (type voicer voicer))
  (>= (voicer-count voicer) (voicer-polyphony voicer)))

(declaim (inline steal-first-voice))
(defun steal-first-voice (voicer)
  (declare (type voicer voicer))
  (let ((first (car (voicer-objects voicer))))
    (when first
      (funcall (voicer-object-free-function voicer) (node-value first)))))

(declaim (inline steal-last-voice))
(defun steal-last-voice (voicer)
  (declare (type voicer voicer))
  (let ((last (cdr (voicer-objects voicer))))
    (when last
      (funcall (voicer-object-free-function voicer) (node-value last)))))

(declaim (inline expand-local-node-pool))
(defun expand-local-node-pool (pool &optional (delta 0))
  (let* ((grow (if (plusp delta)
                   delta
                   (incudine.util::cons-pool-grow pool)))
         (lst (with-spinlock-held (*voicer-pool-spinlock*)
                (cons-pool-pop-list *node-pool* grow))))
    (cons-pool-push-list pool lst)
    pool))

(declaim (inline expand-local-pool))
(defun expand-local-pool (pool &optional (delta 0))
  (let* ((grow (if (plusp delta)
                   delta
                   (incudine.util::cons-pool-grow pool)))
         (lst (with-spinlock-held (*voicer-pool-spinlock*)
                (cons-pool-pop-list *voicer-pool* grow))))
    (cons-pool-push-list pool lst)
    pool))

(declaim (inline node-pool-pop))
(defun node-pool-pop (voicer)
  #+(or cmu sbcl) (declare (values node))
  (let* ((entry (cons-pool-pop-cons (voicer-node-pool voicer)))
         (value (car entry)))
    (cons-pool-push-cons (voicer-generic-pool voicer) entry)
    value))

(declaim (inline node-pool-push))
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
    (let ((object-tlist (car #1=(gethash tag (voicer-object-hash voicer))))
          (pool (voicer-generic-pool voicer)))
      (declare (type list object-tlist))
      (if object-tlist
          (tlist-add-right object-tlist new pool)
          (let ((object-tlist (make-tlist pool))
                (entry (cons-pool-pop-cons pool)))
            (setf (car entry) object-tlist
                  (cdr entry) (make-tlist pool))
            (tlist-add-left object-tlist new pool)
            (setf #1# entry)))
      (counter-inc voicer)
      new)))

(defun remove-node (voicer obj)
  (declare #.*standard-optimize-settings*
           (type voicer voicer))
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
  (declare (type node node)
           (type (or node null) prev next))
  (setf (node-value node) value
        (node-to-release node) nil
        (node-tag node) tag
        (node-prev node) prev
        (node-next node) next))

(declaim (inline unsafe-trigger))
(defun unsafe-trigger (voicer tag)
  (declare #.*standard-optimize-settings* (type voicer voicer))
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
    (add-node voicer tag new-node)))

(declaim (inline trigger))
(defun trigger (voicer tag)
  (with-safe-change (voicer) (unsafe-trigger voicer tag)))

(declaim (inline unsafe-release))
(defun unsafe-release (voicer tag &optional (object-free-p t) free-function)
  (declare #.*standard-optimize-settings* (type voicer voicer)
           (type (or function null) free-function))
  (funcall (voicer-release-function voicer) voicer tag object-free-p)
  (when free-function (funcall free-function)))

(declaim (inline release))
(defun release (voicer tag &optional (object-free-p t) free-function)
  (with-safe-change (voicer)
    (unsafe-release voicer tag object-free-p free-function)))

(defmacro init-voicer-arguments (arguments hash)
  (with-gensyms (value)
    `(progn
       (clrhash ,hash)
       ,@(mapcar (lambda (arg)
                   `(setf (gethash ',arg ,hash)
                          (cons (lambda (,value) (setf ,arg ,value))
                                (lambda () ,arg))))
                 arguments))))

(declaim (inline remove-unused-maps))
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
                         (release ,voicer ,tag nil
                                  (lambda ()
                                    (cons-pool-push-cons ,pool ,free-hook)))))
                 (,func-name ,@args
                    :free-hook ,free-hook
                    :action (lambda (,node)
                              (incudine:fast-nrt-funcall
                                (lambda ()
                                  (with-safe-change (,voicer)
                                    (cond ((node-to-release ,vnode)
                                           ;; Very short event
                                           (incudine:fast-rt-funcall
                                             (lambda ()
                                               (funcall
                                                (voicer-object-free-function ,v)
                                                (incudine::node-id ,node)))))
                                          (t (setf (node-value ,vnode)
                                                   (incudine::node-id
                                                     ,node)))))))))))))))

(defmacro %create-voicer (old-voicer obj &optional polyphony)
  (with-gensyms (voicer)
    (let* ((func-name (car obj))
           (dsp-properties (gethash func-name incudine.vug::*dsps*)))
      (if dsp-properties
          `(let (,@(mapcar (lambda (arg value) `(,arg ,value))
                           #1=(incudine.vug::dsp-arguments dsp-properties)
                           (cdr obj))
                 (,voicer ,(or old-voicer `(make-voicer ,polyphony))))
             (setf (voicer-object-free-function ,voicer)
                   (if (member "GATE" ',#1# :key #'symbol-name
                               :test #'string-equal)
                       (lambda (id)
                         (when id
                           (incudine.util:rt-eval ()
                             (incudine:set-control id :gate 0))))
                       (lambda (id) (when id (incudine:free id)))))
             (init-voicer-arguments
               ,(incudine.vug::dsp-arguments dsp-properties)
               (voicer-arguments ,voicer))
             (%set-default-trigger-function ,voicer ,func-name ,#1#)
             ,@(if old-voicer `((unless (null ',#1#)
                                  (remove-unused-maps
                                   (voicer-argument-maps ,voicer) ',#1#))))
             ,voicer)
          (incudine:incudine-error "Unknown DSP")))))

(defmacro create (polyphony obj)
  `(%create-voicer nil ,obj ,polyphony))

(defmacro update (voicer obj)
  `(%create-voicer ,voicer ,obj))

(declaim (inline unsafe-control-value))
(defun unsafe-control-value (voicer control-name)
  (declare #.*standard-optimize-settings*
           (type voicer voicer) (type symbol control-name))
  (let ((entry (gethash control-name (voicer-arguments voicer))))
    (if entry
        (values (funcall (the function (cdr entry))) t)
        (values nil nil))))

(declaim (inline unsafe-set-control))
(defun unsafe-set-control (voicer control-name value)
  (declare #.*standard-optimize-settings*
           (type voicer voicer) (type symbol control-name))
  (let ((entry (gethash control-name (voicer-arguments voicer))))
    (when entry (funcall (the function (car entry)) value))))

(defsetf unsafe-control-value unsafe-set-control)

(declaim (inline control-value))
(defun control-value (voicer control-name)
  (with-safe-change (voicer)
    (unsafe-control-value voicer control-name)))

(declaim (inline set-control))
(defun set-control (voicer control-name value)
  (with-safe-change (voicer)
    (unsafe-set-control voicer control-name value)))

(defsetf control-value set-control)

(defun unsafe-get-controls (voicer)
  (declare (type voicer voicer))
  (let ((hash (voicer-arguments voicer)))
    (loop for key being the hash-keys in hash
          for value being the hash-values in hash
          collect key
          collect (funcall (the function (cdr value))))))

(declaim (inline get-controls))
(defun get-controls (voicer)
  (declare (type voicer voicer))
  (with-safe-change (voicer) (unsafe-get-controls voicer)))

(defmacro unsafe-set-controls (voicer &rest arguments)
  `(progn
     ,@(loop for l on arguments by #'cddr collect
            `(setf (unsafe-control-value ,voicer ',(ensure-symbol (car l)))
                   ,(cadr l)))))

(defmacro set-controls (voicer &rest arguments)
  `(with-safe-change (,voicer)
     (unsafe-set-controls ,voicer ,@arguments)))

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

(defmacro unsafe-define-map (name voicer controls &body function-body)
  `(with-controls ,controls ,voicer
     (setf (gethash ',name (voicer-argument-maps ,voicer))
           (cons (lambda () ,@function-body) ',controls))
     ,voicer))

(defmacro define-map (name voicer controls &body function-body)
  `(with-safe-change (,voicer)
     (unsafe-define-map ,name ,voicer ,controls ,@function-body (values))))

(declaim (inline unsafe-remove-map))
(defun unsafe-remove-map (voicer map-name)
  (declare (type voicer voicer) (type symbol map-name))
  (remhash map-name (voicer-argument-maps voicer)))

(declaim (inline remove-map))
(defun remove-map (voicer map-name)
  (with-safe-change (voicer)
    (unsafe-remove-map voicer map-name)))

(declaim (inline remove-all-maps))
(defun remove-all-maps (voicer)
  (with-safe-change (voicer)
    (clrhash (voicer-argument-maps voicer))))

(defun unsafe-mapvoicer (function voicer)
  (declare #.*standard-optimize-settings*
           (type function function) (type voicer voicer))
  (labels ((rec (obj)
             (funcall function obj)
             (when #1=(node-next obj) (rec #1#))))
    (let ((first (car (voicer-objects voicer))))
      (when first (rec first)))))

(defun mapvoicer (function voicer)
  (with-safe-change (voicer) (unsafe-mapvoicer function voicer)))

(defun unsafe-panic (voicer)
  (declare (type voicer voicer))
  (unsafe-mapvoicer (lambda (obj) (funcall (voicer-object-free-function voicer)
                                           (node-value obj)))
                    voicer)
  voicer)

(defun panic (voicer)
  (with-safe-change (voicer) (unsafe-panic voicer)))
