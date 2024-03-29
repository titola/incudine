;;; Copyright (c) 2013-2023 Tito Latini
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

(in-package :incudine.edf)

;;; Earliest Deadline First scheduling (EDF)

(defstruct (node (:constructor make-node ()) (:copier nil))
  "Heap node type."
  (time +sample-zero+ :type sample)
  ;; Two nodes have different order also if the time is the same.
  (order 0 :type non-negative-fixnum)
  (function #'corrupted-heap :type function)
  (args nil :type list))

(setf (documentation 'make-node 'function)
      "Create and return a new heap NODE structure.")

(define-constant +root-node+ 1
  :documentation "Integer identifier for the root node of the EDF heap.")

(define-constant +first-parent+ 2)

(defstruct (heap (:constructor %make-heap))
  "Heap type."
  (data (incudine:incudine-missing-arg "EDF heap data required.")
        :type simple-vector)
  (next-node +root-node+ :type positive-fixnum)
  (temp-node (make-node) :type node)
  (time-offset +sample-zero+ :type sample)
  (ordering-tag 0 :type non-negative-fixnum))

(defmethod print-object ((obj heap) stream)
  (print-unreadable-object (obj stream)
    (princ "EDF:HEAP" stream)))

(defun corrupted-heap ()
  (incudine:incudine-error "Corrupted EDF heap."))

(defvar *dummy-node* (make-node))
(declaim (type node *dummy-node*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *heap-size*
    (if (typep *rt-edf-heap-size* 'positive-fixnum)
        (if (power-of-two-p *rt-edf-heap-size*)
            *rt-edf-heap-size*
            (next-power-of-two *rt-edf-heap-size*))
        1024)
    "Max number of scheduled events. It is assumed to be a power of two.")
  (declaim (type non-negative-fixnum *heap-size*)))

(defun make-heap-data (size)
  (make-array size :element-type 'node
              :initial-contents (loop repeat size collect (make-node))))

(defun make-heap (&optional (size *heap-size*))
  "Create and return a new HEAP structure to schedule at maximum SIZE events.

SIZE defaults to *heap-size*."
  (declare (type positive-fixnum size))
  (let ((size (next-power-of-two size)))
    (%make-heap :data (make-heap-data size))))

(defvar *heap* (make-heap)
  "Default EDF heap.")
(declaim (type heap *heap*))

(defvar *rt-heap* *heap*)
(declaim (type heap *rt-heap*))

(declaim (inline rt-heap-p))
(defun rt-heap-p ()
  (eq *heap* *rt-heap*))

(declaim (inline heap-empty-p))
(defun heap-empty-p ()
  "Whether the EDF heap is empty."
  (= (heap-next-node *heap*) +root-node+))

(declaim (inline heap-count))
(defun heap-count ()
  "Return the number of entries in the EDF heap."
  (1- (heap-next-node *heap*)))

(declaim (inline heap-node))
(defun heap-node (index)
  (svref (heap-data *heap*) index))

(declaim (inline set-heap-node))
(defun set-heap-node (index node)
  (setf (svref (heap-data *heap*) index) node))

(defsetf heap-node set-heap-node)

(declaim (inline node-update))
(defun node-update (node time order function args)
  (setf (node-time node) time
        (node-order node) order
        (node-function node) function
        (node-args node) args)
  node)

(declaim (inline node-copy))
(defun node-copy (src dest)
  (node-update dest (node-time src) (node-order src) (node-function src)
               (node-args src)))

(declaim (inline node-move))
(defun node-move (src dest)
  (node-copy src dest)
  (node-update src +sample-zero+ 0 #'corrupted-heap nil)
  dest)

(defun node-time> (n0 n1)
  (or (> (node-time n0) (node-time n1))
      (and (= (node-time n0) (node-time n1))
           (> (node-order n0) (node-order n1)))))

(declaim (inline schedule-at))
(defun schedule-at (time function args)
  "Schedule FUNCTION to be called with the supplied ARGUMENTS at TIME samples."
  (declare (type sample time) (type function function)
           (type list args))
  (unless (>= (heap-next-node *heap*) *heap-size*)
    (let ((curr (heap-next-node *heap*))
          (t0 (sample time)))
      (declare #.*standard-optimize-settings*
               (type positive-fixnum curr) (type sample t0))
      (loop while (> curr +root-node+) do
           (let ((parent (ash curr -1)))
             (declare (type positive-fixnum parent))
             (cond ((< t0 (node-time (heap-node parent)))
                    (node-copy (heap-node parent) (heap-node curr))
                    (setf curr parent))
                   (t (return)))))
      (node-update (heap-node curr) t0 (heap-ordering-tag *heap*) function args)
      (setf (heap-ordering-tag *heap*)
            ;; Masking unnecessary in practice but safe.
            (logand (1+ (heap-ordering-tag *heap*))
                    #.(1- (ash 1 (1- incudine.util::n-fixnum-bits)))))
      (incf (heap-next-node *heap*)))))

(defun %at (time function args)
  (declare (type sample time) (type function function) (type list args))
  (if (or (null *rt-thread*) (rt-thread-p) (not (rt-heap-p)))
      (schedule-at time function args)
      (incudine:fast-nrt-funcall
        (lambda ()
          (incudine:fast-rt-funcall
            (lambda () (schedule-at time function args))))))
  (values))

(declaim (inline at))
(defun at (time function &rest arguments)
  "Schedule FUNCTION to be called with the supplied ARGUMENTS from the
real-time thread at TIME samples."
  (%at (sample time) function arguments))

;;; Anaphoric macro for AT.
(defmacro aat (time function &rest arguments)
  "Like AT, except bind the TIME to IT for the scope of the rest of
the ARGUMENTS.

Example:

    (defun simple-test (time)
      (set-controls 1 :freq (+ 100 (random 1000)))
      (aat (+ time #[1 beat]) #'simple-test it))"
  (let ((it (intern "IT")))
    `(let ((,it ,time))
       (at ,it ,function ,@arguments))))

(defun get-heap ()
  (declare #.*standard-optimize-settings*
           #+(or cmu sbcl) (values node))
  (cond ((> (heap-next-node *heap*) +root-node+)
         ;; node 0 used for the result
         (node-copy (heap-node +root-node+) (heap-node 0))
         (decf (heap-next-node *heap*))
         (node-move (heap-node (heap-next-node *heap*)) (heap-node +root-node+))
         (node-copy (heap-node +root-node+) (heap-temp-node *heap*))
         (let ((parent +root-node+)
               (curr +first-parent+))
           (declare (type positive-fixnum parent curr))
           (loop while (< curr (heap-next-node *heap*)) do
                (let ((sister (1+ curr)))
                  (when (and (< sister (heap-next-node *heap*))
                             (node-time> (heap-node curr) (heap-node sister)))
                    (setf curr sister))
                  (cond ((node-time> (heap-temp-node *heap*) (heap-node curr))
                         (node-copy (heap-node curr) (heap-node parent))
                         (setf parent curr curr (ash parent 1)))
                        (t (return)))))
           (node-copy (heap-temp-node *heap*) (heap-node parent))
           (when (= (heap-next-node *heap*) +root-node+)
             (setf (heap-ordering-tag *heap*) 0))
           (heap-node 0)))
        (t *dummy-node*)))

(declaim (inline got-node))
(defun got-node () (heap-node 0))

(declaim (inline current-got-time))
(defun current-got-time () (node-time (heap-node 0)))

(defmacro call-node-function (node)
  (with-gensyms (c n)
    `(handler-case
         (let ((,n ,node))
           (declare (type node ,n))
           (apply (node-function ,n) (node-args ,n)))
       (condition (,c)
         (incudine:nrt-funcall
           (lambda () (format *error-output* "ERROR: ~A~%"
                              (the condition ,c))))))))

(defmacro sched-loop ()
  "Earliest Deadline First scheduling loop to call the functions
scheduled at the current time."
  `(loop while (and (> (heap-next-node *heap*) ,+root-node+)
                    (>= (+ (incudine:now) ,(sample 0.5))
                        (node-time (heap-node ,+root-node+))))
         do (call-node-function (get-heap))))

(defun next-time (&optional function start-time)
  "Return the time in samples of the next scheduled FUNCTION, starting
its search at START-TIME (zero by default). Otherwise, return zero if
the searched event is not available.

If FUNCTION is NIL (default), return the time of the next scheduled event.

Example:

    ;; The bass line is synchronized with the next drum pattern.
    ;; Note: the time is more reliable from rt-thread.
    (rt-eval ()
      (at (incudine.edf:next-time #'drum-pattern) #'bass-line ...))"
  (declare (type (or function null) function)
           (type (or non-negative-real null) start-time))
  (let ((end (heap-next-node *heap*)))
    (declare (type positive-fixnum end))
    (labels ((next (i)
               (declare (type positive-fixnum i))
               (when (< i end)
                 (if (and (or (null function)
                              (eq function (node-function (heap-node i))))
                          (or (null start-time)
                              (>= (node-time (heap-node i)) start-time)))
                     (heap-node i)
                     (let ((left (ash i 1)))
                       (declare (positive-fixnum left))
                       (test (next left) (next (1+ left)))))))
             (test (left right)
               (declare (type (or node null) left right))
               (if (and (node-p left)
                        (node-p right))
                   (if (node-time> right left) left right)
                   (or left right))))
      (let ((obj (next +root-node+)))
        (declare (type (or node null) obj))
        (if obj (node-time obj) +sample-zero+)))))

(defun last-time ()
  "Return the time in samples of the last scheduled function to call."
  (declare #.*standard-optimize-settings* #.incudine.util:*reduce-warnings*)
  (labels ((rec (i t0)
             (declare (type non-negative-fixnum i) (type sample t0))
             (if (= i (heap-next-node *heap*))
                 t0
                 (rec (1+ i) (max t0 (node-time (heap-node i)))))))
    (let ((start (if (power-of-two-p (heap-next-node *heap*))
                     (ash (heap-next-node *heap*) -1)
                     (let ((n (ash (heap-next-node *heap*) -2)))
                       (if (< n 2) (+ n 1) (next-power-of-two n))))))
      (declare (type non-negative-fixnum start))
      (rec (1+ start) (node-time (heap-node start))))))

(defun resize-heap (size)
  (declare (type positive-fixnum size))
  (let ((obj *heap*)
        (size (next-power-of-two size)))
    (unless (= size (length (heap-data obj)))
      (let ((data (make-heap-data size)))
        (dotimes (i (min size (1- (heap-next-node obj))))
          (setf (svref data i)
                (svref (heap-data obj) i)))
        (setf (heap-data obj) data)))
    (if (= size *heap-size*)
        size
        (setf *heap-size* size))))

(defun check-heap-size (size)
  (= size (length (heap-data *heap*))))

(defvar *flush-pending-hook* nil
  "List of functions without arguments to invoke during the next call
to FLUSH-PENDING. The functions are called in non-realtime thread.
The list is empty after FLUSH-PENDING.")

(incudine.util::defglobal *flush-pending-spinlock*
  (incudine.util:make-spinlock "FLUSH-PENDING"))
(declaim (type incudine.util:spinlock *flush-pending-spinlock*))

(defun add-flush-pending-hook (function)
  "Regiter a hook FUNCTION to be run when FLUSH-PENDING is called."
  (incudine.util:with-spinlock-held (*flush-pending-spinlock*)
    (push function *flush-pending-hook*)
    function))

(defun remove-flush-pending-hook (function)
  "Unregister the flush-pending hook FUNCTION."
  (incudine.util:with-spinlock-held (*flush-pending-spinlock*)
    (setf *flush-pending-hook*
          (delete function *flush-pending-hook* :test #'eq))
    (values)))

(defun clear-flush-pending-hook ()
  (incudine.util:with-spinlock-held (*flush-pending-spinlock*)
    (setf *flush-pending-hook* nil)))

(defun force-pending-events (time-step)
  (declare (type non-negative-real time-step))
  (when (> (heap-next-node *heap*) +root-node+)
    (call-node-function (get-heap))
    (schedule-at (+ (incudine:now) time-step) #'force-pending-events
                 (list time-step)))
  (values))

(defun flush-pending-with-time-step (time-step)
  (declare (type non-negative-real time-step))
  (flet ((rt-flush ()
           (force-pending-events time-step)))
    (if (or (rt-thread-p) (null *rt-thread*))
        (rt-flush)
        (incudine:fast-nrt-funcall
          (lambda () (incudine:fast-rt-funcall #'rt-flush))))))

(defgeneric force-scheduled-function-p (obj))

(defmethod force-scheduled-function-p ((obj function)) nil)

(defun force-scheduled-event-p (node)
  (force-scheduled-function-p (node-function node)))

;;; Move the crucial internal events at the beginning of the heap
;;; and discard the rest.
(defun ignore-non-crucial-events ()
  (let ((number-of-events (heap-count)))
    ;; Reset the counters.
    (setf (heap-next-node *heap*) +root-node+)
    (setf (heap-ordering-tag *heap*) 0)
    (loop for i from +root-node+ to number-of-events
          for node = (heap-node i) do
          (when (force-scheduled-event-p node)
            ;; Reinsert the event.
            (schedule-at (node-time node) (node-function node)
                         (node-args node))))))

(defun delete-event (index)
  (declare (type positive-fixnum index))
  (let ((parent +root-node+)
        (curr +first-parent+))
    (declare (type positive-fixnum parent curr)
             #.*standard-optimize-settings*)
    ;; DELETE-EVENT is called from UNSCHEDULE-IF therefore INDEX is
    ;; always less than the index of the next free node.
    (when (> index +root-node+)
      ;; Move the hole from node INDEX to root-node.
      (do* ((child index parent)
            (parent (ash index -1) (ash parent -1)))
           ((< child 2))
        (node-copy (heap-node parent) (heap-node child))))
    (decf (heap-next-node *heap*))
    ;; Fill the hole with the last node and reorder the heap.
    (node-move (heap-node (heap-next-node *heap*)) (heap-node +root-node+))
    (node-copy (heap-node +root-node+) (heap-temp-node *heap*))
    (loop while (< curr (heap-next-node *heap*)) do
         (let ((sister (1+ curr)))
           (when (and (< sister (heap-next-node *heap*))
                      (node-time> (heap-node curr) (heap-node sister)))
             (setf curr sister))
           (cond ((node-time> (heap-temp-node *heap*) (heap-node curr))
                  (node-copy (heap-node curr) (heap-node parent))
                  (setf parent curr curr (ash parent 1)))
                 (t (return)))))
    (node-copy (heap-temp-node *heap*) (heap-node parent))
    (when (= (heap-next-node *heap*) +root-node+)
      (setf (heap-ordering-tag *heap*) 0))
    (values)))

(defun unschedule-if (test)
  "Cancel the scheduled events that satisfy the TEST.

The function TEST takes three arguments: time in samples, function and
function arguments of a scheduled event.

Example:

    ;; Cancel the events with function PLAY-FUNCTION (i.e. scheduled
    ;; from a sequencer during the playback and cancelled after stop).
    (unschedule-if
      (lambda (time function args)
        (declare (ignore time args))
        ;; Warning: it returns NIL if PLAY-FUNCTION is an inlined function.
        (eq function #'play-function)))"
  (declare (type function test))
  (incudine.util:rt-eval ()
    (do ((i +root-node+))
        ((>= i (heap-next-node *heap*)))
      (declare (type positive-fixnum i))
      (let ((node (heap-node i)))
        (cond ((and (not (force-scheduled-event-p node))
                    (funcall test (node-time node) (node-function node)
                             (node-args node)))
               (delete-event i)
               ;; Restart from the beginning of the current tree-level.
               (setf i (1- (integer-length (logior i 2)))))
              (t
               (incf i)))))))

(defun flush-pending (&optional time-step)
  "If TIME-STEP is NIL (default), cancel all the scheduled events.
If TIME-STEP is a number, the evaluation of a pending event is
forced every TIME-STEP samples."
  (declare (type (or null non-negative-real) time-step))
  (if time-step
      (flush-pending-with-time-step time-step)
      (flet ((rt-flush ()
               (ignore-non-crucial-events))
             (nrt-flush ()
               (dolist (fun *flush-pending-hook*)
                 (funcall fun))
               (clear-flush-pending-hook)))
        (cond ((rt-thread-p)
               (rt-flush)
               (incudine:nrt-funcall #'nrt-flush))
              ((null *rt-thread*)
               (rt-flush)
               (nrt-flush))
              (t (incudine:fast-nrt-funcall
                   (lambda () (incudine:fast-rt-funcall #'rt-flush)))
                 (nrt-flush)))
        (values))))

(define-constant +heap-pool-size+
    (if (boundp 'incudine.config::*edf-heap-pool-size*)
        incudine.config::*edf-heap-pool-size*
        (max 2 (/ 4096 *heap-size*))))

(define-constant +heap-pool-grow+
    (if (boundp 'incudine.config::*edf-heap-pool-grow*)
        incudine.config::*edf-heap-pool-grow*
        (max 1 (ash +heap-pool-size+ -1))))

(declaim (inline expand-heap-pool))
(defun expand-heap-pool (pool &optional (delta 1))
  (incudine.util:expand-cons-pool pool delta (make-heap *rt-edf-heap-size*)))

(defvar *heap-pool*
  (incudine.util:make-cons-pool
    :data (loop repeat +heap-pool-size+ collect (make-heap *rt-edf-heap-size*))
    :size +heap-pool-size+
    :expand-function #'expand-heap-pool
    :grow +heap-pool-grow+)
  "Pool of EDFs.")
(declaim (type incudine.util:cons-pool *heap-pool*))

(incudine.util::defglobal *heap-pool-spinlock*
  (incudine.util:make-spinlock "EDF-HEAP-POOL"))
(declaim (type incudine.util:spinlock *heap-pool-spinlock*))

(defun heap-pool-pop ()
  (incudine.util:with-spinlock-held (*heap-pool-spinlock*)
    (let ((cons (incudine.util:cons-pool-pop-cons *heap-pool*)))
      (prog1 (car cons)
        (setf (heap-next-node (car cons)) +root-node+)
        (incudine.util:nrt-global-pool-push-cons cons)))))

(defun heap-pool-push (heap)
  (declare (type heap heap))
  (incudine.util:with-spinlock-held (*heap-pool-spinlock*)
    (let ((cons (incudine.util:nrt-global-pool-pop-cons)))
      (setf (car cons) heap)
      (incudine.util:cons-pool-push-cons *heap-pool* cons))))

(defun reduce-heap-pool ()
  "Reset the heap-pool size to the value of the configuration variable
*EDF-HEAP-POOL-SIZE*."
  (incudine.util:with-spinlock-held (*heap-pool-spinlock*)
    (let ((len (incudine.util:cons-pool-size *heap-pool*)))
      (when (> len +heap-pool-size+)
        (loop for i from +heap-pool-size+ below len
              do (incudine.util:nrt-global-pool-push-cons
                   (incudine.util:cons-pool-pop-cons *heap-pool*))))
      (values *heap-pool* (- len +heap-pool-size+)))))

(defmacro with-rt-heap ((heap-var) &body body)
  `(let ((*heap* ,heap-var)
         (*rt-thread* (bt:current-thread)))
     (declare (special *heap* *rt-thread*))
     ,@body))

(defmacro with-next-node ((node-var time-var heap) &body body)
  `(let* ((,node-var (get-heap))
          (,time-var (+ (node-time ,node-var) (heap-time-offset ,heap))))
     ,@body))

;;; Pour the content of the next EDF heap node on the realtime EDF heap.
(defmacro with-rt-next-node ((node-var time-var heap rt-heap) &body body)
  `(with-next-node (,node-var ,time-var ,heap)
     (with-rt-heap (,rt-heap)
       (schedule-at ,time-var (node-function ,node-var) (node-args ,node-var))
       ,@body)))

;;; Note: POUR-ON-RT-HEAP is invoked from the realtime thread.
(defun pour-on-rt-heap (heap end-action)
  (declare (type heap heap) (type function end-action)
           (optimize speed (safety 0)))
  (flet ((the-end ()
           (remove-flush-pending-hook end-action)
           (incudine:nrt-funcall end-action)))
    (let ((rt-heap *heap*)
          (*heap* heap))
      (declare (special *heap*))
      (loop while (and (> (heap-next-node heap) +root-node+)
                       (>= (+ (the sample (incudine:now)) (sample 0.5))
                           (+ (node-time (heap-node +root-node+))
                              (heap-time-offset heap)))) do
              (let ((curr-node (get-heap)))
                (declare (type node curr-node))
                (with-rt-heap (rt-heap)
                  (call-node-function curr-node))))
      (cond ((heap-empty-p)
             (with-rt-heap (rt-heap) (the-end)))
            ((= (heap-count) 1)
             (with-rt-next-node (next-node time heap rt-heap)
               (the-end)))
            (t (with-rt-next-node (next-node time heap rt-heap)
                 ;; Continue to pour at the next tick.
                 (schedule-at (1+ (the sample (incudine:now)))
                              #'pour-on-rt-heap (list heap end-action)))))
      (values))))

(defun extract-schedule-specs (body)
  (labels ((extract (specs rest)
             (if (and (listp (car rest)) (keywordp (caar rest)))
                 (extract (cons (car rest) specs) (cdr rest))
                 (values (nreverse specs) rest))))
    (extract nil body)))

(defun get-schedule-spec (name specs)
  (cdr (assoc name specs)))

(defmacro with-schedule (&body body)
  "Fast way to schedule multiple events in realtime without an extensive use
of memory barriers and/or CAS. It fills a temporary queue in non-realtime,
then it pours the content of the queue on the realtime EDF heap.

The temporary queue introduces a generally unspecified latency.
If the first form in BODY is a list

    (:start-time value-form)

VALUE-FORM is the absolute time offset in samples of the first
scheduled event if

    (>= value-form (+ (now) latency))

If it is necessary to change the start-time within BODY, the
specification is

    (:start-time initial-value-form variable)

In this case, a VARIABLE is bound to a time-getter function. It is a
function because the computation occurs in rt-thread.

Examples:

    (with-schedule
      (:start-time (tempo-sync #[4 beats]))
      ...)

    (with-schedule
      (:start-time (incudine.edf:next-time #'phrase-7))
      ...)

    (with-schedule
      (:start-time 0 start)
      ;; Initially, the variable START is `(lambda () 0)'
      ...
      (setf start (lambda () (tempo-sync #[1 b])))
      ...)

The sample counter is zero and read-only inside WITH-SCHEDULE:

    (with-schedule (incf (now)) (princ (now)))
    ;; => 0.0d0

However, we can require a SETFable local counter:

    (with-schedule (with-local-time () (incf (now)) (princ (now))))
    ;; => 1.0d0"
  (multiple-value-bind (specs body) (extract-schedule-specs body)
    (let* ((tmp-heap (gensym "TMP-HEAP"))
           (heap (gensym "HEAP"))
           (time-offset (gensym "TIME-OFFSET"))
           (start-time-spec (get-schedule-spec :start-time specs))
           (start-time-form (first start-time-spec))
           (start-time-var (second start-time-spec)))
      `(labels ((sched ()
                  (let ((,tmp-heap *heap*)
                        (*heap* (if (incudine::nrt-edf-heap-p)
                                    *heap*
                                    (heap-pool-pop)))
                        ,@(and start-time-var
                               `((,start-time-var
                                    (lambda () ,(or start-time-form 0))))))
                    (declare (special *heap*))
                    (flet ((sched-body () (progn ,@body)))
                      (if (and (incudine::nrt-edf-heap-p)
                               (plusp (current-got-time)))
                          ;; Nested WITH-SCHEDULE within a recursive function
                          ;; scheduled through nrt-edf-heap.
                          (incudine:with-local-time (:start (current-got-time))
                            (sched-body))
                          (incudine::with-null-counter (sched-body))))
                    (unless (eq *heap* ,tmp-heap)
                      (let ((,heap *heap*)
                            ;; Realtime EDF heap.
                            (*heap* ,tmp-heap))
                        (declare (special *heap*))
                        ;; The content of the EDF HEAP is poured on the
                        ;; realtime EDF heap.
                        (at 0 (lambda (heap
                                       ,@(and start-time-var
                                              `(start-time-getter)))
                                (let ((end-action (lambda ()
                                                    (heap-pool-push heap)))
                                      ;; Time offset computed form rt-thread.
                                      ,@(and start-time-spec
                                             `((,time-offset
                                                (sample ,(if start-time-var
                                                             `(funcall start-time-getter)
                                                             start-time-form))))))
                                  (setf (heap-time-offset heap)
                                        ,(if start-time-spec
                                             `(if (> ,time-offset (incudine:now))
                                                  ,time-offset
                                                  (incudine:now))
                                             '(incudine:now)))
                                  (add-flush-pending-hook end-action)
                                  (pour-on-rt-heap heap end-action)))
                            ,heap ,@(and start-time-var `(,start-time-var))))))))
         (if (and (not (incudine::nrt-edf-heap-p)) (rt-thread-p))
             (incudine:nrt-funcall #'sched)
             (sched))))))
