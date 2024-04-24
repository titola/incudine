;;; Copyright (c) 2024 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; NODE-ADD is a good entry point to profile the initialization
  ;; function for a DSP.
  (setf (symbol-function 'node-add-function) #'incudine::node-add))

(defglobal *profiled-dsp* (make-hash-table))

(defglobal *profile-lock* (bordeaux-threads:make-lock "PROFILER"))

(defmacro %%profile (&rest names)
  `(sb-profile:profile ,@names))

(defun %profile (&optional names)
  ;; SB-PROFILE just provides the macro.
  (eval `(%%profile ,@names)))

(defmacro %%unprofile (&rest names)
  `(sb-profile:unprofile ,@names))

(defun %unprofile (&optional names)
  (eval `(%%unprofile ,@names)))

(defmacro unprofiled-dsp-function (info) `(first ,info))

(defmacro profiled-init-function-name (info) `(second ,info))

(defmacro profiled-perf-function-names (info) `(cddr ,info))

;;; Return NIL if a profiled DSP was redefined.
(declaim (inline profiled-dsp-auxiliary-function-p))
(defun profiled-dsp-auxiliary-function-p (name)
  (eq (symbol-function name)
      (get name 'dsp-auxiliary-wrapper-function)))

(defun profile-performance-time-function (dsp-name function-name)
  (bordeaux-threads:with-lock-held (*profile-lock*)
    (let ((info (gethash dsp-name *profiled-dsp*)))
      (when info
        (setf #1=(profiled-perf-function-names info) (cons function-name #1#))
        (%profile (list function-name))))))

(defun dsp-aux-keyword-arguments (name arguments)
  (when arguments
    (multiple-value-bind (args defaults) (incudine.vug:dsp-lambda-list name)
      (let ((keys (append (when defaults
                            (mapcar (lambda (x) (make-keyword (first x))) args))
                          (list :id :head :tail :before :after :replace :action
                                :stop-hook :free-hook :fade-time :fade-curve))))
        (do ((list arguments (rest list)))
            ((member (first list) keys) list))))))

;;; Wrapper for the auxiliary function created by VUG:DSP! macro.
(defun performance-wrapper-function (dsp-name dsp-aux function-namestring counter)
  (declare (type symbol dsp-name) (type function dsp-aux)
           (type string function-namestring) (type (unsigned-byte 24) counter))
  (lambda (&rest args)
    (let ((key-args (dsp-aux-keyword-arguments dsp-name args)))
      (symbol-macrolet ((action (getf key-args :action)))
        (let ((first-action action))
          (unless first-action
            (let ((action-property (list :action nil)))
              (if key-args
                  (setf (cdr (last key-args)) action-property)
                  (setf key-args action-property
                        args (append args key-args)))))
          (setf action
                (lambda (target)
                  (declare (type incudine:node target))
                  (when first-action
                    (funcall (the function first-action) target))
                  (let ((wrapper-name
                          (make-symbol
                            (format nil "~A.~D/NODE-~D"
                              function-namestring
                              (setf counter (logand (1+ counter) #xffffff))
                              (incudine:node-id target))))
                        (prof-node nil))
                    (declare (type (or null incudine:node) prof-node))
                    (setf (symbol-function wrapper-name)
                          (incudine::node-function target))
                    ;; The profiled function is called from a neighbor.
                    (incudine:pause target)
                    (incudine:play
                      (lambda () (funcall (symbol-function wrapper-name)))
                      :name (symbol-name wrapper-name)
                      :before target
                      :id (incudine::next-large-node-id)
                      :action (lambda (n)
                                (incudine:pause n)
                                (setf prof-node n))
                      :free-hook (list (lambda (n)
                                         (declare (ignore n))
                                         (incudine:free target))))
                    (push (lambda (n)
                            (declare (ignore n))
                            (incudine:free prof-node))
                          (incudine:free-hook target))
                    (incudine:nrt-funcall
                      (lambda ()
                        (profile-performance-time-function dsp-name wrapper-name)
                        (incudine:unpause prof-node)
                        (setf (get wrapper-name 'node) prof-node)))))))))
    (apply dsp-aux args)))

(defun profile-dsp (name)
  (bordeaux-threads:with-lock-held (*profile-lock*)
    (let ((info (gethash name *profiled-dsp*)))
      (%profile
        (list
          (cond (info
                 (let ((fnames (profiled-perf-function-names info)))
                   (when fnames
                     (%unprofile fnames)
                     (setf (profiled-perf-function-names info) nil)))
                 (unless (profiled-dsp-auxiliary-function-p name)
                   ;; The DSP was redefined.
                   (setf (unprofiled-dsp-function info) (symbol-function name)))
                 ;; Reprofiled to reset the counter.
                 (profiled-init-function-name info))
                (t
                 (let ((init-name (make-symbol (format nil "INIT-~A" name))))
                   (setf (symbol-function init-name) #'node-add-function)
                   (setf (symbol-function 'incudine::node-add)
                         (lambda (&rest args)
                           (macrolet ((dsp-name () '(sixth args)))
                             (let ((info (gethash (dsp-name) *profiled-dsp*)))
                               (apply (if (and info
                                               (profiled-dsp-auxiliary-function-p name))
                                          (symbol-function
                                            (profiled-init-function-name info))
                                          #'node-add-function)
                                      args)))))
                   (setf (gethash name *profiled-dsp*)
                         (list (symbol-function name) init-name))
                   init-name)))))
      (setf (symbol-function name)
            (performance-wrapper-function
              name (if info (unprofiled-dsp-function info) (symbol-function name))
              (symbol-name name) 0))
      (setf (get name 'dsp-auxiliary-wrapper-function) (symbol-function name)))
    (values)))

(defun unprofile-dsp (name)
  (bordeaux-threads:with-lock-held (*profile-lock*)
    (let ((info (gethash name *profiled-dsp*)))
      (when info
        (let ((fnames (cons (profiled-init-function-name info)
                            (profiled-perf-function-names info))))
          (mapc 'fmakunbound fnames)
          (when (profiled-dsp-auxiliary-function-p name)
            (setf (symbol-function name) (unprofiled-dsp-function info))
            (remf (symbol-plist name) 'dsp-auxiliary-wrapper-function))
          (remhash name *profiled-dsp*)
          (when (zerop (hash-table-count *profiled-dsp*))
            (setf (symbol-function 'incudine::node-add) #'node-add-function))
          (%unprofile fnames))))))

(defmacro profile (&rest names)
  "Extend SB-PROFILE:PROFILE by wrapping profiling code around the
init-time and performance-time functions of the named DSPs."
  (let (dsp-names other-names)
    (dolist (name names)
      (if (and (symbolp name) (incudine.vug::dsp name))
          (push name dsp-names)
          (push name other-names)))
    `(progn
       ,@(when dsp-names `((mapc 'profile-dsp ',dsp-names)))
       ,(if (or (not names) other-names)
            `(%%profile ,@other-names)
            `(values)))))

(defmacro unprofile (&rest names)
  "Extend SB-PROFILE:UNPROFILE by unwrapping any profiling code around
the functions of the named DSPs."
  (let (dsp-names other-names)
    (dolist (name names)
      (if (and (symbolp name) (incudine.vug::dsp name))
          (push name dsp-names)
          (push name other-names)))
    `(progn
       ,@(when (or (not names) dsp-names)
           `((mapc 'unprofile-dsp
                   ,(or (and dsp-names (list 'quote dsp-names))
                        '(alexandria:hash-table-keys *profiled-dsp*)))))
       ,(when (or (not names) other-names) `(%%unprofile ,@other-names)))))

(defun profile-report (&key limit (print-no-call-list t))
  "See documentation for SB-PROFILE:REPORT."
  (sb-profile:report :limit limit :print-no-call-list print-no-call-list))

(defun profile-reset ()
  "Unwrap any profiling code around unreferenced DSP functions and call
SB-PROFILE:RESET."
  (bordeaux-threads:with-lock-held (*profile-lock*)
    (let (to-unprofile)
      (maphash (lambda (dsp-name info)
                 (declare (ignore dsp-name))
                 (let (unreferenced)
                   (dolist (fname (profiled-perf-function-names info))
                     (let ((prof-node (get fname 'node)))
                       (when (or (incudine:null-node-p prof-node)
                                 (not (eq (symbol-name fname)
                                          (incudine:node-name prof-node))))
                         (push fname unreferenced)
                         (push fname to-unprofile))))
                   (when unreferenced
                     (setf #1=(profiled-perf-function-names info)
                           (set-difference #1# unreferenced)))))
               *profiled-dsp*)
      (when to-unprofile
        (%unprofile to-unprofile)))
    (sb-profile:reset)))

(in-package #:incudine)

(defun incudine.util:profile-node (node max-number-of-calls)
  "Wrap profiling code around the functions of a live NODE, then call
PROFILE-REPORT and remove profiling code after MAX-NUMBER-OF-CALLS to
the wrapped functions, or when the target node is freed.

If the target node was paused, it is internally unpaused for
MAX-NUMBER-OF-CALLS to the profiled performance-time function.

If the target node is a group node, the paused nodes of that group are
not internally unpaused.

Note: the argument name MAX-NUMBER-OF-CALLS instead of MAX-SAMPLES in
this context avoids confusion between audio samples and program
execution samples."
  (declare (type (or node fixnum) node)
           (type positive-fixnum max-number-of-calls))
  (bordeaux-threads:with-lock-held (incudine.util::*profile-lock*)
    (let ((target (get-node node)))
      (when (null-node-p target)
        (warn "ignoring the null node ~S" node)
        (return-from incudine.util:profile-node))
      (let ((fname (make-symbol
                     (format nil "~@[~A/~]~:[~;GROUP-~]NODE-~D"
                       (node-name target) (group-p target) (node-id target))))
            (pause-p (node-pause-p target))
            (prof-node nil)
            (i max-number-of-calls)
            (trace-output *trace-output*))
        (declare (type (or null node) prof-node)
                 (type fixnum i))
        (unless pause-p (pause target))
        (setf (symbol-function fname)
              (if (group-p target)
                  (lambda ()
                    (dogroup (node target)
                      (unless (or (group-p node) (pause-p node))
                        (funcall (node-function node)))))
                  (node-function target)))
        (incudine.util::%profile (list fname))
        (play (lambda ()
                (funcall (symbol-function fname))
                (decf i)
                (when (or (null-node-p target) (<= i 0))
                  (free prof-node)))
               :before target
               :id (next-large-node-id)
               :name (format nil "Profiling ~S" target)
               :action (lambda (n) (setf prof-node n))
               :free-hook
                 (list (lambda (n)
                         (declare (ignore n))
                         (unless pause-p (unpause target))
                         (nrt-funcall
                           (lambda ()
                             (let ((*trace-output* trace-output))
                               (profile-report)
                               (incudine.util::%unprofile (list fname))
                               ;; The uninterned symbol was accessible through
                               ;; (symbol-function (nth i (profile)))
                               (fmakunbound fname)))))))))))
