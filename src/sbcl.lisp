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

(import
 '(sb-int:constant-form-value
   sb-ext:octets-to-string sb-ext:string-to-octets
   sb-ext:gc
   sb-ext:get-time-of-day
   sb-ext:truly-the))

(define-constant n-machine-word-bits sb-vm:n-machine-word-bits)

(define-constant n-fixnum-bits sb-vm:n-fixnum-bits)

(defvar *reduce-warnings*
  '(sb-ext:muffle-conditions sb-ext:compiler-note)
  "Declaration to muffle compiler-notes.")

(defvar *null-output*
  (sb-sys:make-fd-stream
    (sb-unix:unix-open #-win32 "/dev/null" #+win32 "nul" sb-unix:o_wronly #o666)
    :name "null output"
    :input nil
    :output t
    :buffering :line
    :element-type :default
    :serve-events nil
    :auto-close t
    :external-format (stream-external-format sb-sys:*stdout*))
  "Output stream for null output.")

(defmacro var-globally-special-p (symbol)
  `(sb-walker:var-globally-special-p ,symbol))

(defmacro reduce-warnings (&body body)
  "Execute BODY by muffling compiler-notes."
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((limit 1000))
    ;; Workaround for the current version of SBCL; see the FIXME
    ;; comment inside the definition of the function
    ;; INLINE-EXPANSION-OK in `sbcl/src/compiler/ir1util.lisp'
    (when (< sb-ext:*inline-expansion-limit* limit)
      (setf sb-ext:*inline-expansion-limit* limit))))

(declaim (inline incudine::int-hash))
(defun incudine::int-hash (x)
  (declare (type fixnum x))
  (sxhash x))

(declaim (inline incudine::file-name))
(defun incudine::file-name (stream)
  ;; WARN: symbol SB-INT:FILE-NAME exported but with a FIXME-note in
  ;; sbcl/src/code/fd-stream.lisp
  (sb-int:file-name stream))

(defmacro compare-and-swap (place old new)
  "See documentation for SB-EXT:COMPARE-AND-SWAP."
  `(sb-ext:compare-and-swap ,place ,old ,new))

(defmacro barrier ((kind) &body body)
  "See documentation for SB-THREAD:BARRIER."
  `(sb-thread:barrier (,kind) ,@body))

(defun thread-pointer (thread)
  (cffi:make-pointer (sb-thread::thread-os-thread thread)))

(defun thread-priority (thread)
  (declare (type sb-thread:thread thread))
  (when (bt:threadp thread)
    (incudine.external::pthread-priority (thread-pointer thread))))

(defun set-thread-priority (thread priority)
  (declare (type sb-thread:thread thread) (type fixnum priority))
  (when (bt:threadp thread)
    (if (zerop (incudine.external:pthread-set-priority
                 (thread-pointer thread) priority))
        priority
        (warn "failed to set scheduling priority ~D" priority))))

(defsetf thread-priority set-thread-priority)

(declaim (inline seed-from-random-state))
(defun seed-from-random-state (state)
  (aref (sb-kernel::random-state-state state) 3))

(declaim (inline seed-random-state))
(defun seed-random-state (&optional state)
  (setf *random-state* (sb-ext:seed-random-state state))
  (incudine.external::gsl-seed-random-state
    (if (numberp state) state (seed-from-random-state *random-state*)))
  (values))

(defmacro without-gcing (&body body)
  `(sb-sys:without-gcing ,@body))

(defmacro without-interrupts (&body body)
  `(sb-sys:without-interrupts ,@body))

(defmacro with-pinned-objects ((&rest objects) &body body)
  `(sb-sys:with-pinned-objects (,@objects) ,@body))

(defmacro with-stop-for-gc-pending (&body body)
  `(when sb-kernel:*stop-for-gc-pending* ,@body))

(defmacro without-float-overflow-trap (&body body)
  `(sb-int:with-float-traps-masked (:underflow :overflow) ,@body))

(defmacro without-float-invalid-op-trap (&body body)
  `(sb-int:with-float-traps-masked (:invalid) ,@body))

(defun add-after-gc-hook (function)
  (pushnew function sb-ext:*after-gc-hooks*))

(defmacro %cudo-eval (mode expr)
  `(let ((sb-ext:*evaluator-mode* ,mode))
     (eval ,expr)))

(defmacro cudo-eval (expr)
  `(%cudo-eval :interpret ,expr))

(defmacro cudo-compile (expr)
  `(%cudo-eval :compile ,expr))

(defmacro %exit (&optional (code 0))
  `(sb-ext:exit :code ,code))

(setf (symbol-function 'stream-fd) (symbol-function 'sb-sys:fd-stream-fd))
(setf (symbol-function 'lseek) (symbol-function 'sb-posix:lseek))

(declaim (inline stdin-fd))
(defun stdin-fd ()
  (sb-sys:fd-stream-fd sb-sys:*stdin*))

(declaim (inline stdout-fd))
(defun stdout-fd ()
  (sb-sys:fd-stream-fd sb-sys:*stdout*))

;;; Finalization

(declaim (inline finalize))
(defun finalize (obj function)
  (sb-ext:finalize obj function :dont-save t))

(declaim (inline cancel-finalization))
(defun cancel-finalization (obj)
  (sb-ext:cancel-finalization obj))

;;; DEBUG

(defun get-bytes-consed-in (time)
  "Rough estimate of the bytes consed in TIME seconds."
  (let ((t0 #1=(sb-ext:get-bytes-consed)))
    (sleep time)
    (- #1# t0)))

;;; SWANK

(defun set-swank-arglist-interface ()
  (let ((defimpl (and (find-package "SWANK/BACKEND")
                      (find-symbol "DEFIMPLEMENTATION" "SWANK/BACKEND"))))
    (when defimpl
      (let ((arglist (find-symbol "ARGLIST" "SWANK/BACKEND")))
        (cond (arglist
               (require 'sb-introspect)
               (eval
                 `(,defimpl ,arglist (fname)
                    (let ((arglist (uiop:symbol-call
                                     :sb-introspect
                                     '#:function-lambda-list fname)))
                      (or (lambda-list-to-star-list arglist) arglist)))))
              (t
               (warn "Undefined SWANK ARGLIST interface.")))))))
