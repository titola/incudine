;;; Copyright (c) 2013-2020 Tito Latini
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
  (require :sb-introspect)
  (import
    '(sb-ext:defglobal)
    "INCUDINE")
  (import
    `(sb-int:constant-form-value
      ,(find-symbol "FUNCTION-LAMBDA-LIST" "SB-INTROSPECT")
      sb-ext:defglobal
      sb-ext:gc
      sb-ext:get-time-of-day
      sb-ext:native-namestring
      sb-ext:octets-to-string
      sb-ext:string-to-octets
      sb-ext:truly-the)))

(define-constant n-machine-word-bits sb-vm:n-machine-word-bits)

(define-constant n-fixnum-bits sb-vm:n-fixnum-bits)

(define-constant incudine.osc::posix-enotconn sb-posix:enotconn)

(defvar *reduce-warnings*
  '(sb-ext:muffle-conditions sb-ext:compiler-note)
  "Declaration to muffle compiler-notes.")

(sb-ext:defglobal *null-output*
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

#+x86-64
(defun old-movsxd-p ()
  ;; Syntax changed in SBCL from 1.4.11. I prefere to avoid
  ;; SB-EXT:ASSERT-VERSION->= because if SBCL is obtained from git
  ;; and compiled without an updated version.lisp-expr file, the
  ;; version is not correct. movsxd is used at least in
  ;; incudine/src/network/sbcl-vops.lisp.
  ;; Tested with sbcl-1.4.10 and before the release of sbcl-1.4.11.
  (let ((x86-64-pkg (find-package "SB-X86-64-ASM"))
        (r8 (find-symbol "R8-TN" "SB-VM"))
        (ecx (find-symbol "ECX-TN" "SB-VM")))
    (when (and x86-64-pkg r8 ecx)
      (let ((segment (sb-assem:make-segment))
            (args (mapcar 'symbol-value (list r8 ecx))))
        (handler-case
            (and (sb-assem:assemble (segment 'nil)
                   (apply (find-symbol "MOVSXD" x86-64-pkg) args segment args))
                 t)
          ;; MOVSXD with two arguments in older SBCL.
          (sb-int:simple-program-error (c) (declare (ignore c)) t)
          (error (c) (declare (ignore c)) nil))))))

(declaim (inline incudine::int-hash))
(defun incudine::int-hash (x)
  (declare (type fixnum x))
  (sxhash x))

(declaim (inline incudine::file-name))
(defun incudine::file-name (stream)
  ;; WARN: symbol SB-INT:FILE-NAME exported but with a FIXME-note in
  ;; sbcl/src/code/fd-stream.lisp
  (sb-int:file-name stream))

(defun %parse-filepath (pathspec)
  (if (stringp pathspec)
      (namestring (sb-ext:parse-native-namestring pathspec))
      pathspec))

;;; PROBE-FILE* and TRUENAME* work with "~/some/path [test one two three]/to/file"
(defun probe-file* (pathspec)
  (probe-file (%parse-filepath pathspec)))

(defun truename* (pathspec)
  (truename (%parse-filepath pathspec)))

(defmacro compare-and-swap (place old new)
  "See documentation for SB-EXT:COMPARE-AND-SWAP."
  `(sb-ext:compare-and-swap ,place ,old ,new))

(defmacro barrier ((kind) &body body)
  "See documentation for SB-THREAD:BARRIER."
  `(sb-thread:barrier (,kind) ,@body))

(defun thread-pointer (thread)
  (cffi:make-pointer (sb-thread::thread-os-thread thread)))

(defun thread-priority (thread)
  "Return the thread priority. Setfable."
  (declare (type sb-thread:thread thread))
  (when (bt:threadp thread)
    (incudine.external::thread-priority (thread-pointer thread))))

(defun set-thread-priority (thread priority)
  (declare (type sb-thread:thread thread) (type fixnum priority))
  (when (bt:threadp thread)
    (if (zerop (incudine.external::thread-set-priority
                 (thread-pointer thread) priority))
        priority
        (warn "failed to set scheduling priority ~D" priority))))

(defsetf thread-priority set-thread-priority)

(define-constant +max-number-of-cpus+ 1024)

(defun affinity-mask-ptr-to-integer (ptr size)
  (loop for i from (1- size) downto 0
        for res = (cffi:mem-aref ptr :unsigned-char i)
                then (logior (ash res 8) (cffi:mem-aref ptr :unsigned-char i))
        finally (return res)))

(defun affinity-mask-integer-to-ptr (ptr size mask)
  (loop for i below size
        while (plusp mask) do
          (setf (cffi:mem-aref ptr :unsigned-char i) (logand mask #xff))
          (setf mask (ash mask -8))))

(defun thread-affinity (thread)
  "Return the CPU affinity mask of THREAD. Setfable.

It is also possible to specify a list of zero-based processors to set
the thread affinity.

For more details on CPU affinity masks, see taskset and/or
sched_setaffinity man page.

Example: 8 processors

    (rt-stop)

    (mapcar (lambda (thr)
              ;; Exclude processor 0. The list is equivalent
              ;; to the affinity mask #b11111110.
              (setf (thread-affinity thr) '(1 2 3 4 5 6 7)))
            (bt:all-threads))

    (rt-start)

    ;; Dedicate processor 0 to real-time thread.
    (setf (thread-affinity *rt-thread*) '(0))
    ;; => 1

    ;; Verify affinity mask.
    (mapcar #'thread-affinity (bt:all-threads))
    ;; => (1 254 254 254)"
  (declare (type sb-thread:thread thread))
  (assert (bt:threadp thread))
  (let ((size #.(ash +max-number-of-cpus+ -3)))
    (cffi:with-foreign-object (ptr :char size)
      (incudine.external:foreign-set ptr 0 size)
      (if (zerop (incudine.external::pthread-getaffinity-np
                   (thread-pointer thread) size ptr))
          (affinity-mask-ptr-to-integer ptr size)
          (incudine:incudine-error "pthread_getaffinity_np failed")))))

(defun set-thread-affinity (thread value)
  (declare (type sb-thread:thread thread) (type (or integer cons) value))
  (assert (bt:threadp thread))
  (let* ((mask (if (listp value)
                   (loop for i in (remove-duplicates value) sum (ash 1 i))
                   value))
         (size (max 1 (ceiling (* (integer-length mask) 1/8)))))
    (cffi:with-foreign-object (ptr :char size)
      (incudine.external:foreign-set ptr 0 size)
      (affinity-mask-integer-to-ptr ptr size mask)
      (if (zerop (incudine.external::pthread-setaffinity-np
                   (thread-pointer thread) size ptr))
          mask
          (incudine:incudine-error
            "failed to set thread affinity ~A" value)))))

(defsetf thread-affinity set-thread-affinity)

(defmacro with-available-mutex ((mutex) &body body)
  `(sb-thread:with-mutex (,mutex :wait-p nil) ,@body))

(declaim (inline seed-from-random-state))
(defun seed-from-random-state (state)
  (aref (sb-kernel::random-state-state state) 3))

(defun seed-random-state (&optional state)
  "Set the current random state."
  (setf *random-state* (sb-ext:seed-random-state state))
  (incudine.external::gsl-seed-random-state
    (if (numberp state) state (seed-from-random-state *random-state*)))
  (values))

(defmacro without-gcing (&body body)
  `(sb-sys:without-gcing ,@body))

(defmacro without-interrupts (&body body)
  "See documentation for SB-SYS:WITHOUT-INTERRUPTS."
  `(sb-sys:without-interrupts ,@body))

(defmacro with-pinned-objects ((&rest objects) &body body)
  "See documentation for SB-SYS:WITH-PINNED-OBJECTS."
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

;; &AUX bindings used to get the optional-key arguments by using
;; LAMBDA-LIST-TO-STAR-LIST.
(defun force-macro-lambda-list (name lambda-list)
  (assert (find '&aux lambda-list))
  (let ((getter (find-symbol "%SIMPLE-FUN-ARGLIST" "SB-KERNEL")))
    (when getter
      (let ((setter (fdefinition `(setf ,getter))))
        (when setter
          (let ((args (funcall getter (macro-function name))))
            (unless (find '&aux args)
              (funcall setter lambda-list (macro-function name))))))))
  name)
