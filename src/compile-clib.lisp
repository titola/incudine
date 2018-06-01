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

(in-package :incudine.config)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-error (format-control &rest format-arguments)
    (error 'incudine:incudine-compile-error
           :format-control format-control
           :format-arguments format-arguments))

  (defvar *sample-rate* 48000)

  (defvar *rt-cpu* nil)
  (declaim (type (or null alexandria:non-negative-fixnum) *rt-cpu*))

  (defvar *rt-block-size* 1)

  (defvar *midi-input-timeout* 1)
  (declaim (type alexandria:positive-fixnum *midi-input-timeout*))

  (defvar *osc-package-nicknames* (list "OSC"))
  (declaim  (type list *osc-package-nicknames*))

  ;;; Velocity of the sound at 22°C, 1 atmosfera
  (defvar *sound-velocity* 345)

  (load (asdf:system-relative-pathname "incudine" "src/config-specials.lisp"))

  (defvar *incudinerc-loaded-p* nil)

  (defun load-incudinerc ()
    (let ((*package* (find-package :incudine.config)))
      (setf *incudinerc-loaded-p*
            (let ((init-file (merge-pathnames ".incudinerc"
                                              (user-homedir-pathname))))
              (if (probe-file init-file)
                  (load init-file
                    :external-format '(:utf-8 :replacement #\replacement_character))
                  t)))))

  (unless *incudinerc-loaded-p*
    (load-incudinerc))

  (defvar *sample-type* 'double-float)

  (defvar *sched-policy*
    #+linux "SCHED_FIFO"
    #-linux "SCHED_RR")

  ;;; Real time audio:
  ;;;
  ;;;     :DUMMY
  ;;;     :JACK
  ;;;     :PORTAUDIO
  ;;;     :PORTAUDIO-JACK
  ;;;
  ;;; :PORTAUDIO and :PORTAUDIO-JACK are the same, but with the last
  ;;; it is possible to set the Jack client name.
  ;;;
  (defvar *audio-driver*
    #+(or linux jack-audio) :jack
    #-(or linux jack-audio) :portaudio
    "Driver for real-time audio.")

  (defvar *enable-jack-midi* nil)

  (defvar *enable-portmidi-output-sample-offset*
    (not (eq *audio-driver* :jack)))

  ;; The default is "cc" but if it is not available, it will be changed
  ;; with "gcc" in CHECK-C-COMPILER
  (defvar *c-compiler* "cc")

  ;;; MINI-EVAL copied from cffi/src/libraries.lisp
  (defun cffi-mini-eval (form)
    "Simple EVAL-like function to evaluate the elements of
CFFI:*FOREIGN-LIBRARY-DIRECTORIES* and CFFI:*DARWIN-FRAMEWORK-DIRECTORIES*."
    (typecase form
      (cons (apply (car form) (mapcar #'cffi-mini-eval (cdr form))))
      (symbol (symbol-value form))
      (t form)))

  (defvar *foreign-library-directories* nil)

  (defvar *c-library-paths*
    (progn
      (setf cffi:*foreign-library-directories*
            (union cffi:*foreign-library-directories*
                   *foreign-library-directories*
                   :test #'equal))
      (format nil "~{ -L\"~A\"~}"
              (mapcar #'cffi-mini-eval cffi:*foreign-library-directories*))))

  (defvar *foreign-header-file-directories* nil)

  (defvar *c-header-file-paths*
    (format nil "~{ -I\"~A\"~}"
            (mapcar #'cffi-mini-eval *foreign-header-file-directories*)))

  (defvar *c-compiler-flags*
    (concatenate 'string "-O3 -Wall"
                 #-(or darwin cygwin) " -fPIC"
                 (if (eq *sample-type* 'double-float)
                     " -D__INCUDINE_USE_64_BIT_SAMPLE__")
                 #+little-endian " -DLITTLE_ENDIAN"
                 (format nil " -D__INCUDINE_~A__" *sched-policy*)
                 *c-header-file-paths*))

  (defvar *c-linker-flags*
    (concatenate 'string
                 #+darwin "-dynamic -bundle -flat_namespace -undefined suppress"
                 #-darwin "-shared"
                 *c-library-paths*))

  (defvar *c-source-dir*
    (make-pathname :name nil :type nil
                   :defaults #.(load-time-value (or *compile-file-pathname*
                                                    *load-pathname*))))

  (defvar *tlsf-source-dir*
    (make-pathname :name nil :type nil
                   :directory (append
                               (butlast (pathname-directory *c-source-dir*))
                               '("contrib" "tlsf" "src"))))

  (defvar *tlsf-block-align* 32)

  (defvar *tlsf-statistic* t)

  (defun c-source-pathname (file &optional (dir *c-source-dir*))
    (merge-pathnames file dir))

  (defun c-object-pathname (cfile-path)
    (make-pathname :name (pathname-name cfile-path) :type "o"
                   :defaults *c-source-dir*))

  (defvar *c-source-deps-alist*
    (let ((common (list (c-source-pathname "common.h"))))
      (cons (list :tlsf (c-source-pathname "tlsf.c" *tlsf-source-dir*))
            (mapcar (lambda (x)
                      (cons (car x)
                            (append (mapcar #'c-source-pathname (cdr x))
                                    common)))
                    '((:jack "rtjack.c" "rtjack.h")
                      (:portaudio "rtpa.c" "rtpa.h")
                      (:mouse "mouse.c" "mouse.h")
                      (:osc "network/osc.c" "network/osc.h")
                      (:util "util.c"))))))

  (defvar *c-source-pathnames*
    (remove-duplicates
      (loop for i in *c-source-deps-alist* append (cdr i))))

  (defvar *c-library-name*
    #-cygwin "libincudine"
    #+cygwin "cygincudine-0")

  (defvar *c-library-type*
    #+(and unix (not darwin)) "so"
    #+darwin "dylib"
    #+cygwin "dll")

  (defvar *c-library-pathname*
    (make-pathname :name *c-library-name* :type *c-library-type*
                   :defaults *c-source-dir*))

  (defun get-c-source-by-key (key)
    (second (assoc key *c-source-deps-alist*)))

  (defun get-c-object-by-key (key)
    (c-object-pathname (get-c-source-by-key key)))

  (defun missing-c-object-p (key)
    (not (probe-file (get-c-object-by-key key))))

  (defun changed-c-files ()
    (let ((c-library-write-date (file-write-date *c-library-pathname*)))
      (remove-if-not (lambda (x) (< c-library-write-date x))
                     *c-source-pathnames* :key #'file-write-date)))

  (defvar *cache-pathname*
    (merge-pathnames "cache.lisp" *c-source-dir*))

  (defun get-compiler-options ()
    (let ((drv (case *audio-driver*
                 ((:jack :portaudio) *audio-driver*)
                 ((:portaudio-jack) :portaudio)
                 (otherwise :dummy))))
      (list
       ;; The CAR is the option and the CDR is the influenced object.
       ;; The keyword :ALL specifies all the objects.
       (cons *c-compiler* :all)
       (cons *sample-type* :all)
       (cons *tlsf-block-align* :tlsf)
       (cons *sched-policy* :util)
       (cons *audio-driver* drv)
       (cons :jack-midi (and *enable-jack-midi* :jack))
       (cons :lisp-features
             `((incudine-fifo-circular-list
                ,(uiop:featurep :incudine-fifo-circular-list))
               (portmidi-output-sample-offset
                ,incudine.config::*enable-portmidi-output-sample-offset*))))))

  (defun store-compiler-options ()
    (with-open-file (f *cache-pathname* :direction :output
                     :if-exists :supersede)
      (write-line ";;;; This file is automatically generated." f)
      (write (get-compiler-options) :stream f)))

  (defun changed-compiler-options (&key exclude)
    (if (not (probe-file *cache-pathname*))
        :all
        (let* ((cached (with-open-file (f *cache-pathname*) (read f)))
               (opts (get-compiler-options))
               (diff (- (length opts) (length cached))))
          (when (plusp diff)
            (setf cached (append cached (make-list diff))))
          (loop for old in cached
                for new in opts
                for value = (cdr new)
                unless (or (equal old new)
                           (member (car old) exclude))
                do (if (eq value :all)
                       (return-from changed-compiler-options :all))
                and collect (if (listp value)
                                (set-difference value (cdr old) :test #'equal)
                                value)))))

  (defun c-objects-to-compile ()
    (flet ((return-all-objects ()
             (mapcar #'car *c-source-deps-alist*)))
      (if (not (probe-file *c-library-pathname*))
          (return-all-objects)
          (let ((changed-opts
                  (changed-compiler-options :exclude '(:lisp-features))))
            (declare (type (or keyword list) changed-opts))
            (if (eq changed-opts :all)
                (return-all-objects)
                (let ((changed-src (changed-c-files)))
                  (union changed-opts
                         (loop for l in *c-source-deps-alist*
                               if (intersection (cdr l) changed-src)
                               collect (car l)))))))))

  (defun changed-lisp-feature (x)
    (loop for opt in (changed-compiler-options)
          if (listp opt) return (and (assoc x opt) t)))

  (defun recompile-source-file-p (file)
    (let ((f (assoc file '(("fifo" . incudine-fifo-circular-list)
                           ("rt" . portmidi-output-sample-offset))
                    :test #'string=)))
      (and f (changed-lisp-feature (cdr f)))))

  (defun run-program (format-control &optional format-arguments output
                      error-output)
    (multiple-value-bind (out err exit-code)
        (uiop:run-program (apply #'format nil format-control format-arguments)
                          :output output :error-output error-output
                          :ignore-error-status t)
      (declare (ignore out err))
      (zerop exit-code)))

  (defvar *c-libtest-fmt*
    (format nil "~A -o /dev/null~A ~S -l" *c-compiler* *c-library-paths*
            (namestring (merge-pathnames "nothing.c" *c-source-dir*))))

  (defun probe-c-library (name)
    (run-program "~A ~A" (list *c-libtest-fmt* name)))

  (defun compile-c-object (key &optional cflags)
    (let* ((src-path (get-c-source-by-key key))
           (obj (namestring (c-object-pathname src-path)))
           (cflags (if cflags
                       (format nil "~A ~A" *c-compiler-flags* cflags)
                       *c-compiler-flags*))
           (cmd (format nil "~A ~A -o ~S -c ~S" *c-compiler*
                        cflags obj (namestring src-path))))
      (write-line cmd)
      (force-output)
      (if (run-program cmd nil t t)
          obj
          (compile-error "C library compilation failed"))))

  (defmacro info (datum &rest arguments)
    `(progn
       (fresh-line *error-output*)
       (princ "INFO: " *error-output*)
       (format *error-output* ,datum ,@arguments)
       (terpri *error-output*)))

  (defun get-audio-driver ()
    (flet ((no-jack-or-pa ()
             (info "Jack or PortAudio not installed; using dummy audio driver")
             (setf *audio-driver* :dummy)))
      (case *audio-driver*
        (:jack
         (cond ((probe-c-library "jack") :jack)
               ((probe-c-library "portaudio")
                (info "Jack not installed; audio driver changed to PortAudio.")
                (setf *audio-driver* :portaudio))
               (t (no-jack-or-pa))))
        (:portaudio
         (cond ((probe-c-library "portaudio") :portaudio)
               ((probe-c-library "jack")
                (info "PortAudio not installed; audio driver changed to Jack")
                (setf *audio-driver* :jack))
               (t (no-jack-or-pa))))
        (:portaudio-jack
         (cond ((probe-c-library "portaudio")
                (if (probe-c-library "jack")
                    :portaudio-jack
                    (progn
                      (info "Jack not installed; ~
                             no JACK-specific extensions for PortAudio.")
                      (setf *audio-driver* :portaudio))))
               ((probe-c-library "jack")
                (info "PortAudio not installed; audio driver changed to Jack")
                (setf *audio-driver* :jack))
               (t (no-jack-or-pa))))
        (:dummy :dummy)
        (t (unless (null *audio-driver*)
             (info "~S is unknown; *AUDIO-DRIVER* must be ~
                    :JACK, :PORTAUDIO, :PORTAUDIO-JACK or :DUMMY"
                   *audio-driver*))
           ;; The default is Jack for Linux and PortAudio for the other OS.
           (setf *audio-driver*
                 (cond
           #+linux ((probe-c-library "jack") :jack)
                   ((probe-c-library "portaudio") :portaudio)
           #-linux ((probe-c-library "jack") :jack)
                   (t (no-jack-or-pa))))))))

  (defmacro add-c-object-to-link (key objlist &optional to-compile-p cflags)
    (let ((to-compile-form `(compile-c-object ,key ,cflags)))
      `(push ,(if to-compile-p
                  to-compile-form
                  `(if (to-compile-p ,key)
                       ,to-compile-form
                       (namestring (get-c-object-by-key ,key))))
             ,objlist)))

  ;;; Probably useful only when "cc" is not available.
  (defun check-c-compiler ()
    (flet ((check (cc)
             (run-program "which ~A" (list cc))))
      (unless (stringp *c-compiler*)
        (info "*C-COMPILER* is ~S but it should be a string" *c-compiler*)
        (setf *c-compiler* "cc"))
      (or (check *c-compiler*)
          ;; Try an alternative.
          (let ((cc (if (string= *c-compiler* "cc") "gcc" "cc")))
            (cond ((check cc)
                   (info "no ~S in the search path; C compiler changed to ~S"
                         *c-compiler* cc)
                   (setf *c-compiler* cc)
                   t)
                  (t (compile-error "Command ~S or ~S not found"
                                    *c-compiler* cc)))))))

  (defun update-features ()
    (when (probe-c-library "X11")
      (pushnew :x11 *features*))
    (values))

  ;;; Test for FFTW and unaligned stack pointer with SBCL on x86.
  ;;;
  ;;;     https://bugs.launchpad.net/sbcl/+bug/539632
  ;;;     https://bugs.launchpad.net/sbcl/+bug/1294906
  ;;;
  ;;; If SBCL crashes, use the planner flag FFTW_NO_SIMD and add :FFTW-NO-SIMD
  ;;; to *FEATURES*.
  ;;;
  ;;; If you want to repeat the test, delete the automatically generated file
  ;;; src/analysis/maybe-fftw-no-simd.lisp
  #+(and sbcl x86 (not darwin))
  (defun fftw-stack-align-test ()
    (let ((path (namestring
                  (merge-pathnames "analysis/maybe-fftw-no-simd.lisp"
                                   *c-source-dir*))))
      (unless (probe-file path)
        (let ((testfile (namestring
                          (merge-pathnames "fftw-stack-align-test.lisp"
                                           path))))
          (run-program "~A --non-interactive --load ~S ~S"
                       (list (first sb-ext:*posix-argv*) testfile path)
                       t t)))))

  (defun check-loaded-c-library ()
    (flet ((clib-error (required)
             (compile-error
               "*AUDIO-DRIVER* is ~S but the loaded foreign library~%~
                ~S~%doesn't depend on ~A. Launch the clean.sh script~2%~4T~
                cd ~S && ./clean.sh~2%~
                then recompile Incudine."
               *audio-driver* *c-library-pathname* required
               (namestring *c-source-dir*))))
    (cond ((and (eq *audio-driver* :jack)
                (not (cffi:foreign-symbol-pointer "ja_start")))
           (clib-error "Jack"))
          ((and (member *audio-driver* '(:portaudio :portaudio-jack))
                (not (cffi:foreign-symbol-pointer "pa_start")))
           (clib-error "PortAudio")))
    (values)))

  (defun %compile-c-library (ofiles libs-dep)
    (let ((cmd (format nil "~A ~A ~A -o ~S~{ ~S~}~{ -l~A~}"
                       *c-compiler* *c-compiler-flags* *c-linker-flags*
                       (namestring *c-library-pathname*) ofiles libs-dep)))
      (write-line cmd)
      (unless (run-program cmd nil t t)
        (compile-error "C library compilation failed"))
      (store-compiler-options)
      (values)))

  (defun compile-c-library (&optional force-p)
    (let ((objs (or (c-objects-to-compile)
                    (and force-p (mapcar #'car *c-source-deps-alist*)))))
      (when objs
        (check-c-compiler)
        (flet ((to-compile-p (key)
                 (or (member key objs) (missing-c-object-p key))))
          (let ((ofiles nil)
                (libs-dep '("pthread" "m"))
                (drv (get-audio-driver)))
            (terpri)
            ;; RT Audio
            (case drv
              ((:jack)
               (push "jack" libs-dep)
               (add-c-object-to-link :jack ofiles))
              ((:portaudio :portaudio-jack)
               (when (to-compile-p :portaudio)
                 (push "portaudio" libs-dep)
                 (add-c-object-to-link :portaudio ofiles t
                                       (if (eq drv :portaudio-jack)
                                         "-DPA_HAVE_JACK")))))
            ;; Mouse support
            (when (probe-c-library "X11")
              (push "X11" libs-dep)
              (pushnew :x11 *features*)
              (add-c-object-to-link :mouse ofiles))
            ;; Open Sound Control
            (add-c-object-to-link :osc ofiles)
            ;; Utilities
            (add-c-object-to-link :util ofiles)
            ;; TLSF Memory Storage allocator
            (add-c-object-to-link :tlsf ofiles nil
              (format nil "~:[~;-DTLSF_STATISTIC=1 ~]-DBLOCK_ALIGN=~D"
                      *tlsf-statistic* *tlsf-block-align*))
            ;; Linking
            (%compile-c-library ofiles libs-dep)))))))
