;;; Copyright (c) 2014 Tito Latini
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
  (defvar *sample-rate* 48000)

  ;;; Velocity of the sound at 22°C, 1 atmosfera
  (defvar *sound-velocity* 345)

  (declaim (special *audio-driver*
                    *sample-type*
                    *c-compiler*
                    *tlsf-block-align*
                    *use-foreign-sample-p*
                    *frames-per-buffer*
                    *client-name*
                    *max-number-of-channels*
                    *number-of-input-bus-channels*
                    *number-of-output-bus-channels*
                    *number-of-bus-channels*
                    *rt-edf-heap-size*
                    *nrt-edf-heap-size*
                    *rt-priority*
                    *nrt-priority*
                    *receiver-default-priority*
                    *max-number-of-nodes*
                    *default-table-size*
                    *fade-curve*
                    *standard-optimize-settings*
                    *foreign-sample-pool-size*
                    *foreign-rt-memory-pool-size*
                    *sndfile-buffer-size*
                    *bounce-to-disk-guard-size*
                    *default-header-type*
                    *default-data-format*))

  (defvar *incudinerc-loaded-p* nil)

  (defun load-incudinerc ()
    (let ((*package* (find-package :incudine.config)))
      (setf *incudinerc-loaded-p*
            (let ((init-file (merge-pathnames ".incudinerc"
                                              (user-homedir-pathname))))
              (if (probe-file init-file)
                  (load init-file)
                  t)))))

  (unless *incudinerc-loaded-p*
    (load-incudinerc))

  (defvar *sample-type* 'double-float)

  ;;; Real time audio:
  ;;;
  ;;;     :JACK
  ;;;     :PORTAUDIO
  ;;;     :PORTAUDIO-JACK
  ;;;
  ;;; :PORTAUDIO and :PORTAUDIO-JACK are the same, but with the last
  ;;; it is possible to set the Jack client name.
  ;;;
  (defvar *audio-driver*
    #+linux :jack
    #-linux :portaudio)

  (defvar incudine.util:*audio-driver* *audio-driver*)

  (defvar incudine.util:*sample-type*
    (if (eq *sample-type* 'single-float)
        'single-float
        'double-float))  ; default

  ;; The default is "cc" but if it is not available, it will be changed
  ;; with "gcc" in CHECK-C-COMPILER
  (defvar *c-compiler* "cc")

  (defvar *c-compiler-flags*
    (concatenate 'string "-O3 -Wall"
                 #-cygwin " -fPIC"
                 #+linux " -DLINUX"
                 (if (eq incudine.util:*sample-type* 'double-float)
                     " -D__INCUDINE_USE_64_BIT_SAMPLE__")))

  (defvar *c-linker-flags*
    (concatenate 'string
                 #+darwin "-bundle"
                 #-darwin "-shared"
                 " -lpthread -lm"))

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

  (defvar *c-library-path*
    (make-pathname :name *c-library-name* :type *c-library-type*
                   :defaults *c-source-dir*))

  (defun get-c-source-by-key (key)
    (second (assoc key *c-source-deps-alist*)))

  (defun get-c-object-by-key (key)
    (c-object-pathname (get-c-source-by-key key)))

  (defun missing-c-object-p (key)
    (not (probe-file (get-c-object-by-key key))))

  (defun changed-c-files ()
    (let ((c-library-write-date (file-write-date *c-library-path*)))
      (remove-if-not (lambda (x) (< c-library-write-date x))
                     *c-source-pathnames* :key #'file-write-date)))

  (defvar *cache-pathname*
    (merge-pathnames "cache.lisp" *c-source-dir*))

  (defun get-compiler-options ()
    (let ((drv (case *audio-driver*
                 ((:jack :portaudio) *audio-driver*)
                 ((:portaudio-jack) :portaudio)
                 (otherwise #+linux :jack #-linux :portaudio))))
      (list
       ;; The CAR is the option and the CDR is the influenced object.
       ;; The keyword :ALL specifies all the objects.
       (cons *c-compiler* :all)
       (cons *sample-type* :all)
       (cons *tlsf-block-align* :tlsf)
       (cons *audio-driver* drv))))

  (defun store-compiler-options ()
    (with-open-file (f *cache-pathname* :direction :output
                     :if-exists :supersede)
      (write-line ";;;; This file is automatically generated." f)
      (write (get-compiler-options) :stream f)))

  (defun changed-compiler-options ()
    (if (not (probe-file *cache-pathname*))
        :all
        (with-open-file (f *cache-pathname*)
          (loop for old in (read f)
                for new in (get-compiler-options)
                unless (equal old new)
                do (if (eq (cdr new) :all)
                       (return-from changed-compiler-options :all))
                and collect (cdr new)))))

  (defun c-objects-to-compile ()
    (flet ((return-all-objects ()
             (mapcar #'car *c-source-deps-alist*)))
      (if (not (probe-file *c-library-path*))
          (return-all-objects)
          (let ((changed-opts (changed-compiler-options)))
            (declare (type (or keyword list) changed-opts))
            (if (eq changed-opts :all)
                (return-all-objects)
                (let ((changed-src (changed-c-files)))
                  (union changed-opts
                         (loop for l in *c-source-deps-alist*
                               if (intersection (cdr l) changed-src)
                               collect (car l)))))))))

  (defun process-exit-code (process)
    #+sbcl (sb-ext:process-exit-code process))

  (defun invoke (program &rest args)
    (let ((exit-code 1))
      (values
        (with-output-to-string (s)
          (setf exit-code
                (process-exit-code
                 #-sbcl (error "Currently, Incudine works only with SBCL.")
                 #+sbcl (sb-ext:run-program program args :output s
                                            :error :output :search t))))
        exit-code)))

  (defmacro exit-code (process-output)
    (alexandria:with-gensyms (out exit-code)
      `(multiple-value-bind (,out ,exit-code) ,process-output
         (declare (ignore ,out))
         ,exit-code)))

  (defvar *c-libtest-fmt*
    (format nil "~A -o /dev/null ~S -l" *c-compiler*
            (namestring (merge-pathnames "nothing.c" *c-source-dir*))))

  (defun probe-c-library (name)
    (zerop (exit-code (invoke "sh" "-c"
                              (concatenate 'string *c-libtest-fmt* name)))))

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
      (if (zerop (exit-code (invoke "sh" "-c" cmd)))
          obj
          (error "compile C library failed"))))

  (defun get-audio-driver ()
    (let ((jack-or-pa "Incudine requires Jack or PortAudio"))
      (case incudine.util:*audio-driver*
        (:jack
         (cond ((probe-c-library "jack") :jack)
               ((probe-c-library "portaudio")
                (warn "Jack not installed; audio driver changed to PortAudio.")
                (setf incudine.util:*audio-driver* :portaudio))
               (t (error jack-or-pa))))
        (:portaudio
         (cond ((probe-c-library "portaudio") :portaudio)
               ((probe-c-library "jack")
                (warn "PortAudio not installed; audio driver changed to Jack")
                (setf incudine.util:*audio-driver* :jack))
               (t (error jack-or-pa))))
        (:portaudio-jack
         (cond ((probe-c-library "portaudio")
                (if (probe-c-library "jack")
                    :portaudio-jack
                    (progn
                      (warn "Jack not installed; ~
                             no JACK-specific extensions for PortAudio.")
                      (setf incudine.util:*audio-driver* :portaudio))))
               ((probe-c-library "jack")
                (warn "PortAudio not installed; audio driver changed to Jack")
                (setf incudine.util:*audio-driver* :jack))
               (t (error jack-or-pa))))
        (t (unless (null incudine.util:*audio-driver*)
             (warn "~S is unknown; *AUDIO-DRIVER* must be ~
                    :JACK, :PORTAUDIO OR :PORTAUDIO-JACK"
                   incudine.util:*audio-driver*))
           ;; The default is Jack for Linux and PortAudio for the other OS.
           (setf incudine.util:*audio-driver*
                 (cond
           #+linux ((probe-c-library "jack") :jack)
                   ((probe-c-library "portaudio") :portaudio)
           #-linux ((probe-c-library "jack") :jack)
                   (t (error jack-or-pa))))))))

  (defmacro force-compile-system ()
    (when (find-package "INCUDINE-SYSTEM")
      `(setf ,(find-symbol "*INCUDINE-FORCE-COMPILE-P*" "INCUDINE-SYSTEM") t)))

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
             (zerop (exit-code (invoke "which" cc)))))
      (unless (stringp *c-compiler*)
        (warn "*C-COMPILER* is ~S but it should be a string" *c-compiler*)
        (setf *c-compiler* "cc"))
      (or (check *c-compiler*)
          ;; Try an alternative.
          (let ((cc (if (string= *c-compiler* "cc") "gcc" "cc")))
            (cond ((check cc)
                   (warn "no ~S in the search path; C compiler changed to ~S"
                         *c-compiler* cc)
                   (setf *c-compiler* cc)
                   t)
                  (t (error "no ~S or ~S in the search path"
                            *c-compiler* cc)))))))

  (defun %compile-c-library (ofiles libs-dep)
    (let ((cmd (format nil "~A ~A ~A ~{ -l~A~} -o ~S~{ ~A~}"
                       *c-compiler* *c-compiler-flags* *c-linker-flags*
                       libs-dep (namestring *c-library-path*) ofiles)))
      (write-line cmd)
      (unless (zerop (exit-code (invoke "sh" "-c" cmd)))
        (error "compilation of C library failed"))
      (store-compiler-options)
      (force-compile-system)
      (values)))

  (defun compile-c-library ()
    (let ((objs (c-objects-to-compile)))
      (when objs
        (check-c-compiler)
        (flet ((to-compile-p (key)
                 (or (member key objs) (missing-c-object-p key))))
          (let ((ofiles nil)
                (libs-dep nil)
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
              (add-c-object-to-link :mouse ofiles))
            ;; Utilities
            (add-c-object-to-link :util ofiles)
            ;; TLSF Memory Storage allocator
            (add-c-object-to-link :tlsf ofiles nil
              (format nil "~:[~;-DTLSF_STATISTIC=1 ~]-DBLOCK_ALIGN=~D"
                      *tlsf-statistic* *tlsf-block-align*))
            ;; Linking
            (%compile-c-library ofiles libs-dep))))))

    (compile-c-library))
