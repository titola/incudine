;;; CL:*FEATURES* is updated using a temporary package before reading
;;; the ASDF system definition for Incudine.

(defpackage "INCUDINE.FEATURES" (:use :cl))
(in-package "INCUDINE.FEATURES")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #.(load-time-value
            (merge-pathnames "config-specials.lisp" *load-pathname*)))

  (define-condition incudine-config-compile-error (simple-error) ())

  (defun compile-error (format-control &rest format-arguments)
    (error 'incudine-config-compile-error
           :format-control format-control
           :format-arguments format-arguments))

  (defmacro add-feature (x) `(pushnew ,x *features*))

  ;;; MINI-EVAL copied from `cffi/src/libraries.lisp'.
  ;;; Simple EVAL-like function to evaluate the elements of
  ;;; CFFI:*FOREIGN-LIBRARY-DIRECTORIES* and CFFI:*DARWIN-FRAMEWORK-DIRECTORIES*.
  (defun cffi-mini-eval (form)
    (typecase form
      (cons (apply (car form) (mapcar #'cffi-mini-eval (cdr form))))
      (symbol (symbol-value form))
      (t form)))

  (defparameter *c-compiler* nil)
  (defparameter *foreign-library-directories* nil)
  (defparameter *foreign-header-file-directories* nil)
  (defparameter *audio-driver* nil)
  (defparameter *enable-jack-midi* nil)
  (defparameter cl-user::__incudine_c_compiler__ "")
  (defparameter cl-user::__incudine_c_library_paths__ "")
  (defparameter cl-user::__incudine_c_header_file_paths__ "")
  (defparameter *c-libtest-fmt* "")

  (defun c-header-test (library-name)
    (cdr (assoc library-name
                '(("jack" . "jack/jack.h")
                  ("portaudio" . "portaudio.h")
                  ("X11" . "X11/Intrinsic.h"))
                :test #'string=)))

  (defun probe-c-library (name)
    (flet ((test-library (&optional header-file error-output)
             (zerop (third (multiple-value-list
                             (uiop:run-program
                               (format nil "~A ~A -DINCUDINE_CONFIG_TEST_HEADER='<~A>'"
                                       *c-libtest-fmt* name (or header-file "stdlib.h"))
                               :ignore-error-status t :error-output error-output))))))
      (when (test-library)
        ;; Testing directory for header files.
        (or (test-library (c-header-test name) t)
            (funcall (if (string= name "X11")
                         ;; X11 is required only for mouse support.
                         #'warn
                         #'compile-error)
              "The header files for the C library ~S are not correctly ~
               installed.~%   See also the configuration variable ~
               *FOREIGN-HEADER-FILE-DIRECTORIES*"
              name)))))

  (defun real-time-audio-error (required libname)
    (compile-error "The configuration variable *AUDIO-DRIVER* is set to ~S~%~
                    but the ~A library was not found."
                   required libname))

  (defun no-real-time-audio-warning ()
    (warn "Incudine without real time audio support."))

  (defun set-real-time-audio-feature (audio-driver)
    (add-feature
      (case audio-driver
        (:jack
         (cond ((probe-c-library "jack") :jack-audio)
               (*audio-driver* (real-time-audio-error :jack "JACK"))
               ((probe-c-library "portaudio") :portaudio)
               (t (no-real-time-audio-warning)
                  :dummy-audio)))
        ((:portaudio :portaudio-jack)
         (cond ((probe-c-library "portaudio")
                (if (and (eq audio-driver :portaudio-jack)
                         (not (probe-c-library "jack")))
                    (real-time-audio-error :portaudio-jack
                      "JACK-specific extensions for PortAudio")
                    :portaudio))
               (*audio-driver* (real-time-audio-error :portaudio "PortAudio"))
               ((probe-c-library "jack") :jack-audio)
               (t (no-real-time-audio-warning)
                  :dummy-audio)))
        (otherwise
         (unless *audio-driver*
           (no-real-time-audio-warning))
         :dummy-audio)))
    (when (and *enable-jack-midi* (uiop:featurep :jack-audio))
      (add-feature :jack-midi))
    nil))

(let ((init-file (merge-pathnames ".incudinerc" (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file
      :external-format '(:utf-8 :replacement #\replacement_character)))
  (setf cl-user::__incudine_c_compiler__
        (flet ((check (cc)
                 (and (zerop
                        (third (multiple-value-list
                                 (uiop:run-program (format nil "which ~A" cc)
                                                   :ignore-error-status t))))
                      cc)))
          (let ((cc (when *c-compiler*
                      (if (stringp *c-compiler*)
                          (or (and (string/= *c-compiler* "")
                                   (check *c-compiler*))
                              (warn "The specified C compiler ~S was not found."
                                    *c-compiler*))
                          (warn "The configuration variable *C-COMPILER* ~
                                 is of type ~A but it should be a string."
                                (type-of *c-compiler*))))))
            (or cc (check "cc") (check "gcc")
                (compile-error "Command \"cc\" or \"gcc\" not found.")))))
  (setf cl-user::__incudine_c_library_paths__
        (progn
          (setf cffi:*foreign-library-directories*
                (union cffi:*foreign-library-directories*
                       *foreign-library-directories*
                       :test #'equal))
          (format nil "~{ -L\"~A\"~}"
            (alexandria:flatten
              (mapcar #'cffi-mini-eval cffi:*foreign-library-directories*)))))
  (setf cl-user::__incudine_c_header_file_paths__
        (format nil "~{ -I\"~A\"~}"
          (alexandria:flatten
            (mapcar #'cffi-mini-eval *foreign-header-file-directories*))))
  (setf *c-libtest-fmt*
        (format nil "~A -o /dev/null~A~A ~S -l"
                cl-user::__incudine_c_compiler__
                cl-user::__incudine_c_header_file_paths__
                cl-user::__incudine_c_library_paths__
                (namestring
                  (merge-pathnames "config_test.c"
                    (make-pathname :name nil :type nil
                      :defaults #.(load-time-value *load-pathname*))))))
  (when (and (boundp '*fifo-buffer-type*)
             (eq *fifo-buffer-type* 'list))
    (add-feature :incudine-fifo-circular-list))
  (set-real-time-audio-feature
    (if *audio-driver*
        (if (member *audio-driver* '(:jack :portaudio :portaudio-jack :dummy))
            *audio-driver*
            (compile-error
              "~S is unknown; the configuration variable *AUDIO-DRIVER* ~
               must be~%  :JACK, :PORTAUDIO, :PORTAUDIO-JACK or :DUMMY"
              *audio-driver*))
        #+linux :jack
        #-linux :portaudio))
  (when (probe-c-library "X11")
    (add-feature :x11)))

(in-package :cl-user)
(delete-package "INCUDINE.FEATURES")
