(defpackage :snd
  (:use :cl)
  (:shadow #:eval #:load)
  (:export #:*program-name* #:*program-args* #:snd-error
           #:run #:eval #:load #:exit #:flush-stream #:close-stream
           #:emacs-mode-p #:enable-sharp-s7-syntax))

(in-package :snd)

(defvar *program-name* "snd"
  "Snd command line program name.")
(declaim (type string *program-name*))

(defvar *program-args* nil
  "Snd command line argument list.")
(declaim (type list *program-args*))

(defvar *snd* nil "Snd process.")
