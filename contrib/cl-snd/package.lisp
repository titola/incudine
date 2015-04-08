(defpackage :snd
  (:use :cl)
  (:shadow #:eval #:load)
  (:export #:*program-name* #:*program-args*
           #:run #:eval #:load #:exit #:flush-stream #:close-stream))

(in-package :snd)

(defvar *program-name* "snd"
  "Snd command line program name.")
(declaim (type string *program-name*))

(defvar *program-args* nil
  "Snd command line argument list.")
(declaim (type list *program-args*))

(defvar *snd* nil "Snd process.")
