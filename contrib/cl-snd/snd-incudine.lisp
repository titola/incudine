;;; Copyright (c) 2015 Tito Latini
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

(in-package :snd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*tmpfile* open-or-update-sound mix sound->buffer
            selection->buffer region->buffer mix->buffer
            buffer->sound buffer->mix)))

(defvar *tmpfile* "/tmp/incudine-snd.snd"
  "Full path of the temporary soundfile.")

(defun open-or-update-sound (file)
     (eval (format nil "
 (let* ((f ~S)
        (s (find-sound f)))
   (if s (update-sound s) (open-sound f)))
"
                   (truenamestring file))))

(defun arg-list (lst)
  (mapcar (lambda (arg)
            (cond ((null arg) "#f")
                  ((eq arg t) "#t")
                  (t arg)))
          lst))

(defun mix (file &optional args (output-p t))
  (declare (type (or pathname string) file) (type list args)
           (type boolean output-p))
  (eval (format nil "(mix ~S~@[ ~{~A~^ ~}~])" (truenamestring file)
                (arg-list args))
        :output-p output-p))

(defmacro with-buffer-load (args control-string &rest format-args)
  (let ((fname (gensym "FILENAME")))
    `(let ((,fname (snd:eval (format nil ,control-string ,@format-args))))
       (when (and (stringp ,fname) (probe-file ,fname))
         (apply #'incudine:buffer-load ,fname ,args)))))

(defun sound->buffer (id-or-path &rest buffer-load-args)
  "The sound ID-OR-PATH is loaded in a new INCUDINE:BUFFER structure.
BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type (or fixnum string) id-or-path))
  (with-buffer-load buffer-load-args "
 (let* ((x ~S)
        (s (if (number? x) (integer->sound x) (find-sound x))))
   (when (sound? s)
     (save-sound-as ~S s)))
"
                    id-or-path *tmpfile*))

(defun selection->buffer (&rest buffer-load-args)
  "The current selection is loaded in a new INCUDINE:BUFFER structure.
BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (with-buffer-load buffer-load-args
     "(save-selection ~S :header-type mus-riff :sample-type mus-ldouble)"
     *tmpfile*))

(defun region->buffer (region-id &rest buffer-load-args)
  "The region REGION-ID is loaded in a new INCUDINE:BUFFER structure.
BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type fixnum region-id))
  (with-buffer-load buffer-load-args
     "(save-region (integer->region ~D) ~S :header-type mus-riff :sample-type mus-ldouble)"
     region-id *tmpfile*))

(defun mix->buffer (mix-id &rest buffer-load-args)
  "The mix MIX-ID is loaded in a new INCUDINE:BUFFER structure.
BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type fixnum mix-id))
  ;; libsndfile doesn't support the next/ldouble format used by Snd,
  ;; therefore the soundfile is rewritten with riff/ldouble header
  ;; on little endian machines.
  (with-buffer-load buffer-load-args "
 (let* ((tmpfile \"~A_\")
        (res (save-mix (integer->mix ~D) tmpfile)))
   (when res
     (unless (and (= (mus-sound-sample-type tmpfile) mus-bdouble)
                  (= (mus-sound-header-type tmpfile) mus-next))
       (open-sound tmpfile)
       (set! res (save-sound-as ~S :header-type mus-riff :sample-type mus-ldouble))
       (close-sound))
     res))
"
                    *tmpfile* mix-id *tmpfile*))

(defmacro with-buffer-save ((buf fname-var path args) &body body)
  `(let ((,fname-var ,path)
         (incudine.util:*default-header-type* "wav")
         (incudine.util:*default-data-format* "double"))
     (apply #'incudine:buffer-save ,buf ,fname-var ,args)
     ,@body))

(defun buffer->sound (buf file &rest buffer-save-args)
  "The content of a INCUDINE:BUFFER structure is saved on the soundfile FILE
and opened in Snd. BUFFER-SAVE-ARGS are the arguments for INCUDINE:BUFFER-SAVE."
  (declare (type incudine:buffer buf) (type (or pathname string) file))
  (with-buffer-save (buf f file buffer-save-args)
    (open-or-update-sound f)))

(defun buffer->mix (buf &rest args)
  "The content of a INCUDINE:BUFFER structure is mixed in Snd.
If the first argument in ARGS is a list, it has the arguments for
the Snd function `mix'. The rest of the arguments are passed to
INCUDINE:BUFFER-SAVE."
  (declare (type incudine:buffer buf))
  (multiple-value-bind (mix-args bsave-args)
      (if (keywordp (car args))
          (values nil args)
          (values (car args) (cdr args)))
    (with-buffer-save (buf f *tmpfile* bsave-args)
      (mix f mix-args))))

(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(bounce-to-snd bounce-to-snd-mix)))

(defmacro with-bounce-to-snd-object ((fname-var path args to-bounce)
                                     &body body)
  `(let ((,fname-var ,path)
         (incudine.util:*default-header-type* "wav")
         (incudine.util:*default-data-format* "double"))
     (bounce-to-disk (,fname-var ,@args) ,@to-bounce)
     ,@body))

(defmacro bounce-to-snd ((outfile &rest args) &body body)
  (with-gensyms (fname)
    `(with-bounce-to-snd-object (,fname ,outfile ,args ,body)
       (snd:open-or-update-sound ,fname))))

(defmacro bounce-to-snd-mix ((outfile &rest args) &body body)
  (with-gensyms (fname)
    (multiple-value-bind (mix-args bounce-args)
        (if (keywordp (car args))
            (values nil args)
            (values (car args) (cdr args)))
      `(with-bounce-to-snd-object (,fname ,outfile ,bounce-args ,body)
         (snd:mix ,fname ,mix-args)))))
