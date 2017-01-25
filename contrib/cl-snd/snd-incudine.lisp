;;; Copyright (c) 2015-2017 Tito Latini
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
            buffer->sound buffer->mix
            map-channel env-channel env-selection)))

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
    `(let ((,fname (eval (format nil ,control-string ,@format-args))))
       (when (and (stringp ,fname) (probe-file ,fname))
         (apply #'incudine:buffer-load ,fname ,args)))))

(defun sound->buffer (id-or-filename &rest buffer-load-args)
  "The sound ID-OR-FILENAME is loaded in a new INCUDINE:BUFFER structure.
BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type (or fixnum string) id-or-filename))
  (with-buffer-load buffer-load-args "
 (let* ((x ~S)
        (s (if (number? x) (integer->sound x) (find-sound x))))
   (when (sound? s)
     (save-sound-as ~S s :header-type mus-riff :sample-type mus-ldouble)))
"
                    id-or-filename *tmpfile*))

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
If the first argument in ARGS is a list, it has the arguments for the
Snd function `mix' (file is optional). The rest of the arguments are
passed to INCUDINE:BUFFER-SAVE."
  (declare (type incudine:buffer buf))
  (multiple-value-bind (mix-args bsave-args sfile)
      (if (keywordp (first args))
          (values nil args *tmpfile*)
          (let ((mix-args (first args)))
            (if (stringp (first mix-args))
                (values (rest mix-args) (rest args) (first mix-args))
                (values mix-args (rest args) *tmpfile*))))
    (with-buffer-save (buf f sfile bsave-args)
      (mix f mix-args))))

(defun map-channel-new-vec (buffer function beg end)
  (do ((i 0 (1+ i))
       (stop nil)
       (vec (make-array (incudine:buffer-size buffer) :adjustable t
                        :fill-pointer 0)))
      ((>= i (incudine:buffer-size buffer)) vec)
    (when (and end (>= i end))
      (setf stop t))
    (let* ((val (incudine:buffer-value buffer i))
           (ret (if (or stop (< i beg))
                    val
                    (funcall function val))))
      (cond ((numberp ret)
             (vector-push-extend ret vec))
            ((and (listp ret) (every #'realp ret))
             (dolist (x ret) (vector-push-extend x vec)))
            ((and (vectorp ret) (every #'realp ret))
             (dotimes (j (length ret))
               (vector-push-extend (svref ret j) vec)))
            ((eq ret t)
             (setf stop t)
             (vector-push-extend val vec))))))

(defun sound-and-channel (snd chn)
  (let* ((snd (cond ((integerp snd)
                     (format nil "(integer->sound ~D)" snd))
                    ((stringp snd)
                     (format nil "(find-sound ~S)" snd))
                    (t
                     "(selected-sound)")))
         (chn (or chn (format nil "(or (selected-channel ~A) 0)" snd))))
    (values snd chn)))

(defun map-channel (function &key (beg 0) dur snd chn
                    (origin "incudine map-channel"))
  "Similar to map-channel in Snd, it applies FUNCTION to each sample
of the channel CHN in SND (id or filename), starting at sample BEG for
DUR samples, replacing the current value with whatever FUNCTION returns.
FUNCTION is a procedure of one argument (the current sample), can
return NIL, which means that the data passed in is deleted (replaced by
nothing), or a number which replaces the current sample, or T which
halts the mapping operation, leaving trailing samples unaffected, or a
sequence the contents of which are spliced into the edited version,
effectively replacing the current sample with any number of samples.
BEG defaults to 0 and DUR defaults to the full length of the sound.
SND and CHN default to the currently selected sound."
  (declare (type function function)
           (type alexandria:non-negative-fixnum beg)
           (type (or alexandria:positive-fixnum null) dur)
           (type (or alexandria:non-negative-fixnum string null) snd)
           (type (or alexandria:non-negative-fixnum null) chn)
           (type string origin))
  (multiple-value-bind (snd chn) (sound-and-channel snd chn)
    (let ((buf (with-buffer-load ()
                 "(let ((s ~A)) ~
                    (when (sound? s) ~
                      (save-sound-as ~S s :channel ~A ~
                                     :header-type mus-riff ~
                                     :sample-type mus-ldouble)))"
                 snd *tmpfile* chn)))
      (when buf
        (unwind-protect
             (let ((vec (map-channel-new-vec buf function beg
                                             (and dur (+ beg dur)))))
               (incudine:with-buffer (new (length vec) :initial-contents vec)
                 (incudine:buffer-save new *tmpfile* :header-type "wav"
                                       :data-format "double")
                 (eval (format nil "(as-one-edit ~
                                      (lambda () ~
                                        (let ((s ~A) (c ~A)) ~
                                          (delete-samples 0 (framples s c) s c) ~
                                          (insert-channel ~S :snd s :chn c) ~
                                          s)) ~
                                      ~S)"
                               snd chn *tmpfile* origin))))
          (incudine:free buf))))))

(defun env-channel (env &key (beg 0) dur snd chn (origin "incudine env-channel"))
  "Similar to env-channel in Snd, it applies the amplitude envelope ENV, an
INCUDINE:ENVELOPE structure, to the given channel CHN in SND starting at
sample BEG for DUR samples.
BEG defaults to 0 and DUR defaults to the full length of the sound.
SND and CHN default to the currently selected sound."
  (declare (type incudine:envelope env)
           (type alexandria:non-negative-fixnum beg)
           (type (or alexandria:positive-fixnum null) dur)
           (type (or alexandria:non-negative-fixnum string null) snd)
           (type (or alexandria:non-negative-fixnum null) chn)
           (type string origin))
  (multiple-value-bind (snd chn) (sound-and-channel snd chn)
    (let ((frames (eval (format nil "(let ((s ~A)) (if (sound? s) (framples s ~A)))"
                                snd chn))))
      (when (and frames (plusp frames))
        (let* ((beg (min beg (1- frames)))
               (maxdur (- frames beg))
               (dur (if dur (min dur maxdur) maxdur)))
          (incudine:with-buffer (buf dur
                                 :fill-function (gen:envelope env :periodic-p nil))
            (incudine:buffer-save buf *tmpfile* :header-type "wav"
                                  :data-format "double")
            (eval (format nil "(let ((s ~A) (rd (make-sampler 0 ~S))) ~
                                 (map-channel (lambda (x) (* x (rd))) ~
                                              ~D ~D s ~A current-edit-position ~
                                              ~S) ~
                                 (free-sampler rd) s)"
                          snd *tmpfile* beg dur chn origin))))))))

(defun env-selection (env &key (origin "incudine env-selection"))
  "Similar to env-selection in Snd, it applies the amplitude envelope ENV,
an INCUDINE:ENVELOPE structure, to the selection."
  (declare (type incudine:envelope env) (type string origin))
  (when (eval "(selection?)")
    (let ((info (eval (format nil
                        "(map (lambda (s) ~
                                (list (sound->integer s) ~
                                      (do ((c 0 (+ c 1)) ~
                                           (res '())) ~
                                          ((= c (channels s)) (reverse res)) ~
                                        (set! res (cons ~
                                                    (list (selection-position s c) ~
                                                          (selection-framples s c)) ~
                                                    res))))) ~
                            (sounds))"))))
      (loop for (snd chans-info) in info
            do (loop for (pos frm) in chans-info
                     for chn from 0
                     when (plusp frm)
                       do (env-channel env :beg pos :dur frm :snd snd :chn chn
                                       :origin origin))))))

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
        (if (keywordp (first args))
            (values nil args)
            (values (first args) (rest args)))
      `(with-bounce-to-snd-object (,fname ,outfile ,bounce-args ,body)
         (snd:mix ,fname ,mix-args)))))
