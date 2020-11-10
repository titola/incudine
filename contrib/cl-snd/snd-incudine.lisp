;;; Copyright (c) 2015-2020 Tito Latini
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
  (export '(*tmpfile* open-or-update-sound mix float-vector
            sound->buffer selection->buffer region->buffer mix->buffer
            float-vector->buffer buffer->sound buffer->mix
            map-channel env-channel env-selection)))

(defvar *tmpfile* "/tmp/incudine-snd.snd"
  "Full path of the temporary soundfile.")

(enable-sharp-s7-syntax)

(defun open-or-update-sound (file)
  "If no sound is found that matches the soundfile FILE, open that
file in Snd. Otherwise, update the sound."
  (eval
    #s7(let* ((f ~S)
              (s (find-sound f)))
         (if s (update-sound s) (open-sound f)))
    :format-arguments (list (truenamestring file))))

(defun arg-list (lst)
  (mapcar (lambda (arg)
            (cond ((null arg) "#f")
                  ((eq arg t) "#t")
                  (t arg)))
          lst))

(defun mix (file &optional arguments (output-p t))
  "Mix the FILE in Snd by applying the Snd function MIX to the ARGUMENTS.

Suppress output if OUTPUT-P is NIL."
  (declare (type (or pathname string) file) (type list arguments)
           (type boolean output-p))
  (eval (format nil "(mix ~S~@[ ~{~A~^ ~}~])" (truenamestring file)
                (arg-list arguments))
        :output-p output-p))

(defmacro with-buffer-load (args control-string &rest format-args)
  (let ((fname (gensym "FILENAME")))
    `(let ((,fname (eval (format nil ,control-string ,@format-args))))
       (when (and (stringp ,fname) (incudine.util::probe-file* ,fname))
         (apply #'incudine:buffer-load ,fname ,args)))))

(defun sound->buffer (id-or-filename &rest buffer-load-args)
  "Create a new INCUDINE:BUFFER structure by loading the sound
ID-OR-FILENAME.

BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type (or fixnum string) id-or-filename))
  (with-buffer-load buffer-load-args
    #s7(let* ((x ~S)
              (s (if (number? x) (integer->sound x) (find-sound x))))
         (when (sound? s)
           (save-sound-as ~S s :header-type mus-riff :sample-type mus-ldouble)))
    id-or-filename *tmpfile*))

(defun selection->buffer (&rest buffer-load-args)
  "Create a new INCUDINE:BUFFER structure by loading the current selection.

BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (with-buffer-load buffer-load-args
    #s7(save-selection ~S :header-type mus-riff :sample-type mus-ldouble)
    *tmpfile*))

(defun region->buffer (region-id &rest buffer-load-args)
  "Create a new INCUDINE:BUFFER structure by loading the region REGION-ID.

BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type fixnum region-id))
  (with-buffer-load buffer-load-args
    #s7(save-region (integer->region ~D) ~S :header-type mus-riff
                    :sample-type mus-ldouble)
    region-id *tmpfile*))

(defun mix->buffer (mix-id &rest buffer-load-args)
  "Create a new INCUDINE:BUFFER structure by loading the mix MIX-ID.

BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type fixnum mix-id))
  ;; libsndfile doesn't support the next/ldouble format used by Snd,
  ;; therefore the soundfile is rewritten with riff/ldouble header
  ;; on little endian machines.
  (with-buffer-load buffer-load-args
    #s7(let* ((tmpfile \"~A_\")
              (res (save-mix (integer->mix ~D) tmpfile)))
         (when res
           (unless (and (= (mus-sound-sample-type tmpfile) mus-bdouble)
                        (= (mus-sound-header-type tmpfile) mus-next))
             (open-sound tmpfile)
             (set! res (save-sound-as ~S :header-type mus-riff
                                      :sample-type mus-ldouble))
             (close-sound))
           res))
    *tmpfile* mix-id *tmpfile*))

(defun float-vector->buffer (fvec-string &rest buffer-load-args)
  "Create a new INCUDINE:BUFFER structure by loading the float-vector
FVEC-STRING.

BUFFER-LOAD-ARGS are the optional arguments for INCUDINE:BUFFER-LOAD."
  (declare (type string fvec-string))
  (with-buffer-load buffer-load-args
    #s7(with-sound (:output ~S :clipped #f :to-snd #f
                    :header-type mus-riff :sample-type mus-ldouble)
         (let* ((v ~A) (end (length v)))
           (do ((i 0 (+ i 1))) ((= i end))
             (outa i (v i)))))
    *tmpfile* fvec-string))

(defmacro with-buffer-save ((buf fname-var path args) &body body)
  `(let ((,fname-var ,path)
         (incudine.util:*default-header-type* "wav")
         (incudine.util:*default-data-format* "double"))
     (apply #'incudine:buffer-save ,buf ,fname-var ,args)
     ,@body))

(defun buffer->sound (buf file &rest buffer-save-args)
  "The content of a INCUDINE:BUFFER structure is saved in the soundfile FILE
and opened in Snd.

BUFFER-SAVE-ARGS are the arguments for INCUDINE:BUFFER-SAVE."
  (declare (type incudine:buffer buf) (type (or pathname string) file))
  (with-buffer-save (buf f file buffer-save-args)
    (open-or-update-sound f)))

(defun buffer->mix (buf &rest args)
  "The content of a INCUDINE:BUFFER structure is mixed in Snd.

If the first argument in ARGS is a list, it has the arguments for the
Snd function MIX (file is optional). The rest of the arguments are
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

(defun buffer->float-vector (buf fvec-name &rest buffer-save-args)
  (declare (type incudine:buffer buf) (type string fvec-name))
  (with-buffer-save (buf f *tmpfile* buffer-save-args)
    (eval
      #s7(begin
           (define ~A
             (let* ((sf ~S)
                    (size (mus-sound-samples sf))
                    (v (make-float-vector size))
                    (rd (make-sampler 0 sf)))
               (do ((i 0 (+ i 1)))
                   ((= i size) (free-sampler rd) v)
                 (float-vector-set! v i (rd)))))
           (length ~A))
      :format-arguments (list fvec-name f fvec-name))))

(defun float-vector (fvec-name obj &optional (start 0) (end 0))
  "Define a float-vector in Snd named FVEC-NAME initialized with the
content of a INCUDINE:BUFFER structure, a list, a vector or END minus
START values generated by a GEN routine.

The sequence is optionally bounded by START and END."
  (declare (type string fvec-name)
           (type (or incudine:buffer cons vector function) obj)
           (type alexandria:non-negative-fixnum start end))
  (cond ((incudine:buffer-p obj)
         (buffer->float-vector obj fvec-name :start start :end end))
        ((or (consp obj) (vectorp obj))
         (let ((seq (if (> end start) (subseq obj start end) obj)))
           (incudine:with-buffer (buf (length seq) :initial-contents seq)
             (buffer->float-vector buf fvec-name))))
        ((and (functionp obj) (> end start))
         (incudine:with-buffer (buf (- end start) :fill-function obj)
           (buffer->float-vector buf fvec-name)))
        (t 0)))

(defun map-channel-new-vec (buffer function beg end)
  (incudine:with-local-time ()
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
                      (prog1 (funcall function val)
                        (incf (incudine:now))))))
        (cond ((numberp ret)
               (vector-push-extend ret vec))
              ((and (listp ret) (every #'realp ret))
               (dolist (x ret) (vector-push-extend x vec)))
              ((and (vectorp ret) (every #'realp ret))
               (dotimes (j (length ret))
                 (vector-push-extend (svref ret j) vec)))
              ((eq ret t)
               (setf stop t)
               (vector-push-extend val vec)))))))

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
  "Apply FUNCTION to each sample of the channel CHN in SND (id or
filename), starting at sample BEG for DUR samples, replacing the
current value with whatever FUNCTION returns.

FUNCTION is a procedure of one argument (the current sample), can
return NIL, which means that the data passed in is deleted (replaced by
nothing), or a number which replaces the current sample, or T which
halts the mapping operation, leaving trailing samples unaffected, or a
sequence the contents of which are spliced into the edited version,
effectively replacing the current sample with any number of samples.

The utility INCUDINE:NOW called from FUNCTION returns the current
local time.

BEG defaults to 0 and DUR defaults to the full length of the sound.

SND and CHN default to the currently selected sound.

See map-channel in Snd."
  (declare (type function function)
           (type alexandria:non-negative-fixnum beg)
           (type (or alexandria:positive-fixnum null) dur)
           (type (or alexandria:non-negative-fixnum string null) snd)
           (type (or alexandria:non-negative-fixnum null) chn)
           (type string origin))
  (multiple-value-bind (snd chn) (sound-and-channel snd chn)
    (let ((buf (with-buffer-load ()
                 #s7(let ((s ~A))
                      (when (sound? s)
                        (save-sound-as ~S s :channel ~A
                                       :header-type mus-riff
                                       :sample-type mus-ldouble)))
                 snd *tmpfile* chn)))
      (when buf
        (incudine::maybe-unwind-protect
             (let ((vec (map-channel-new-vec buf function beg
                                             (and dur (+ beg dur)))))
               (incudine:with-buffer (new (length vec) :initial-contents vec)
                 (incudine:buffer-save new *tmpfile* :header-type "wav"
                                       :data-format "double")
                 (eval
                   #s7(as-one-edit
                       (lambda ()
                         (let ((s ~A) (c ~A))
                           (delete-samples 0 (framples s c) s c)
                           (insert-channel ~S :snd s :chn c)
                           s))
                       ~S)
                   :format-arguments (list snd chn *tmpfile* origin))))
          (incudine:free buf))))))

(defun env-channel (env &key (beg 0) dur snd chn (origin "incudine env-channel"))
  "Apply the amplitude envelope ENV, an INCUDINE:ENVELOPE structure,
to the given channel CHN of SND starting at sample BEG for DUR samples.

BEG defaults to 0 and DUR defaults to the full length of the sound.

SND and CHN default to the currently selected sound.

See env-channel in Snd."
  (declare (type incudine:envelope env)
           (type alexandria:non-negative-fixnum beg)
           (type (or alexandria:positive-fixnum null) dur)
           (type (or alexandria:non-negative-fixnum string null) snd)
           (type (or alexandria:non-negative-fixnum null) chn)
           (type string origin))
  (multiple-value-bind (snd chn) (sound-and-channel snd chn)
    (let ((frames (eval #s7(let ((s ~A)) (if (sound? s) (framples s ~A)))
                        :format-arguments (list snd chn))))
      (when (and frames (plusp frames))
        (let* ((beg (min beg (1- frames)))
               (maxdur (- frames beg))
               (dur (if dur (min dur maxdur) maxdur)))
          (incudine:with-buffer (buf dur
                                 :fill-function (gen:envelope env :periodic-p nil))
            (incudine:buffer-save buf *tmpfile* :header-type "wav"
                                  :data-format "double")
            (eval
              #s7(let ((s ~A) (rd (make-sampler 0 ~S)))
                   (map-channel (lambda (x) (* x (rd)))
                                ~D ~D s ~A current-edit-position ~S)
                   (free-sampler rd) s)
              :format-arguments (list snd *tmpfile* beg dur chn origin))))))))

(defun env-selection (env &key (origin "incudine env-selection"))
  "Apply the amplitude envelope ENV, an INCUDINE:ENVELOPE structure,
to the selection.

See env-selection in Snd."
  (declare (type incudine:envelope env) (type string origin))
  (when (eval "(selection?)")
    (let ((info (eval
                  #s7(map (lambda (s)
                            (list (sound->integer s)
                                  (do ((c 0 (+ c 1))
                                       (res '()))
                                      ((= c (channels s)) (reverse res))
                                    (set! res (cons
                                                (list (selection-position s c)
                                                      (selection-framples s c))
                                                res)))))
                          (sounds)))))
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
  "Apply BOUNCE-TO-DISK to OUTFILE and the arguments ARGS, then open
OUTFILE in Snd."
  (with-gensyms (fname)
    `(with-bounce-to-snd-object (,fname ,outfile ,args ,body)
       (snd:open-or-update-sound ,fname))))

(defmacro bounce-to-snd-mix ((outfile &rest args) &body body)
  "Apply BOUNCE-TO-DISK to OUTFILE and the arguments ARGS, then mix
OUTFILE in Snd by calling MIX.

If the first argument in ARGS is a list, it has the arguments for the
Snd function MIX (without file). The rest of the arguments are passed
to BOUNCE-TO-DISK.

Example:

    (dsp! hello-snd (c i a o)
      (stereo (+ (sine c i) (sine a o))))

    (bounce-to-snd-mix (\"mix-1.wav\" '(48000 t))
      (hello-snd 1000 .2 1220 .1))"
  (with-gensyms (fname)
    (multiple-value-bind (mix-args bounce-args)
        (if (keywordp (first args))
            (values nil args)
            (values (first args) (rest args)))
      `(with-bounce-to-snd-object (,fname ,outfile ,bounce-args ,body)
         (snd:mix ,fname ,mix-args)))))
