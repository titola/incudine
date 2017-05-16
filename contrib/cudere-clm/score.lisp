;;; Incudine version of CLM
;;; Copyright (c) 2017 Tito Latini
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

(in-package :cudere-clm)

;;; Copied from clm-5/mus.lisp
(defun clm-cleanup ()
  (setf *statistics* nil)
  (setf *interrupted* 0)
  (setf *offset* 0)
  (clm-close-output)
  (clm-close-reverb))

;;; Copied from clm-5/sound.lisp
(defun clm-reset ()
  (clm-cleanup)
  (setf *clm-with-sound-depth* 0)
  (setf *clm-within-with-sound* nil))

;;; Copied from clm-5/sound.lisp
(defun clm-check-for-reset ()
  (when (or *clm-within-with-sound* (zerop *clm-with-sound-depth*))
    (clm-reset)))

;;; Copied from clm-5/mus.lisp
(defun initialize-statistics (stats ofile &optional rfile)
  (setf *statistics* stats)
  (setf clm-start-time (get-internal-real-time))
  (setf clm-last-begin-time 0)
  (setf clm-outfile-name ofile)
  (setf clm-revfile-name rfile))

;;; Edited from clm-5/mus.lisp
(defun search-full-merge-pathnames (pathname &optional default backup)
  (flet ((find-path (name path)
           (probe-file (merge-pathnames name path))))
    (or (find-path pathname (or default ""))
        (and backup (find-path pathname backup))
        (dolist (path *clm-search-list*)
          (let ((found (and path (find-path pathname path))))
            (when found (return found)))))))

(defun reopen-output (old-output)
  (if (soundfile:open-p old-output)
      old-output
      (soundfile:open
        (soundfile:path old-output) :direction :output :if-exists :mix)))

;;; Edited from clm-5/mus.lisp
(defun* clm-open-input (file (start 0) (channel 0))
  (declare (ignore channel))
  (let ((fname (search-full-merge-pathnames file *clm-file-name* "test.snd")))
    (cond (fname
           (let ((sf (make-file->sample (namestring fname))))
             (when (> start 0)
               (setf (soundfile:position sf) start))
             sf))
          (t
           (warn "can't find ~S" file)))))

(defun clm-close-output ()
  (when *output*
    (soundfile:close *output*)
    (setf *output* nil)))

(defun clm-close-reverb ()
  (when *reverb*
    (soundfile:close *reverb*)
    (setf *reverb* nil)))

(defun clm-mix (outfile infile out-start out-framples in-start)
  (declare (type (or string pathname) outfile infile)
           (type non-negative-fixnum64 out-start out-framples in-start))
  (when (probe-file outfile)
    (with-open-soundfile (in infile)
      (with-open-soundfile (out outfile :direction :output :if-exists :mix)
        (let ((framples (min out-framples
                             (- (the non-negative-fixnum64 (soundfile:frames in))
                                in-start)))
              (channels (min (the non-negative-fixnum (soundfile:channels in))
                             (the non-negative-fixnum (soundfile:channels out)))))
          (declare (type non-negative-fixnum64 framples)
                   (type non-negative-fixnum channels)
                   #.*clm-optimize-settings*
                   #-64-bit #.*reduce-warnings*)
          (loop for i of-type non-negative-fixnum64
                      from out-start below (+ out-start framples)
                for j of-type non-negative-fixnum64
                      from in-start do
                  (loop for ch of-type non-negative-fixnum
                               below channels do
                          (soundfile:write out i (the double-float
                                                   (soundfile:read in j ch))
                                           ch))))))
    outfile))

;;; Edited from clm-5/sound.lisp
(defun* mix (filename (input-frample 0) (output-frample 0) framples output)
  (unless (or output *output*)
    (warn "mix called with no output file open?"))
  (let* ((outname (or output (mus-file-name *output*)))
	 (old-output *output*)
	 (old-reverb *reverb*))
    (when (and *output* (string= outname (mus-file-name *output*)))
      (clm-close-output)
      (when *reverb* (clm-close-reverb)))
    (clm-mix outname filename (round (+ output-frample *offset*))
	     (or framples (sound-framples filename)) input-frample)
    (setf *clm-ins* nil)
    (when old-output
      (setf *output* (reopen-output old-output)))
    (when old-reverb
      (setf *reverb* (reopen-output old-reverb)))
    outname))

;;; Edited from clm-5/sound.lisp
(defun mix-wrapper (file output-sample input-file dur)
  (let ((framples (if dur (floor (* dur *srate*)) (sound-framples input-file))))
    (declare (type non-negative-fixnum64 framples))
    (mix input-file :output (mus-file-name file) :output-frample output-sample
         :framples framples)
    (when *statistics*
      (setf clm-last-begin-time
            (max clm-last-begin-time (+ output-sample (floor *offset*)))))))

;;; Copied from clm-5/sound.lisp
(defun make-typed-file-name (name ext)
  (make-pathname :type ext :defaults name))

(defun format-temp-file (orig-filename)
  (concatenate 'string (namestring orig-filename) ".temp"))

(defun sound-rev-file (path)
  (namestring
    (make-pathname :defaults path
                   :name (concatenate 'string (pathname-name path) ".rev"))))

(defun sound-file-type-p (element-type file-type sound-file-type)
  (or (eq element-type :sound)
      (string-equal file-type sound-file-type)
      (find file-type '("snd" "aiff" "wav") :test #'string-equal)))

(defun sound-file-name (name element-type verbose force-recomputation)
  (declare (type (member nil :sound :clm :cm :lisp) element-type))
  (let* ((fname (or name last-open-input-file-name *clm-file-name*))
         (snd-file-name fname)
         (snd-file (make-typed-file-name snd-file-name
                     (pathname-type (if (eq element-type :sound)
                                        name
                                        *clm-file-name*))))
         (snd-file-str (namestring snd-file)))
    (if (sound-file-type-p element-type (pathname-type fname)
                           (pathname-type snd-file))
        (let ((filename (search-full-merge-pathnames
                          fname *clm-file-name* "test.snd")))
          (if filename (namestring filename) snd-file-name))
        (flet ((write-date (file)
                 (if (probe-file file) (file-write-date (truename file)) -1)))
          (let* ((*open-input-verbose* (or verbose *open-input-verbose*))
                 (*open-input-pathname* name)
                 (cm-file (make-typed-file-name fname "cm"))
                 (clm-file (make-typed-file-name fname "clm"))
                 (cm-date (write-date cm-file))
                 (clm-date (write-date clm-file)))
            (if (and (not force-recomputation)
                     (probe-file snd-file)
                     (let ((snd-date (file-write-date (truename snd-file))))
                       (and (<= cm-date snd-date)
                            (<= clm-date snd-date))))
                snd-file
                (let ((old-output *output*)
                      (old-reverb *reverb*))
                  (when (and *output*
                             (string= snd-file-str (mus-file-name *output*)))
                    (warn "we're about to overwrite ~A..."
                          (mus-file-name *output*)))
                  (when old-output (clm-close-output))
                  (when old-reverb (clm-close-reverb))
                  (let ((*clm-file-name* snd-file-str)
                        (*open-input-explicit-output* snd-file-str)
                        (*open-input-explicit-reverb* (sound-rev-file snd-file))
                        (*open-input-truename*
                          (namestring (cond ((> cm-date clm-date) cm-file)
                                            ((< cm-date clm-date) clm-file)
                                            ;; Forced reload of cm or clm file.
                                            ((minusp clm-date) cm-file)
                                            (t clm-file)))))
                    (when *open-input-verbose*
                      (format t "update ~A via ~A~% "
                              *open-input-explicit-output*
                              *open-input-truename*))
                    (load *open-input-truename*)
                    (when (and (>= cm-date 0)
                               (eq cm-file *open-input-truename*))
                      (let ((clm-date (write-date clm-file))
                            (snd-date (write-date snd-file)))
                        (when (or (and (minusp snd-date) (>= clm-date 0))
                                  (> clm-date snd-date))
                          ;; New clm file generated from cm file.
                          (let ((*open-input-truename* (namestring clm-file)))
                            ;; Reload clm file to write a sound file.
                            (load *open-input-truename*)))))
                    (when old-output
                      (setf *output* (reopen-output old-output)))
                    (when old-reverb
                      (setf *reverb* (reopen-output old-reverb)))
                    (truename *open-input-explicit-output*)))))))))

(defun* open-input (name verbose element-type (if-does-not-exist :error)
                    mix-at mix-duration (channel 0) (start 0)
                    (force-recomputation *force-recomputation*))
  (let ((sf (sound-file-name name element-type verbose force-recomputation)))
    (cond ((and sf (probe-file sf))
           (setf last-open-input-file-name sf)
           (if mix-at
               (let ((beg (floor (* mix-at *srate*))))
                 (mix-wrapper *output* beg
                   (namestring (if (pathnamep sf) sf (truename sf)))
                   mix-duration))
               (clm-open-input :file sf :start start :channel channel)))
          ((eq if-does-not-exist :error)
           (error "can't find ~A~A" name (unless (eq name sf)
                                           (format nil " (~A)" sf) ""))))))

(defun* open-input* (name (start 0) (channel 0) restartable)
  (labels ((search-path (name)
             (let ((fname (search-full-merge-pathnames name *clm-file-name*)))
               (cond (fname
                      (clm-open-input :file fname :start start :channel channel))
                     (restartable
                      (restart-case (break "can't find ~S" name)
                        (use-value (file-name)
                          :report "try again with a new file name."
                          :interactive (lambda ()
                                         (princ "open-input* file: ")
                                         (list (eval (read *query-io*))))
                          (search-path file-name))))
                     (t
                      (warn "can't find ~S" name))))))
    (search-path name)))

(declaim (inline close-input))
(defun close-input (i-stream)
  (soundfile:close i-stream))

(defun begin-with-sound (srate channels out-file revf statistics
                         continue-old-file reverb-channels header-type
                         data-format comment temp-file-p)
  (setf *clm-within-with-sound* t)
  (when statistics
    (initialize-statistics statistics out-file revf))
  (setf *output* (soundfile:open out-file :direction :output
                   :if-exists (if continue-old-file :mix :supersede)
                   :sample-rate srate :channels channels
                   :header-type (mus-to-sf-header-type
                                  (if temp-file-p
                                      *clm-tempfile-header-type*
                                      header-type))
                   :data-format (mus-to-sf-data-format
                                  (if temp-file-p
                                      *clm-tempfile-data-format*
                                      data-format))
                   :buffer-size *clm-file-buffer-size*))
  (when (and comment (not temp-file-p))
    (setf (soundfile:metadata *output* 'comment) comment))
  (when revf
    (setf *reverb* (soundfile:open revf :direction :output
                     :if-exists (if continue-old-file :mix :supersede)
                     :sample-rate srate :channels reverb-channels
                     :header-type (mus-to-sf-header-type
                                    *clm-tempfile-header-type*)
                     :data-format (mus-to-sf-data-format
                                    *clm-tempfile-data-format*)
                     :buffer-size *clm-file-buffer-size*)))
  out-file)

(defun end-with-sound (scaled-to out-file revf temp-file statistics
                       header-type-str data-format-str rev-func decay-time
                       rev-data channels play scaled-by to-snd comment)
  (when revf
    (let ((pad (floor (* decay-time *clm-srate*))))
      (declare (type non-negative-fixnum pad))
      (when (plusp pad)
        ;; Zero padding.
        (soundfile:write *reverb*
          (+ (soundfile:current-frame *reverb*) (1- pad)) 0d0))
      (clm-close-reverb)
      (with-open-soundfile (*reverb* revf)
        (let ((*offset* 0))
          (apply rev-func 0 (soundfile:duration *reverb*) rev-data)))
      (when (and *clm-delete-reverb* (probe-file revf))
        (delete-file revf))))
  (clm-close-output)
  (when statistics
    (print-statistics statistics channels *clm-logger-stream*
                      (or scaled-to scaled-by)))
  (when temp-file
    (soundfile:convert temp-file out-file header-type-str data-format-str
                       :scale-by scaled-by :scale-to scaled-to)
    (when comment
      (setf (soundfile:metadata out-file 'comment) comment))
    (delete-file temp-file))
  (setf *clm-within-with-sound* nil)
  (when (and (not *open-input-explicit-output*) (= *clm-with-sound-depth* 1))
    (setf last-dac-filename out-file)
    (cond ((and (find-package "SND") (or to-snd *to-snd*))
           (funcall (find-symbol "OPEN-OR-UPDATE-SOUND" "SND") out-file)
           (when play
             (funcall (find-symbol "EVAL" "SND")
                      (format nil "(begin (update-time-graph) (play :wait #~A))"
                              (if *clm-dac-wait-default* #\t #\f)))))
          (play
           (dac out-file))))
  out-file)

(defmacro with-offset (val &body body)
  (with-gensyms (old-offset old-rev-offset)
    `(let ((*offset* (+ *offset* (round (* ,val *srate*))))
           (,old-offset nil)
           (,old-rev-offset nil))
       (when *output*
         (setf ,old-offset (soundfile:offset *output*))
         (setf (soundfile:offset *output*) *offset*))
       (when *reverb*
         (setf ,old-rev-offset (soundfile:offset *reverb*))
         (setf (soundfile:offset *reverb*) *offset*))
       (unwind-protect (progn ,@body)
         (when (and ,old-offset *output*)
           (setf (soundfile:offset *output*) ,old-offset))
         (when (and ,old-rev-offset *reverb*)
           (setf (soundfile:offset *reverb*) ,old-rev-offset))))))

(defmacro with-sound (&whole whole
                      (&key (output *clm-file-name*) continue-old-file
                       (channels *clm-channels*) info comment
                       (srate *clm-srate*) reverb reverb-data
                       (reverb-channels *clm-reverb-channels*)
                       revfile (decay-time 1.0) reverb-func
                       reverb-args (play *clm-play*) force-recomputation
                       (notehook *clm-notehook*) (statistics *clm-statistics*)
                       (header-type *clm-header-type*)
                       (data-format *clm-data-format*) save-body
                       (verbose *clm-verbose*) scaled-to (clipped *clm-clipped*)
                       scaled-by sampling-rate (to-snd *to-snd*)
                       &allow-other-keys)
		      &body body)
  (with-gensyms (outfile sf comm header-type-str data-format-str revf rev-func
                 rev-args temp-file-p old-output old-reverb scl-to scl-by)
    `(let* ((*verbose* ,verbose)
            (*offset* 0)
            (*statistics* nil)
            (*notehook* ,notehook)
            (*force-recomputation* ,force-recomputation)
            (*clm-srate* ,srate)
            (*clm-channels* ,channels)
            (*clm-with-sound-body* (and ,save-body ',whole))
            (*clm-with-sound-depth* (1+ *clm-with-sound-depth*))
            (*srate* *clm-srate*)
            (*clipped* ,clipped)
            (,outfile ,output)
            (,revf ,revfile)
            (,comm (or ,comment ,info))
            (,rev-func (or ,reverb-func ',reverb))
            (,rev-args (or ,reverb-args ',reverb-data))
            (,header-type-str (mus-to-sf-header-type ,header-type))
            (,data-format-str (mus-to-sf-data-format ,data-format))
            (,scl-to ,scaled-to)
            (,scl-by ,scaled-by)
            (,temp-file-p (and (or ,scl-to ,scl-by ,revf ,rev-func
                                   (string/= ,data-format-str "double"))
                               t))
            (,sf (if ,temp-file-p (format-temp-file ,outfile) ,outfile))
            (,revf (and ,rev-func
                        (or ,revf *open-input-explicit-reverb*
                            (namestring
                              (make-pathname :defaults ,sf :type "rev")))))
            (,old-output *output*)
            (,old-reverb *reverb*))
       (incudine.util::with-local-sample-rate (*clm-srate*)
         (clm-close-output)
         (clm-close-reverb)
         (setf *clm-ins* nil)
         (when (or ,comm *clm-with-sound-body*)
           (setf ,comm (concatenate 'string ,comm
                         (when *clm-with-sound-body*
                           (with-output-to-string (s)
                             (write *clm-with-sound-body*
                                    :stream s :pretty t))))))
         (unwind-protect
              (progn
                (begin-with-sound
                  (or ,sampling-rate *clm-srate*) ,channels ,sf ,revf
                  ,statistics ,continue-old-file ,reverb-channels
                  ,header-type ,data-format ,comm ,temp-file-p)
                (incudine:with-cleanup ,@body)
                (end-with-sound
                  ,scaled-to ,outfile ,revf (and ,temp-file-p ,sf) ,statistics
                  ,header-type-str ,data-format-str ,rev-func ,decay-time
                  ,rev-args ,channels ,play ,scaled-by ,to-snd ,comm)
                (when ,old-output
                  (setf *output* (reopen-output ,old-output)))
                (when ,old-reverb
                  (setf *reverb* (reopen-output ,old-reverb)))
                ,outfile)
           (setf *clm-ins* nil)
           (setf *force-recomputation* nil)
           (clm-check-for-reset))))))

(defun clm-load (pathname &key (output *clm-file-name*) continue-old-file
                 (channels *clm-channels*) info comment (srate *clm-srate*)
                 reverb reverb-data (reverb-channels 1) revfile (decay-time 1.0)
                 (play *clm-play*) force-recomputation (notehook *clm-notehook*)
                 statistics (header-type *clm-header-type*)
                 (data-format *clm-data-format*) save-body
                 (verbose *clm-verbose*) scaled-to (clipped *clm-clipped*)
                 scaled-by (load-package *package*) sampling-rate to-snd)
  (with-sound (:output output :continue-old-file continue-old-file
               :channels channels :comment comment :info info
               :srate (or sampling-rate srate) :reverb-func reverb
               :revfile revfile :reverb-args reverb-data
               :reverb-channels reverb-channels :decay-time decay-time
	       :play play :force-recomputation force-recomputation
	       :statistics statistics :notehook notehook
               :header-type header-type :data-format data-format
               :verbose verbose :clipped clipped :save-body save-body
               :scaled-to scaled-to :scaled-by scaled-by :to-snd to-snd)
    (let ((*package* (if (packagep load-package)
			 load-package
                         (find-package load-package))))
      (load pathname))))

;;; Copied from clm-5/sound.lisp
(defun get-mix-calls (f)
  (setf *clm-mix-calls* nil)
  (setf *clm-mix-options* nil)
  (let ((com (sound-comment f)))
    (if (and com (stringp com))
        (let ((len (length com))
              (pos 0)
              (form nil))
          (loop while (< pos len) do
                  (multiple-value-setq (form pos)
                    (read-from-string com t :EOF :start pos))
               (if (not (eq form :EOF))
                   (eval form)
                   (setf pos (1+ len)))))))
  (write-to-string *clm-mix-calls*))

;;; Copied from clm-5/sound.lisp
(defun get-mix-options ()
  (write-to-string *clm-mix-options*))

;;; Edited from clm-5/sound.lisp
(defun rev-mix-in (source-file begin-time reverb-file)
  (let ((res (open-input source-file :mix-at begin-time)))
    (when (and *reverb* reverb-file)
      (mix-wrapper *reverb* (floor (* begin-time *srate*)) reverb-file
                   (soundfile:duration reverb-file)))
    res))

(defun complete-mix-options (options revfile)
  (let ((default (loop for opt in `((:channels (mus-channels *output*))
                                    (:srate (floor *srate*))
                                    (:revfile ,revfile))
                       unless (getf options (first opt))
                         append opt)))
    `(list* ,@default ',options)))

(defun write-clm-file (clmf chkpt-file options option-str call-str body)
  (with-open-file (fil clmf :direction :output :if-does-not-exist :create
                   :if-exists :supersede)
    (format fil ";; Temporary notelist for ~A~%~A~%" chkpt-file (make-banner))
    (format fil "(with-sound (~{~S ~}~%~14T~
                              :play nil~%~14T~
                              :info ~S)~%~{  ~S~^~%~})~%"
            options
            (format nil "~A~%(setf *clm-mix-calls* '~A)~%~
                             (setf *clm-mix-options* '~A)~%"
                    (make-banner) call-str option-str)
             body)))

(defun reuse-mix (beg sndf-str revf-str)
  (when *verbose*
    (princ! (format nil "; Mixing ~A " sndf-str)))
  (rev-mix-in sndf-str beg (and *reverb* revf-str)))

(defun recompute-mix (beg clmf chkpt-file errf options sndf-str revf-str
                      option-str call-str body)
  (let ((finished-ok nil))
    (unwind-protect
         (progn
           (when *verbose*
             (princ! (if (probe-file errf)
                         (format nil "; ~A was interrupted during previous ~
                                        computation -- will recompute it~%"
                                 sndf-str)
                         (format nil "; Computing ~A " sndf-str))))
           (write-clm-file clmf chkpt-file options option-str call-str body)
           (rev-mix-in clmf beg (and *reverb* revf-str))
           (when (probe-file errf)
             (delete-file errf))
           (setf finished-ok t))
      (unless finished-ok
        (close (open errf :direction :output :if-exists :supersede))))))

(defmacro with-mix (options ur-chkpt-file ur-beg &body body)
  (declare (type list options))
  (with-gensyms (chkpt-file beg old-recompute src call-str option-str
                 sndf sndf-str errf revf revf-str ext)
    `(let ((,chkpt-file ,ur-chkpt-file)
           (,beg ,ur-beg)
           (,old-recompute *force-recomputation*)
           (,src ',body))
       (declare (type (or string pathname) ,chkpt-file) (type real ,beg))
       (if (null ,src)
           (open-input ,chkpt-file :mix-at ,beg)
           (flet ((path-with-type (,ext)
                    (merge-pathnames (make-typed-file-name ,chkpt-file ,ext)
                                     (or *clm-file-name* ""))))
             (let* ((,call-str (write-to-string ,src))
                    (,option-str (write-to-string ',options))
                    (,sndf (path-with-type (pathname-type *clm-file-name*)))
                    (,sndf-str (namestring ,sndf))
                    (,revf (path-with-type "rev"))
                    (,revf-str (namestring ,revf))
                    (,errf (path-with-type "error")))
               (if (and (not *force-recomputation*)
                        (probe-file ,sndf)
                        (or (not *reverb*) (probe-file ,revf))
                        (not (probe-file ,errf))
                        (string-equal (get-mix-calls ,sndf-str) ,call-str)
                        (string-equal (get-mix-options) ,option-str))
                   (reuse-mix ,beg ,sndf-str ,revf-str)
                   (recompute-mix
                     ,beg (path-with-type "clm") ,chkpt-file ,errf
                     ,(complete-mix-options options revf-str)
                     ,sndf-str ,revf-str ,option-str ,call-str ,src)))))
       (setf *force-recomputation* ,old-recompute)
       nil)))

;;; Edited from clm-5/sound.lisp
(defmacro sound-let (sounds &body body)
  (with-gensyms (old-output sound-files old-recompute index opts)
    `(let* ((,old-output *output*)
            (,sound-files
              (map 'vector
                   (lambda (,opts)
                     (namestring
                       (merge-pathnames (or (getf ,opts :output)
                                            (string (gensym "snd")))
                                        (or *clm-file-name* ""))))
                   ',(mapcar #'second sounds)))
            (,old-recompute *force-recomputation*)
            (*open-input-explicit-output* nil)
            (*open-input-explicit-reverb* nil))
       (declare (ignorable ,old-output))
       (let* ,(loop for (snd opts . calls) in sounds
                    for i from 0 do
                      (loop for opt in `((:notehook nil)
                                         (:srate (soundfile:sample-rate
                                                   ,old-output))
                                         (:channels (mus-channels ,old-output))
                                         (:output (aref ,sound-files ,i))) do
                              (unless (getf opts (first opt))
                                (setf opts (append opt opts))))
                    collect `(,snd (with-sound (,@opts :play nil)
                                     ,@calls)))
         ,@body)
       (setf *force-recomputation* ,old-recompute)
       (setf *clm-ins* nil)
       (dotimes (,index (length ,sound-files))
         (delete-file (aref ,sound-files ,index))))))

;;; Common Music interface to WITH-SOUND

(defstruct wsdat
  revfun revdat revdecay outtype play stats wait
  scaled-to format file tmpf channels scaled-by to-snd comment)

;;; Edited from clm-5/sound.lisp
(defun init-with-sound (&key (output *clm-file-name*) sndfile
                        (channels *clm-channels*) (srate *clm-srate*)
                        continue-old-file reverb reverb-data
                        (reverb-channels *clm-reverb-channels*)
                        revfile (decay-time 1.0) (play *clm-play*)
                        (notehook *clm-notehook*) (statistics *clm-statistics*)
                        type (header-type *clm-header-type*)
                        (data-format *clm-data-format*) scaled-to scaled-by
                        (clipped *clm-clipped*) force-recomputation
                        (verbose *clm-verbose*) comment to-snd)
  (let* ((*clipped* clipped)
         (out-file (or *open-input-explicit-output*
		       (namestring
                         (merge-pathnames (or sndfile output)
                                          (or *clm-file-name* "")))))
	 (our-type (or type header-type))
	 (our-format data-format)
         (temp-file-p (and (or scaled-to scaled-by revfile reverb) t))
         (sf (if temp-file-p (format-temp-file out-file) out-file))
	 (revf (and reverb
                    (or revfile *open-input-explicit-reverb*
                        (namestring
                          (make-pathname :defaults sf :type "rev"))))))
    (when srate
      (setf *srate* srate))
    (setf *verbose* verbose)
    (setf *offset* 0)
    (setf *interrupted* 0)
    (setf *statistics* nil)
    (setf *notehook* notehook)
    (setf *force-recomputation* force-recomputation)
    (begin-with-sound
      srate channels sf revf statistics continue-old-file
      reverb-channels our-type our-format comment temp-file-p)
    (make-wsdat :revfun reverb :revdat reverb-data :revdecay decay-time
		:outtype (mus-to-sf-header-type our-type) :play play
                :stats statistics :channels channels
		:format (mus-to-sf-data-format our-format) :scaled-to scaled-to
                :file out-file :scaled-by scaled-by :tmpf (and temp-file-p sf)
                :to-snd to-snd :comment comment)))

;;; Edited from clm-5/sound.lisp
(defun finish-with-sound (wsd)
  (incudine.util::with-struct-slots
      ((file scaled-to tmpf stats outtype format revfun revdecay revdat channels
        play scaled-by to-snd comment)
       wsd wsdat)
    (setf file (end-with-sound
                 scaled-to file (and *reverb* (mus-file-name *reverb*))
                 tmpf stats outtype format revfun revdecay revdat channels
                 play scaled-by to-snd comment)))
  (clm-reset))

;;; Edited from clm-5/sound.lisp
(defmacro with-current-sound ((&key output comment scaled-to scaled-by)
                              &body body)
  `(with-sound (:output ,output :channels *channels* :comment ,comment
                :srate *srate* :revfile *ws-reverb-file*
                :force-recomputation *force-recomputation* :notehook *notehook*
		:header-type *header-type* :data-format *data-format*
		:verbose *verbose* :clipped *clipped* :scaled-to ,scaled-to
		:scaled-by ,scaled-by)
     ,@body))

;;; Edited from clm-5/sound.lisp
(defmacro scaled-by (val &body body)
  (with-gensyms (tempf)
    `(let ((,tempf (make-pathname :name "temp_clm_scaling" :type "snd"
                                  :defaults (soundfile:path *output*))))
       (with-current-sound (:output ,tempf :scaled-by ,val) ,@body)
       (mix ,tempf)
       (delete-file ,tempf))))

;;; Edited from clm-5/sound.lisp
(defmacro scaled-to (val &body body)
  (with-gensyms (tempf)
    `(let ((,tempf (make-pathname :name "temp_clm_scaling" :type "snd"
                                  :defaults (soundfile:path *output*))))
       (with-current-sound (:output ,tempf :scaled-to ,val) ,@body)
       (mix ,tempf)
       (delete-file ,tempf))))
