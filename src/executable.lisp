;;; Copyright (c) 2013-2024 Tito Latini
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

(in-package :incudine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-ext:*posix-argv* "INCUDINE.SCRATCH")
  (export '(*argv* read-arg))
  ;; The initialization will occur after the toplevel
  (setf sb-ext:*init-hooks*
        (delete *core-config-and-init-function* sb-ext:*init-hooks*))
  ;; Load "$HOME/.incudinerc" before the toplevel
  (pushnew 'incudine.config:load-incudinerc sb-ext:*init-hooks*)

  #+linedit
  (let ((repl-prompt-fun sb-int:*repl-prompt-fun*)
        (repl-read-form-fun sb-int:*repl-read-form-fun*)
        (linedit-repl-p nil))
    (when (and (find-symbol "MAYBE-RESOLVE-SYNONYM-STREAM" "SB-IMPL")
               (find-symbol "ENCODING-REPLACEMENT-ADJUST-CHARPOS" "SB-IMPL"))
      (defun %repl-read-form-fun (in out)
        (let ((real (funcall (find-symbol "MAYBE-RESOLVE-SYNONYM-STREAM" "SB-IMPL") out)))
          (when (sb-sys:fd-stream-p real)
            ;; ENCODING-REPLACEMENT-ADJUST-CHARPOS #\Newline sets FD-STREAM-OUTPUT-COLUMN
            ;; to 0 (see REPL-FUN in sbcl/src/code/toplevel.lisp; the last checked version
            ;; is sbcl-2.4.1).
            (funcall (find-symbol "ENCODING-REPLACEMENT-ADJUST-CHARPOS" "SB-IMPL")
                     #\Newline real))
          (prog1 (funcall repl-read-form-fun in out) (fresh-line out)))))

    (defun uninstall-linedit-repl (&optional connection)
      (declare (ignore connection))
      (linedit:uninstall-repl)
      (setf sb-int:*repl-read-form-fun* repl-read-form-fun))

    (defun install-linedit-repl (&optional closed-connection)
      (unless closed-connection
        (setf linedit-repl-p t))
      (when linedit-repl-p
        (when (fboundp '%repl-read-form-fun)
          (setf sb-int:*repl-read-form-fun* #'%repl-read-form-fun))
        (linedit::install-repl :wrap-current t :eof-quits t
          :history (merge-pathnames ".incudine_history" (user-homedir-pathname)))))

    (defun clear-screen (chord editor)
      (declare (ignore chord editor))
        (ti:tputs ti:clear-screen)
        (funcall repl-prompt-fun *terminal-io*)
        (force-output *terminal-io*))

    (linedit::defcommand "C-L" 'clear-screen)

    (let ((sb-ext:*muffled-warnings* 'style-warning))
      ;; Disable beep, otherwise there is an unwanted character and
      ;; it's difficult to edit the line.
      (defmethod linedit::beep ((b linedit::terminal))
        (declare (ignore b))
        nil)
      ;; Quit without asking.
      (defun linedit::eof-handler (lisp-name quit-fn)
        (declare (ignore lisp-name quit-fn))
        (fresh-line)
        (sb-ext:exit)))))

(defun set-default-logger-time (&optional (format :sec))
  (unless (logger-time)
    (setf (logger-time) format)))

(defstruct (toplevel-options (:copier nil))
  ;; Tlist of the read options
  (consumed (let ((cmd-pathname
                   (list (sb-ext:native-namestring
                          sb-ext:*runtime-pathname* :as-file t)
                         ;; The first option after #! in the header of the fasl
                         ;; file is not --script, so we can directly use, if we
                         ;; want, a sequence of compiled files.
                         "--")))
              (cons cmd-pathname (cdr cmd-pathname)))
            :type cons)
  (outfile nil :type (or string null))
  (infile nil :type (or string null))
  (ochans 0 :type channel-number)
  (ichans 0 :type channel-number)
  (duration 0 :type real)
  (rego-files nil :type list)
  (score-args nil :type list)
  (swank-server-port -1 :type fixnum)
  (compile-rego-contents-p nil :type boolean)
  (sf-metadata nil :type list)
  (debug-p nil :type boolean)
  (logtime-p nil :type boolean)
  (script nil :type (or string null))
  (sysinit nil :type (or string null))
  (sysinit-p t :type boolean)
  (userinit nil :type (or string null))
  (userinit-p t :type boolean)
  (inform-p t :type boolean)
  (print-p t :type boolean)
  (disable-debugger-p nil :type symbol)
  (quit-p nil :type boolean)
  (interactive-p nil :type boolean)
  (functions (list #'set-default-logger-time) :type list))

(defvar *toplevel-options* (make-hash-table :test 'equal))
(declaim (type hash-table *toplevel-options*))

(defvar *rt-executable-sync* (make-sync-condition "rt executable"))
(declaim (type sync-condition *rt-executable-sync*))

(defun find-toplevel-option (opt)
  (gethash opt *toplevel-options*))

(defun add-toplevel-function (function tlevel-opt)
  (push function (toplevel-options-functions tlevel-opt)))

(defun add-consumed-option (toplevel-opt option &optional quoted-p)
  (let ((new (list (if quoted-p (format nil "~S" option) option)))
        (tl (toplevel-options-consumed toplevel-opt)))
    (if (tlist-empty-p tl)
        (setf (car tl) new)
        (setf (cddr tl) new))
    (setf (cdr tl) new)))

(defmacro shift-toplevel-option (opt options)
  `(add-consumed-option ,opt (pop ,options)))

(defun consumed-options (opt)
  (car (toplevel-options-consumed opt)))

(defun changed-number-of-channels-p (opt)
  (or (plusp (toplevel-options-ichans opt))
      (plusp (toplevel-options-ochans opt))))

;;; Return a list with the name of the command and the arguments
;;; written between `#!' and `--script\n#' in the header of a
;;; fasl-file. It is useful to retrieve the options that precede
;;; the lisp file compiled by the command "incudine".
(defun read-options-from-fasl (fasl-pathname)
  (declare (type pathname fasl-pathname))
  (with-open-file (f fasl-pathname)
    (when (and (char= (read-char f nil nil) #\#)
               (char= (read-char f nil nil) #\!))
      (let ((acc) (result) (quoted-p) (escape-p))
        (declare (type list acc result) (type boolean quoted-p escape-p))
        (do ((c (read-char f nil nil) (read-char f nil nil)))
            ((or (null c) (char= c #\#))
             (nreverse result))
          (cond ((and (null acc) (char= c #\"))
                 ;; Start of a string
                 (setf quoted-p t)
                 (push c acc))
                ((char= c #\\)
                 (setf escape-p t)
                 (push c acc))
                ((and quoted-p (char= c #\"))
                 (if escape-p
                     (setf escape-p nil)
                     ;; End of the string
                     (setf quoted-p nil))
                 (push c acc))
                ((and (not quoted-p) (char= c #\Space))
                 ;; Collect the read option
                 (push (coerce (nreverse acc) 'string) result)
                 (setf acc nil))
                (t (push c acc))))))))

(defun fasl-file-p (pathname)
  (and (probe-file pathname)
       (with-open-file (f pathname)
         (when (and (char= (read-char f nil nil) #\#)
                    (char= (read-char f nil nil) #\!))
           (let ((seq (make-list 6)))
             ;; Skip the first line
             (read-line f nil nil)
             (read-sequence seq f)
             (equal seq '(#\# #\Space #\F #\A #\S #\L)))))))

(defun stop-rt-executable ()
  (nrt-funcall (lambda () (sync-condition-signal *rt-executable-sync*))))

(declaim (inline find-rt-executable-closing-time))
(defun find-rt-executable-closing-time (pad)
  (if (incudine.edf:heap-empty-p)
      ;; Stop The Rock.
      (stop-rt-executable)
      ;; Stop PAD seconds after the last event if it is really the last.
      (at (+ (incudine.edf:last-time) pad)
          #'find-rt-executable-closing-time pad)))

(defun rt-executable-closing-time (duration)
  `(lambda ()
     ,(let ((dur (* *sample-rate* duration)))
        (if (plusp duration)
            ;; Stop after DURATION seconds.
            `(at (+ (now) ,dur) #'stop-rt-executable)
            (let ((pad (if (minusp duration)
                           (- dur)
                           (* *sample-rate* 1/10))))
              ;; Perhaps stop after PAD seconds.
              `(at (+ (now) ,pad) #'find-rt-executable-closing-time ,pad))))))

(defun rego-render-function-body (opt fname &optional pathname)
  (let ((infile (toplevel-options-infile opt))
        (outfile (or (toplevel-options-outfile opt)
                     ;; stdout by default if rego from stdin
                     (and (null pathname) "-")))
        (duration (toplevel-options-duration opt))
        (func (ensure-complex-gensym (string fname))))
    `(flet ((,func ()
              (apply ',fname ,(toplevel-options-score-args opt))))
       (cond (*rt-thread*
              ;; Realtime
              (sync-condition-flush *rt-executable-sync*)
              (,func)
              (at 0 ,(rt-executable-closing-time duration))
              (sync-condition-wait *rt-executable-sync*))
             (t
              ;; Non realtime
              (bounce-to-disk
                  (,(cond (outfile
                           (prog1 outfile
                             ;; Output file valid only for this rego file.
                             (setf (toplevel-options-outfile opt) nil)))
                          (t (make-pathname :name (pathname-name pathname)
                                            :type *default-header-type*)))
                   ,@(when infile `(:input-filename ,infile))
                   :duration ,duration
                   :metadata ',(toplevel-options-sf-metadata opt))
                (,func)))))))

(defun rego-render-form (opt fname pathname)
  (let ((name (format-symbol *package* "~A-RENDER" fname)))
    `(progn
       (defun ,name () ,(rego-render-function-body opt fname pathname))
       (,name))))

(defun rego-from-stdin-form (opt)
  (let ((fname (gensym)))
    `(progn
       (funcall ,(rego-stream->sexp *standard-input* fname
                   (toplevel-options-compile-rego-contents-p opt)))
       (funcall (lambda () ,(rego-render-function-body opt fname))))))

;;; Complete a file obtained with REGOFILE->LISPFILE. The modified
;;; file is a script executable both in realtime and non-rt.
(defun %complete-score (pathname opt fname)
  (declare (type pathname pathname) (type toplevel-options opt)
           (type symbol fname))
  (with-open-file (f pathname :direction :output :if-exists :append)
    (terpri f)
    (write (rego-render-form opt fname pathname) :stream f)
    (terpri f)
    pathname))

;; The command line options are stored in the FASL header between `#!'
;; and `--script\n#'. From sbcl-1.4.13, SB-EXT:*RUNTIME-PATHNAME* is
;; not special anymore, so that global variable is temporarily changed
;; to insert `--' and the command line options before `--script' in the
;; FASL header.
(defmacro fasl-header-hack ((options) &body body)
  (with-gensyms (true-runtime-pathname)
    `(let ((,true-runtime-pathname sb-ext:*runtime-pathname*))
       (unwind-protect
            (progn
              (setf sb-ext:*runtime-pathname* (format nil "~{~A~^ ~}" ,options))
              ,@body)
         (setf sb-ext:*runtime-pathname* ,true-runtime-pathname)))))

(defun load-compiled-cudo-file (pathname opt &optional rego-file-p)
  (declare (type (or string pathname) pathname) (type toplevel-options opt)
           (type boolean rego-file-p))
  (flet ((try-with-type (type)
           (probe-file (make-pathname :defaults pathname :type type)))
         (set-quit-and-disable-debugger (opt)
           (unless #1=(toplevel-options-disable-debugger-p opt) (setf #1# t))
           (unless #2=(toplevel-options-quit-p opt) (setf #2# t)))
         (fasl-updated-p (fasl-pathname src-pathname consumed-options)
           (and fasl-pathname
                (or (null src-pathname)
                    (and ;; Check if the options are changed. The options used
                         ;; during the compilation are in the header of the
                         ;; fasl, between #! and #
                         (equal (read-options-from-fasl fasl-pathname)
                                consumed-options)
                         ;; Check if the source file is changed
                         (> (file-write-date fasl-pathname)
                            (file-write-date src-pathname))))))
         (load-fasl (fasl-pathname)
           (let ((*standard-output* *logger-stream*))
             (load fasl-pathname)))
         (score-function-name ()
           (format-symbol *package* "SCORE-~:@(~A~)" (pathname-name pathname))))
    (let* ((type (pathname-type pathname))
           (stdin-p (equal pathname "-"))
           (lisp-pathname
            (cond ((and (not rego-file-p)
                        (not stdin-p)
                        (or (null type)
                            (string= type sb-fasl:*fasl-file-type*)))
                   (or (try-with-type "cudo")
                       (try-with-type "lisp")
                       (let ((type (pathname-type pathname)))
                         (when (or (and type
                                        (string/= type
                                                  sb-fasl:*fasl-file-type*))
                                   (not (fasl-file-p pathname)))
                           (probe-file pathname)))))
                  (rego-file-p
                   ;; Name of the intermediate lisp file
                   (make-pathname :defaults pathname :type "rexi"))
                  (t pathname)))
           (rego-pathname (if rego-file-p pathname))
           (src-pathname (if rego-file-p rego-pathname lisp-pathname))
           (fasl-pathname (and (not stdin-p)
                               (try-with-type sb-fasl:*fasl-file-type*)))
           (consumed-options (copy-list (consumed-options opt))))
      (if (fasl-updated-p fasl-pathname src-pathname consumed-options)
          (lambda (opt options)
            (set-quit-and-disable-debugger opt)
            (add-toplevel-function (lambda ()
                                     (msg debug "load ~S" fasl-pathname)
                                     (load-fasl fasl-pathname))
                                   opt)
            options)
          (when lisp-pathname
            (lambda (opt options)
              (set-quit-and-disable-debugger opt)
              (add-toplevel-function
                (lambda ()
                  (let ((fasl-pathname (and (not stdin-p)
                                            (or fasl-pathname
                                                (try-with-type
                                                   sb-fasl:*fasl-file-type*)))))
                    (cond ((fasl-updated-p fasl-pathname src-pathname
                                           consumed-options)
                           (load-fasl fasl-pathname))
                          (t
                           (when rego-file-p
                             (let ((fname (score-function-name)))
                               ;; Generate the intermediate file of the rego file
                               (regofile->lispfile rego-pathname fname
                                 lisp-pathname
                                 (toplevel-options-compile-rego-contents-p opt))
                               ;; The produced file becomes a script usable
                               ;; both in realtime and non-realtime.
                               (%complete-score lisp-pathname opt fname)))
                           (fasl-header-hack (consumed-options)
                             (if stdin-p
                                 (eval (rego-from-stdin-form opt))
                                 (load (compile-file lisp-pathname))))
                           (setf (toplevel-options-score-args opt) nil)
                           (when (and rego-file-p
                                      ;; Preserve the intermediate file of the
                                      ;; score in debug mode.
                                      (not (toplevel-options-debug-p opt)))
                             (delete-file lisp-pathname))))))
                opt)
              options))))))

(defmacro maybe-read-from-string (value read-p)
  (if read-p
      `(let ((*package* (find-package "INCUDINE.SCRATCH")))
         (read-from-string ,value))
      value))

(defun usage ()
  (princ "Usage: incudine [OPTIONS] [FILE1 [FILE2 ...]]
                [OPTIONS] [FILE1 [FILE2 ...]] ...
                [USER-OPTIONS]

  -b, --sndfile-buffer-size <int>  Buffer size to read/write a soundfile.
  -B, --block-size <frames>    Block size for block-by-block processing.
  -c, --channels <int>         Number of the output channels.
  --client-name <name>         Name of the client for the audio server.
  --compile-score-contents     Compile the contents of the score.
  -F, --data-format <type>     Format of the sample for the output file.
  --data-format-list           Available audio formats.
  --default-table-size <int>   Default size of a table for an oscillator.
  -d, --duration <seconds>     Duration of the output file.
  --debug                      Print debug info.
  --disk-guard-size <seconds>  Size of the output file with undefined duration.
  -h, --help                   Print this message and exit.
  -H, --header-type <type>     Type of the header for the output file.
  --header-type-list           Available header types.
  -i, --infile <filename>      Sound input filename or `-' for standard input.
  --input-channels <int>       Number of the input channels.
  --interpret-score-contents   Interpret the contents of the score (default).
  -L, --logfile <filename>     Logging file.
  --lisp-version               Print version information of SBCL and exit.
  --logtime ( sec | samp )     Log message with time in seconds or in samples.
  --max-number-of-channels <int>  Max number of the channels.
  --max-number-of-nodes <int>  Max number of the nodes.
  -N, --no-realtime            Stop realtime.
  --nrt-edf-heap-size <int>    Max number of the events in non realtime.
  --nrt-pool-size <bytes>      Size of the pool for temporary C malloc in nrt.
  --nrt-priority <int>         Priority of the non-realtime thread.
  -o, --outfile <filename>     Sound output filename or `-' for standard output.
  -p, --period <int>           Frames per buffer (used only with PortAudio).
  --pad <seconds>              Extend the duration of the output file.
  -r, --rate <int>             Sample rate.
  -R, --realtime               Start realtime.
  --receiver-priority <int>    Priority of the thread for a receiver.
  --rt-edf-heap-size <int>     Heap size for realtime scheduling.
  --rt-pool-size <bytes>       Size of the pool for the C heap used in realtime.
  --rt-priority <int>          Priority of the realtime thread.
  -s <filename>                Process a score file or `-' for standard input.
  --sample-pool-size <bytes>   Size of the pool for the C arrays defined in DSP!
  --score-args <list>          Score function arguments.
  --sound-velocity <real>      Velocity of the sound at 22°C, 1 atmosfera.
  --swank-server <port>        Start Swank server.
  -T, --tempo <bpm>            Initial tempo in beats per minute.
  -v, --verbose                More verbose.
  --version                    Print version information and exit.

Metadata:

  --title <title>
  --copyright <copyright>
  --software <software>
  --artist <artist>
  --comment <comment>
  --date <date>
  --album <album>
  --license <license>
  --tracknumber <track>
  --genre <genre>

SBCL options:

  --sysinit <filename>         System-wide init-file to use instead of default.
  --userinit <filename>        Per-user init-file to use instead of default.
  --no-sysinit                 Inhibit processing of any system-wide init-file.
  --no-userinit                Inhibit processing of any per-user init-file.
  --disable-debugger           Invoke sb-ext:disable-debugger.
  --noinform                   Suppress the printing of the banner at startup.
  --noprint                    Run a Read-Eval Loop without printing results.
  --script [<filename>]        Skip #! line, disable debugger, avoid verbosity.
  --quit                       Exit with code 0 after option processing.
  --non-interactive            Sets both --quit and --disable-debugger.
  --eval <form>                Form to eval when processing this option.
  --load <filename>            File to load when processing this option.

")
  (sb-ext:exit :code 1))

(macrolet ((def-toplevel-opt (option &body function-body)
             (with-gensyms (fn)
               `(let ((,fn (lambda (opt options)
                             ;; Desired variable capture to avoid the same names
                             ;; of the arguments for all the definitions.
                             (declare (ignorable opt options))
                             ,@function-body)))
                  ,(if (atom option)
                       `(setf (gethash ,option *toplevel-options*) ,fn)
                       `(setf (gethash ,(car option) *toplevel-options*) ,fn
                              (gethash ,(cdr option) *toplevel-options*) ,fn)))))
           (set-option (obj &optional read-p)
             `(progn
                (setf ,obj (maybe-read-from-string (car options) ,read-p))
                (add-consumed-option opt (car options) t)
                (cdr options)))
           (push-fn ((error-msg &optional value-var) &body form)
             `(add-toplevel-function
               (lambda ()
                 (with-simple-restart (continue ,error-msg
                                                ,@(if value-var `(,value-var)))
                   ,@form))
               opt))
           (with-eval-form ((value-var error-msg &optional read-p) &body form)
             (if value-var
                 (with-gensyms (curr)
                   `(let* ((,curr (car options))
                           (,value-var (maybe-read-from-string ,curr ,read-p)))
                      (push-fn (,error-msg ,value-var) ,@form)
                      (add-consumed-option opt ,curr t)
                      (cdr options)))
                 `(progn
                    (push-fn (,error-msg) ,@form)
                    options)))
           (sf-format-list (count-fn get-fn control-string)
             `(let ((formats (loop for i below (,count-fn)
                                   for info = (,get-fn i)
                                   collect (cons (sf:format info) info))))
                (maphash (lambda (k v)
                           (let ((fmt (cdr (assoc v formats :test #'=))))
                             (when fmt
                               (format t ,control-string k (sf:name fmt)))))
                         sf::*formats*)))
           (set-metadata ()
             (with-gensyms (value)
               `(progn
                  ,@(mapcar
                     (lambda (key)
                       (let ((err-msg (format nil "Failed to set the ~(~A~) ~~S"
                                              key)))
                         `(def-toplevel-opt ,(format nil "--~(~A~)" key)
                            (with-eval-form (,value ,err-msg)
                              (setf (getf (toplevel-options-sf-metadata opt)
                                          ,key)
                                    ,value)))))
                     incudine.util::*sf-metadata-keywords*)))))

  (defun sf-header-type-list ()
    (sf-format-list sf:get-format-major-count sf:get-format-major
                    "~A~12t~A~%"))

  (defun sf-data-format-list ()
    (sf-format-list sf:get-format-subtype-count sf:get-format-subtype
      #.(format nil "~~A~~~Dt~~A~~%"
          (+ 2 (reduce #'max (alexandria:hash-table-keys sf::*formats*)
                       :key #'length)))))

  ;;; SBCL options

  (def-toplevel-opt "--script"
    (setf (toplevel-options-userinit-p opt) nil
          (toplevel-options-sysinit-p opt) nil
          (toplevel-options-disable-debugger-p opt) t)
    (set-option (toplevel-options-script opt)))

  (def-toplevel-opt "--sysinit"
    (set-option (toplevel-options-sysinit opt)))

  (def-toplevel-opt "--no-sysinit"
    (setf (toplevel-options-sysinit-p opt) nil)
    options)

  (def-toplevel-opt "--userinit"
    (set-option (toplevel-options-userinit opt)))

  (def-toplevel-opt "--no-userinit"
    (setf (toplevel-options-userinit-p opt) nil)
    options)

  (def-toplevel-opt "--eval"
    (let ((value (car options)))
      (add-toplevel-function
       (lambda ()
         ;; Based on SB-IMPL::PROCESS-EVAL/LOAD-OPTIONS
         (with-simple-restart (continue "Ignore runtime option --eval ~S."
                                        value)
           (multiple-value-bind (expr pos) (read-from-string value)
             (if (eq value (read-from-string value nil value :start pos))
                 (eval expr)
                 (error "Multiple expressions in --eval option: ~S"
                        value)))))
       opt)
      (add-consumed-option opt value t)
      (cdr options)))

  (def-toplevel-opt "--load"
    (with-eval-form (value "Ignore runtime option --load ~S.")
      (let ((*standard-output* *logger-stream*))
        (load (sb-ext:native-pathname value)))))

  (def-toplevel-opt "--disable-debugger"
    (setf (toplevel-options-disable-debugger-p opt) :force)
    options)

  (def-toplevel-opt "--noinform"
    (setf (toplevel-options-inform-p opt) nil)
    options)

  (def-toplevel-opt "--noprint"
    (setf (toplevel-options-print-p opt) nil)
    options)

  (def-toplevel-opt "--quit"
    (setf (toplevel-options-quit-p opt) t)
    options)

  (def-toplevel-opt "--non-interactive"
    (setf (toplevel-options-quit-p opt) t
          (toplevel-options-disable-debugger-p opt) t)
    options)

  ;;; Incudine options

  (def-toplevel-opt ("-b" . "--sndfile-buffer-size")
    (with-eval-form (bufsize "Failed to set the buffer size ~D" t)
      (setf *sndfile-buffer-size* bufsize)))

  (def-toplevel-opt ("-B" . "--block-size")
    (with-eval-form (block-size "Failed to set the block size ~D" t)
      (eval `(set-rt-block-size ,block-size))))

  (def-toplevel-opt ("-c" . "--channels")
    (with-eval-form (outputs "Failed to set the number of output channels ~D" t)
      (setf (toplevel-options-ochans opt) outputs)
      (if (eq (rt-status) :started)
          (set-number-of-channels (if (zerop (toplevel-options-ichans opt))
                                      *number-of-input-bus-channels*
                                      (toplevel-options-ichans opt))
                                  outputs)
          (setf *number-of-output-bus-channels* outputs))))

  (def-toplevel-opt "--client-name"
    (set-option *client-name*))

  (def-toplevel-opt "--compile-score-contents"
    (with-eval-form (nil "Cannot compile the contents of the score")
      (setf (toplevel-options-compile-rego-contents-p opt) t)))

  (def-toplevel-opt ("-F" . "--data-format")
    (with-eval-form (df "Failed to set the data format ~A")
      (setf *default-data-format* df)))

  (def-toplevel-opt ("--data-format-list")
    (sf-data-format-list)
    (sb-ext:exit))

  (def-toplevel-opt "--default-table-size"
    (set-option *default-table-size* t))

  (def-toplevel-opt ("-d" . "--duration")
    (with-eval-form (dur "Failed to set the duration to ~A seconds" t)
      (setf (toplevel-options-duration opt) dur)))

  (def-toplevel-opt "--debug"
    (setf (toplevel-options-debug-p opt) t)
    (with-eval-form (nil "Failed to set the logger level")
      (setf (logger-level) :debug)))

  (def-toplevel-opt "--disk-guard-size"
    (with-eval-form (size "Failed to set the disk guard size ~D" t)
      (setf *bounce-to-disk-guard-size* size)))

  (def-toplevel-opt ("-h" . "--help")
    (usage)
    (sb-ext:exit))

  (def-toplevel-opt ("-H" . "--header-type")
    (with-eval-form (ht "Failed to set the header type ~A")
      (setf *default-header-type* ht)))

  (def-toplevel-opt ("--header-type-list")
    (sf-header-type-list)
    (sb-ext:exit))

  (def-toplevel-opt ("-i" . "--infile")
    (with-eval-form (infile "Failed to set the input ~S")
      (setf (toplevel-options-infile opt) infile)))

  (def-toplevel-opt "--input-channels"
    (with-eval-form (inputs "Failed to set the number of input channels ~D" t)
      (setf (toplevel-options-ichans opt) inputs)
      (if (eq (rt-status) :started)
          (set-number-of-channels inputs
                                  (if (zerop (toplevel-options-ochans opt))
                                      *number-of-output-bus-channels*
                                      (toplevel-options-ochans opt)))
          (setf *number-of-input-bus-channels* inputs))))

  (def-toplevel-opt "--interpret-score-contents"
    (with-eval-form (nil "Cannot interpret the contents of the score")
      (setf (toplevel-options-compile-rego-contents-p opt) nil)))

  (def-toplevel-opt ("-L" . "--logfile")
    (setf (toplevel-options-disable-debugger-p opt) t)
    (with-eval-form (pathname "Failed to open ~S")
      (setf *logger-stream*
            (open pathname :direction :output :if-exists :supersede))))

  (def-toplevel-opt "--lisp-version"
    (write-line #.(format nil "SBCL ~A" (lisp-implementation-version)))
    (sb-ext:exit))

  (def-toplevel-opt "--logtime"
    (setf (toplevel-options-disable-debugger-p opt) t)
    (setf (toplevel-options-logtime-p opt) t)
    (with-eval-form (value "Failed to set logtime ~S")
      (let ((time-fmt (subseq value 0 3)))
        (setf (logger-time)
              (cond ((string-equal time-fmt "sam") :samp)
                    ((string-equal time-fmt "sec") :sec))))))

  (def-toplevel-opt "--max-number-of-channels"
    (set-option *max-number-of-channels* t))

  (def-toplevel-opt "--max-number-of-nodes"
    (set-option *max-number-of-channels* t))

  (def-toplevel-opt ("-N" . "--no-realtime")
    (setf (toplevel-options-disable-debugger-p opt) t)
    (with-eval-form (nil "Failed to stop realtime")
      (rt-stop)))

  (def-toplevel-opt "--nrt-edf-heap-size"
    (set-option *rt-edf-heap-size* t))

  (def-toplevel-opt "--nrt-pool-size"
    (set-option incudine.util::*foreign-nrt-memory-pool-size* t))

  (def-toplevel-opt "--nrt-priority"
    (set-option *nrt-priority* t))

  (def-toplevel-opt ("-o" . "--outfile")
    (with-eval-form (outfile "Failed to set the output ~S")
      (setf (toplevel-options-outfile opt) outfile)))

  (def-toplevel-opt ("-p" . "--period")
    (set-option (rt-params-frames-per-buffer *rt-params*) t))

  (def-toplevel-opt "--pad"
    (with-eval-form (pad "Failed to extend the duration by ~A seconds" t)
      (setf (toplevel-options-duration opt) (- pad))))

  (def-toplevel-opt ("-r" . "--rate")
    (with-eval-form (sr "Failed to set the sample rate ~A" t)
      (set-sample-rate sr)))

  (def-toplevel-opt ("-R" . "--realtime")
    (setf (toplevel-options-disable-debugger-p opt) t)
    (setf *fill-rt-memory-pools-p* t)
    (with-eval-form (nil "Failed to start realtime")
      (when (changed-number-of-channels-p opt)
        ;; Change the size of the rt buffers.
        (set-number-of-channels (toplevel-options-ichans opt)
                                (toplevel-options-ochans opt)))
      (rt-start)))

  (def-toplevel-opt "--receiver-priority"
    (set-option *receiver-default-priority* t))

  (def-toplevel-opt "--rt-edf-heap-size"
    (set-option *rt-edf-heap-size* t))

  (def-toplevel-opt "--rt-priority"
    (set-option *rt-priority* t))

  (def-toplevel-opt "--rt-pool-size"
    (set-option incudine.util::*foreign-rt-memory-pool-size* t))

  (def-toplevel-opt "--sample-pool-size"
    (set-option incudine.util::*foreign-sample-pool-size* t))

  (def-toplevel-opt "--score-args"
    (with-eval-form (args "Failed to set the score arguments ~S" t)
      (setf (toplevel-options-score-args opt) args)))

  (def-toplevel-opt "--sound-velocity"
    (set-option incudine.config::*sound-velocity* t))

  (def-toplevel-opt "--swank-server"
    (with-eval-form (port "Failed to start Swank server at port ~D" t)
      (assert (typep port '(unsigned-byte 16)))
      (setf (toplevel-options-swank-server-port opt) port)))

  (def-toplevel-opt ("-T" . "--tempo")
    (with-eval-form (bpm "Failed to set the BPM ~A" t)
      (setf *default-bpm* bpm)))

  (def-toplevel-opt ("-v" . "--verbose")
    (setf (logger-level) :info)
    options)

  (def-toplevel-opt "--version"
    (write-line #.(concatenate 'string "Incudine " (incudine-version)))
    (sb-ext:exit))

  (set-metadata))

(defun posix-argv-to-array ()
  (let ((argv (if (equal (second sb-ext:*posix-argv*) "--")
                  ;; Skip "--", possibly used to separate toplevel and
                  ;; user options.
                  (cons (car sb-ext:*posix-argv*) (cddr sb-ext:*posix-argv*))
                  sb-ext:*posix-argv*)))
    (make-array (length argv) :initial-contents argv)))

(declaim (special *argv*) (type simple-vector *argv*))

(defun read-arg (index &optional (parse-p t))
  "Read an argument passed to the command line. If PARSE-P is T (default),
the argument is parsed with READ-FROM-STRING."
  (declare (type non-negative-fixnum index))
  (cond ((< index (length *argv*))
         (let ((val (aref *argv* index)))
           (declare (type string val))
           (if parse-p
               (values (read-from-string val))
               val)))
        (parse-p nil)
        (t "")))

(defun banner ()
  (format t "Welcome to Incudine ~A~%~A ~A on ~A ~A"
          (incudine-version) (lisp-implementation-type)
          (lisp-implementation-version) (software-type)
          (machine-type))
  #+sb-aclrepl (format t "~%Type `:help' for the list of commands.")
  #-linedit (terpri))

#+linedit
(defun swank-connection-hooks ()
  (flet ((q (s) (find-symbol s "SWANK")))
    (let ((new-connection-hook (q "*NEW-CONNECTION-HOOK*")))
      (if (null new-connection-hook)
          (warn "No SWANK::*NEW-CONNECTION-HOOK* variable")
          (let ((connection-closed-hook (q "*CONNECTION-CLOSED-HOOK*")))
            (if (null connection-closed-hook)
                (warn "No SWANK::*CONNECTION-CLOSED-HOOK* variable")
                (flet ((add-hook (place function)
                         (pushnew function (symbol-value place))))
                  (add-hook new-connection-hook 'uninstall-linedit-repl)
                  (add-hook connection-closed-hook 'install-linedit-repl))))))))

(defun start-swank-server (port log-p)
  (flet ((q (s) (find-symbol s "SWANK")))
    (let ((res (string-trim '(#\; #\Space #\Tab #\Newline)
                 (with-output-to-string (s)
                   (progv (list (q "*LOG-OUTPUT*")) (list s)
                     #+linedit (swank-connection-hooks)
                     (funcall (q "CREATE-SERVER") :port port :dont-close t))))))
      (when log-p (format t "~&~A" res)))))

;;; Adapted to PROCESS-INIT-FILE in SB-IMPL::TOPLEVEL-INIT
(defun process-sbcl-init-file (context specified-pathname default-function)
  (let ((thing (or specified-pathname (funcall default-function))))
    (when thing
      (with-open-file (stream (if specified-pathname
                                  (sb-ext:parse-native-namestring thing)
                                  (pathname thing))
                       :if-does-not-exist nil)
        (cond (stream
               (let* ((*load-pathname* (handler-case (pathname stream)
                                         (error () nil)))
                      (*load-truename* (when *load-pathname*
                                         (handler-case (truename stream)
                                           (file-error () nil)))))
                 (sb-int:load-as-source stream :context context)))
              (specified-pathname
               (cerror "Ignore missing init file"
                       "The specified ~A file ~A was not found."
                       context specified-pathname)))))))

(defun parse-filename (string)
  (if (string= string "-")
      string
      (sb-ext:parse-native-namestring string)))

;;; Adapted to SB-IMPL::TOPLEVEL-INIT in `sbcl/src/code/toplevel.lisp'.
(defun incudine-toplevel ()
  (let ((options (cdr sb-ext:*posix-argv*))
        (opt (make-toplevel-options))
        (*fill-nrt-memory-pools-p* nil)
        (*fill-rt-memory-pools-p* nil))
    (declare (type list options))
    (loop while options do
         (let* ((option (car options))
                (fn (if (char= (char option 0) #\-)
                        (cond ((char= (char option 1) #\s)
                               ;; rego file
                               (shift-toplevel-option opt options)
                               (load-compiled-cudo-file
                                 (parse-filename (car options)) opt
                                 (not (string= (car options) "-"))))
                              (t (find-toplevel-option option)))
                        (load-compiled-cudo-file
                          (parse-filename option) opt))))
           (declare (type (or function null) fn))
           (cond (fn (shift-toplevel-option opt options)
                     (setf options (funcall fn opt options))
                     (when (toplevel-options-script opt) (return)))
                 ((string= option "--end-toplevel-options")
                  (return))
                 ((find "--end-toplevel-options" options :test #'string=)
                  (msg error "bad toplevel option: ~S" option)
                  (sb-ext:exit :code 1))
                 (t (return)))))
    (when sb-ext:*posix-argv*
      (setf (cdr sb-ext:*posix-argv*) options)
      (setf *argv* (posix-argv-to-array)))
    (when (toplevel-options-disable-debugger-p opt)
      (sb-ext:disable-debugger))
    (catch 'toplevel-catcher
      (restart-case
          (progn
            (unless (or (toplevel-options-quit-p opt)
                        (toplevel-options-script opt))
              ;; Pre-allocated objects in REPL.
              (setf *fill-nrt-memory-pools-p* t)
              (setf *fill-rt-memory-pools-p* t))
            (funcall *core-init-function*)
            (let ((pkg *package*))
              (when (toplevel-options-sysinit-p opt)
                (process-sbcl-init-file
                  "sysinit" (toplevel-options-sysinit opt)
                  sb-ext:*sysinit-pathname-function*)
                (unless (or (eq pkg *package*)
                            (null (toplevel-options-sysinit opt)))
                  ;; sysinit file of incudine changes the current package.
                  (setf pkg *package*)))
              (when (toplevel-options-userinit-p opt)
                (process-sbcl-init-file
                  "userinit" (toplevel-options-userinit opt)
                  sb-ext:*userinit-pathname-function*)
                (unless (or (eq pkg *package*) (toplevel-options-userinit opt))
                  ;; The default init files of SBCL don't change the
                  ;; current package.
                  (setf *package* pkg))))
            (handler-case
                (progn
                  (dolist (option (nreverse (toplevel-options-functions opt)))
                    (funcall option))
                  (when (toplevel-options-quit-p opt)
                    (sb-ext:exit)))
              (sb-sys:interactive-interrupt ()
                (sb-ext:exit :code 130)))
            (when #1=(toplevel-options-script opt)
              (sb-impl::process-script #1#)
              (incudine-error "BUG: PROCESS-SCRIPT returned")))
        (abort ()
          :report (lambda (s)
                    (write-string
                     (if (toplevel-options-script opt)
                         "Abort script, exiting lisp."
                         "Skip to toplevel READ/EVAL/PRINT loop.")
                     s))
          (values))
        (exit ()
          :report "Exit Incudine."
          :test (lambda (c) (declare (ignore c)) (not #1#))
          (sb-ext:exit :code 1))))
    (unless (eq (toplevel-options-disable-debugger-p opt) :force)
      (sb-ext:enable-debugger))
    (sb-int:flush-standard-output-streams)
    (when (toplevel-options-inform-p opt) (banner))
    #+linedit
    (if (and (interactive-stream-p *standard-input*)
             (toplevel-options-inform-p opt)
             (toplevel-options-print-p opt))
        (install-linedit-repl)
        #+sb-aclrepl (when (toplevel-options-inform-p opt) (terpri)))
    (when (plusp (toplevel-options-swank-server-port opt))
      (let ((sb-ext:*muffled-warnings* 'style-warning))
        (funcall (read-from-string "swank-loader:init"))
        (start-swank-server (toplevel-options-swank-server-port opt)
                            (toplevel-options-inform-p opt))
        (incudine.util::set-swank-arglist-interface)))
    (unless (toplevel-options-logtime-p opt)
      (setf (logger-time) nil))
    (sb-impl::toplevel-repl (not (toplevel-options-print-p opt)))))
