;;; Copyright (c) 2013 Tito Latini
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

(defvar *bounce-to-disk-guard-size* 300
  "Max size in seconds of the sndfile bounced to disk
when the duration is undefined.")

(defvar *sample-size* 4)
(declaim (type (integer 0 15) *sample-size*))

(defvar *nrt-fifo* (make-fifo 512))
(declaim (type fifo *nrt-fifo*))

(defvar *nrt-from-world-fifo* (make-fifo +fifo-size+))
(declaim (type fifo *nrt-from-world-fifo*))

(defvar *nrt-node-root*
  (let ((group (make-node 0 *max-number-of-nodes*)))
    (setf (node-prev group) :dummy-node
          (node-funcons group) nil
          (node-last group) :dummy-node)
    group))
(declaim (type node *nrt-node-root*))

(defvar *nrt-node-hash* (make-node-hash *max-number-of-nodes*))
(declaim (type int-hash-table *nrt-node-hash*))

(defvar *nrt-bus-channels-size* (+ *max-number-of-channels*   ; outputs
                                   *max-number-of-channels*   ; inputs
                                   *number-of-bus-channels*))
(declaim (type bus-number *nrt-bus-channels-size*))

(defvar *nrt-bus-channels* (foreign-alloc-sample *nrt-bus-channels-size*))
(declaim (type foreign-pointer *nrt-bus-channels*))

(defvar %nrt-bus-pointer-offset (* *max-number-of-channels*
                                   +foreign-sample-size+))
(declaim (type non-negative-fixnum %nrt-bus-pointer-offset))

(defvar *nrt-input-pointer* (inc-pointer *nrt-bus-channels*
                                         %nrt-bus-pointer-offset))
(declaim (type foreign-pointer *nrt-input-pointer*))

(defvar *nrt-bus-pointer* (inc-pointer *nrt-input-pointer*
                                       %nrt-bus-pointer-offset))
(declaim (type foreign-pointer *nrt-bus-pointer*))

(defvar *nrt-output-peak-values*
  (foreign-alloc-sample *max-number-of-channels*))
(declaim (type foreign-pointer *nrt-output-peak-values*))

(defvar *nrt-out-of-range-counter* (make-array *max-number-of-channels*
                                               :initial-element 0))
(declaim (type simple-vector *nrt-out-of-range-counter*))

(defvar *nrt-temp-node* (incudine.edf::make-node))
(declaim (type incudine.edf::node *nrt-temp-node*))

(defvar *nrt-edf-heap-size* 65536)
(declaim (type non-negative-fixnum *nrt-edf-heap-size*))

(defvar *nrt-heap* (make-array *nrt-edf-heap-size* :element-type 'incudine.edf::node
                     :initial-contents (loop repeat *nrt-edf-heap-size*
                                             collect (incudine.edf::make-node))))
(declaim (type simple-vector *nrt-heap*))

(defvar *nrt-sample-counter* (foreign-alloc :double :initial-element 0.0d0))
(declaim (type foreign-pointer *nrt-sample-counter*))

(defvar *nrt-tempo* (make-tempo 60))
(declaim (type tempo *nrt-tempo*))

(defmacro perform-fifos ()
  `(loop until (and (fifo-empty-p *nrt-fifo*)
                    (fifo-empty-p *nrt-from-world-fifo*)) do
        (fifo-perform-functions *nrt-fifo*)
        (unless (fifo-empty-p *nrt-from-world-fifo*)
          ;; Probably *nrt-from-world-fifo* is always empty and
          ;; never performed
          (fast-nrt-perform-functions))
        (incudine.edf::sched-loop)))

(defun flush-all-fifos ()
  (rt-eval ()
    (dolist (f (list *to-engine-fifo* incudine::*from-engine-fifo*
                     *from-world-fifo* *fast-from-engine-fifo*
                     *fast-to-engine-fifo*))
      (fifo-flush f))))

(defmacro read-snd-buffer (buf remain index channels)
  (with-gensyms (ch)
    `(foreach-channel (,ch ,channels)
       (setf (data-ref *input-pointer* ,ch)
             (data-ref ,buf ,index))
       (decf ,remain)
       (incf ,index))))

(defmacro write-snd-buffer (buf index channels)
  (with-gensyms (ch)
    `(foreach-channel (,ch ,channels)
       (setf (data-ref ,buf ,index)
             (data-ref *output-pointer* ,ch))
       (incf ,index))))

(declaim (inline clear-inputs))
(defun clear-inputs ()
  (foreign-zero-sample *input-pointer*
                       *number-of-input-bus-channels*))

(declaim (inline clear-outputs))
(defun clear-outputs ()
  (foreign-zero-sample *output-pointer*
                       *number-of-output-bus-channels*))

(defmacro read-sample (sndfile ptr items)
  `(#+double-samples sf:read-double
    #-double-samples sf:read-float
    ,sndfile ,ptr ,items))

(defmacro write-sample (sndfile ptr items)
  `(#+double-samples sf:write-double
    #-double-samples sf:write-float
    ,sndfile ,ptr ,items))

(defmacro nrt-loop (snd data bufsize count channels)
  `(progn
     (incudine.edf::sched-loop)
     (perform-fifos)
     (tick-func)
     (write-snd-buffer ,data ,count ,channels)
     (when (= ,count ,bufsize)
       (write-sample ,snd ,data ,bufsize)
       (setf ,count 0))
     (clear-outputs)
     (incf-sample-counter)))

(defmacro nrt-loop-with-infile (snd-in data-in snd-out data-out
                                bufsize count out-channels input-remain
                                input-index in-channels input-eof-p)
  `(progn
     (unless ,input-eof-p
       (cond ((plusp ,input-remain)
              (read-snd-buffer ,data-in ,input-remain ,input-index ,in-channels))
             (t ;; Fill the input buffer
              (setf ,input-remain (read-sample ,snd-in ,data-in ,bufsize))
              (setf ,input-index 0)
              (cond ((zerop ,input-remain)
                     (clear-inputs)
                     (setf ,input-eof-p t))
                    (t (read-snd-buffer ,data-in ,input-remain ,input-index
                                        ,in-channels))))))
     (nrt-loop ,snd-out ,data-out ,bufsize ,count ,out-channels)))

(declaim (inline zeroes-nrt-bus-channels))
(defun zeroes-nrt-bus-channels ()
  (foreign-zero-sample *nrt-bus-channels* *nrt-bus-channels-size*))

(declaim (inline nrt-cleanup))
(defun nrt-cleanup ()
  (flush-all-fifos)
  ;; Flush the EDF
  (flush-pending)
  (node-free *node-root*)
  (incudine.edf::sched-loop))

(defmacro with-sf-info ((var frames sample-rate channels header-type
                         data-format) &body body)
  `(let ((,var (sf:make-info :frames ,frames
                             :sample-rate (sample->fixnum ,sample-rate)
                             :channels ,channels
                             :format (sf:get-format (list ,header-type
                                                          ,data-format)))))
     ,@body))

(defmacro with-sf-input ((var path data-var channels-var bufsize
                          input-remain input-index max-frames
                          pad-at-the-end-p)
                         &body body)
  (with-gensyms (info)
    `(let ((,info (sf:make-info)))
       (sf:with-open (,var ,path :info ,info :mode sf:sfm-read)
         (with-foreign-object (,data-var 'sample ,bufsize)
           (let ((,channels-var (sf:channels ,info))
                 (,input-remain 0)
                 (,input-index 0))
             (declare (type non-negative-fixnum ,channels-var ,input-remain
                            ,input-index))
             (when ,pad-at-the-end-p
               (setf ,max-frames (sf:frames ,info)))
             ,@body))))))

(defmacro with-nrt ((channels &key (bpm 60)) &body body)
  `(let ((*to-engine-fifo* *nrt-fifo*)
         (*from-engine-fifo* *nrt-fifo*)
         (*from-world-fifo* *nrt-from-world-fifo*)
         (*fast-from-engine-fifo* *nrt-fifo*)
         (*fast-to-engine-fifo* *nrt-fifo*)
         (*rt-thread* (bt:current-thread))
         (*node-hash* *nrt-node-hash*)
         (*node-root* *nrt-node-root*)
         (*bus-channels* *nrt-bus-channels*)
         (*output-pointer* *nrt-bus-channels*)
         (*input-pointer* *nrt-input-pointer*)
         (*bus-pointer* *nrt-bus-pointer*)
         (*number-of-output-bus-channels* ,channels)
         (*output-peak-values* *nrt-output-peak-values*)
         (*out-of-range-counter* *nrt-out-of-range-counter*)
         (incudine.edf::*next-node* incudine.edf::+node-root+)
         (incudine.edf::*temp-node* *nrt-temp-node*)
         (incudine.edf::*heap* *nrt-heap*)
         (incudine.edf::*heap-size* *nrt-edf-heap-size*)
         (*tempo* *nrt-tempo*)
         (*sample-counter* *nrt-sample-counter*))
     (setf (bpm *tempo*) ,bpm)
     ,@body))

(defun %bounce-to-disk (output-filename duration channels header-type
                        data-format function)
  (declare (type (or string pathname) output-filename) (type function function)
           (type channel-number channels) (type real duration))
  (with-nrt (channels)
    (let* (;; If DURATION is negative or zero, the rendering terminates
           ;; -DURATION seconds after the last event.
           (pad-at-the-end-p (<= duration 0))
           (remain (sample->fixnum (* (abs duration) *sample-rate*)))
           (max-frames (if pad-at-the-end-p
                           ;; Upper limit to avoid to fill the disk when
                           ;; the duration is undefined
                           (- (* (sample->fixnum *sample-rate*)
                                 (max *bounce-to-disk-guard-size* remain))
                              remain)
                           remain))
           (bufsize (* *sndfile-buffer-size* *sample-size*))
           (frame 0)
           (count 0))
      (declare (type non-negative-fixnum bufsize count)
               (type non-negative-fixnum64 remain max-frames frame)
               (type boolean pad-at-the-end-p))
      (nrt-cleanup)
      (zeroes-nrt-bus-channels)
      (reset-sample-counter)
      (%reset-peak-meters)
      (handler-case
          (progn
            ;; Fill the EDF
            (funcall function)
            (with-sf-info (info max-frames *sample-rate* channels
                           header-type data-format)
              (sf:with-open (snd output-filename :info info :mode sf:sfm-write)
                (with-foreign-object (data 'sample bufsize)
                  (locally (declare #.*standard-optimize-settings*
                                    #.*reduce-warnings*)
                    (do ((i 0 (1+ i)))
                        ((or (incudine.edf:heap-empty-p)
                             (= i max-frames))
                         (unless pad-at-the-end-p
                           (setf frame i)))
                      (declare (type non-negative-fixnum64 i))
                      (nrt-loop snd data bufsize count channels))
                    (loop while (< frame remain) do
                         (nrt-loop snd data bufsize count channels)
                         (incf frame))
                    (when (plusp count)
                      (write-sample snd data count))
                    (node-free *node-root*)
                    (incudine.edf::sched-loop)
                    (perform-fifos))))))
        (condition (c) (progn (msg error "~A" c)
                              (nrt-cleanup))))
      (print-peak-info channels)
      output-filename)))

(defun %bounce-to-disk-with-infile (input-filename output-filename duration
                                    channels header-type data-format function)
  (declare (type (or string pathname) input-filename output-filename)
           (type real duration) (type function function)
           (type channel-number channels))
  (with-nrt (channels)
    (let* (;; If DURATION is negative or zero, the rendering terminates
           ;; -DURATION seconds after the last event.
           (pad-at-the-end-p (<= duration 0))
           (remain (sample->fixnum (* (abs duration) *sample-rate*)))
           (max-frames (if pad-at-the-end-p
                           ;; Frames of the input file
                           0
                           remain))
           (bufsize (* *sndfile-buffer-size* *sample-size*))
           (frame 0)
           (count 0)
           (input-eof-p nil))
      (declare (type non-negative-fixnum bufsize count)
               (type non-negative-fixnum64 remain max-frames frame)
               (type boolean pad-at-the-end-p input-eof-p))
      (nrt-cleanup)
      (zeroes-nrt-bus-channels)
      (reset-sample-counter)
      (%reset-peak-meters)
      (handler-case
          (progn
            ;; Fill the EDF
            (funcall function)
            (with-sf-input (snd-in input-filename data-in in-channels bufsize
                            input-remain input-index max-frames
                            pad-at-the-end-p)
              (with-sf-info (info max-frames *sample-rate* channels
                             header-type data-format)
                (sf:with-open (snd-out output-filename :info info :mode sf:sfm-write)
                  (with-foreign-object (data-out 'sample bufsize)
                    (locally (declare #.*standard-optimize-settings*
                                      #.*reduce-warnings*)
                      (do ((i 0 (1+ i)))
                          ((= i max-frames)
                           (unless pad-at-the-end-p
                             (setf frame i)))
                        (declare (type non-negative-fixnum64 i))
                        (nrt-loop-with-infile snd-in data-in snd-out data-out bufsize
                                              count channels input-remain input-index
                                              in-channels input-eof-p))
                      (loop while (< frame remain) do
                           (nrt-loop-with-infile snd-in data-in snd-out data-out bufsize
                                                 count channels input-remain input-index
                                                 in-channels input-eof-p)
                           (incf frame))
                      (when (plusp count)
                        (write-sample snd-out data-out count))
                      (node-free *node-root*)
                      (incudine.edf::sched-loop)
                      (perform-fifos)))))))
        (condition (c) (progn (msg error "~A" c)
                              (nrt-cleanup))))
      (print-peak-info channels)
      output-filename)))

(defmacro bounce-to-disk ((output-filename &key input-filename
                           (channels *number-of-output-bus-channels*)
                           duration (pad 2) (header-type *default-header-type*)
                           (data-format *default-data-format*))
                          &body body)
  `(,@(if input-filename
          `(%bounce-to-disk-with-infile ,input-filename)
          '(%bounce-to-disk))
    ,output-filename ,(or duration `(- ,pad)) ,channels ,header-type ,data-format
    ,(let ((fst (car body)))
       (if (and (null (cdr body))
                (eq (car fst) 'funcall)
                (null (cddr fst)))
           ;; Simplify a single (FUNCALL FN)
           (cadr fst)
           `(lambda () ,@body)))))

;;; Score file ala MusicN, Csound, etc.. with a little difference:
;;; there are not statements of the score but only function names.
;;;
;;; Format of a line in the note-list file:
;;;
;;;   start-time-in-seconds   function-name   [arg1]   [arg2]   ...
;;;
;;; TODO: time in beats with the possibility to add a tempo-envelope
;;;
(declaim (inline score-skip-line-p))
(defun score-skip-line-p (line)
  (declare (type string line))
  (let ((non-blank (find-if-not (lambda (x)
                                  (or (char= x #\space)
                                      (char= x #\tab)))
                                line)))
    (or (null non-blank)
        (char= non-blank #\;))))

(defmacro %at-sample (offset seconds func-symbol &rest args)
  `(at (+ ,offset (* ,seconds *sample-rate*))
       #',func-symbol ,@args))

(defun scofile->sexp (path)
  (flet ((remove-comment (line)
           (subseq line 0 (position #\; line))))
    `(lambda ()
       (let ((time (now)))
         ,@(with-open-file (score path)
             (loop for line = (read-line score nil nil)
                   while line
                   unless (score-skip-line-p line)
                   collect (macroexpand-1
                            (read-from-string
                             (concatenate 'string "(incudine::%at-sample TIME "
                                          (remove-comment line) ")")))))))))

(defmacro scofile->function (path)
  (scofile->sexp path))

(defun scofile->lispfile (score-file lisp-file)
  (with-open-file (lfile lisp-file :direction :output
                   :if-exists :supersede)
    (write (scofile->sexp score-file) :stream lfile)
    (terpri lfile)
    lisp-file))
