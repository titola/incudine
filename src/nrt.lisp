;;; Copyright (c) 2013-2016 Tito Latini
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

(defvar *%nrt-input-pointer*
  (foreign-alloc-sample (* *max-buffer-size*
                           %number-of-input-bus-channels)))
(declaim (type foreign-pointer *%nrt-input-pointer*))

(defvar *nrt-input-pointer*
  (cffi:foreign-alloc :pointer :initial-element *%nrt-input-pointer*))
(declaim (type foreign-pointer *nrt-input-pointer*))

(defvar *%nrt-output-pointer*
  (foreign-alloc-sample (* *max-buffer-size*
                           *number-of-output-bus-channels*)))
(declaim (type foreign-pointer *%nrt-output-pointer*))

(defvar *nrt-output-pointer*
  (cffi:foreign-alloc :pointer :initial-element *%nrt-output-pointer*))
(declaim (type foreign-pointer *nrt-output-pointer*))

(defvar *nrt-bus-pointer* (foreign-alloc-sample *number-of-bus-channels*))
(declaim (type foreign-pointer *nrt-bus-pointer*))

(defvar *nrt-output-peak-values*
  (foreign-alloc-sample *max-number-of-channels*))
(declaim (type foreign-pointer *nrt-output-peak-values*))

(defvar *nrt-out-of-range-counter* (make-array *max-number-of-channels*
                                               :initial-element 0))
(declaim (type simple-vector *nrt-out-of-range-counter*))

(defvar *nrt-edf-heap* (incudine.edf:make-heap *nrt-edf-heap-size*))
(declaim (type incudine.edf:heap *nrt-edf-heap*))

(defvar *nrt-sample-counter*
  (foreign-alloc 'sample :initial-element +sample-zero+))
(declaim (type foreign-pointer *nrt-sample-counter*))

(defvar *nrt-tempo* (make-tempo *default-bpm*))
(declaim (type tempo *nrt-tempo*))

(declaim (inline nrt-edf-heap-p))
(defun nrt-edf-heap-p ()
  (eq incudine.edf:*heap* *nrt-edf-heap*))

(defmacro perform-fifos ()
  `(loop until (and (fifo-empty-p *nrt-fifo*)
                    (fifo-empty-p *nrt-from-world-fifo*)) do
        (fifo-perform-functions *nrt-fifo*)
        (unless (fifo-empty-p *nrt-from-world-fifo*)
          ;; Probably *nrt-from-world-fifo* is always empty and
          ;; never performed.
          (fast-nrt-perform-functions))
        (incudine.edf:sched-loop)))

(defun flush-all-fifos ()
  (rt-eval ()
    (dolist (f (list *to-engine-fifo* incudine::*from-engine-fifo*
                     *from-world-fifo* *fast-from-engine-fifo*
                     *fast-to-engine-fifo*))
      (fifo-flush f))))

(defmacro read-snd-buffer (buf remain index channels)
  (with-gensyms (ch)
    `(dochannels (,ch ,channels)
       (setf (smp-ref (input-pointer) ,ch)
             (smp-ref ,buf ,index))
       (decf ,remain)
       (incf ,index))))

(defmacro write-snd-buffer (buf index channels)
  (with-gensyms (ch)
    `(dochannels (,ch ,channels)
       (setf (smp-ref ,buf ,index)
             (smp-ref (output-pointer) ,ch))
       (incf ,index))))

(declaim (inline clear-inputs))
(defun clear-inputs ()
  (foreign-zero-sample (input-pointer) *number-of-input-bus-channels*))

(declaim (inline clear-outputs))
(defun clear-outputs ()
  (foreign-zero-sample (output-pointer) *number-of-output-bus-channels*))

(defmacro read-sample (sndfile ptr items)
  `(#+double-samples sf:read-double
    #-double-samples sf:read-float
    ,sndfile ,ptr ,items))

(defmacro write-sample (sndfile ptr items)
  `(#+double-samples sf:write-double
    #-double-samples sf:write-float
    ,sndfile ,ptr ,items))

(defmacro nrt-loop (snd data bufsize count channels &optional stop-if-empty-p)
  `(progn
     (incudine.edf:sched-loop)
     (perform-fifos)
     ,(when stop-if-empty-p
        `(if (incudine.edf:heap-empty-p) (return)))
     (compute-tick)
     (write-snd-buffer ,data ,count ,channels)
     (when (= ,count ,bufsize)
       (write-sample ,snd ,data ,bufsize)
       (setf ,count 0))
     (clear-outputs)
     (incf-sample-counter)))

(defmacro nrt-loop-with-infile (snd-in data-in snd-out data-out
                                bufsize count out-channels input-remain
                                input-index in-channels input-eof-p
                                &optional stop-if-empty-p)
  `(progn
     (unless ,input-eof-p
       (cond ((plusp ,input-remain)
              (read-snd-buffer ,data-in ,input-remain ,input-index
                               ,in-channels))
             (t ;; Fill the input buffer.
              (setf ,input-remain (read-sample ,snd-in ,data-in ,bufsize))
              (setf ,input-index 0)
              (cond ((zerop ,input-remain)
                     (clear-inputs)
                     (setf ,input-eof-p t))
                    (t (read-snd-buffer ,data-in ,input-remain ,input-index
                                        ,in-channels))))))
     (nrt-loop ,snd-out ,data-out ,bufsize ,count ,out-channels ,stop-if-empty-p)))

(declaim (inline zeroes-nrt-bus-channels))
(defun zeroes-nrt-bus-channels ()
  (foreign-zero-sample *%nrt-input-pointer* *number-of-input-bus-channels*)
  (foreign-zero-sample *%nrt-output-pointer* *number-of-output-bus-channels*)
  (foreign-zero-sample *nrt-bus-pointer* *number-of-bus-channels*))

(declaim (inline nrt-cleanup))
(defun nrt-cleanup ()
  (flush-all-fifos)
  ;; Flush the EDF.
  (flush-pending)
  (node-free *node-root*)
  (incudine.edf:sched-loop))

(defun write-sf-metadata-plist (sf plist)
  (macrolet ((metadata-constants ()
               `(list ,@(loop for key in incudine.util::*sf-metadata-keywords*
                              collect key
                              collect (format-symbol (find-package :sndfile)
                                                     "STR-~A" key)))))
    (labels ((write-md (key val)
               (let ((str-type (getf (metadata-constants) key)))
                 (if str-type (sf:set-string sf str-type val))))
             (write-all-md (pl)
               (when pl
                 (write-md (car pl) (cadr pl))
                 (write-all-md (cddr pl)))))
      (write-all-md plist)
      sf)))

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
  (with-gensyms (info %path-or-stdin path-or-stdin open-stdin-p)
    `(let ((,%path-or-stdin ,path)
           (,info (sf:make-info)))
       (multiple-value-bind (,path-or-stdin ,open-stdin-p)
           (if (and (stringp ,%path-or-stdin)
                    (string= ,%path-or-stdin "-"))
               ;; Read from standard input.
               (values (incudine.util::stdin-fd) t)
               (values ,%path-or-stdin nil))
         (sf:with-open (,var ,path-or-stdin :info ,info :mode sf:sfm-read
                             :open-fd-p ,open-stdin-p)
           (with-foreign-array (,data-var 'sample ,bufsize)
             (let ((,channels-var (sf:channels ,info))
                   (,input-remain 0)
                   (,input-index 0))
               (declare (type non-negative-fixnum ,channels-var ,input-remain
                              ,input-index))
               (when ,pad-at-the-end-p
                 (setf ,max-frames (sf:frames ,info)))
               ,@body)))))))

(defmacro with-nrt ((channels sample-rate &key (bpm *default-bpm*)) &body body)
  `(incudine.util::with-local-sample-rate (,sample-rate)
     (let ((*to-engine-fifo* *nrt-fifo*)
           (*from-engine-fifo* *nrt-fifo*)
           (*from-world-fifo* *nrt-from-world-fifo*)
           (*fast-from-engine-fifo* *nrt-fifo*)
           (*fast-to-engine-fifo* *nrt-fifo*)
           (*rt-thread* (bt:current-thread))
           (*allow-rt-memory-pool-p* nil)
           (*node-hash* *nrt-node-hash*)
           (*node-root* *nrt-node-root*)
           (*bus-pointer* *nrt-bus-pointer*)
           (*output-pointer* *nrt-output-pointer*)
           (*input-pointer* *nrt-input-pointer*)
           (*number-of-output-bus-channels* ,channels)
           (*block-size* 1)
           (*block-samples* ,channels)
           (*output-peak-values* *nrt-output-peak-values*)
           (*out-of-range-counter* *nrt-out-of-range-counter*)
           (incudine.edf:*heap* *nrt-edf-heap*)
           (incudine.edf:*heap-size* *nrt-edf-heap-size*)
           (*tempo* *nrt-tempo*)
           (*sample-counter* *nrt-sample-counter*))
       (setf (bpm *tempo*) ,bpm)
       ,@body)))

(defmacro write-to-disk-loop ((index limit) &body body)
  `(loop while (< ,index ,limit) do ,@body (incf ,index)))

(defmacro write-to-disk ((frame-var max-frames remain snd bufsize data-var
                          metadata count padding-p &optional stop-if-empty-p)
                         form)
  `(progn
     (write-sf-metadata-plist ,snd ,metadata)
     (with-foreign-array (,data-var 'sample ,bufsize)
       (locally (declare #.*standard-optimize-settings*
                         #.*reduce-warnings*)
         (write-to-disk-loop (,frame-var ,max-frames)
           ,(if stop-if-empty-p `(,@form ,padding-p) form))
         (when ,padding-p
           (setf ,frame-var 0))
         (write-to-disk-loop (,frame-var ,remain) ,form)
         (when (plusp ,count)
           (write-sample ,snd ,data-var ,count))
         (node-free *node-root*)
         (incudine.edf:sched-loop)
         (perform-fifos)))))

(defun %bounce-to-disk (output-filename duration channels sample-rate
                        header-type data-format metadata function)
  (declare (type (or string pathname) output-filename) (type function function)
           (type channel-number channels) (type real duration))
  (with-nrt (channels sample-rate)
    (let* (;; If DURATION is negative or zero, the rendering terminates
           ;; -DURATION seconds after the last event.
           (pad-at-the-end-p (<= duration 0))
           (remain (sample->fixnum (* (abs duration) *sample-rate*)))
           (max-frames (if pad-at-the-end-p
                           ;; Upper limit to avoid to fill the disk when
                           ;; the duration is undefined.
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
            ;; Fill the EDF.
            (funcall function)
            (with-sf-info (info max-frames *sample-rate* channels
                           header-type data-format)
              (multiple-value-bind (path-or-stdout open-stdout-p)
                  (if (and (stringp output-filename)
                           (string= output-filename "-"))
                      ;; Write to standard output.
                      (values (incudine.util::stdout-fd) t)
                      (values output-filename nil))
                (sf:with-open (snd path-or-stdout :info info :mode sf:sfm-write
                               :open-fd-p open-stdout-p)
                  (write-to-disk (frame max-frames remain snd bufsize data
                                  metadata count pad-at-the-end-p t)
                    ;; COUNT is incremented by CHANNELS.
                    (nrt-loop snd data bufsize count channels))))))
        (condition (c) (progn (msg error "~A" c)
                              (nrt-cleanup))))
      (print-peak-info channels)
      output-filename)))

(defun %bounce-to-disk-with-infile (input-filename output-filename duration
                                    channels sample-rate header-type data-format
                                    metadata function)
  (declare (type (or string pathname) input-filename output-filename)
           (type real duration) (type function function)
           (type channel-number channels))
  (with-nrt (channels sample-rate)
    (let* (;; If DURATION is negative or zero, the rendering terminates
           ;; -DURATION seconds after the last event.
           (pad-at-the-end-p (<= duration 0))
           (remain (sample->fixnum (* (abs duration) *sample-rate*)))
           (max-frames (if pad-at-the-end-p
                           ;; Frames of the input file.
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
            ;; Fill the EDF.
            (funcall function)
            (with-sf-input (snd-in input-filename data-in in-channels bufsize
                            input-remain input-index max-frames
                            pad-at-the-end-p)
              (with-sf-info (info max-frames *sample-rate* channels
                             header-type data-format)
                (multiple-value-bind (path-or-stdout open-stdout-p)
                    (if (and (stringp output-filename)
                             (string= output-filename "-"))
                        ;; Write to standard output.
                        (values (incudine.util::stdout-fd) t)
                        (values output-filename nil))
                  (sf:with-open (snd-out path-or-stdout :info info
                                 :mode sf:sfm-write :open-fd-p open-stdout-p)
                    (write-to-disk (frame max-frames remain snd-out bufsize
                                    data-out metadata count pad-at-the-end-p)
                      ;; COUNT and INPUT-INDEX are incremented respectively
                      ;; by CHANNELS and IN-CHANNELS.
                      ;; INPUT-REMAIN is decremented by IN-CHANNELS.
                      (nrt-loop-with-infile snd-in data-in snd-out
                                  data-out bufsize count channels input-remain
                                  input-index in-channels input-eof-p)))))))
        (condition (c) (progn (msg error "~A" c)
                              (nrt-cleanup))))
      (print-peak-info channels)
      output-filename)))

(defmacro bounce-function (form)
  (let ((fst (car form)))
    (if (and (null (cdr form))
             (eq (car fst) 'funcall)
             (null (cddr fst)))
        ;; Simplify a single (FUNCALL FN).
        (cadr fst)
        `(lambda () ,@form))))

(defmacro bounce-to-disk ((output-filename &key input-filename
                           (channels *number-of-output-bus-channels*)
                           duration (pad 2) (sample-rate *sample-rate*)
                           header-type data-format metadata)
                          &body body)
  `(,@(if input-filename
          `(%bounce-to-disk-with-infile ,input-filename)
          '(%bounce-to-disk))
    ,output-filename ,(or duration `(- ,pad)) ,channels ,sample-rate
    ,(or header-type '*default-header-type*)
    ,(or data-format '*default-data-format*)
    ,metadata (bounce-function ,body)))

(defmacro nrt-write-buffer-loop (in-data out-data in-count out-count
                                 in-channels out-channels in-size mix-p)
  (with-gensyms (ch value)
    `(progn
       (when (and ,in-data (< ,in-count ,in-size))
         (dochannels (,ch ,in-channels)
           (setf (smp-ref (input-pointer) ,ch)
                 (smp-ref ,in-data ,in-count))
           (incf ,in-count)))
       (incudine.edf:sched-loop)
       (perform-fifos)
       ;; Update the peak meters if MIX-P is NIL.
       (compute-tick (null ,mix-p))
       (dochannels (,ch ,out-channels)
         (cond (,mix-p
                (incf (smp-ref ,out-data ,out-count)
                      (smp-ref (output-pointer) ,ch))
                ;; Update the peak meters after the mix.
                (let ((,value (smp-ref ,out-data ,out-count)))
                  (when (> ,value (smp-ref *output-peak-values* ,ch))
                    (setf (smp-ref *output-peak-values* ,ch) ,value))
                  (when (> ,value 1)
                    (setf (svref *out-of-range-counter* ,ch)
                          (the positive-fixnum
                            (1+ (the positive-fixnum
                                  (svref *out-of-range-counter* ,ch))))))))
               (t (setf (smp-ref ,out-data ,out-count)
                        (smp-ref (output-pointer) ,ch))))
         (incf ,out-count))
       (clear-outputs)
       (incf-sample-counter))))

(defun %bounce-to-buffer (output-buffer input-buffer start frames sample-rate
                          mix-p function)
  (declare (type buffer output-buffer) (type (or buffer null) input-buffer)
           (type function function) (type non-negative-real start)
           (type (or non-negative-real null) frames) (type boolean mix-p))
  (let* ((out-channels (buffer-channels output-buffer))
         (start (floor start))
         (max-frames (max 0 (- (buffer-frames output-buffer) start)))
         (frames (if frames (min (floor frames) max-frames) max-frames))
         (begin (* start out-channels))
         (out-data (buffer-data output-buffer)))
    (declare (type non-negative-fixnum out-channels start max-frames
                   frames begin))
    (multiple-value-bind (in-channels in-size in-data)
        (if input-buffer
            (values (buffer-channels input-buffer) (buffer-size input-buffer)
                    (buffer-data input-buffer)))
      (declare (type (or non-negative-fixnum null) in-channels in-size))
      (with-nrt (out-channels sample-rate)
        (nrt-cleanup)
        (zeroes-nrt-bus-channels)
        (reset-sample-counter)
        (%reset-peak-meters)
        (handler-case
            (progn
              (funcall function)
              (locally (declare #.*standard-optimize-settings*)
                (do ((i 0 (1+ i))
                     (in-count 0)
                     (out-count begin))
                    ((= i frames))
                  (declare (type non-negative-fixnum i in-count out-count))
                  ;; IN-COUNT and OUT-COUNT are incremented respectively
                  ;; by IN-CHANNELS (if INPUT-BUFFER exists) and OUT-CHANNELS.
                  (nrt-write-buffer-loop in-data out-data in-count out-count
                                         in-channels out-channels in-size
                                         mix-p))
                (node-free *node-root*)
                (incudine.edf:sched-loop)
                (perform-fifos)))
          (condition (c) (progn (msg error "~A" c)
                                (nrt-cleanup))))
        (unless (= (buffer-sample-rate output-buffer) *sample-rate*)
          (setf (buffer-sample-rate output-buffer) *sample-rate*))
        (print-peak-info out-channels)
        output-buffer))))

(defmacro bounce-to-buffer ((output-buffer &key input-buffer (start 0) frames
                            (sample-rate *sample-rate*) mix-p) &body body)
  `(%bounce-to-buffer ,output-buffer ,input-buffer ,start ,frames ,sample-rate
                      ,mix-p (bounce-function ,body)))
