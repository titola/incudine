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

(defvar *receivers* (make-hash-table))
(declaim (type hash-table *receivers*))

(defstruct (receiver (:constructor %make-receiver)
                     (:copier nil))
  "Receiver type."
  %stream
  (functions nil :type list)
  (status nil :type boolean)
  (thread nil :type (or null bt:thread))
  (timeout 0 :type non-negative-fixnum))

(defgeneric valid-input-stream-p (obj))

(defun make-receiver (stream &key functions status (timeout 0))
  (when (valid-input-stream-p stream)
    (%make-receiver :%stream stream
                    :functions functions
                    :status status
                    :timeout timeout)))

(defun receiver-stream (obj)
  "Return the input stream of a receiver."
  (declare (type receiver obj))
  (receiver-%stream obj))

(defmethod print-object ((obj receiver) stream)
  (print-unreadable-object (obj stream)
    (format stream "RECEIVER ~S ~A"
            (type-of (receiver-stream obj))
            (if (receiver-status obj) :RUNNING :STOPPED))))

(defun receiver (stream)
  "Return the receiver related to STREAM if it exists. Otherwise,
return NIL."
  (values (gethash stream *receivers*)))

(defun all-receivers ()
  "Return the list of the receivers."
  (alexandria:hash-table-values *receivers*))

(defgeneric recv-start (stream &key)
  (:documentation "Start receiving from STREAM.

PRIORITY is the priority of the receiver-thread that defaults to
*RECEIVER-DEFAULT-PRIORITY*.

If UPDATE-MIDI-TABLE-P is T (default), update the MIDI table used by
DSP and UGEN instances.

PortMidi receiver is started with polling TIMEOUT that defaults to
*MIDI-INPUT-TIMEOUT*.

If STREAM is a CL:INPUT-STREAM, the required READ-FUNCTION is the
function of one argument used to read from the stream. This function
returns the value passed to the responders of STREAM. If an end of
file occurs, the receiver is stopped."))

(defgeneric recv-stop (stream)
  (:documentation "Stop receiving from STREAM."))

(defun recv-status (stream)
  "Receiver status for STREAM. Return :RUNNING, :STOPPED or :UNKNOWN."
  (let ((recv (receiver stream)))
    (if recv
        (if (receiver-status recv) :RUNNING :STOPPED)
        :UNKNOWN)))

(defun recv-functions (stream)
  "Return the list of the receiver-functions called whenever there is
input available from STREAM."
  (let ((recv (receiver stream)))
    (when recv (receiver-functions recv))))

(defun recv-set-priority (thread priority)
  (when (bt:threadp thread)
    (setf (thread-priority thread) priority)))

(defun recv-unset-thread (receiver)
  (loop for i from 0
        with recv-thread = (receiver-thread receiver)
        while (bt:thread-alive-p recv-thread) do
       (sleep .1)
       ;; Force DESTROY-THREAD after 5 seconds.
       (cond ((= i 50)
              (msg warn "forcing destroy-thread on ~A" recv-thread)
              (bt:destroy-thread recv-thread))
             ((= i 100)
              (setf i 70)
              (msg warn "the thread ~A is still alive" recv-thread))))
  (setf (receiver-thread receiver) nil)
  receiver)

(defun add-receiver (stream recv start-function priority)
  (let ((result (funcall start-function recv)))
    (when result
      (recv-set-priority result priority)
      (when (bt:threadp result)
        (setf (receiver-thread recv) result))
      (setf (gethash stream *receivers*) recv)
      (dotimes (i 50 recv)
        (if (receiver-status recv)
            (return recv)
            (sleep .1))))))

(defun remove-receiver (stream)
  "Remove the receiver related to STREAM."
  (when stream
    (recv-stop stream)
    (remhash stream *receivers*)))

;;; CL:INPUT-STREAM

(defmethod valid-input-stream-p ((obj stream))
  (input-stream-p obj))

(defun start-cl-stream-recv (receiver read-function)
  (declare (type receiver receiver) (type function read-function)
           #.*standard-optimize-settings*)
  (bt:make-thread
    (lambda ()
      (let ((stream (receiver-stream receiver)))
        (setf (receiver-status receiver) t)
        ;; Flush pending writes.
        (if (serial-stream-p stream)
            (serial-flush stream :direction :input)
            (clear-input stream))
        (handler-case
            (loop while (receiver-status receiver) do
                    (let ((res (funcall read-function stream)))
                      (when (receiver-status receiver)
                        (dolist (fn (receiver-functions receiver))
                          (funcall (the function fn) res)))))
          (end-of-file ()
            (bt:make-thread
              (lambda ()
                (recv-stop stream)
                (msg error "end of file~%stop receiving from ~A" stream))
              :name "recv-stop"))
          (condition (c) (nrt-msg error "~A" c)))))
    :name (let ((stream (receiver-stream receiver)))
            (declare (type stream stream))
            (format nil "~A recv ~A" (type-of stream) stream))))

(defmethod recv-start ((stream stream) &key read-function
                       (priority *receiver-default-priority*))
  (unless (eq (recv-status stream) :running)
    (unless read-function
      (incudine-missing-arg "READ-FUNCTION is mandatory for STREAM."))
    (add-receiver stream (or (receiver stream) (make-receiver stream))
                  (lambda (receiver)
                    (start-cl-stream-recv receiver read-function))
                  priority)))

(defmethod recv-stop ((stream stream))
  (let ((recv (receiver stream)))
    (when (and recv (receiver-status recv))
      (compare-and-swap (receiver-status recv) t nil)
      (let ((stream (receiver-stream recv)))
        (msg debug "~A receiver for ~S stopped." (type-of stream) stream)
        (sleep 1)
        (when (and (bt:thread-alive-p (receiver-thread recv))
                   (not (listen stream)))
          (bt:destroy-thread (receiver-thread recv)))
        (recv-unset-thread recv)
        recv))))

;;; RESPONDER

(defvar *responders* (make-hash-table))
(declaim (type hash-table *responders*))

(defstruct (responder (:constructor %make-responder) (:copier nil))
  "Responder type."
  (receiver nil :type (or receiver null))
  (function nil :type (or function null)))

(defun all-responders (stream)
  "Return the list of the responders for STREAM."
  (values (gethash stream *responders*)))

(defun %add-responder (resp stream)
  (declare (type responder resp))
  (let* ((old-resp-list (gethash stream *responders*))
         (new-resp-list (cons resp old-resp-list))
         (old-func-list (receiver-functions (responder-receiver resp)))
         (new-func-list (cons (responder-function resp) old-func-list)))
    (when (eq (compare-and-swap (receiver-functions (responder-receiver resp))
                                old-func-list new-func-list)
              old-func-list)
      (setf (gethash stream *responders*) new-resp-list)))
  resp)

(defun add-responder (resp)
  "Add the function of the responder RESP to the list of the
receiver-functions."
  (declare (type responder resp))
  (let ((recv (responder-receiver resp)))
    (let ((resp-list (gethash (receiver-stream recv) *responders*)))
      (unless (find resp resp-list :test #'eq)
        (%add-responder resp (receiver-stream recv))))))

(defun midi-responder-wrapper (function)
    (let* ((lambda-list (incudine.util::function-lambda-list function))
           (len (length lambda-list))
           (key-pos (position '&key lambda-list)))
      (if (and key-pos (<= key-pos 2))
          ;; Optional keywords are ignored with one argument `(stream &key ...)'
          ;; but an error is signaled with more arguments.
          (setf len key-pos))
      ;; &optional makes sense only with three mandatory arguments.
      (case (position '&optional lambda-list)
        ((nil) (case len
                 (1 ;; (func stream)
                  (lambda (status data1 data2 stream)
                    (declare (ignore status data1 data2))
                    (funcall function stream)))
                 (3 ;; (func status data1 data2)
                  (lambda (status data1 data2 stream)
                    (declare (ignore stream))
                    (funcall function status data1 data2)))
                 (4 ;; No wrapper:
                    ;; (func status data1 data2 stream)
                  function)
                 (otherwise
                  (if (eq (first lambda-list) '&rest)
                      ;; No wrapper:
                      ;; (func &rest arguments)
                      function
                      (error 'alexandria:simple-program-error
                        :format-control "MIDI responder function with invalid ~
                                         number of arguments: ~D"
                        :format-arguments (list len))))))
        ;; No wrapper:
        ;; (func status data1 data2 &optional stream)
        (3 function))))

(defgeneric responder-wrapper (stream function))

(defmethod responder-wrapper ((stream t) function)
  function)

(defun make-responder (stream function)
  "Create and return a responder for STREAM.

FUNCTION is added to the list of receiver-functions called whenever
there is input available from STREAM.

If STREAM is a MIDI input stream, the function requires one, three
or four arguments:

    (func stream)
    (func status-byte data-byte-1 data-byte-2)
    (func status-byte data-byte-1 data-byte-2 stream)
    (func status-byte data-byte-1 data-byte-2 &optional stream)
    (func &rest arguments)

If STREAM is a NET:INPUT-STREAM, the function takes the stream as
argument.

If STREAM is a CL:INPUT-STREAM, the function takes the value
returned by the read-function passed to RECV-START."
  (declare (type function function))
  (%add-responder
    (%make-responder
      :receiver (or (receiver stream)
                    (setf (gethash stream *receivers*)
                          (make-receiver stream)))
      :function (responder-wrapper stream function))
    stream))

(defun remove-responder (resp)
  "Remove the function of the responder RESP from the list of the
receiver-functions."
  (declare (type responder resp))
  (let ((recv (responder-receiver resp)))
    (let ((resp-list (gethash (receiver-stream recv) *responders*)))
      (when resp-list
        (let* ((old-functions (receiver-functions recv))
               (new-functions (remove (responder-function resp)
                                      old-functions :test #'eq)))
          (when (eq (compare-and-swap (receiver-functions recv) old-functions
                                      new-functions)
                    old-functions)
            (let ((new-list (delete resp resp-list)))
              (if new-list
                  (setf (gethash (receiver-stream recv) *responders*)
                        new-list)
                  (remhash (receiver-stream recv) *responders*)))))
        (values)))))

(defun remove-all-responders (&optional stream)
  "Remove the responders for STREAM or all the responders if STREAM is NIL."
  (if stream
      (dolist (resp (all-responders stream))
        (remove-responder resp))
      (maphash-keys #'remove-all-responders *responders*))
  (values))

(defun remove-all-receivers ()
  "Remove all the receivers."
  (alexandria:maphash-keys #'recv-stop *receivers*)
  (clrhash *receivers*)
  (values))

(defun remove-receiver-and-responders (stream)
  (recv-stop stream)
  (remove-receiver stream)
  (remove-all-responders stream)
  stream)
