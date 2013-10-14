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

(defvar *receiver-hash* (make-hash-table))
(declaim (type hash-table *receiver-hash*))

(defstruct (receiver (:constructor %make-receiver)
                     (:copier nil))
  stream
  (functions nil :type list)
  (status nil :type boolean))

(declaim (inline make-receiver))
(defun make-receiver (stream &optional functions status)
  (when (valid-stream-p stream)
    (%make-receiver :stream stream
                    :functions functions
                    :status status)))

(declaim (inline valid-stream-p))
(defun valid-stream-p (obj)
  ;; Currently there is only PortMidi
  (portmidi-input-stream-p obj))

(declaim (inline get-receiver))
(defun get-receiver (stream)
  (values (gethash stream *receiver-hash*)))

(declaim (inline recv-status))
(defun recv-status (stream)
  (let ((recv (get-receiver stream)))
    (if recv
        (if (receiver-status recv)
            :RUNNING
            :STOPPED)
        :UNKNOWN)))

(declaim (inline recv-functions))
(defun recv-functions (stream)
  (let ((recv (get-receiver stream)))
    (when recv (receiver-functions recv))))

(declaim (inline recv-funcall-all))
(defun recv-funcall-all (recv status data1 data2)
  (declare (type receiver recv)
           (type (unsigned-byte 8) status data1 data2))
  (dolist (fn (receiver-functions recv))
    (funcall (the function fn) status data1 data2)))

(declaim (inline recv-set-priority))
(defun recv-set-priority (thread priority)
  (when (bt:threadp thread)
    (thread-set-priority thread priority)))

(defmacro add-receiver (stream recv start-function priority)
  (with-gensyms (result)
    `(let ((,result (,start-function recv)))
       (when ,result
         (recv-set-priority ,result ,priority)
         (setf (gethash ,stream *receiver-hash*) ,recv)))))

(declaim (inline remove-receiver))
(defun remove-receiver (stream)
  (when stream
    (recv-stop stream)
    (remhash stream *receiver-hash*)))

(defun recv-start (stream &key (priority *receiver-default-priority*))
  (let ((recv (or (get-receiver stream)
                  (make-receiver stream))))
    (cond ((portmidi-stream-p stream)
           (add-receiver stream recv start-portmidi-recv
                         priority)))))

(defun recv-stop (stream)
  (let ((recv (get-receiver stream)))
    (compare-and-swap (receiver-status recv) t nil)
    recv))

;;; PORTMIDI

(declaim (inline portmidi-stream-p))
(defun portmidi-stream-p (obj)
  (eq (type-of obj) 'portmidi:stream))

(declaim (inline portmidi-input-stream-p))
(defun portmidi-input-stream-p (obj)
  (and (portmidi-stream-p obj)
       (eq (slot-value obj 'pm::direction) :input)))

(defun start-portmidi-recv (receiver)
  (declare #.*standard-optimize-settings*
           (type receiver receiver))
  (let ((stream (receiver-stream receiver)))
    (pm:with-receiver ((receiver-status receiver) stream msg)
      (multiple-value-bind (status data1 data2)
          (pm:decode-message msg)
        (incudine.vug::set-midi-message status data1 data2)
        (recv-funcall-all receiver status data1 data2)))))

;;; RESPONDER

(defvar *responder-hash* (make-hash-table))
(declaim (type hash-table *responder-hash*))

(defstruct (responder (:constructor %make-responder)
                      (:copier nil))
  (receiver nil :type (or receiver null))
  (function nil :type (or function null)))

(declaim (inline get-responder-list))
(defun get-responder-list (stream)
  (values (gethash stream *responder-hash*)))

(defun %add-responder (resp stream)
  (declare (type responder resp))
  (let* ((old-resp-list (gethash stream *responder-hash*))
         (new-resp-list (cons resp old-resp-list))
         (old-func-list (receiver-functions (responder-receiver resp)))
         (new-func-list (cons (responder-function resp) old-func-list)))
    (when (eq (compare-and-swap (receiver-functions
                                 (responder-receiver resp))
                                old-func-list new-func-list)
              old-func-list)
      (setf (gethash stream *responder-hash*) new-resp-list)))
  resp)

(defun add-responder (resp)
  (declare (type responder resp))
  (let ((recv (responder-receiver resp)))
    (let ((resp-list (gethash (receiver-stream recv) *responder-hash*)))
      (unless (find resp resp-list :test #'eq)
        (%add-responder resp (receiver-stream recv))))))

(declaim (inline make-responder))
(defun make-responder (stream function)
  (declare (type function function))
  (let ((recv (get-receiver stream)))
    (when recv
      (let ((resp (%make-responder :receiver recv
                                   :function function)))
        (%add-responder resp stream)))))

(defun remove-responder (resp)
  (declare (type responder resp))
  (let ((recv (responder-receiver resp)))
    (let ((resp-list #1=(gethash (receiver-stream recv) *responder-hash*)))
      (when resp-list
        (let* ((old-functions (receiver-functions recv))
               (new-functions (remove (responder-function resp)
                                      old-functions :test #'eq)))
          (when (eq (compare-and-swap (receiver-functions recv)
                                      old-functions new-functions)
                    old-functions)
            (let ((new-list (delete resp resp-list)))
              (if new-list
                  (setf #1# new-list)
                  (remhash (receiver-stream recv) *responder-hash*)))))
        (values)))))

(defun remove-all-responders (&optional stream)
  (if stream
      (dolist (resp (get-responder-list stream))
        (remove-responder resp))
      (maphash-keys #'remove-all-responders
                    *responder-hash*))
  (values))
