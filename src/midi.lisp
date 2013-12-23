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

(declaim (inline midiout))
(defun midiout (status data1 data2 stream)
  "Send a generic MIDI message to a MIDI OUTPUT STREAM."
  (declare (type (unsigned-byte 8) status data1 data2)
           (type pm:stream stream))
  (pm:write-short stream 0 (pm:message status data1 data2)))

(defun sysex-sequence->foreign-array (seq)
  (declare (type sequence seq))
  (let* ((size (length seq))
         (fix-last-p (/= (the fixnum (elt seq (1- size))) #xF7))
         (obj (make-foreign-array (if fix-last-p (1+ size) size)
                                  :unsigned-char
                                  :initial-contents seq)))
    (when fix-last-p
      (setf (mem-aref (foreign-array-data obj) :unsigned-char size) #xF7))
    obj))

(declaim (inline midiout-sysex))
(defun midiout-sysex (seq stream)
  "Send a MIDI SysEx message to a MIDI OUTPUT STREAM."
  (declare (type sequence seq) (type pm:stream stream))
  (let ((obj (sysex-sequence->foreign-array seq)))
    (prog1 (pm:write-sysex stream 0 (foreign-array-data obj))
      (free obj))))
