;;; Copyright (c) 2013-2019 Tito Latini
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(in-package :lv2)

(defctype handle :pointer)

(defcstruct feature
  (uri  :string)
  (data :pointer))

(defcstruct descriptor
  (uri            :string)
  (instantiate    :pointer)
  (connect-port   :pointer)
  (activate       :pointer)
  (run            :pointer)
  (deactivate     :pointer)
  (cleanup        :pointer)
  (extension-data :pointer))

(defcstruct atom
  (size :uint32)
  (type :uint32))

(defcunion atom-event-time
  (frames :uint64)
  (beats :double))

(defcstruct atom-event
  (time (:union atom-event-time))
  (body (:struct atom)))

(defcstruct atom-sequence-body
  (unit :uint32)
  (pad :uint32))

(defcstruct atom-sequence
  (atom (:struct atom))
  (body (:struct atom-sequence-body)))

(defcstruct event
  (frames    :uint32)
  (subframes :uint32)
  (type      :uint16)
  (size      :uint16))

(defcstruct event-buffer
  (data        :pointer)
  (header-size :uint16)
  (stamp-type  :uint16)
  (event-count :uint32)
  (capacity    :uint32)
  (size        :uint32))

(defcstruct event-feature
  (callback-data   :pointer)
  (lv2-event-ref   :pointer)
  (lv2-event-unref :pointer))

(defcstruct urid-map
  (handle :pointer)
  (map :pointer))

(defcstruct urid-unmap
  (handle :pointer)
  (map :pointer))

(define-constant +worker-success+ 0)
(define-constant +worker-err-unknown+ 1)
(define-constant +worker-err-no-space+ 2)

(defcstruct worker-interface
  (work :pointer)
  (work-response :pointer)
  (end-run :pointer))

(defcstruct worker-schedule
  (handle :pointer)
  (schedule-work :pointer))
