;;; Copyright (c) 2014 Tito Latini
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

(in-package :ladspa)

(cffi:defctype data :float)

;;; Special Plugin Properties

(cffi:defctype properties :int)

(define-constant PROPERTY-REALTIME         1)
(define-constant PROPERTY-INPLACE-BROKEN   2)
(define-constant PROPERTY-HARD-RT-CAPABLE  4)

(declaim (inline realtime-p inplace-broken-p hard-rt-capable-p))

(defun realtime-p (x) (logtest x PROPERTY-REALTIME))
(defun inplace-broken-p (x) (logtest x PROPERTY-INPLACE-BROKEN))
(defun hard-rt-capable-p (x) (logtest x PROPERTY-HARD-RT-CAPABLE))

;;; Plugin Ports

(cffi:defctype port-descriptor :int)

(define-constant PORT-INPUT    1)
(define-constant PORT-OUTPUT   2)
(define-constant PORT-CONTROL  4)
(define-constant PORT-AUDIO    8)

(declaim (inline port-input-p port-output-p port-control-p port-audio-p))

(defun port-input-p (x) (logtest x PORT-INPUT))
(defun port-output-p (x) (logtest x PORT-OUTPUT))
(defun port-control-p (x) (logtest x PORT-CONTROL))
(defun port-audio-p (x) (logtest x PORT-AUDIO))

;;; Plugin Port Range Hints

(cffi:defctype port-range-hint-descriptor :int)

(define-constant HINT-BOUNDED-BELOW    #x0001)
(define-constant HINT-BOUNDED-ABOVE    #x0002)
(define-constant HINT-TOGGLED          #x0004)
(define-constant HINT-SAMPLE-RATE      #x0008)
(define-constant HINT-LOGARITHMIC      #x0010)
(define-constant HINT-INTEGER          #x0020)
(define-constant HINT-DEFAULT-MASK     #x0030)
(define-constant HINT-DEFAULT-NONE     #x0000)
(define-constant HINT-DEFAULT-MINIMUM  #x0040)
(define-constant HINT-DEFAULT-LOW      #x0080)
(define-constant HINT-DEFAULT-MIDDLE   #x00C0)
(define-constant HINT-DEFAULT-HIGH     #x0100)
(define-constant HINT-DEFAULT-MAXIMUM  #x0140)
(define-constant HINT-DEFAULT-0        #x0200)
(define-constant HINT-DEFAULT-1        #x0240)
(define-constant HINT-DEFAULT-100      #x0280)
(define-constant HINT-DEFAULT-440      #x02C0)

(declaim (inline hint-bounded-below-p hint-bounded-above-p hint-toggled-p
                 hint-sample-rate-p hint-logarithmic-p hint-integer-p
                 hint-has-default-p hint-default= hint-default-minimum-p
                 hint-default-low-p hint-default-middle-p hint-default-high-p
                 hint-default-maximum-p hint-default-0-p hint-default-1-p
                 hint-default-100-p hint-default-440-p))

(defun hint-bounded-below-p (x) (logtest x HINT-BOUNDED-BELOW))
(defun hint-bounded-above-p (x) (logtest x HINT-BOUNDED-ABOVE))
(defun hint-toggled-p (x) (logtest x HINT-TOGGLED))
(defun hint-sample-rate-p (x) (logtest x HINT-SAMPLE-RATE))
(defun hint-logarithmic-p (x) (logtest x HINT-LOGARITHMIC))
(defun hint-integer-p (x) (logtest x HINT-INTEGER))

(defun hint-has-default-p (x) (logtest x HINT-DEFAULT-MASK))

(defun hint-default= (x hint)
  (= (logand x HINT-DEFAULT-MASK) hint))

(defun hint-default-minimum-p (x) (hint-default= x HINT-DEFAULT-MINIMUM))
(defun hint-default-low-p (x) (hint-default= x HINT-DEFAULT-LOW))
(defun hint-default-middle-p (x) (hint-default= x HINT-DEFAULT-MIDDLE))
(defun hint-default-high-p (x) (hint-default= x HINT-DEFAULT-HIGH))
(defun hint-default-maximum-p (x) (hint-default= x HINT-DEFAULT-MAXIMUM))
(defun hint-default-0-p (x) (hint-default= x HINT-DEFAULT-0))
(defun hint-default-1-p (x) (hint-default= x HINT-DEFAULT-1))
(defun hint-default-100-p (x) (hint-default= x HINT-DEFAULT-100))
(defun hint-default-440-p (x) (hint-default= x HINT-DEFAULT-440))

(cffi:defcstruct port-range-hint
  (hint-descriptor port-range-hint-descriptor)
  (lower-bound data)
  (upper-bound data))

;;; Descriptor for a Type of Plugin

(cffi:defcstruct descriptor
  (unique-id :unsigned-long)
  (label :string)
  (properties properties)
  (name :string)
  (maker :string)
  (copyright :string)
  (port-count :unsigned-long)
  (port-descriptors :pointer)
  (port-names :pointer)
  (port-range-hints :pointer)
  (implementation-data :pointer)
  (instantiate :pointer)
  (connect-port :pointer)
  (activate :pointer)
  (run :pointer)
  (run-adding :pointer)
  (set-run-adding-gain :pointer)
  (deactivate :pointer)
  (cleanup :pointer))

(declaim (inline descriptor-slot-value))
(defun descriptor-slot-value (descriptor slot-name)
  (cffi:foreign-slot-value descriptor '(:struct descriptor) slot-name))
