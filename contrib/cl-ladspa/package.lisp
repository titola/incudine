;;; Copyright (c) 2014-2017 Tito Latini
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

(in-package :cl-user)

(defpackage ladspa
  (:use #:cl)
  (:import-from #:alexandria #:define-constant)
  (:export
   #:*ladspa-path*
   #:ladspa-error
   #:data
   #:properties
   #:PROPERTY-REALTIME
   #:PROPERTY-INPLACE-BROKEN
   #:PROPERTY-HARD-RT-CAPABLE
   #:realtime-p
   #:inplace-broken-p
   #:hard-rt-capable-p
   #:port-descriptor
   #:PORT-INPUT
   #:PORT-OUTPUT
   #:PORT-CONTROL
   #:PORT-AUDIO
   #:port-input-p
   #:port-output-p
   #:port-control-p
   #:port-audio-p
   #:port-range-hint-descriptor
   #:HINT-BOUNDED-BELOW
   #:HINT-BOUNDED-ABOVE
   #:HINT-TOGGLED
   #:HINT-SAMPLE-RATE
   #:HINT-LOGARITHMIC
   #:HINT-INTEGER
   #:HINT-DEFAULT-MASK
   #:HINT-DEFAULT-NONE
   #:HINT-DEFAULT-MINIMUM
   #:HINT-DEFAULT-LOW
   #:HINT-DEFAULT-MIDDLE
   #:HINT-DEFAULT-HIGH
   #:HINT-DEFAULT-MAXIMUM
   #:HINT-DEFAULT-0
   #:HINT-DEFAULT-1
   #:HINT-DEFAULT-100
   #:HINT-DEFAULT-440
   #:hint-bounded-below-p
   #:hint-bounded-above-p
   #:hint-toggled-p
   #:hint-sample-rate-p
   #:hint-logarithmic-p
   #:hint-integer-p
   #:hint-has-default-p
   #:hint-default-minimum-p
   #:hint-default-low-p
   #:hint-default-middle-p
   #:hint-default-high-p
   #:hint-default-maximum-p
   #:hint-default-0-p
   #:hint-default-1-p
   #:hint-default-100-p
   #:hint-default-440-p
   #:port-range-hint
   #:descriptor
   #:descriptor-slot-value
   #:handle
   #:pointer
   #:active-p
   #:deleted-p
   #:unique-id
   #:label
   #:properties
   #:name
   #:maker
   #:copyright
   #:port-count
   #:port-descriptors
   #:port-names
   #:port-range-hints
   #:implementation-data
   #:instantiate
   #:connect-port
   #:has-activate-p
   #:activate
   #:run
   #:has-run-adding-p
   #:run-adding
   #:has-set-run-adding-gain-p
   #:set-run-adding-gain
   #:has-deactivate-p
   #:deactivate
   #:cleanup
   #:load-plugin-library
   #:unload-plugin-library
   #:unload-all-plugins
   #:plugin-descriptor))
