;;; Copyright (c) 2013-2018 Tito Latini
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

(in-package :incudine.vug)

(define-vug env-follower (in attack-time decay-time)
  "Envelope follower."
  (lag-ud (abs in) attack-time decay-time))

;;; RMS, GAIN and BALANCE from Music V family.

(define-vug rms (in hp)
  "RMS value of the input IN.

The response curve's half-power point HP of the low-pass filter
defaults to 10 Hz."
  (:defaults 0 10)
  (sqrt (the non-negative-sample (cs-tone (* in in) hp))))

(define-vug gain (in rms hp)
  "Scale the input IN to match a RMS value.

The response curve's half-power point HP of the low-pass filter
defaults to 10 Hz."
  (:defaults 0 0 10)
  (with-samples ((g (rms in hp)))
    (maybe-expand rms)
    (* in (if (zerop g) rms (/ rms g)))))

(define-vug balance (in comp hp)
  "Scale the input IN according to a RMS value of the comparator signal COMP.

The response curve's half-power point HP of the low-pass filter
defaults to 10 Hz.

Example:

    (dsp! balance-test (scl dur)
      (with-samples ((in (white-noise scl)))
        (out (balance (bpf in (expon 100 4000 dur #'free) 100) in))))"
  (:defaults 0 0 10)
  (gain in (rms comp hp) hp))
