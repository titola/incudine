;;; Copyright (c) 2014-2015 Tito Latini
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

(in-package :incudine.voicer)

(defstruct (event (:copier nil))
  voicer
  responder
  (freq-keyword :freq :type (or keyword null))
  (freq-function #'identity :type function)
  (amp-keyword :amp :type (or keyword null))
  (amp-mult 0.2)
  (amp-function #'identity :type function)
  (gate-keyword :gate :type keyword)
  (gate-value 1.0))
