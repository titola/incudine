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

(defglobal *sine-table*
  (make-buffer *default-table-size* :fill-function (gen:partials '(1)))
  "BUFFER structure of size *DEFAULT-TABLE-SIZE* with a single cycle sinusoid.")
(declaim (type buffer *sine-table*))

(defglobal *cosine-table*
  (make-buffer *default-table-size* :fill-function (gen:partials '((1 1 .25))))
  "BUFFER structure of size *DEFAULT-TABLE-SIZE* with a single cycle cosinusoid.")
(declaim (type buffer *cosine-table*))
