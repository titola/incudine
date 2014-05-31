;;; Copyright (c) 2013-2014 Tito Latini
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

(defstruct (sync-condition (:constructor %make-sync-condition)
                           (:copier nil))
  (variable (error "Missing variable"))
  (mutex (error "Missing mutex"))
  (read 0 :type non-negative-fixnum)
  (write 0 :type non-negative-fixnum))

(defun make-sync-condition (name)
  (%make-sync-condition :variable (bt:make-condition-variable :name name)
                        :mutex (bt:make-lock name)))

(defmacro inc-condition-counter (cond counter-name)
  `(setf #1=(,(format-symbol (find-package :incudine) "SYNC-CONDITION-~A"
                             counter-name)
             ,cond)
         (logand (the non-negative-fixnum
                   (1+ (the non-negative-fixnum #1#)))
                 #xffffff)))

(declaim (inline sync-condition-flush))
(defun sync-condition-flush (cond)
  (setf (sync-condition-read cond) (sync-condition-write cond)))

(defmacro sync-condition-wait (cond)
  `(bt:with-lock-held ((sync-condition-mutex ,cond))
     (loop while (= (sync-condition-read ,cond)
                    (sync-condition-write ,cond))
           do (bt:condition-wait (sync-condition-variable ,cond)
                                 (sync-condition-mutex ,cond)))
     (inc-condition-counter ,cond read)))

(declaim (inline sync-condition-signal))
(defun sync-condition-signal (cond)
  (bt:with-lock-held ((sync-condition-mutex cond))
    (inc-condition-counter cond write)
    (bt:condition-notify (sync-condition-variable cond))))
