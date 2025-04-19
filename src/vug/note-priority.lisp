;;; Copyright (c) 2014 Tito Latini
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

;;; Lowest and highest note priority for a monosynth synthesizer.
;;; The following algorithm uses three (64bit arch) or six (32bit arch)
;;; fixnums instead of an array to store the status of 128 keynums.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +number-of-notes+ 128)

  ;; Max number of flags for a fixnum (24 or 48).
  (define-constant +note-priority-slot-max-size+
    (* 24 (truncate (min 64 (log most-positive-fixnum 2)) 24)))

  ;; Number of the fixnums used to store the keynums (3 or 6).
  (define-constant +note-priority-integers+
    (ceiling (/ +number-of-notes+ +note-priority-slot-max-size+)))

  ;; The two slots at the end of the vector are respectively the keynum
  ;; of the last note-on and the keynum of the last note-off.
  (define-constant +note-priority-vector-size+ (+ +note-priority-integers+ 2))

  ;; Three bits are used as index for the note-priority vector.
  (define-constant +note-priority-slot-index-mask+ 7)

  ;; Four bits are used for the offset of the keynum.
  (define-constant +note-priority-slot-offset-mask+ #x78)

  (unless (constantp '+note-priority-flag-vector+)
    ;; Offsets for the keynums.
    (define-constant +note-priority-offset-vector+
      (coerce (loop for i below +number-of-notes+
                    by +note-priority-slot-max-size+
                    collect i)
              'vector))

    ;; A flag represents the status of a keynum: 1 is on, 0 is off.
    (define-constant +note-priority-flag-vector+
      (make-array +note-priority-slot-max-size+
        :initial-contents (loop for i below +note-priority-slot-max-size+
                                collect (ash 1 i))))

    (define-constant +note-priority-slot-vector+
      (make-array +number-of-notes+
        :initial-contents (loop for i below +number-of-notes+
                                for j from 0
                                with k = 0 and offset = 0
                                when (= j +note-priority-slot-max-size+) do
                                  (incf k)
                                  (incf offset j)
                                  (setf j 0)
                                collect (+ k offset))))))

(deftype note-priority-vector ()
  `(simple-vector ,+note-priority-vector-size+))

(deftype note-velocity-vector ()
  `(simple-vector ,+number-of-notes+))

(defun make-note-priority-vector ()
  (make-array +note-priority-vector-size+ :initial-element 0))

(defun make-note-velocity-vector ()
  (make-array +number-of-notes+ :initial-element 0))

(declaim (inline %last-note-off))
(defun %last-note-off (priority-vec)
  (svref priority-vec +note-priority-integers+))

(declaim (inline %set-last-note-off))
(defun %set-last-note-off (priority-vec keynum)
  (declare (type note-priority-vector priority-vec)
           (type (integer 0 127) keynum))
  (setf (svref priority-vec +note-priority-integers+) keynum))

(defsetf %last-note-off %set-last-note-off)

(declaim (inline %last-note-on))
(defun %last-note-on (priority-vec)
  (svref priority-vec (1+ +note-priority-integers+)))

(declaim (inline %set-last-note-on))
(defun %set-last-note-on (priority-vec keynum)
  (declare (type note-priority-vector priority-vec)
           (type (integer 0 127) keynum))
  (setf (svref priority-vec (1+ +note-priority-integers+)) keynum))

(defsetf %last-note-on %set-last-note-on)

(defun note-priority-add (priority-vec velocity-vec keynum velocity)
  (declare (type note-priority-vector priority-vec)
           (type note-velocity-vector velocity-vec)
           (type (unsigned-byte 8) keynum velocity)
           #.*standard-optimize-settings*)
  (let* ((slot (svref +note-priority-slot-vector+ keynum))
         (index (logand slot +note-priority-slot-index-mask+)))
    (declare (type (unsigned-byte 8) slot index))
    ;; Set the flag related to KEYNUM.
    (setf (svref priority-vec index)
          (logior (the fixnum
                    (svref +note-priority-flag-vector+
                           (- keynum
                              (logand slot
                                      +note-priority-slot-offset-mask+))))
                  (the fixnum (svref priority-vec index))))
    (setf (svref velocity-vec keynum) velocity)
    (setf (%last-note-on priority-vec) keynum)
    (values)))

(defun note-priority-remove (priority-vec velocity-vec keynum)
  (declare (type (unsigned-byte 8) keynum)
           (type note-priority-vector priority-vec)
           (type note-velocity-vector velocity-vec)
           #.*standard-optimize-settings*)
  (let* ((slot (svref +note-priority-slot-vector+ keynum))
         (index (logand slot +note-priority-slot-index-mask+))
         (flag (svref +note-priority-flag-vector+
                      (- keynum (logand slot
                                        +note-priority-slot-offset-mask+)))))
    (declare (type (unsigned-byte 8) slot index) (type fixnum flag))
    ;; Zeroes the flag.
    (setf (svref priority-vec index)
          (logxor flag
                  ;; Add the flag if it is absent, otherwise LOGXOR fails.
                  (logior flag (the fixnum (svref priority-vec index)))))
    (setf (svref velocity-vec keynum) 0)
    (setf (%last-note-off priority-vec) keynum)
    (values)))

(defun highest-note-priority (priority-vec)
  (declare (type note-priority-vector priority-vec)
           #.*standard-optimize-settings*)
  (loop for index of-type (signed-byte 8)
                  from (1- +note-priority-integers+) downto 0
        for flags of-type fixnum = (the fixnum (svref priority-vec index))
        when (plusp flags)
          return (+ (1- (integer-length flags))
                    (the (unsigned-byte 8)
                      (svref +note-priority-offset-vector+ index)))
        finally (return (%last-note-off priority-vec))))

(defun lowest-note-priority (priority-vec)
  (declare (type note-priority-vector priority-vec)
           #.*standard-optimize-settings*)
  (loop for index of-type (unsigned-byte 8) below +note-priority-integers+
        for flags of-type fixnum = (the fixnum (svref priority-vec index))
        when (plusp flags)
          return (+ (integer-length (logand (lognot flags) (1- flags)))
                    (the (unsigned-byte 8)
                      (svref +note-priority-offset-vector+ index)))
        finally (return (%last-note-off priority-vec))))

(defun reset-note-priority (priority-vec velocity-vec)
  (declare (type note-priority-vector priority-vec)
           (type note-velocity-vector velocity-vec)
           #.*standard-optimize-settings*)
  (dotimes (i +note-priority-vector-size+)
    (setf (svref priority-vec i) 0))
  (dotimes (i +number-of-notes+)
    (setf (svref velocity-vec i) 0)))
