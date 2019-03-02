;;; Copyright (c) 2019 Tito Latini
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

(in-package :incudine.analysis)

;;; Analysis file format for PVbuffer data:
;;;
;;;     chunk "CUDOPVOC"
;;;       subchunk "header"
;;;       subchunk "window"
;;;       subchunk "data"
;;;
(cffi:defcstruct pvoc-file-header
  (magic :uint64) ; "CUDOPVOC"
  (chunk-size :uint64)
  (header-subchunk :uint64)
  (header-subchunk-size :uint64)
  (size :uint64)
  (frames :uint32)
  (channels :uint32)
  (sample-rate :double)
  (scale-factor :double)
  (data-type :uint64) ; "COMPLEX", "MAGPHASE" or "MAGFREQ"
  (fft-size :uint32)
  (block-size :uint32)
  (hop-size :uint32)
  (normalized-p :char)
  (zero-phase-window-p :char)
  (pad :uint16))

(define-constant +pvoc-file-header-size+
  (cffi:foreign-type-size '(:struct pvoc-file-header)))

;;; Window data header.
(cffi:defcstruct pvoc-file-window
  (window-chunk :uint64) ; "window"
  (window-chunk-size :uint32))

;; Structure length without padding.
(define-constant +pvoc-file-window-size+ (+ 8 4))

;;; Data header.
(cffi:defcstruct pvoc-file-data
  (data-subchunk :uint64) ; "data"
  (data-subchunk-size :uint64))

(define-constant +pvoc-file-data-size+ (+ 8 8))

(define-condition analysis-file-error
    (incudine:incudine-simple-error file-error) ()
  (:documentation "All types of error conditions about analysis files
inherit from this condition.

Subtype of INCUDINE-SIMPLE-ERROR and FILE-ERROR."))

(defun pvfile-u32 (integer)
  #+little-endian integer
  #-little-endian (swap-bytes:swap-bytes-32 integer))

(defun pvfile-u64 (integer)
  #+little-endian integer
  #-little-endian (swap-bytes:swap-bytes-64 integer))

(defun pvfile-string (string)
  (loop for c across #+little-endian string
                     #-little-endian (reverse string)
        for i from 0 by 8
        sum (ash (char-code c) i)))

(defun pvfile-chunk-size (pvbuf)
  (+ +pvoc-file-header-size+
     +pvoc-file-window-size+
     +pvoc-file-data-size+
     (* 8 (pvbuffer-window-size pvbuf))
     (* 8 (pvbuffer-size pvbuf) (pvbuffer-channels pvbuf))
     ;; Minus chunk-name and chunk-size.
     (* -2 8)))

(defun pvfile-data-type (obj)
  (declare (type (or pvbuffer integer) obj))
  (if (pvbuffer-p obj)
      (pvfile-string
        (case (pvbuffer-data-type obj)
          (:complex "COMPLEX")
          (:magnitude-phase "MAGPHASE")
          (:magnitude-frequency "MAGFREQ")))
      (cond ((= (pvfile-string "COMPLEX") obj) :complex)
            ((= (pvfile-string "MAGPHASE") obj) :magnitude-phase)
            ((= (pvfile-string "MAGFREQ") obj) :magnitude-frequency))))

(defun pvfile-flag (x)
  (if (numberp x)
      (/= x 0)
      (if x 1 0)))

#-little-endian
(defun pvfile-header-swap-float (ptr)
  (let ((sr-ptr (cffi:foreign-slot-pointer
                  ptr '(:struct pvoc-file-header) 'sample-rate))
        (scl-ptr (cffi:foreign-slot-pointer
                   ptr '(:struct pvoc-file-header) 'scale-factor)))
    (setf (cffi:mem-ref sr-ptr :uint64)
          (pvfile-u64 (cffi:mem-ref sr-ptr :uint64)))
    (setf (cffi:mem-ref scl-ptr :uint64)
          (pvfile-u64 (cffi:mem-ref scl-ptr :uint64)))))

#+little-endian
(defun pvfile-write-sample-data (fd ptr size)
  (let ((len (* size 8)))
    (assert (= len (sb-posix:write fd ptr len)))
    len))

#-little-endian
(defun pvfile-write-sample-data (fd ptr size)
  (let ((bufsize (min 1280000 size)) ; max 10 MB
        (i 0))
    (cffi:with-foreign-object (buf :uint64 bufsize)
      (loop while (< i size) do
           (loop for j below bufsize
                 while (< i size) do
                   (setf (cffi:mem-aref buf :uint64 j)
                         (pvfile-u64 (cffi:mem-aref ptr :uint64 i)))
                   (incf i 8)
                 finally (sb-posix:write fd buf j))))
    (* size 8)))

(defun pvfile-write-window (fd pvbuf)
  (cffi:with-foreign-object (ptr '(:struct pvoc-file-window))
    (cffi:with-foreign-slots ((window-chunk window-chunk-size) ptr
                              (:struct pvoc-file-window))
      (setf window-chunk (pvfile-string "window"))
      (setf window-chunk-size
            (pvfile-u32 (* 8 (pvbuffer-window-size pvbuf))))
      (sb-posix:write fd ptr +pvoc-file-window-size+)
      (pvfile-write-sample-data fd (pvbuffer-window-buffer pvbuf)
                                (pvbuffer-window-size pvbuf))
      pvbuf)))

(defun pvfile-write-header (fd pvbuf)
  (cffi:with-foreign-object (ptr '(:struct pvoc-file-header))
    (cffi:with-foreign-slots
      ((magic chunk-size header-subchunk header-subchunk-size size frames
        channels sample-rate scale-factor data-type fft-size block-size
        hop-size normalized-p zero-phase-window-p pad)
       ptr (:struct pvoc-file-header))
      (setf magic (pvfile-string "CUDOPVOC")
            chunk-size (pvfile-u64 (pvfile-chunk-size pvbuf))
            header-subchunk (pvfile-string "header")
            header-subchunk-size (pvfile-u64 (- +pvoc-file-header-size+ (* 8 4)))
            size (pvfile-u64 (pvbuffer-size pvbuf))
            frames (pvfile-u32 (pvbuffer-frames pvbuf))
            channels (pvfile-u32 (pvbuffer-channels pvbuf))
            sample-rate (pvbuffer-sample-rate pvbuf)
            scale-factor (pvbuffer-scale-factor pvbuf)
            data-type (pvfile-data-type pvbuf)
            fft-size (pvfile-u32 (pvbuffer-fft-size pvbuf))
            block-size (pvfile-u32 (pvbuffer-block-size pvbuf))
            hop-size (pvfile-u32 (pvbuffer-hop-size pvbuf))
            normalized-p (pvfile-flag (pvbuffer-normalized-p pvbuf))
            zero-phase-window-p (pvfile-flag (pvbuffer-zero-phase-window-p pvbuf))
            pad 0)
      #-little-endian
      (pvfile-header-swap-float ptr)
      (sb-posix:write fd ptr +pvoc-file-header-size+)
      pvbuf)))

(defun pvfile-write-data (fd pvbuf)
  (cffi:with-foreign-object (ptr '(:struct pvoc-file-data))
    (cffi:with-foreign-slots ((data-subchunk data-subchunk-size) ptr
                              (:struct pvoc-file-data))
      (let ((data-size (* (pvbuffer-size pvbuf) (pvbuffer-channels pvbuf))))
        (setf data-subchunk (pvfile-string "data")
              data-subchunk-size (pvfile-u64 (* 8 data-size)))
        (sb-posix:write fd ptr +pvoc-file-data-size+)
        (dotimes (ch (pvbuffer-channels pvbuf))
          (pvfile-write-sample-data
            fd (pvbuffer-data pvbuf 0 ch) (pvbuffer-size pvbuf)))
        pvbuf))))

(defun pvbuffer-save (instance path)
  "Save the PVbuffer analysis data to the file PATH.

PATH is of type string or pathname."
  (declare (type pvbuffer instance) (type (or string pathname) path))
  (let ((fd (sb-posix:creat (namestring path) #o644)))
    (unwind-protect
         (progn
           (pvfile-write-header fd instance)
           (pvfile-write-window fd instance)
           (pvfile-write-data fd instance)
           path)
      (sb-posix:close fd))))

(defun pvfile-read-header (fd buffer path)
  (let ((len (sb-posix:read fd buffer +pvoc-file-header-size+)))
    (when (or (/= +pvoc-file-header-size+ len)
              (/= (cffi:mem-aref buffer :uint64) (pvfile-string "CUDOPVOC")))
      (error 'analysis-file-error
             :format-control "Invalid file ~S"
             :format-arguments (list path)))
    (unless (and (<= (+ (* 2 8) (pvfile-u64 (cffi:mem-aref buffer :uint64 1)))
                     (with-open-file (f path) (file-length f)))
                 (= (cffi:mem-aref buffer :uint64 2) (pvfile-string "header")))
      (error 'analysis-file-error
             :format-control "Corrupted file ~S"
             :format-arguments (list path)))
    (cffi:with-foreign-slots
      ((header-subchunk header-subchunk-size size frames
        channels sample-rate scale-factor data-type fft-size block-size
        hop-size normalized-p zero-phase-window-p)
       buffer (:struct pvoc-file-header))
      (unless (and (= header-subchunk (pvfile-string "header"))
                   (= (+ header-subchunk-size (* 4 8))
                      (pvfile-u64 +pvoc-file-header-size+)))
        (error 'analysis-file-error
               :format-control "File ~S with corrupted header chunk"
               :format-arguments (list path)))
      #-little-endian
      (pvfile-header-swap-float buffer)
      (let ((pvbuf (make-pvbuffer (pvfile-u32 frames) (pvfile-u32 fft-size)
                     :channels (pvfile-u32 channels)
                     :sample-rate sample-rate
                     :data-type (pvfile-data-type (pvfile-u64 data-type))
                     :window-function nil
                     :hop-size (pvfile-u32 hop-size)
                     :normalized-p (pvfile-flag normalized-p))))
        (handler-case
            (progn
              (setf (pvbuffer-size pvbuf) (pvfile-u64 size))
              (setf (pvbuffer-block-size pvbuf) (pvfile-u32 block-size))
              (setf (pvbuffer-zero-phase-window-p pvbuf)
                    (pvfile-flag zero-phase-window-p))
              (setf (pvbuffer-scale-factor pvbuf) scale-factor)
              pvbuf)
          (condition (c) (free pvbuf) (error c)))))))

(defun pvfile-read-sample-data (fd ptr size)
  (let ((len (* size 8)))
    (assert (= len (sb-posix:read fd ptr len)))
    #-little-endian
    (dotimes (i size)
      (setf (cffi:mem-aref ptr :uint64 i)
            (pvfile-u64 (cffi:mem-aref ptr :uint64 i))))
    len))

(defun pvfile-read-window (fd buffer path pvbuf)
  (flet ((corrupted-window-chunk ()
           (error 'analysis-file-error
                  :format-control "File ~S with corrupted window chunk"
                  :format-arguments (list path))))
    (unless (and (= (sb-posix:read fd buffer +pvoc-file-window-size+)
                    +pvoc-file-window-size+)
                 (= (cffi:mem-aref buffer :uint64) (pvfile-string "window")))
      (corrupted-window-chunk))
    (let* ((window-bytes (pvfile-u32 (cffi:mem-aref buffer :uint32 2)))
           (window-size (ash window-bytes -3)))
      (unless (= window-size (/ window-bytes 8))
        (corrupted-window-chunk))
      (setf (window-size pvbuf) window-size)
      (unless (= (sb-posix:read fd (pvbuffer-window-buffer pvbuf) window-bytes)
                 window-bytes)
        (corrupted-window-chunk))
      pvbuf)))

(defun pvfile-read-data (fd pvbuf)
  (dotimes (chan (pvbuffer-channels pvbuf) pvbuf)
    (pvfile-read-sample-data
      fd (pvbuffer-data pvbuf 0 chan) (pvbuffer-size pvbuf))))

(defun pvbuffer-load (path)
  "Create a new PVbuffer by loading the file PATH.

PATH is of type string or pathname."
  (declare (type (or string pathname) path))
  (let* ((path (namestring (truename path)))
         (fd (sb-posix:open path sb-posix:o-rdonly)))
    (unwind-protect
         (cffi:with-foreign-object (buf '(:struct pvoc-file-header))
           (let ((pvbuf (pvfile-read-header fd buf path)))
             (handler-case
                 (progn
                   (pvfile-read-window fd buf path pvbuf)
                   (pvfile-read-data fd pvbuf))
               (condition (c) (free pvbuf) (error c)))))
      (sb-posix:close fd))))
