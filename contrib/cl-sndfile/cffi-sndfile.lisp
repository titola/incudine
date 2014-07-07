;;; Copyright (c) 2013 Tito Latini
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

(in-package :sndfile)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library sndfile
    (:darwin "libsndfile.dylib")
    (:unix "libsndfile.so")
    (:cygwin "cygsndfile-0.dll")
    (t (:default "libsndfile")))

  (defun load-sndfile-library ()
    (cffi:use-foreign-library sndfile))

  (unless (cffi:foreign-library-loaded-p 'sndfile)
    (load-sndfile-library)))

(defstruct (sndfile (:constructor %make-sndfile)
                    (:copier nil))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (struct-ptr (:constructor %make-struct-ptr))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct (sndinfo (:include struct-ptr)))

(declaim (inline make-sndfile))
(defun make-sndfile (pointer)
  (let ((obj (%make-sndfile :pointer pointer)))
    (tg:finalize obj (lambda () (close pointer)))
    obj))

(declaim (inline make-struct-ptr))
(defun make-struct-ptr (pointer)
  (let ((obj (%make-struct-ptr :pointer pointer)))
    (tg:finalize obj (lambda () (cffi:foreign-free pointer)))
    obj))

(defun sndfile-null-p (sndfile)
  (declare (sndfile sndfile))
  (cffi:null-pointer-p (sndfile-pointer sndfile)))

(cffi:define-foreign-type sndfile-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser sndfile))

(cffi:define-foreign-type struct-ptr-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser struct-ptr))

(cffi:define-foreign-type sndinfo-type (struct-ptr-type)
  ()
  (:simple-parser sndinfo))

(defmethod cffi:translate-from-foreign (ptr (type sndfile-type))
  (make-sndfile ptr))

(defmethod cffi:translate-from-foreign (ptr (type sndinfo-type))
  (make-struct-ptr ptr))

(defmethod cffi:translate-to-foreign (handle (type sndfile-type))
  (slot-value handle 'pointer))

(defmethod cffi:translate-to-foreign (handle (type sndinfo-type))
  (slot-value handle 'pointer))

(cffi:defcenum format
  ;; major formats
  (:sf-format-wav       #x010000)
  (:sf-format-aiff      #x020000)
  (:sf-format-au        #x030000)
  (:sf-format-raw       #x040000)
  (:sf-format-paf       #x050000)
  (:sf-format-svx       #x060000)
  (:sf-format-nist      #x070000)
  (:sf-format-voc       #x080000)
  (:sf-format-ircam     #x0a0000)
  (:sf-format-w64       #x0b0000)
  (:sf-format-mat4      #x0c0000)
  (:sf-format-mat5      #x0d0000)
  (:sf-format-pvf       #x0e0000)
  (:sf-format-xi        #x0f0000)
  (:sf-format-htk       #x100000)
  (:sf-format-sds       #x110000)
  (:sf-format-avr       #x120000)
  (:sf-format-wavex     #x130000)
  (:sf-format-sd2       #x160000)
  (:sf-format-flac      #x170000)
  (:sf-format-caf       #x180000)
  (:sf-format-wve       #x190000)
  (:sf-format-ogg       #x200000)
  (:sf-format-mpc2k     #x210000)
  (:sf-format-rf64      #x220000)
  ;; subtypes from here on
  (:sf-format-pcm-s8    #x0001)
  (:sf-format-pcm-16    #x0002)
  (:sf-format-pcm-24    #x0003)
  (:sf-format-pcm-32    #x0004)
  (:sf-format-pcm-u8    #x0005)
  (:sf-format-float     #x0006)
  (:sf-format-double    #x0007)
  (:sf-format-ulaw      #x0010)
  (:sf-format-alaw      #x0011)
  (:sf-format-ima-adpcm #x0012)
  (:sf-format-ms-adpcm  #x0013)
  (:sf-format-gsm610    #x0020)
  (:sf-format-vox-adpcm #x0021)
  (:sf-format-g721-32   #x0030)
  (:sf-format-g723-24   #x0031)
  (:sf-format-g723-40   #x0032)
  (:sf-format-dwvw-12   #x0040)
  (:sf-format-dwvw-16   #x0041)
  (:sf-format-dwvw-24   #x0042)
  (:sf-format-dwvw-n    #x0043)
  (:sf-format-dpcm-8    #x0050)
  (:sf-format-dpcm-16   #x0051)
  (:sf-format-vorbis    #x0060)
  ;; endian-ness options
  (:sf-endian-file      #x00000000)
  (:sf-endian-little    #x10000000)
  (:sf-endian-big       #x20000000)
  (:sf-endian-cpu       #x30000000)
  (:sf-format-submask   #x0000ffff)
  (:sf-format-typemask  #x0fff0000)
  (:sf-format-endmask   #x30000000))

(macrolet
    ((define-constants (lst)
       `(progn
          ,@(mapcar (lambda (x)
                      (cons 'define-constant x))
                    lst))))
  (define-constants
      (;; string types that can be set and read from files
       (str-title       #x01)
       (str-copyright   #x02)
       (str-software    #x03)
       (str-artist      #x04)
       (str-comment     #x05)
       (str-date        #x06)
       (str-album       #x07)
       (str-license     #x08)
       (str-tracknumber #x09)
       (str-genre       #x10)
       ;; true and false
       (false 0)
       (true  1)
       ;; modes for opening files
       (sfm-read              #x10)
       (sfm-write             #x20)
       (sfm-rdwr              #x30)
       (ambisonic-none     #x40)
       (ambisonic-b-format #x41)
       ;; error number
       (err-no-error             0)
       (err-unrecognised-format  1)
       (err-system               2)
       (err-malformed-file       3)
       (err-unsupported-encoding 4)
       ;; channel map values
       (channel-map-invalid                0)
       (channel-map-mono                   1)
       (channel-map-left                   2)
       (channel-map-right                  3)
       (channel-map-center                 4)
       (channel-map-front-left             5)
       (channel-map-front-right            6)
       (channel-map-front-center           7)
       (channel-map-rear-center            8)
       (channel-map-rear-left              9)
       (channel-map-rear-right            10)
       (channel-map-lfe                   11)
       (channel-map-front-left-of-center  12)
       (channel-map-front-right-of-center 13)
       (channel-map-side-left             14)
       (channel-map-side-right            15)
       (channel-map-top-center            16)
       (channel-map-top-front-left        17)
       (channel-map-top-front-right       18)
       (channel-map-top-front-center      19)
       (channel-map-top-rear-left         20)
       (channel-map-top-rear-right        21)
       (channel-map-top-rear-center       22)
       (channel-map-ambisonic-b-w         23)
       (channel-map-ambisonic-b-x         24)
       (channel-map-ambisonic-b-y         25)
       (channel-map-ambisonic-b-z         26)
       (channel-map-max                   27)
       ;; dither
       (sfd-default-level          0)
       (sfd-custom-level  #x40000000)
       (sfd-no-dither            500)
       (sfd-white                501)
       (sfd-triangular-pd        502)
       ;; loop mode field
       (loop-none        800)
       (loop-forward     801)
       (loop-backward    802)
       (loop-alternating 803)

       (count-max #x7fffffffffffffff))))

(cffi:defctype sf-count :int64)

(cffi:defcstruct info
  (frames      sf-count)
  (sample-rate :int)
  (channels    :int)
  (format      :int)
  (sections    :int)
  (seekable    :int))

(cffi:defcstruct format-info
  (format    :int)
  (name      :string)
  (extension :string))

(cffi:defcstruct dither-info
  (type  :int)
  (level :double)
  (name  :string))

(cffi:defcstruct embed-file-info
  (offset sf-count)
  (length sf-count))

(cffi:defcstruct instr-loop
  (mode  :int)
  (start :unsigned-int)
  (end   :unsigned-int)
  (count :unsigned-int))

(cffi:defcstruct instrument
  (gain        :int)
  (basenote    :char)
  (detune      :char)
  (velocity-lo :char)
  (velocity-hi :char)
  (key-lo      :char)
  (key-hi      :char)
  (loop-count  :int)
  ;; Use an array of int because it fails
  ;(loops       (:struct instr-loop) :count 16))
  (loops       :int :count 64))

(cffi:defcstruct loop-info
  (time-sig-num :short)
  (time-sig-den :short)
  (loop-mode    :int)
  (num-beats    :int)
  (bpm          :float)
  (root-key     :int)
  (future       :int :count 6))

(cffi:defcstruct broadcast-info
  (description          :char :count 256)
  (originator           :char :count  32)
  (originator-reference :char :count  32)
  (origination-date     :char :count  10)
  (origination-time     :char :count   8)
  (time-reference-low   :unsigned-int)
  (time-reference-high  :unsigned-int)
  (version              :short)
  (umid                 :char :count  64)
  (reserved             :char :count 190)
  (coding-history-size  :unsigned-int)
  (coding-history       :char :count 256))

(cffi:defcstruct virtual-io
  (get-filelen :pointer)
  (seek        :pointer)
  (read        :pointer)
  (write       :pointer)
  (tell        :pointer))

(cffi:defcfun ("sf_open" %open) sndfile
  (path   :string)
  (mode   :int)
  (sfinfo sndinfo))

(cffi:defcfun ("sf_open_fd" open-fd) sndfile
  (fd         :int)
  (mode       :int)
  (sfinfo     sndinfo)
  (close-desc :int))

(cffi:defcfun ("sf_open_virtual" open-virtual) sndfile
  (sfvirtual :pointer)
  (mode      :int)
  (sfinfo    sndinfo)
  (user-data :pointer))

(cffi:defcfun ("sf_error" error) :int
  (sndfile sndfile))

(cffi:defcfun ("sf_strerror" strerror) :string
  (sndfile sndfile))

(cffi:defcfun ("sf_error_number" error-number) :string
  (errnum :int))

(declaim (inline command))
(cffi:defcfun ("sf_command" command) :int
  (sndfile  sndfile)
  (command  :int)
  (data     :pointer)
  (datasize :int))

(cffi:defcfun ("sf_format_check" format-check) :boolean
  (info sndinfo))

(cffi:defcfun ("sf_seek" seek) sf-count
  (sndfile sndfile)
  (frames  sf-count)
  (whence  :int))

(cffi:defcfun ("sf_set_string" set-string) :int
  (sndfile  sndfile)
  (str-type :int)
  (str      :string))

(cffi:defcfun ("sf_get_string" get-string) :string
  (sndfile  sndfile)
  (str-type :int))

(cffi:defcfun ("sf_version_string" version-string) :string)

(cffi:defcfun ("sf_read_raw" read-raw) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (bytes   sf-count))

(cffi:defcfun ("sf_write_raw" write-raw) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (bytes   sf-count))

(cffi:defcfun ("sf_readf_short" readf-short) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_writef_short" writef-short) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_readf_int" readf-int) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_writef_int" writef-int) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_readf_float" readf-float) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_writef_float" writef-float) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_readf_double" readf-double) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_writef_double" writef-double) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (frames  sf-count))

(cffi:defcfun ("sf_read_short" read-short) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_write_short" write-short) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_read_int" read-int) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_write_int" write-int) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_read_float" read-float) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_write_float" write-float) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_read_double" read-double) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_write_double" write-double) sf-count
  (sndfile sndfile)
  (ptr     :pointer)
  (items   sf-count))

(cffi:defcfun ("sf_close" %close) :int
  (sndfile sndfile))

(cffi:defcfun ("sf_write_sync" write-sync) :void
  (sndfile sndfile))
