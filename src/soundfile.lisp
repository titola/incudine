;;; Copyright (c) 2017-2022 Tito Latini
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

(in-package :soundfile)

(define-condition soundfile-error (incudine-simple-error file-error) ()
  (:documentation "All types of error conditions about sound files
inherit from this condition.

Subtype of INCUDINE-SIMPLE-ERROR and FILE-ERROR."))

(defstruct (stream (:copier nil))
  "Sound file stream type."
  (sf-pointer (incudine-missing-arg "Missing sndfile pointer.")
              :type cffi:foreign-pointer :read-only t)
  (sf-position 0 :type non-negative-fixnum64)
  (curr-frame 0 :type non-negative-fixnum64)
  (buffer-pointer (incudine-missing-arg "Missing buffer pointer.")
                  :type cffi:foreign-pointer :read-only t)
  (buffer-size 262144 :type positive-fixnum :read-only t)
  (buffer-index 0 :type non-negative-fixnum)
  (buffer-end 0 :type non-negative-fixnum)
  (buffer-frames 0 :type non-negative-fixnum)
  (buffer-offset 0 :type non-negative-fixnum64)
  (buffer-max-frames 262144 :type non-negative-fixnum :read-only t)
  (size 0 :type non-negative-fixnum64 :read-only t)
  (frames 0 :type non-negative-fixnum64)
  (channels 1 :type positive-fixnum :read-only t)
  (sample-rate (floor *sample-rate*) :type positive-fixnum :read-only t)
  (pathname (incudine-missing-arg "Missing pathname.") :type pathname
            :read-only t)
  (header-type *default-header-type* :type string :read-only t)
  (data-format *default-data-format* :type string :read-only t)
  (open-p nil :type boolean))

(defstruct (input-stream (:include stream) (:copier nil))
  "Sound file input stream type.")

(defstruct (output-stream (:include stream) (:copier nil))
  "Sound file output stream type."
  (dirty-p nil :type boolean)
  (buffer-written-frames 0 :type non-negative-fixnum)
  (mix-p nil :type boolean)
  (overwrite-p nil :type boolean :read-only t)
  ;; Used to set MIX-P if the file is opened with IF-EXISTS :ERROR or
  ;; :SUPERSEDE. If CURR-FRAME is less than FRAME-THRESHOLD, MIX-P is
  ;; changed to T. In practice MIX-P becomes T after a jump back.
  ;; MIX-P is always NIL in overwrite mode.
  (frame-threshold 0 :type non-negative-fixnum64)
  (sf-position-offset 0 :type non-negative-fixnum64))

(setf
  (documentation 'input-stream-p 'function)
  "Return T if object is of type SOUNDFILE:INPUT-STREAM."
  (documentation 'output-stream-p 'function)
  "Return T if object is of type SOUNDFILE:OUTPUT-STREAM.")

(defmethod print-object ((obj soundfile:stream) stream)
  (print-unreadable-object (obj stream :type t)
    (if (stream-open-p obj)
        (format stream ":FRAMES ~D :CHANNELS ~D :SR ~F"
                (stream-frames obj) (stream-channels obj)
                (stream-sample-rate obj))
        (format stream "(closed)"))))

(defun sf-open (file input-p info-ptr if-exists)
  (when (and (eq if-exists :supersede) (probe-file file))
    ;; Truncate to length 0 before to reopen in access mode read/write.
    (cl:with-open-file (f file :direction :output :if-exists :supersede)))
  (cffi:foreign-funcall "sf_open"
                        :string (incudine.util::native-namestring file)
                        :int (if input-p SF:SFM-READ SF:SFM-RDWR)
                        :pointer info-ptr :pointer))

(cffi:defcfun "sf_close" :int (sf :pointer))

(declaim (inline sf-seek))
(defun sf-seek (sf pos)
   (cffi:foreign-funcall "sf_seek" :pointer (stream-sf-pointer sf)
                         sf:count pos :int SF:SEEK-SET sf:count))

(defun read-into-buffer (sf &optional (update-status-p t))
  "Fill the internal stream buffer of SOUNDFILE:STREAM SF by reading
the next data samples.

If UPDATE-STATUS-P is T (default), update the status of SF."
  (let ((frames (cffi:foreign-funcall "sf_readf_double"
                                      :pointer (stream-sf-pointer sf)
                                      :pointer (stream-buffer-pointer sf)
                                      sf:count (stream-buffer-max-frames sf)
                                      sf:count)))
    (declare (type non-negative-fixnum frames))
    (when update-status-p
      (when (> frames 0)
        (incf (stream-sf-position sf) frames)
        (setf (stream-buffer-index sf) 0))
      (setf (stream-buffer-end sf) (* frames (stream-channels sf)))
      (setf (stream-buffer-frames sf) frames))
    frames))

(declaim (inline read-before-mix))
(defun read-before-mix (sf)
  (declare (type soundfile:output-stream sf))
  (let ((frames (read-into-buffer sf nil)))
    (declare (type non-negative-fixnum frames))
    (when (< frames (stream-buffer-max-frames sf))
      ;; Zero padding.
      (incudine.external:foreign-zero-sample
        (cffi:inc-pointer (stream-buffer-pointer sf)
                          (the non-negative-fixnum
                            (* (the non-negative-fixnum
                                 (* frames (stream-channels sf)))
                               #.(cffi:foreign-type-size :double))))
        (- (stream-buffer-max-frames sf) frames)))))

(declaim (inline position))
(defun position (sf)
  "Return the current position in frames of SOUNDFILE:STREAM SF.
Setfable."
  (stream-sf-position sf))

(defun set-position (sf pos)
  (declare (type soundfile:stream sf) (type non-negative-fixnum64 pos))
  (incudine-optimize
    #-64-bit (declare #.*reduce-warnings*)
    (when (soundfile:output-stream-p sf)
      (write-buffered-data sf)
      (when (< pos (output-stream-frame-threshold sf))
        (setf (output-stream-mix-p sf) t)))
    (let* ((offset (if (and (soundfile:output-stream-p sf)
                            (> (output-stream-sf-position-offset sf) 0))
                       (output-stream-sf-position-offset sf)
                       0))
           (pos (- (sf-seek sf (+ pos offset)) offset)))
      (declare (type non-negative-fixnum64 offset)
               (type (integer -1 #.(1- (ash 1 59))) pos))
      (cond ((< pos 0) (position sf))
            (t
             (when (and (soundfile:output-stream-p sf)
                        (output-stream-mix-p sf))
               (read-before-mix sf))
             (setf (stream-buffer-index sf) 0
                   (stream-buffer-frames sf) 0
                   (stream-buffer-end sf) 0
                   (stream-buffer-offset sf) pos
                   (stream-curr-frame sf) pos
                   (stream-sf-position sf) pos))))))

(defsetf position set-position)

(declaim (inline offset))
(defun offset (sf)
  "Return the stream position offset in frames of SOUNDFILE:STREAM SF.
Setfable."
  (output-stream-sf-position-offset sf))

(declaim (inline set-offset))
(defun set-offset (sf os)
  (declare (type non-negative-fixnum64 os))
  (setf (output-stream-sf-position-offset sf) os)
  (setf (position sf) 0)
  os)

(defsetf offset set-offset)

(declaim (inline buffer-size))
(defun buffer-size (sf)
  "Return the data size of the sample data of SOUNDFILE:STREAM SF."
  (stream-buffer-size sf))

(declaim (inline buffer-data))
(defun buffer-data (sf)
  "Return the foreign pointer to the sample data of SOUNDFILE:STREAM SF."
  (stream-buffer-pointer sf))

(declaim (inline buffer-index))
(defun buffer-index (sf)
  "Return the stream buffer index of SOUNDFILE:STREAM SF for the first
channel of the current frame."
  (stream-buffer-index sf))

(declaim (inline current-frame))
(defun current-frame (sf)
  "Return the current frame of SOUNDFILE:STREAM SF."
  (stream-curr-frame sf))

(declaim (inline path))
(defun path (sf)
  "Return the pathname of the SOUNDFILE:STREAM MF."
  (stream-pathname sf))

(defun data-location (filename)
  "Return the data location of the sound file FILENAME."
  (with-open-file (f filename)
    (let ((fd (incudine.util::stream-fd f)))
      (cffi:with-foreign-object (info '(:struct sf:info))
        (let ((sf (cffi:foreign-funcall "sf_open_fd"
                    :int fd :int SF:SFM-READ :pointer info
                    :pointer (cffi:null-pointer) :pointer)))
          (unwind-protect (incudine.util::lseek fd 0 SF:SEEK-CUR)
            (cffi:foreign-funcall "sf_close" :pointer sf :int)))))))

(defun check-format (header-type data-format)
  (unless (sf::major-format-p header-type)
    (error 'soundfile-error
           :format-control "The header type ~S is not recognised."
           :format-arguments (list header-type)))
  (unless (sf::minor-format-p data-format)
    (error 'soundfile-error
           :format-control "The data format ~S is not recognised."
           :format-arguments (list data-format))))

(defun check-opened-sound (sf header-type data-format)
  (declare (type (or sf:sndfile soundfile:stream) sf)
           (type string header-type data-format))
  (let ((ptr (if (stream-p sf)
                 (soundfile::stream-sf-pointer sf)
                 (sf:pointer sf))))
    (when (cffi:null-pointer-p ptr)
      (error 'soundfile-error
        ;; The error string could introduce directives.
        :format-control "~A"
        :format-arguments
          (list (if (= SF:ERR-UNRECOGNISED-FORMAT
                       (cffi:foreign-funcall "sf_error" :pointer ptr :int))
                    (format nil "The sound file format with header type ~S~%~
                                 and data format ~S is not recognised."
                            header-type data-format)
                    (cffi:foreign-funcall "sf_strerror" :pointer ptr :string)))))))

(defun update-sf-info (ptr sample-rate frames channels header-type data-format)
  (cffi:with-foreign-slots ((sf:sample-rate sf:frames sf:channels sf:format
                             sf:sections sf:seekable)
                            ptr (:struct sf:info))
    (setf sf:sample-rate (floor sample-rate)
          sf:frames frames
          sf:channels channels
          sf:format (sf:get-format (list header-type data-format))
          sf:sections 0 sf:seekable 1)
    (values)))

(defun open-file (file &optional (input-p t) sample-rate chans header-type
                  data-format if-exists)
  (cffi:with-foreign-object (info-ptr '(:struct sf:info))
    (let ((read-info-p (or input-p
                           (and (not (eq if-exists :supersede))
                                (probe-file file)))))
      (unless read-info-p
        (update-sf-info info-ptr sample-rate 0 chans header-type data-format))
      (let ((sf (sf-open file input-p info-ptr if-exists)))
        (if read-info-p
            (handler-case
                (cffi:with-foreign-slots
                    ((sf:sample-rate sf:frames sf:channels sf:format)
                     info-ptr (:struct sf:info))
                  (multiple-value-bind (header-type data-format)
                      (sf:decode-format sf:format)
                    (when (or (= sf:sample-rate 0) (= sf:channels 0)
                              (null header-type) (null data-format))
                      (error 'soundfile-error
                             :format-control "Bad header: ~{~S ~}"
                             :format-arguments `((:sample-rate ,sf:sample-rate
                                                  :channels ,sf:channels
                                                  :header-type ,header-type
                                                  :data-format ,data-format))))
                    (values sf sf:sample-rate sf:frames sf:channels
                            header-type data-format)))
              (condition (c) (sf-close sf) (error c)))
            (values sf sample-rate 0 chans header-type data-format))))))

(defgeneric read-header (obj)
  (:documentation "Read the header of a sound file and return five values:
sampling rate, number of frames, number of channels, header type and
data format."))

(defmethod read-header ((obj soundfile:stream))
  (values (stream-sample-rate obj) (stream-frames obj) (stream-channels obj)
          (stream-header-type obj) (stream-data-format obj)))

(defmethod read-header ((obj pathname))
  (multiple-value-bind (ptr sample-rate frames channels header-type data-format)
      (open-file obj)
    (sf-close ptr)
    (values sample-rate frames channels header-type data-format)))

(defmethod read-header ((obj string))
  (read-header (incudine.util::truename* obj)))

(defmacro header-value (obj n)
  (let* ((lst '(a b c d e))
         (value (nth n lst)))
    `(multiple-value-bind ,lst (read-header ,obj)
       (declare (ignore ,@(remove value lst)))
       (values ,value))))

(defun sample-rate (obj)
  "Return the sampling rate of the sound file."
  (values (read-header obj)))

(defun frames (obj)
  "Return the number of frames of the sound file."
  (header-value obj 1))

(defun channels (obj)
  "Return the number of channels of the sound file."
  (header-value obj 2))

(defun header-type (obj)
  "Return the header type of the sound file."
  (header-value obj 3))

(defun data-format (obj)
  "Return the data format of the sound file."
  (header-value obj 4))

(defun duration (obj)
  "Return the duration in seconds of the sound data in sound file."
  (multiple-value-bind (sr frames) (read-header obj)
    (declare (type positive-fixnum sr) (type non-negative-fixnum64 frames))
    (float (/ frames sr))))

(defun free-foreign-pointers (sf-ptr buf-ptr)
  (flet ((to-free-p (ptr)
           (and (typep ptr 'cffi:foreign-pointer)
                (not (cffi:null-pointer-p ptr)))))
    (when (to-free-p sf-ptr)
      (sf-close sf-ptr))
    (when (to-free-p buf-ptr)
      (cffi:foreign-free buf-ptr))
    (values)))

(defun check-file (filename direction if-exists)
  (cond ((and (eq direction :input)
              (null (incudine.util::probe-file* filename)))
         (error 'soundfile-error
                :pathname filename
                :format-control "error opening ~S: File not found"
                :format-arguments (list filename)))
        ((and (eq direction :output)
              (eq if-exists :error)
              (incudine.util::probe-file* filename))
         (error 'soundfile-error
                :pathname filename
                :format-control "error opening ~S: File exists"
                :format-arguments (list filename)))))

(defun open (filename &key (direction :input) (if-exists :error)
             (sample-rate *sample-rate*) (channels 1)
             (header-type *default-header-type*)
             (data-format *default-data-format*)
             (buffer-size *sndfile-buffer-size*)
             (input-stream-constructor #'make-input-stream)
             (output-stream-constructor #'make-output-stream))
  "Create, open and return a SOUNDFILE:STREAM that is connected to the
file specified by FILENAME.

DIRECTION is :INPUT (default) or :OUTPUT to return a SOUNDFILE:INPUT-STREAM
or a SOUNDFILE:OUTPUT-STREAM respectively.

IF-EXISTS should be one of :APPEND, :ERROR (default), :MIX, :OVERWRITE or
:SUPERSEDE. If it is :SUPERSEDE and there is a jump back of the file position
during the output operations on the stream, the writing continues in mix mode).

SAMPLE-RATE, CHANNELS, HEADER-TYPE and DATA-FORMAT determine the header of
the new sound file and default to *SAMPLE-RATE*, 1, *DEFAULT-HEADER-TYPE*
and *DEFAULT-DATA-FORMAT*, respectively. See INCUDINE:BOUNCE-TO-DISK for the
list of available header types and data formats.

BUFFER-SIZE is the size of the internal stream buffer and defaults to
*SNDFILE-BUFFER-SIZE*."
  (declare (type (or string pathname) filename)
           (type (member :input :output) direction)
           (type (member :append :error :mix :overwrite :supersede) if-exists)
           (type alexandria:positive-real sample-rate)
           (type non-negative-fixnum channels buffer-size)
           (type string header-type data-format))
  (check-file filename direction if-exists)
  (let* ((buffer-size (max 8192 buffer-size))
         (buf-ptr (cffi:foreign-alloc :double :count buffer-size
                                      :initial-element 0d0))
         (sf-ptr nil)
         (input-p (eq direction :input))
         (pathname (if input-p
                       (incudine.util::truename* filename)
                       (make-pathname
                         :defaults (incudine.util::%parse-filepath filename)))))
    (check-format header-type data-format)
    (handler-case
        (let ((sf (multiple-value-bind (ptr sample-rate frames channels
                                        header-type data-format)
                      (open-file pathname input-p sample-rate channels
                                 header-type data-format if-exists)
                    (setf sf-ptr ptr)
                    (let ((buf-max-frames (floor (/ buffer-size channels))))
                      (apply (if input-p
                                 input-stream-constructor
                                 output-stream-constructor)
                             (list*
                               :sf-pointer sf-ptr
                               :buffer-pointer buf-ptr
                               :buffer-size (* buf-max-frames channels)
                               :buffer-max-frames buf-max-frames
                               :size (* frames channels)
                               :frames frames
                               :channels channels
                               :sample-rate (floor sample-rate)
                               :pathname pathname
                               :header-type header-type
                               :data-format data-format
                               :open-p t
                               (when (not input-p)
                                 (list :mix-p (eq if-exists :mix)
                                       :sf-position-offset
                                          (if (eq if-exists :append)
                                              frames
                                              0)))))))))
          (check-opened-sound sf header-type data-format)
          (unless input-p
            (case if-exists
              (:append
               ;; Offset by SF-POSITION-OFFSET
               (setf (position sf) 0))
              (:mix
               (sf-seek sf 0)
               (read-before-mix sf))
              (:overwrite
               (sf-seek sf 0))))
          (incudine-finalize sf
            (lambda () (free-foreign-pointers sf-ptr buf-ptr))
            nil))
      (condition (c)
        (free-foreign-pointers sf-ptr buf-ptr)
        (error c)))))

(declaim (inline open-p))
(defun open-p (sf)
  "Whether SF is an open SOUNDFILE:STREAM."
  (stream-open-p sf))

(defun close (sf)
  "Close a SOUNDFILE:STREAM."
  (when (open-p sf)
    (when (soundfile:output-stream-p sf)
      (write-buffered-data sf))
    (free-foreign-pointers (stream-sf-pointer sf) (stream-buffer-pointer sf))
    (incudine-cancel-finalization sf)
    (setf (stream-open-p sf) nil))
  sf)

(defmacro with-open-soundfile ((stream filespec &rest options) &body body)
  "Use SOUNDFILE:OPEN to create a SOUNDFILE:STREAM. When control leaves
the body, either normally or abnormally, the SOUNDFILE:STREAM is
automatically closed."
  `(let ((,stream (open ,filespec ,@options)))
     (unwind-protect (progn ,@body) (close ,stream))))

(defun update-header (sf &key (auto-p nil auto))
  "Update the header of the file connected to the SOUNDFILE:OUTPUT-STREAM SF.

If AUTO-P is T, the file header is updated after each write to the stream.

Turn off the auto header update if AUTO-P NIL is explicitly specified.

Return T if the sound file header is updated, or if the auto header update
is turned on. Otherwise, return NIL."
  (declare (type soundfile:output-stream sf))
  (multiple-value-bind (cmd value ret)
      (if auto
          (values
            SF:SFC-SET-UPDATE-HEADER-AUTO (if auto-p SF:TRUE SF:FALSE) SF:TRUE)
          (values SF:SFC-UPDATE-HEADER-NOW 0 0))
    (= ret
       (cffi:foreign-funcall "sf_command"
         :pointer (stream-sf-pointer sf) :int cmd
         :pointer (cffi:null-pointer) :int value :int))))

(declaim (inline eof-p))
(defun eof-p (sf)
  "Whether end of file for SOUNDFILE:STREAM SF is reached."
  #-64-bit (declare #.*reduce-warnings*)
  (>= (stream-curr-frame sf) (stream-frames sf)))

(defgeneric metadata (obj type)
  (:documentation "Return the string metadata TYPE of the sound file.

TYPE is of type SYMBOL or STRING and should be one of title, copyright,
software, artist, comment, date, album, license, tracknumber or genre.

Setfable."))

(defgeneric (setf metadata) (string obj type))

(defun metadata-sf-type (type)
  (declare (type (or symbol string) type))
  (cdr (assoc (string type)
              `(("title" . ,SF:STR-TITLE) ("copyright" . ,SF:STR-COPYRIGHT)
                ("software" . ,SF:STR-SOFTWARE) ("artist" . ,SF:STR-ARTIST)
                ("comment" . ,SF:STR-COMMENT) ("date" . ,SF:STR-DATE)
                ("album" . ,SF:STR-ALBUM) ("license" . ,SF:STR-LICENSE)
                ("tracknumber" . ,SF:STR-TRACKNUMBER) ("genre" . ,SF:STR-GENRE))
              :test 'string-equal)))

(defmethod metadata ((obj soundfile:stream) type)
  (let ((sf-type (metadata-sf-type type)))
    (when sf-type
      (cffi:foreign-funcall "sf_get_string" :pointer (stream-sf-pointer obj)
                            :int sf-type :string))))

(defmethod metadata ((obj pathname) type)
  (with-open-soundfile (sf obj) (metadata sf type)))

(defmethod metadata ((obj string) type)
  (metadata (incudine.util::truename* obj) type))

(defmethod (setf metadata) ((string string) (obj soundfile:stream) type)
  (let ((sf-type (metadata-sf-type type)))
    (and sf-type
         (= (cffi:foreign-funcall "sf_set_string"
                                  :pointer (stream-sf-pointer obj)
                                  :int sf-type :string string :int)
            SF:ERR-NO-ERROR)
         string)))

(defmethod (setf metadata) ((string string) (obj pathname) type)
  (with-open-soundfile (sf obj :direction :output :if-exists :mix)
    (setf (metadata sf type) string)))

(defmethod (setf metadata) ((string string) (obj string) type)
  (setf (metadata (incudine.util::truename* obj) type) string))

(declaim (inline clear-buffer))
(defun clear-buffer (sf frames)
  (incudine.external:foreign-zero-sample
    (stream-buffer-pointer sf)
    (the non-negative-fixnum (* frames (stream-channels sf)))))

(defun write-buffered-data (sf)
  (declare (type soundfile:output-stream sf))
  (incudine-optimize
    #-64-bit (declare #.*reduce-warnings*)
    (if (output-stream-dirty-p sf)
        (let ((frames (cffi:foreign-funcall "sf_writef_double"
                        :pointer (stream-sf-pointer sf)
                        :pointer (stream-buffer-pointer sf)
                        sf:count (1+ (output-stream-buffer-written-frames sf))
                        sf:count)))
          (declare (type non-negative-fixnum frames))
          (when (> frames 0)
            (incf (stream-sf-position sf) frames)
            (setf (stream-buffer-index sf) 0)
            (clear-buffer sf (1+ (output-stream-buffer-written-frames sf)))
            (setf (output-stream-dirty-p sf) nil)
            (setf (output-stream-buffer-written-frames sf) 0))
          frames)
        0)))

(defmacro maybe-read-before-test (sf test)
  `(progn
     (when (= (stream-buffer-frames ,sf) 0)
       (read-into-buffer ,sf))
     ,test))

(defun buffer-value (sf index)
  "Return the stream buffer value of SOUNDFILE:STREAM SF specified by INDEX."
  (cffi:mem-ref (stream-buffer-pointer sf) :double
                (the non-negative-fixnum
                  (* index #.(cffi:foreign-type-size :double)))))

(define-compiler-macro buffer-value (sf index)
  (let ((type-size #.(cffi:foreign-type-size :double)))
    (if (constantp index)
        `(cffi:mem-ref (stream-buffer-pointer ,sf) :double
                       ,(* (eval index) type-size))
        `(cffi:mem-ref (stream-buffer-pointer ,sf) :double
                       (* ,index ,type-size)))))

(defun set-buffer-value (sf index value)
  (setf (cffi:mem-ref (stream-buffer-pointer sf) :double
                      (the non-negative-fixnum
                        (* index #.(cffi:foreign-type-size :double))))
        (coerce value 'double-float)))

(define-compiler-macro set-buffer-value (sf index value)
  (let ((type-size #.(cffi:foreign-type-size :double)))
    (if (constantp index)
        `(setf (cffi:mem-ref (stream-buffer-pointer ,sf) :double
                             ,(* (eval index) type-size))
               ,(if (constantp value)
                    (coerce (eval value) 'double-float)
                    `(coerce ,value 'double-float)))
        `(setf (cffi:mem-ref (stream-buffer-pointer ,sf) :double
                             (the non-negative-fixnum
                               (* ,index ,type-size)))
             (coerce ,value 'double-float)))))

(defsetf buffer-value set-buffer-value)

(defmacro current-buffer-value (sf channel)
  `(buffer-value ,sf (+ (stream-buffer-index ,sf) ,channel)))

(defun read-forward (sf &optional (channel 0) peek-p)
  (declare (type soundfile:stream sf) (type non-negative-fixnum channel)
           (type boolean peek-p))
  (incudine-optimize
    (let ((channels (stream-channels sf)))
      (declare (type non-negative-fixnum channels))
      (if (or (not (open-p sf))
              (>= channel channels)
              (eof-p sf)
              (maybe-read-before-test sf (= (stream-buffer-frames sf) 0))
              (and (>= (stream-buffer-index sf) (stream-buffer-end sf))
                   (= (the non-negative-fixnum64 (read-into-buffer sf)) 0)))
          0d0
          (let ((res (current-buffer-value sf channel)))
            (declare #.*reduce-warnings*)
            (unless peek-p
              (incf (stream-curr-frame sf))
              (incf (stream-buffer-index sf) channels))
            res)))))

(defun read-backward (sf &optional (channel 0) peek-p)
  (declare (type soundfile:stream sf) (type non-negative-fixnum channel)
           (type boolean peek-p))
  (incudine-optimize
    #-64-bit (declare #.*reduce-warnings*)
    (let ((channels (stream-channels sf)))
      (declare (type non-negative-fixnum channels))
      (if (or (not (open-p sf))
              (>= channel channels)
              (maybe-read-before-test sf (< (position sf) channels)))
          0d0
          (let ((res (current-buffer-value sf channel))
                (curr-frame (stream-curr-frame sf))
                (max-frames (stream-buffer-max-frames sf))
                (delta (if peek-p 0 1)))
            (cond
              ((= curr-frame 0)
               (setf (position sf) 0))
              ((< (stream-buffer-index sf) channels)
               (multiple-value-bind (pos buffer-frame)
                   (if (> curr-frame max-frames)
                       (values (1+ (- curr-frame max-frames))
                               (- (stream-buffer-frames sf) 1 delta))
                       (values 0 (- curr-frame delta)))
                 (setf (position sf) pos)
                 (read-into-buffer sf)
                 (setf (stream-curr-frame sf) (- curr-frame delta))
                 (setf (stream-buffer-index sf) (* buffer-frame channels))))
              ((not peek-p)
               (decf (stream-curr-frame sf))
               (decf (stream-buffer-index sf) channels)))
            (reduce-warnings res))))))

(declaim (inline read-next))
(defun read-next (sf &optional (channel 0) peek-p (forward-p t))
  "If PEEK-P is NIL (default), read and return the next value from
SOUNDFILE:STREAM SF at CHANNEL (default 0). If PEEK-P is T, return the
value of the current frame.

If PEEK-P is NIL, read forward if FORWARD-P is T (default) or backward
if FORWARD-P is NIL."
  (declare (type soundfile:stream sf) (type non-negative-fixnum channel)
           (type boolean forward-p peek-p))
  (the double-float
    (values (if forward-p
                (read-forward sf channel peek-p)
                (read-backward sf channel peek-p)))))

(declaim (inline buffer-index-fwd))
(defun buffer-index-fwd (sf frame limit)
  (let* ((fdiff (- frame (stream-buffer-offset sf)))
         (diff (* fdiff (stream-channels sf))))
    (declare (type non-negative-fixnum64 fdiff diff))
    (when (< diff limit)
      (when (and (soundfile:output-stream-p sf)
                 (> fdiff (output-stream-buffer-written-frames sf)))
        (setf (output-stream-buffer-written-frames sf) fdiff))
      (setf (stream-buffer-index sf) diff)
      (setf (stream-curr-frame sf) frame))))

(declaim (inline buffer-index-rew))
(defun buffer-index-rew (sf frame)
  (let* ((fdiff (- frame (stream-buffer-offset sf)))
         (diff (* fdiff (stream-channels sf))))
    (declare (type non-negative-fixnum64 fdiff diff))
    (when (< diff (stream-buffer-index sf))
      (setf (stream-buffer-index sf) diff)
      (setf (stream-curr-frame sf) frame))))

(declaim (inline move-to-frame))
(defun move-to-frame (sf frame buffer-end)
  #-64-bit (declare #.*reduce-warnings*)
  (let ((curr (stream-curr-frame sf)))
    (unless (or (= frame curr)
                (and (> frame curr) (buffer-index-fwd sf frame buffer-end))
                (and (< frame curr)
                     (not (< frame (stream-buffer-offset sf)))
                     (buffer-index-rew sf frame)))
      ;; Frame out of buffer.
      (setf (position sf) frame))
    (when (and (soundfile:output-stream-p sf)
               (not (output-stream-dirty-p sf)))
      (setf (output-stream-dirty-p sf) t))
    sf))

(defun read (sf &optional frame (channel 0) (peek-p t) (forward-p t))
  "Read and return a value from SOUNDFILE:STREAM SF at FRAME (current
by default) and CHANNEL (0 by default).

If PEEK-P is T (default), don't increment the frame counter.
If PEEK-P is NIL, read forward if FORWARD-P is T (default) or backward
if FORWARD-P is NIL."
  (declare (type soundfile:stream sf)
           (type (or null non-negative-fixnum64) frame)
           (type non-negative-fixnum channel)
           (type boolean forward-p peek-p))
  (incudine-optimize
    #-64-bit (declare #.*reduce-warnings*)
    (cond ((or (not (open-p sf))
               (>= channel (stream-channels sf)))
           0d0)
          ((>= (or frame (setf frame (stream-curr-frame sf)))
               (stream-frames sf))
           (unless (= frame (stream-curr-frame sf))
             (setf (stream-curr-frame sf) (stream-frames sf)))
           0d0)
          (t
           (read-next (move-to-frame sf frame (stream-buffer-end sf))
                      channel peek-p forward-p)))))

(declaim (inline update-frame-threshold))
(defun update-frame-threshold (sf frame)
  #-64-bit (declare #.*reduce-warnings*)
  (cond ((or (output-stream-overwrite-p sf) (output-stream-mix-p sf))
         nil)
        ((> frame (output-stream-frame-threshold sf))
         (setf (output-stream-frame-threshold sf) frame))
        ((< frame (output-stream-frame-threshold sf))
         (setf (output-stream-mix-p sf) t)))
  (values))

(declaim (inline write-double))
(defun write-double (sf frame data channel)
  (declare (type soundfile:output-stream sf)
           (type non-negative-fixnum64 frame)
           (type double-float data)
           (type non-negative-fixnum channel))
  (incudine-optimize
    (when (and (open-p sf) (< channel (stream-channels sf)))
      (update-frame-threshold
        (move-to-frame sf frame (stream-buffer-size sf)) frame)
      (when (< (stream-buffer-index sf) (stream-buffer-size sf))
        (if (output-stream-mix-p sf)
            (incf (buffer-value sf (+ (stream-buffer-index sf) channel)) data)
            (setf (buffer-value sf (+ (stream-buffer-index sf) channel)) data)))
      sf)))

(declaim (inline write))
(defun write (sf frame value &optional (channel 0))
  "Write VALUE to SOUNDFILE:OUTPUT-STREAM SF at FRAME and CHANNEL
(0 by default)."
  (declare (type soundfile:output-stream sf)
           (type non-negative-fixnum64 frame)
           (type real value)
           (type non-negative-fixnum channel))
  (write-double sf frame (coerce value 'double-float) channel))

(declaim (inline foreign-read))
(defun foreign-read (sf buffer-pointer buffer-size
                     &optional update-position-p)
  "A foreign buffer of type double float pointed to by BUFFER-POINTER is
filled with the next BUFFER-SIZE items read from the SOUNDFILE:STREAM SF.

Return the number of items read."
  (declare (type soundfile:stream sf)
           (type cffi:foreign-pointer buffer-pointer)
           (type non-negative-fixnum buffer-size))
  (if (open-p sf)
      (let ((items (cffi:foreign-funcall "sf_read_double"
                     :pointer (stream-sf-pointer sf) :pointer buffer-pointer
                     sf:count buffer-size sf:count)))
        (declare (type non-negative-fixnum items))
        (when (> items 0)
          (when update-position-p
            (incf (stream-sf-position sf)
                  (truncate items (stream-channels sf))))
          (setf (stream-buffer-index sf) 0))
        items)
      0))

(declaim (inline foreign-write))
(defun foreign-write (sf buffer-pointer buffer-size)
  "Write BUFFER-SIZE values stored in a foreign buffer of type double
float pointed to by BUFFER-POINTER to the SOUNDFILE:OUTPUT-STREAM SF.

Return the number of the items written."
  (declare (type soundfile:output-stream sf)
           (type cffi:foreign-pointer buffer-pointer)
           (type non-negative-fixnum buffer-size)
           #-64-bit #.*reduce-warnings*)
  (if (open-p sf)
      (let ((items (cffi:foreign-funcall "sf_write_double"
                     :pointer (output-stream-sf-pointer sf)
                     :pointer buffer-pointer sf:count buffer-size
                     sf:count)))
        (declare (type non-negative-fixnum items))
        (when (> items 0)
          (incf (stream-sf-position sf)
                (truncate items (stream-channels sf)))
          (setf (stream-buffer-index sf) 0)
          (setf (output-stream-dirty-p sf) nil)
          (setf (output-stream-buffer-written-frames sf) 0))
        items)
      0))

(defmacro maxamp-loop ((sf count index chan curr max) &body update-max)
  (alexandria:with-gensyms (frames channels j)
    `(loop for ,count of-type non-negative-fixnum64 = 0 then (+ ,count ,frames)
           for ,frames of-type non-negative-fixnum = (read-into-buffer ,sf nil)
           with ,channels of-type non-negative-fixnum = (channels ,sf)
           while (> ,frames 0) do
             (loop for ,index below ,frames do
                     (loop for ,chan below ,channels
                           for ,j of-type non-negative-fixnum
                                  from (* ,index ,channels) do
                             (setf ,curr (abs (buffer-value ,sf ,j)))
                             (when (> ,curr ,max)
                               ,@update-max))))))

(defun maxamp-for-channel (infile)
  (with-open-soundfile (in infile)
    (let* ((chans (channels in))
           (max-arr (make-array chans :element-type 'double-float
                                :initial-element 0d0))
           (pos-arr (make-array chans :element-type '(unsigned-byte 64)
                                :initial-element 0))
           (max-chan 0)
           (val 0d0))
      (maxamp-loop (in count index ch val (aref max-arr ch))
        (setf (aref max-arr ch) val)
        (setf (aref pos-arr ch) (+ count index))
        (when (and (/= max-chan ch) (> val (aref max-arr max-chan)))
          (setf max-chan ch)))
      (values max-arr pos-arr max-chan))))

(defun maxamp-all-channels (infile)
  (cffi:with-foreign-objects ((max :double)
                              (curr :double)
                              (pos :uint64))
    (setf (cffi:mem-ref max :double) 0d0)
    (setf (cffi:mem-ref pos :uint64) 0)
    (with-open-soundfile (in infile)
      (maxamp-loop (in count index ch (cffi:mem-ref curr :double)
                    (cffi:mem-ref max :double))
        (setf (cffi:mem-ref max :double) (cffi:mem-ref curr :double))
        (setf (cffi:mem-ref pos :uint64) (+ count index))))
    (values (cffi:mem-ref max :double) (cffi:mem-ref pos :uint64))))

(defun maxamp (infile &optional channel)
  "Return the maximum amplitude of the sound file INFILE.

INFILE is of type PATHNAME or STRING.

If CHANNEL is an integer, return the maximum amplitude of that CHANNEL."
  (declare (type (or pathname string) infile)
           (type (or boolean non-negative-fixnum) channel))
  (if channel
      (multiple-value-bind (max-arr pos-arr ch) (maxamp-for-channel infile)
        (if (numberp channel)
            (if (< channel (length max-arr))
                (values (aref max-arr channel) (aref pos-arr channel) channel)
                (error 'soundfile-error
                       :format-control "Channel index ~D but ~S is a sound file ~
                                        with ~D channels."
                       :format-arguments (list channel infile (length max-arr))))
            (values max-arr pos-arr ch)))
      (maxamp-all-channels infile)))

(defun convert (infile outfile header-type data-format
                &key normalize scale-by scale-to)
  (declare (type (or pathname string) infile)
           (type (or pathname string) outfile)
           (type string header-type data-format)
           (type (or null real) normalize scale-by scale-to))
  "Convert the sound file INFILE to another format with HEADER-TYPE
and DATA-FORMAT. The resultant sound file OUTFILE is overwritten if it
already exists.

The sample data is possibly normalized to NORMALIZE dB or SCALE-TO [0.0,1.0],
or scaled by SCALE-BY.

See INCUDINE:BOUNCE-TO-DISK for the list of available header types and data
formats."
  (flet ((r-maxamp (in)
           (incudine-optimize (/ (the double-float (maxamp in))))))
    (let ((mult (or (and scale-by (coerce scale-by 'double-float))
                    (and normalize (* (db->linear normalize) (r-maxamp infile)))
                    (and scale-to (* scale-to (r-maxamp infile))))))
      (declare (type (or null double-float) mult)
               #.*standard-optimize-settings*)
      (with-open-soundfile (in infile)
        (let ((channels (stream-channels in)))
          (declare (type non-negative-fixnum channels))
          (with-open-soundfile (out outfile :direction :output
                                :if-exists :supersede
                                :sample-rate (stream-sample-rate in)
                                :channels channels :header-type header-type
                                :data-format data-format)
            (loop for items of-type non-negative-fixnum
                            = (* (the non-negative-fixnum
                                   (read-into-buffer in nil))
                                 channels)
                  while (> items 0) do
                    (when mult
                      (loop for i below items do
                              (setf (buffer-value in i)
                                    (* (buffer-value in i) mult))))
                    (foreign-write out (stream-buffer-pointer in) items))
            outfile))))))

(defun check-soundfiles-to-combine (input-files)
  (multiple-value-bind (sr frames chans) (read-header (first input-files))
    (declare (ignore frames))
    (dolist (in (rest input-files))
      (with-open-soundfile (sf in)
        (unless (= (sample-rate sf) sr)
          (error 'soundfile-error
            :format-control "Input files with different sample rate."))
        (unless (= (channels sf) chans)
          (error 'soundfile-error
            :format-control "Input files with different number of channels."))))
    (values sr chans)))

(defun concatenate (output-file input-files &key (if-exists :error)
                    (header-type *default-header-type*)
                    (data-format *default-data-format*)
                    (buffer-size *sndfile-buffer-size*))
  "OUTPUT-FILE is the concatenation of the sound files INPUT-FILES.

The input files must have the same sampling rate and number of channels.

OUTPUT-FILE is written with HEADER-TYPE and DATA-FORMAT that default to
*DEFAULT-HEADER-TYPE* and *DEFAULT-DATA-FORMAT*. See INCUDINE:BOUNCE-TO-DISK
for the list of available header types and data formats.

IF-EXISTS is :ERROR by default. See SOUNDFILE:OPEN for details.

BUFFER-SIZE is the size of the internal stream buffer."
  (declare (type non-negative-fixnum buffer-size))
  (multiple-value-bind (sr chans) (check-soundfiles-to-combine input-files)
    (cffi:with-foreign-object (buf :double buffer-size)
      (with-open-soundfile (out output-file :direction :output
                            :if-exists if-exists :sample-rate sr
                            :channels chans :header-type header-type
                            :data-format data-format)
        (dolist (input input-files)
          (with-open-soundfile (in input)
            (loop for items = (foreign-read in buf *sndfile-buffer-size*)
                  while (> items 0) do
                    (foreign-write out buf items)))))))
  output-file)

(defun merge (output-file input-files &key (if-exists :error)
              (header-type *default-header-type*)
              (data-format *default-data-format*)
              (buffer-size *sndfile-buffer-size*)
              normalize scale-by scale-to)
  "OUTPUT-FILE is the mix of the sound files INPUT-FILES.

The input files must have the same sampling rate and number of channels.

OUTPUT-FILE is written with HEADER-TYPE and DATA-FORMAT that default to
*DEFAULT-HEADER-TYPE* and *DEFAULT-DATA-FORMAT*. See INCUDINE:BOUNCE-TO-DISK
for the list of available header types and data formats.

IF-EXISTS is :ERROR by default. See SOUNDFILE:OPEN for details.

The mix is possibly normalized to NORMALIZE dB or SCALE-TO [0.0,1.0],
or scaled by SCALE-BY.

BUFFER-SIZE is the size of the internal stream buffer."
    (declare (type non-negative-fixnum buffer-size))
    (multiple-value-bind (sr chans) (check-soundfiles-to-combine input-files)
      (multiple-value-bind (temp-file-p file ht df)
          (if (or normalize scale-to)
              (values t (make-pathname :defaults output-file :type "tmp")
                      "wav" "double")
              (values nil output-file header-type data-format))
        (cffi:with-foreign-objects ((buf :double buffer-size)
                                    (tmp :double buffer-size))
          (with-open-soundfile (out file :direction :output :if-exists if-exists
                                :sample-rate sr :channels chans :header-type ht
                                :data-format df)
            (incudine:with-cleanup
              (let ((inputs (mapcar #'soundfile:open input-files))
                    (items buffer-size)
                    (tmp-items 0))
                (declare (type non-negative-fixnum items tmp-items))
                (loop while (> items 0) do
                        (setf items (foreign-read (first inputs) buf buffer-size))
                        (dolist (in (rest inputs))
                          (setf tmp-items (foreign-read in tmp buffer-size))
                          (setf items (max items tmp-items))
                          (dotimes (i tmp-items)
                            (incf (cffi:mem-aref buf :double i)
                                  (cffi:mem-aref tmp :double i))))
                        (when scale-by
                          (dotimes (i items)
                            (setf (cffi:mem-aref buf :double i)
                                  (* (cffi:mem-aref buf :double i) scale-by))))
                        (foreign-write out buf items))))))
        (when temp-file-p
          (convert file output-file header-type data-format :normalize normalize
                   :scale-to scale-to)
          (delete-file file))))
    output-file)
