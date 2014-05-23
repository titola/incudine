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

(deftype flag () 'cons)

(declaim (inline make-flag))
(defun make-flag (&optional value) (list value))

(defmacro flag-value (x) `(car ,x))

(defstruct handle
  (ptr (cffi:null-pointer) :type cffi:foreign-pointer)
  (descriptor (cffi:null-pointer) :type cffi:foreign-pointer)
  (activated (make-flag) :type flag)
  (deleted (make-flag) :type flag))

(declaim (inline pointer))
(defun pointer (instance)
  (handle-ptr instance))

(declaim (inline descriptor))
(defun descriptor (instance)
  (handle-descriptor instance))

(declaim (inline active-p))
(defun active-p (instance)
  (flag-value (handle-activated instance)))

(declaim (inline deleted-p))
(defun deleted-p (instance)
  (flag-value (handle-deleted instance)))

(defmethod print-object ((obj handle) stream)
  (format stream "#<~S ~S :ACTIVE-P ~A :DELETED-P ~A>"
          (type-of obj) (label (descriptor obj)) (active-p obj)
          (deleted-p obj)))

(macrolet ((descriptor-slot-simple-readers (names)
             `(progn
                (declaim (inline ,@names))
                ,@(mapcar (lambda (name)
                            `(defun ,name (descriptor)
                               (descriptor-slot-value descriptor ',name)))
                          names))))
  (descriptor-slot-simple-readers (unique-id label properties name maker
                                   copyright port-count implementation-data)))

(defun port-descriptors (descriptor)
  (let ((ptr (descriptor-slot-value descriptor 'port-descriptors)))
    (loop for i below (port-count descriptor)
          collect (cffi:mem-aref ptr 'port-descriptor i))))

(defun port-names (descriptor)
  (let ((ptr (descriptor-slot-value descriptor 'port-names)))
    (loop for i below (port-count descriptor)
          collect (cffi:mem-aref ptr :string i))))

(defun port-range-hints (descriptor)
  (let ((ptr (descriptor-slot-value descriptor 'port-range-hints)))
    (loop for i below (port-count descriptor)
          collect (let ((hint (cffi:mem-aptr ptr '(:struct port-range-hint) i)))
                    (cffi:with-foreign-slots
                        ((hint-descriptor lower-bound upper-bound) hint
                         (:struct port-range-hint))
                      (list hint-descriptor lower-bound upper-bound))))))

(declaim (inline call-descriptor))
(defun call-descriptor (descriptor-cb index)
  (cffi:foreign-funcall-pointer descriptor-cb nil :unsigned-long index
                                :pointer))

(defmacro ladspa-funcall (descriptor cb-name &rest args)
  `(cffi:foreign-funcall-pointer (descriptor-slot-value ,descriptor ',cb-name)
                                 nil ,@args))

(declaim (inline has-non-null-slot-p))
(defun has-non-null-slot-p (descriptor slot-name)
  (not (cffi:null-pointer-p (descriptor-slot-value descriptor slot-name))))

(defun instantiate (callback descriptor sample-rate)
  (let* ((ptr (cffi:foreign-funcall-pointer callback nil :pointer descriptor
                                            :unsigned-long sample-rate
                                            :pointer))
         (activated (make-flag))
         (deleted (make-flag))
         (obj (make-handle :ptr ptr :descriptor descriptor
                           :activated activated :deleted deleted)))
    (tg:finalize obj (lambda () (%cleanup descriptor ptr activated deleted)))))

(declaim (inline connect-port))
(defun connect-port (callback instance port data-location)
  (cffi:foreign-funcall-pointer callback nil :pointer (handle-ptr instance)
                                :unsigned-long port :pointer data-location
                                :void))

(declaim (inline has-activate-p))
(defun has-activate-p (descriptor)
  (has-non-null-slot-p descriptor 'activate))

(defun activate (instance)
  (let ((descriptor (handle-descriptor instance)))
    (when (and (has-activate-p descriptor) (not (deleted-p instance)))
      (when (active-p instance)
        (deactivate instance))
      (ladspa-funcall descriptor activate :pointer (handle-ptr instance) :void)
      (setf (flag-value (handle-activated instance)) t))))

(declaim (inline run))
(defun run (callback instance sample-count)
  (cffi:foreign-funcall-pointer callback nil :pointer (handle-ptr instance)
                                :unsigned-long sample-count :void))

(declaim (inline has-run-adding-p))
(defun has-run-adding-p (descriptor)
  (has-non-null-slot-p descriptor 'run-adding))

;;; Note: the RUN-ADDING slot of the descriptor is optional and we
;;; can use the follow function only when HAS-RUN-ADDING-P is T.
(declaim (inline run-adding))
(defun run-adding (callback instance sample-count)
  (cffi:foreign-funcall-pointer callback nil :pointer (handle-ptr instance)
                                :unsigned-long sample-count :void))

(declaim (inline has-set-run-adding-gain-p))
(defun has-set-run-adding-gain-p (descriptor)
  (and (has-run-adding-p descriptor)
       (has-non-null-slot-p descriptor 'set-run-adding-gain)))

;;; Note: the SET-RUN-ADDING-GAIN slot of the descriptor is optional and we
;;; can use the follow function only when HAS-SET-RUN-ADDING-GAIN-P is T.
(declaim (inline set-run-adding-gain))
(defun set-run-adding-gain (callback instance gain)
  (cffi:foreign-funcall-pointer callback nil :pointer (handle-ptr instance)
                                data gain :void))

(declaim (inline has-deactivate-p))
(defun has-deactivate-p (descriptor)
  (has-non-null-slot-p descriptor 'deactivate))

(declaim (inline %deactivate))
(defun %deactivate (descriptor instance-ptr activated)
  (when (and (has-deactivate-p descriptor) (flag-value activated))
    (ladspa-funcall descriptor deactivate :pointer instance-ptr :void)
    (setf (flag-value activated) nil)))

(declaim (inline deactivate))
(defun deactivate (instance)
  (let ((descriptor (handle-descriptor instance)))
    (unless (deleted-p instance)
      (%deactivate descriptor (handle-ptr instance)
                   (handle-activated instance)))))

(defun %cleanup (descriptor instance-ptr activated deleted)
  (unless (flag-value deleted)
    (%deactivate descriptor instance-ptr activated)
    (ladspa-funcall descriptor cleanup :pointer instance-ptr :void)
    (setf (flag-value activated) nil)
    (setf (flag-value deleted) t)))

(declaim (inline cleanup))
(defun cleanup (instance)
  (tg:cancel-finalization instance)
  (%cleanup (handle-descriptor instance) (handle-ptr instance)
            (handle-activated instance) (handle-deleted instance)))

(defvar *ladspa-path*
  (or (cffi:foreign-funcall "getenv" :string "LADSPA_PATH" :string)
      (namestring (make-pathname :directory '(:absolute "usr"
                                              #+x86-64 "lib64"
                                              #-x86-64 "lib"
                                              "ladspa")))))

(defvar *ladspa-path-separator* #\:)

(defun %find-plugin-filename (name)
  (labels ((find-path (paths separator start)
             (let ((end (position separator paths :test #'char= :start start)))
               (let ((dir (subseq paths start end)))
                 (when dir
                   (let ((path (merge-pathnames (truename dir) name)))
                     (cond ((probe-file path) path)
                           (end (find-path paths separator (1+ end))))))))))
    (let ((path (find-path *ladspa-path* *ladspa-path-separator* 0)))
      (if path
          (namestring path)
          (error "file ~S not found" name)))))

(defun find-plugin-filename (name)
  (declare (type string name))
  (if (pathname-directory name)
      ;; NAME is a complete path
      (if (probe-file name)
          name
          (error "file ~S not found" name))
      (let ((name (merge-pathnames name "?.so")))
        (if (probe-file name)
            name
            (%find-plugin-filename name)))))

(defun plugin-descriptor-alist (handle)
  (let ((descr-cb (dlsym handle "ladspa_descriptor")))
    (if (cffi:null-pointer-p descr-cb)
        (error "plugin descriptor callback is absent: ~A" (dlerror))
        (loop with acc = nil
              for i from 0
              for descr = (call-descriptor descr-cb i)
              if (cffi:null-pointer-p descr)
                return (nreverse acc)
              else do
                (push (cons (descriptor-slot-value descr 'label) descr) acc)))))

(defvar *plugin-libraries* (make-hash-table :test #'equal))

(defmacro plugin-libraries-cache (path)
  `(gethash ,path *plugin-libraries*))

(declaim (inline stored-handle))
(defun stored-handle (path)
  (car (plugin-libraries-cache path)))

(declaim (inline stored-descriptions))
(defun stored-descriptions (path)
  (cdr (plugin-libraries-cache path)))

(declaim (inline free-plugin-libraries-cache))
(defun free-plugin-libraries-cache (path)
  (remhash path *plugin-libraries*))

(defun load-plugin-library (filename)
  (let* ((path (find-plugin-filename filename))
         (descr-alist (stored-descriptions path)))
    (or descr-alist
        (let ((handle (dlopen path *dlopen-default-flags*)))
          (if (cffi:null-pointer-p handle)
              (error "loading plugin ~S failed: ~A" filename (dlerror))
              (let ((descr-alist (plugin-descriptor-alist handle)))
                (setf (plugin-libraries-cache path) (cons handle descr-alist))
                descr-alist))))))

(defun unload-plugin-library (filename)
  (let* ((path (find-plugin-filename filename))
         (handle (stored-handle path)))
    (when handle
      (if (zerop (dlclose handle))
          (free-plugin-libraries-cache path)
          (error "unloading plugin ~S failed: ~A" filename
                 (cffi:foreign-funcall "dlerror" :string))))))

(defun unload-all-plugins ()
  (alexandria:maphash-keys #'unload-plugin-library *plugin-libraries*)
  (or (zerop (hash-table-count *plugin-libraries*))
      (warn "UNLOAD-ALL-PLUGINS failed")))

(declaim (inline plugin-descriptor))
(defun plugin-descriptor (filename label)
  (cdr (assoc label (load-plugin-library filename) :test #'string=)))
