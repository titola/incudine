;;; Copyright (c) 2013 Tito Latini
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(incudine.analysis:abuffer
     incudine.analysis:abuffer-realpart
     incudine.analysis:abuffer-imagpart
     incudine.analysis:abuffer-size
     incudine.analysis:abuffer-nbins
     incudine.analysis:compute
     incudine.analysis:dofft
     incudine.analysis:dofft-polar
     incudine.analysis:dofft-complex
     incudine.analysis:to-polar
     incudine.analysis:to-complex))

  (object-to-free %make-local-fft update-local-fft)
  (object-to-free %make-local-ifft update-local-ifft))

(declaim (inline %make-local-fft))
(defun %make-local-fft (size window-size window-function)
  (declare (type non-negative-fixnum size window-size)
           (type function window-function))
  (incudine.analysis:make-fft size
                              :window-size window-size
                              :window-function window-function
                              :real-time-p t))

(defmacro make-local-fft (size &optional (window-size size) window-function)
  (with-gensyms (fft)
    `(with ((,fft (%make-local-fft ,size ,window-size
                                   ,(or window-function
                                        '(gen:sine-window)))))
       ,fft)))

(defmacro update-local-fft (vug-varname args)
  (with-gensyms (size window-size)
    `(let ((,size ,(first args))
           (,window-size ,(second args)))
       (declare (type non-negative-fixnum ,size ,window-size))
       (cond ((and (= ,size (slot-value ,vug-varname 'incudine.analysis::size))
                   (= ,window-size (slot-value ,vug-varname 'incudine.analysis::window-size)))
              (setf (incudine.analysis:analysis-time ,vug-varname) (coerce -1.0 'sample))
              (foreign-zero-sample
               (slot-value ,vug-varname 'incudine.analysis::input-buffer) ,size)
              (foreign-zero-sample (incudine.analysis::ring-buffer-data
                                    (slot-value ,vug-varname 'incudine.analysis::ring-buffer))
                                   ,window-size))
             (t (incudine:free ,vug-varname)
                (setf ,vug-varname (%make-local-fft ,@args)))))))

(declaim (inline %make-local-ifft))
(defun %make-local-ifft (size window-size window-function)
  (declare (type non-negative-fixnum size window-size)
           (type function window-function))
  (incudine.analysis:make-ifft size
                               :window-size window-size
                               :window-function window-function
                               :real-time-p t))

(defmacro make-local-ifft (size &optional (window-size size) window-function)
  (with-gensyms (ifft)
    `(with ((,ifft (%make-local-ifft ,size ,window-size
                                     ,(or window-function
                                          '(gen:sine-window)))))
       ,ifft)))

(defmacro update-local-ifft (vug-varname args)
  (with-gensyms (size window-size)
    `(let ((,size ,(first args))
           (,window-size ,(second args)))
       (declare (type non-negative-fixnum ,size ,window-size))
       (cond ((and (= ,size (slot-value ,vug-varname 'incudine.analysis::size))
                   (= ,window-size (slot-value ,vug-varname 'incudine.analysis::window-size)))
              (setf (incudine.analysis:analysis-time ,vug-varname) (coerce -1.0 'sample))
              (foreign-zero-sample
               (slot-value ,vug-varname 'incudine.analysis::output-buffer) ,size)
              (foreign-zero-sample (incudine.analysis::ring-buffer-data
                                    (slot-value ,vug-varname 'incudine.analysis::ring-buffer))
                                   ,window-size))
             (t (incudine:free ,vug-varname)
                (setf ,vug-varname (%make-local-ifft ,@args)))))))
