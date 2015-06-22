;;; Copyright (c) 2015 Tito Latini
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

(in-package :fluidsynth)

(defun setting (settings name)
  (unless (deleted-p settings)
    (let ((type (settings-get-type settings name)))
      (if (= type NO-TYPE)
          (warn "unknown setting ~A" name)
          (multiple-value-bind (get-cb foreign-type)
              (case type
                (#.NUM-TYPE (values #'settings-getnum :double))
                (#.INT-TYPE (values #'settings-getint :int))
                (#.STR-TYPE (values #'settings-getstr :string))
                (otherwise (error "unknown setting type")))
            (cffi:with-foreign-object (ptr :double)
              (funcall get-cb settings name ptr)
              (cffi:mem-ref ptr foreign-type)))))))

(defun set-setting (settings name value)
  (unless (deleted-p settings)
    (let ((type (settings-get-type settings name)))
      (if (= type NO-TYPE)
          (warn "unknown setting ~A" name)
          (multiple-value-bind (set-cb value)
              (cond ((and (numberp value) (= type NUM-TYPE))
                     (values #'settings-setnum (coerce value 'double-float)))
                    ((and (numberp value) (= type INT-TYPE))
                     (values #'settings-setint (if (integerp value)
                                                   value
                                                   (floor (realpart value)))))
                    ((and (stringp value) (= type STR-TYPE))
                     (values #'settings-setstr value))
                    (t (error "unknown setting type")))
            (and (plusp (funcall set-cb settings name value))
                 value))))))

(defsetf setting set-setting)

(defun new-settings (&optional setting-list)
  (let ((obj (%new-settings)))
    (loop for (name value) in setting-list
          do (setf (setting obj name) value))
    obj))
