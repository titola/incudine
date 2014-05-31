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

(define-constant +int-hash-table-size+ 1024
  :documentation "Default size for a new INT-HASH-TABLE object.")

(defstruct (int-hash-table (:constructor %make-int-hash-table)
                           (:copier nil))
  (count 0 :type non-negative-fixnum)
  (size 0 :type non-negative-fixnum)
  (mask 0 :type non-negative-fixnum)
  (initial-element-fn nil)
  (items nil :type (or simple-vector null)))

(defun make-items (size initial-element-fn)
  (if initial-element-fn
      (map-into (make-array size) initial-element-fn)
      (make-array size :initial-element nil)))

(defmethod print-object ((obj int-hash-table) stream)
  (format stream "#<~S :COUNT ~D>" (type-of obj) (int-hash-table-count obj)))

(defun make-int-hash-table (&key (size +int-hash-table-size+)
                            initial-element-fn)
  (let* ((size (next-power-of-two size))
         (double-size (* size 2)))
    (%make-int-hash-table :size size
                          :mask (1- size)
                          :initial-element-fn initial-element-fn
                          :items (make-items double-size initial-element-fn))))

(defmacro index-for (in-hash-id in-key items items-type int-hash-table
                     get-hash-fn get-key-fn null-item-p-fn)
  (with-gensyms (index mask item %items hash-id)
    `(let ((,hash-id ,in-hash-id)
           (,%items ,items)
           (,mask (int-hash-table-mask ,int-hash-table)))
       (do* ((,index (logand ,hash-id ,mask) (logand (1+ ,index) ,mask))
             (,item (svref ,%items ,index) (svref ,%items ,index)))
            ((or (,null-item-p-fn ,item)
                 (and (= ,hash-id (the fixnum (,get-hash-fn ,item)))
                      (= ,in-key (the fixnum (,get-key-fn ,item)))))
             ,index)
         (declare (type non-negative-fixnum ,mask ,index))
         ,@(when items-type `((declare (type ,items-type ,item))))))))

(defmacro getihash (key int-hash-table get-hash-fn get-key-fn null-item-p-fn
                    items-type)
  `(svref (int-hash-table-items ,int-hash-table)
          (index-for (the fixnum (int-hash ,key)) ,key
                     (int-hash-table-items ,int-hash-table) ,items-type
                     ,int-hash-table ,get-hash-fn ,get-key-fn ,null-item-p-fn)))

(defmacro fix-collisions-from (key int-hash-table get-hash-fn get-key-fn
                               null-item-p-fn items-type)
  (with-gensyms (mask items old-item old-key new-item new-key curr-item)
    `(let ((,mask (int-hash-table-mask ,int-hash-table))
           (,items (int-hash-table-items ,int-hash-table)))
       (declare (type positive-fixnum ,mask))
       (symbol-macrolet ((,old-item (svref ,items ,old-key))
                         (,new-item (svref ,items ,new-key)))
         (do* ((,old-key (logand (1+ ,key) ,mask)
                         (logand (1+ ,old-key) ,mask))
               (,curr-item ,old-item ,old-item))
              ((,null-item-p-fn ,curr-item))
           (declare (type fixnum ,old-key))
           ,@(when items-type `((declare (type ,items-type ,curr-item))))
           (let ((,new-key (index-for (the fixnum (,get-hash-fn ,old-item))
                                      (the fixnum (,get-key-fn ,old-item))
                                      ,items ,items-type ,int-hash-table
                                      ,get-hash-fn ,get-key-fn
                                      ,null-item-p-fn)))
             (unless (= ,old-key ,new-key)
               (setf ,old-item ,new-item ,new-item ,curr-item))))))))
