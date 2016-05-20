;;; Common Lisp interface to interact with the sound editor Snd.
;;;
;;; Copyright (c) 2015 Tito Latini
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

(in-package :snd)

(defun open-process-stream-p ()
  (open-stream-p (process-stream)))

(defun flush-stream ()
  (when *snd*
    (loop while (read-char-no-hang (process-stream) nil t))))

(defun close-stream ()
  (when (and *snd* (open-process-stream-p))
    (close (process-stream))))

(defun |#t-reader| (stream subchar arg)
  (declare (ignore stream subchar arg))
  t)

(defun |#f-reader| (stream subchar arg)
  (declare (ignore stream subchar arg))
  nil)

;;; Used with the Snd output to transform #<...> in (...)
(defun |#<-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((acc (list #\()))
    (do ((c (read-char stream) (read-char stream)))
        ((char= c #\>))
      (push c acc))
    (push #\) acc)
    (read-from-string (coerce (nreverse acc) 'string))))

(defvar *snd-readtable*
  (let ((rtab (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\t #'|#t-reader| rtab)
    (set-dispatch-macro-character #\# #\f #'|#f-reader| rtab)
    (set-dispatch-macro-character #\# #\< #'|#<-reader| rtab)
    rtab)
  "Readtable for Snd output.")

(define-condition snd-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (let ((str (text condition)))
               (format stream "SND:EVAL ~A"
                       (subseq str 0 (position #\Return str)))))))

(defun error-p (str)
  (let* ((s0 (string-right-trim " \t\n\r" str))
         (s1 "#<unspecified>")
         (d (- (length s0) (length s1))))
    (and (plusp d) (string= s0 s1 :start1 d))))

(defun default-parser (str)
  (declare (type string str))
  (let ((*readtable* *snd-readtable*))
    (flet ((first-char= (character)
             (char= (char str 0) character))
           (no-more-words-after-p (character)
             (let ((pos (position character str)))
               (or (null pos)
                   (string= (string-trim '(#\Space) (subseq str (1+ pos)))
                            "")))))
      (cond ((string= str "#t") t)
            ((member str '("" "#f") :test #'string=) nil)
            ((and (eq (string< "#<" str) 2) (no-more-words-after-p #\>))
             (values (read-from-string str nil)))
            ((first-char= #\#) str)
            ((or (first-char= #\() (no-more-words-after-p #\Space))
             (values (read-from-string str nil)))
            ((error-p str)
             (format *error-output* "Snd error:~%  ~A" (remove #\Return str))
             (error 'snd-error :text str))))))

(defun to-process (string)
  (flush-stream)
  (write-line string (process-stream))
  (force-output (process-stream)))

(defun from-process (&optional (parser #'default-parser))
  (with-interrupts
    (loop for c = (read-char (process-stream) nil nil t)
          with acc = nil
          until (and (char= c #\Newline)
                     (push c acc)
                     (null (setf c (read-char-no-hang
                                     (process-stream) nil nil t))))
          do (push c acc)
             (when (char= c #\Newline)
               ;; Probably never.
               (if (setf c (read-char-no-hang (process-stream) nil nil t))
                   (push c acc)
                   (return)))
          finally (return (funcall parser
                                   ;; Output string ends with "\r\n"
                                   (coerce (nreverse (cddr acc)) 'string))))))

(defun run (&optional (program-name *program-name*) (args *program-args*))
  "Start Snd."
  (cond ((process-alive-p) *snd*)
        (t (close-stream)
           (setf *snd* (run-program program-name args :pty :stream
                                    :wait nil)))))

(defun exit ()
  "Terminate Snd."
  (when *snd*
    (when (open-process-stream-p)
      (to-process "(exit 0)"))
    (dotimes (i 20)
      (if (process-exited-p) (return) (sleep .1)))
    (close-stream)
    (let ((code (process-exit-code)))
      (and code (zerop code)))))

(defun eval (string &key (output-p t) (parser #'default-parser))
  "Evaluates STRING in Snd."
  (declare (type string string) (type boolean output-p) (type function parser))
  (when *snd*
    (to-process string)
    (if output-p (from-process parser) (flush-stream))))

(defun truenamestring (pathspec)
  (namestring (truename pathspec)))

(defun load (scmfile &optional env)
  "Loads the scheme FILE in Snd."
  (eval (format nil "(load ~S~@[ ~(~A~)~])" (truenamestring scmfile) env)))
