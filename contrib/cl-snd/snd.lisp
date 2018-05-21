;;; Common Lisp interface to interact with the sound editor Snd.
;;;
;;; Copyright (c) 2015-2018 Tito Latini
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
  "Flush anything waiting on the Snd stream."
  (when *snd*
    (loop while (read-char-no-hang (process-stream) nil t))))

(defun close-stream ()
  "Close the Snd stream."
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

(defvar *emacs-mode-p* nil)
(declaim (type boolean *emacs-mode-p*))

(defun emacs-mode-p ()
  "Return T to enable the interaction through the Emacs' Snd-Scheme mode.
Setfable.

It requires SLIME-ENABLE-EVALUATE-IN-EMACS T on the Emacs side."
  *emacs-mode-p*)

(defun eval-in-emacs (form)
  (funcall (find-symbol "EVAL-IN-EMACS" "SWANK") form))

(defun emacs-mode-utils ()
  (eval-in-emacs
    '(unless (fboundp 'incudine-snd-send-string)
       (defun incudine-snd-send-string (string)
         (let* ((str "")
                (comint-output-filter-functions
                  (list (lambda (text) (setq str (concat str text)))))
                (buf (inf-snd-proc-buffer)))
           (with-current-buffer buf
             (snd-send-invisible string)
             (accept-process-output (get-buffer-process buf)))
           str)))))

(defun set-emacs-mode-p (enable-p)
  (declare (type boolean enable-p))
  (when enable-p
    (emacs-mode-utils))
  (setf *emacs-mode-p* enable-p))

(defsetf emacs-mode-p set-emacs-mode-p)

(defun run (&optional (program-name *program-name*) (args *program-args*))
  "Start Snd."
  (cond (*emacs-mode-p*
         (emacs-mode-utils)
         (eval-in-emacs
          `(run-snd-scheme ,(format nil "~{~A~^ ~}" (cons program-name args)))))
        ((process-alive-p)
         *snd*)
        (t
         (close-stream)
         (setf *snd* (run-program program-name args :pty :stream
                                  :wait nil)))))

(defun exit ()
  "Terminate the Snd process started by RUN."
  (cond (*emacs-mode-p*
         (eval "(exit 0)"))
        (*snd*
         (when (open-process-stream-p)
           (to-process "(exit 0)"))
         (dotimes (i 20)
           (if (process-exited-p) (return) (sleep .1)))
         (close-stream)
         (let ((code (process-exit-code)))
           (and code (zerop code))))))

(defun eval (string &key (output-p t) (parser #'default-parser))
  "The STRING is evaluated by Snd."
  (declare (type string string) (type boolean output-p) (type function parser))
  (cond (*emacs-mode-p*
         (let ((str (eval-in-emacs `(incudine-snd-send-string ,string))))
           (when output-p
             (funcall parser
                      (subseq str 0 (position #\newline str :from-end t))))))
        (*snd*
         (to-process string)
         (if output-p (from-process parser) (flush-stream)))))

(defun truenamestring (pathspec)
  (namestring (truename pathspec)))

(defun load (scmfile &optional s7-env-string)
  "Load the scheme FILE in Snd.

The optional string S7-ENV-STRING is the s7 environment."
  (eval (format nil "(load ~S~@[ ~(~A~)~])" (truenamestring scmfile)
                s7-env-string)))

(defvar *sharp-s-function* (get-dispatch-macro-character #\# #\s))

(defun simple-sharp-char (stream subchar arg)
  (declare (ignore stream arg))
  (make-symbol (format nil "#~A" (char-downcase subchar))))

(declaim (special *s7-readtable*))

(defun |#s7-reader| (stream subchar arg)
  (cond ((or (char= (peek-char nil stream) #\7)
             (and arg (= arg 7)))
         (unless arg (read-char stream))
         (assert (char= (read-char stream) #\())
         (labels ((rec (x)
                    (cond ((consp x)
                           (cons (if (null (car x))
                                     (make-symbol "()")
                                     (rec (car x)))
                                 (rec (cdr x))))
                          ((stringp x)
                           (format nil "~S" x))
                          ((keywordp x)
                           (make-symbol (format nil "~S" x)))
                          (t x))))
           (let ((*readtable* *s7-readtable*))
             (format nil "~A"
               (rec (read-delimited-list #\) stream t))))))
        ((char= (peek-char nil stream) #\n)
         (read-char stream)
         (assert (and (char= (read-char stream) #\d)
                      (char= (peek-char nil stream) #\()))
         (list 'snd:eval (funcall #'|#s7-reader| stream subchar 7)
               :output-p (not (and arg (= arg 0)))))
        (t
         (funcall *sharp-s-function* stream subchar arg))))

(defun set-sharp-s7-syntax ()
  (set-dispatch-macro-character #\# #\s #'|#s7-reader|)
  (setf *s7-readtable* (copy-readtable))
  (setf (readtable-case *s7-readtable*) :preserve)
  (loop for c across "tf" do
          (set-dispatch-macro-character #\# c #'simple-sharp-char
                                        *s7-readtable*))
  t)

(defun add-sharp-s7-syntax ()
  (setf *readtable* (copy-readtable))
  (set-sharp-s7-syntax))

(defmacro enable-sharp-s7-syntax ()
 "Enable the reader syntax used to format and eval s7 forms.

Example:

    #s7(let ((snd (open-sound \"foo.wav\")))
         (play snd :wait #t))

    ;; => \"(let ((snd (open-sound \\\"foo.wav\\\"))) (play snd :wait #t))\"

    #s7(quote #.(LOOP REPEAT 8 COLLECT (RANDOM 1.0)))

    ;; => \"(quote
    ;;      (0.5520501 0.4115485 0.35940528 0.0056368113 0.31019592
    ;;       0.4214077 0.32522345 0.2879219))\"

    (format nil #s7(new-sound \"/tmp/foo.wav\" :channels 1 :size ~D)
            (floor incudine.util:*sample-rate*))

    ;; => \"(new-sound \\\"/tmp/foo.wav\\\" :channels 1 :size 48000)\"

    (snd:eval *)   ; => (SOUND 0)

    #snd(...)   is equivalent to  (snd:eval #s7(...))
    #0snd(...)  is equivalent to  (snd:eval #s7(...) :output-p nil)

    hidden side effect:  #7s is equivalent to #s7

    (defstruct point x y)
    #s(point)
    ;; => #S(POINT :X NIL :Y NIL)"
  `(eval-when (:compile-toplevel :execute)
     (add-sharp-s7-syntax)))
