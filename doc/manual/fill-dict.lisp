(require "sb-introspect")
(require "incudine")

(defpackage :incudine.doc
  (:use :cl)
  (:import-from #:sb-ext #:defined-type-name-p)
  (:import-from #:sb-introspect #:function-lambda-list)
  (:export #:write-incudine-version #:write-doc #:write-undocumented-symbols))

(in-package :incudine.doc)

(defvar *incudine-packages*
  '("INCUDINE"
    "INCUDINE.ANALYSIS"
    "INCUDINE.CONFIG"
    "INCUDINE.EDF"
    "INCUDINE.EXTERNAL"
    "INCUDINE.GEN"
    "INCUDINE.MIDIFILE"
    "INCUDINE.NET"
    "INCUDINE.OSC"
    "INCUDINE.SOUNDFILE"
    "INCUDINE.UTIL"
    "INCUDINE.VOICER"
    "INCUDINE.VUG"
    "INCUDINE.VUG-FOREIGN"))

(defvar *forced-symbol-packages*
   '((function
      (incudine:envelope . "INCUDINE.VUG"))
     (type
      (incudine:envelope . "INCUDINE"))))

(defun check-symbol-package (sym pkg type)
  (let ((forced (cdr (assoc sym (cdr (assoc type *forced-symbol-packages*))))))
    (or (null forced)
        (eq (find-package forced) pkg))))

(defun source-file (relative-path)
  (merge-pathnames relative-path
                   (asdf:system-source-directory "incudine")))

(defvar *dictionary-dependence*
  `(("cache/cudere-clm.dict"
     :system "cudere-clm"
     :source-files ,(list (source-file "contrib/cudere-clm/config.lisp")
                          (source-file "contrib/cudere-clm/cudere-clm.lisp")
                          (source-file "contrib/cudere-clm/fft.lisp")
                          (source-file "contrib/cudere-clm/score.lisp")
                          (source-file "contrib/cudere-clm/util.lisp")))
    ("cache/fluidsynth.dict"
     :system "incudine-fluidsynth"
     :source-files ,(list (source-file "contrib/cl-fluidsynth/cffi-fluidsynth.lisp")
                          (source-file "contrib/cl-fluidsynth/fluidsynth.lisp")
                          (source-file "contrib/cl-fluidsynth/tuning.lisp")))
    ("cache/jack.dict"
     :required-features (:jack-audio :jack-midi)
     :source-files ,(list (source-file "src/jack.lisp")
                          (source-file "src/jackmidi.lisp")))
    ("cache/ladspa.dict"
     :system "incudine-ladspa"
     :source-files ,(list (source-file "contrib/cl-ladspa/cffi-ladspa.lisp")
                          (source-file "contrib/cl-ladspa/ladspa.lisp")
                          (source-file "src/vug/ladspa.lisp")))
    ("cache/lv2.dict"
     :system "incudine-lv2"
     :source-files ,(list (source-file "contrib/cl-lilv/cffi-lilv.lisp")
                          (source-file "contrib/cl-lilv/cffi-lv2.lisp")
                          (source-file "contrib/cl-lilv/error.lisp")
                          (source-file "contrib/cl-lilv/lilv.lisp")
                          (source-file "contrib/cl-lilv/lv2.lisp")
                          (source-file "contrib/cl-lilv/map.lisp")
                          (source-file "src/vug/lv2.lisp")))
    ("cache/portaudio.dict"
     :required-features (:portaudio)
     :source-files ,(list (source-file "src/portaudio.lisp")))
    ("cache/portmidi.dict"
     :source-files ,(list (source-file "contrib/cl-portmidi/cffi-portmidi.lisp")
                          (source-file "contrib/cl-portmidi/error.lisp")
                          (source-file "contrib/cl-portmidi/portmidi.lisp")))
    ("cache/snd.dict"
     :system "incudine-snd"
     :source-files ,(list (source-file "contrib/cl-snd/sbcl.lisp")
                          (source-file "contrib/cl-snd/snd.lisp")
                          (source-file "contrib/cl-snd/snd-incudine.lisp")))))

(defvar *incudine-symbols-to-ignore*
  '(;; Moved in dictionary cache/portaudio.dict
    incudine::portaudio-device-info
    incudine::portaudio-input-latency
    incudine::portaudio-output-latency
    incudine::portaudio-set-device))

;;; Replace `&sym' with `@andsym'.
(defun format-lambda-list-keywords (args)
  (mapcar
   (lambda (x)
     (if (consp x)
         (format-lambda-list-keywords x)
         (let ((name (string x)))
           (if (char= (char name 0) #\&)
               (intern (coerce (append (list #\@ #\A #\N #\D)
                                       (remove #\- (cdr (coerce name 'list)))
                                       (list #\{ #\}))
                               'string))
               x))))
   args))

(defun format-texinfo-block (name args docstring stream
                             &optional (block-name "defun") block-type)
  (format stream "#+attr_texinfo: :options ~@[{~A} ~]~(~A~{ ~A~}~)~%~
                  #+begin_~A~%~@[~A~]~&#+end_~A~2%"
          block-type name (format-lambda-list-keywords args)
          block-name docstring block-name))

(defun format-texinfo (name args type docstring stream)
  (case type
    (function (format-texinfo-block name args docstring stream))
    ((standard-generic-function macro ugen vug vug-macro)
     (format-texinfo-block
       name args docstring stream "deffn"
       (cdr (assoc type
                   '((macro . "Macro")
                     (standard-generic-function . "Generic Function")
                     (ugen . "UGen")
                     (vug . "VUG")
                     (vug-macro . "VUG Macro"))))))
    ((structure-class standard-class)
     (format-texinfo-block name nil docstring stream "deftp"
                           (if (eq type 'standard-class)
                               "Class"
                               "Structure")))
    ((condition type variable constant)
     (format-texinfo-block name nil docstring stream
                           (if (member type '(constant variable))
                               "defvr"
                               "deftp")
                           (format nil "~@(~A~)" type)))))

(defun deprecated-p (symbol)
  (and (assoc symbol incudine::*deprecated*) t))

(defun ignore-incudine-symbol-p (sym)
  (and (member sym *incudine-symbols-to-ignore*) t))

(defun all-incudine-symbols ()
  (let ((acc nil))
    (dolist (pkg *incudine-packages* (nreverse acc))
      (do-external-symbols (sym pkg)
        (let ((pkg-name (package-name (symbol-package sym))))
          (when (and (member pkg-name *incudine-packages* :test #'string=)
                     (string/= pkg-name "INCUDINE.CONFIG")
                     (not (ignore-incudine-symbol-p sym))
                     (not (deprecated-p sym)))
            (pushnew sym acc)))))))

(defun write-incudine-version (version-file)
  (unless (probe-file version-file)
    (with-open-file (f version-file :direction :output)
      (format f "#+macro: version ~A~%"
              (incudine.util:incudine-version)))))

(defun very-first (x)
  (if (consp x) (very-first (first x)) x))

(defun function-arguments (lambda-list)
  (labels ((rec (lst args skip)
             (if lst
                 (let ((next (car lst)))
                   (rec (cdr lst)
                        (cons (if (consp next)
                                  (if skip
                                      (very-first next)
                                      (nreverse (rec next nil t)))
                                  next)
                              args)
                        (or skip
                            (and (atom next)
                                 (char= (char (symbol-name next) 0) #\&)))))
                 args)))
    (nreverse (rec lambda-list nil nil))))

(defun external-symbol-p (name package)
  (multiple-value-bind (n w) (find-symbol name package)
    (declare (ignore n))
    (eq w :external)))

(defun lambda-star-arguments (lambda-list)
  (let ((args (incudine.util:lambda-list-to-star-list lambda-list)))
    (and args (cons '&optional-key (cdr args)))))

(defun whitespace-p (c)
  (member c '(#\Space #\Tab)))

(defun parenthesis-p (c)
  (member c '(#\( #\) #\[ #\] #\{ #\})))

(defvar *special-char-lists*
  (mapcar (lambda (str) (nreverse (coerce str 'list)))
          '("C" "DC" "DSP" "FFT" "IFFT" "STFT" "LADSPA" "LV2" "MIDI" "NTP" "OSC"
            "UGEN" "URID" "VUG")))

(defun tildelize-p (char-list)
  (null (or (let ((pos (position-if #'alpha-char-p char-list)))
              (when pos
                (member (if (or (= pos 0) (every #'alphanumericp char-list))
                            char-list
                            (subseq char-list pos))
                        *special-char-lists* :test #'equal)))
            (find #\_ char-list)
            (char= (first (last char-list)) #\"))))

(defun tildelize (char-list last-char)
  (if (and (tildelize-p char-list)
           (some #'alpha-char-p char-list)
           ;; Ignore 'A' after a dot (i.e. "blabla. A blabla")
           (not (and (char= last-char #\.)
                     (equal char-list '(#\A)))))
      (flet ((skip-at-start-p (c)
               (member c '(#\. #\, #\: #\; #\')))
             (skip-at-end-p (c)
               (member c '(#\. #\, #\; #\'))))
        (let ((start (position-if-not #'skip-at-start-p char-list))
              (end (1+ (position-if-not #'skip-at-end-p char-list :from-end t))))
          (nconc (subseq char-list 0 start)
                 (cons #\~ (mapcar #'char-downcase
                                   (subseq char-list start end)))
                 (list #\~)
                 (subseq char-list end))))
      char-list))

;;; Convert VALUE in ~value~ for org-mode.
(defun filter-docstring-line (line)
  (let ((out nil)
        (tmp nil)
        (wait t)
        (last-dotting #\.))
    (labels ((add-pending (transform-p)
                 (setf out (nconc (if transform-p
                                      (tildelize tmp last-dotting)
                                      tmp)
                                  out))
                 (setf tmp nil))
             (add-char (c)
               (cond ((lower-case-p c)
                      (when wait
                        (setf wait nil)
                        (if tmp (add-pending nil)))
                      (push c out))
                     (wait
                      (push c tmp))
                     ((and out (whitespace-p (car out)))
                      (push c tmp)
                      (unless wait (setf wait t)))
                     (t
                      (push c out)))
               (unless (alpha-char-p c)
                 (setf last-dotting c)))
             (add-whitespace-or-parens (c)
               (when wait
                 (setf wait nil)
                 (if tmp (add-pending t)))
               (push c out)))
      (map nil (lambda (c)
                 (if (or (whitespace-p c) (parenthesis-p c))
                     (add-whitespace-or-parens c)
                     (add-char c)))
           line)
      (if tmp (add-pending t))
      (coerce (nreverse out) 'string))))

(defun substringp (string0 string1)
  (let ((test (string< string0 string1)))
    (and test (= test (length string0)))))

(defun doc-example-p (str)
  (substringp "    " str))

(defun empty-line-p (str)
  (every #'whitespace-p str))

(defun write-pending-empty-lines (n example-p stream)
  (when (plusp n)
    (dotimes (i n)
      (if example-p (write-line ":" stream) (terpri stream))))
  stream)

(defun write-docstring-line (line example-p empty-lines stream)
  (write-line (if example-p
                  (concatenate 'string ": " (subseq line 4))
                  (filter-docstring-line line))
              (write-pending-empty-lines empty-lines example-p stream)))

(defun filter-docstring (str)
  (with-output-to-string (out)
    (with-input-from-string (in str)
      (loop for l = (read-line in nil nil)
            with pending-example-p = nil
            with empty-lines = 0
            while l do
              (if (and pending-example-p (empty-line-p l))
                  (incf empty-lines)
                  (let ((example-p (doc-example-p l)))
                    (write-docstring-line l example-p empty-lines out)
                    (setf empty-lines 0)
                    (when (or (and example-p (not pending-example-p))
                              (and (not example-p) pending-example-p))
                      (setf pending-example-p (not pending-example-p)))))))))

(defun maybe-package-nickname (pkg)
  (let ((names (package-nicknames pkg)))
    (if names
        (car names)
        (package-name pkg))))

(defun write-incudine-doc (symbol package format-fun-function stream)
  (let* ((name (symbol-name symbol))
         (long-name (if (eq package (find-package "INCUDINE"))
                        name
                        (format nil "~A:~A"
                                (maybe-package-nickname package) name)))
         (*print-pretty* nil))
    (when (boundp symbol)
      (let ((doc (documentation symbol 'variable)))
        (funcall format-fun-function long-name nil
                 (if (constantp symbol) 'constant 'variable)
                 (and doc (filter-docstring doc))
                 stream)))
    (when (and (defined-type-name-p symbol)
               (check-symbol-package symbol package 'type))
      (let* ((doc (documentation symbol 'type))
             (class (find-class symbol nil))
             (type (if class
                       (if (subtypep class 'condition)
                           'condition
                           (type-of class))
                       'type)))
        (funcall format-fun-function long-name nil type
                 (and doc (filter-docstring doc))
                 stream)))
    (when (and (fboundp symbol)
               (check-symbol-package symbol package 'function))
      (let ((doc (documentation symbol 'function))
            (lambda-list (function-lambda-list symbol)))
        (funcall format-fun-function long-name
                 (function-arguments (or (lambda-star-arguments lambda-list)
                                         lambda-list))
                 (cond ((and (eq package (find-package "INCUDINE.VUG"))
                             (incudine.vug:vug symbol))
                        (if (incudine.vug:vug-macro-p (incudine.vug:vug symbol))
                            'vug-macro
                            'vug))
                       ((and (eq package (find-package "CUDERE-CLM.UGENS")))
                        'ugen)
                       ((macro-function symbol) 'macro)
                       (t (type-of (symbol-function symbol))))
                 (and doc (filter-docstring doc))
                 stream)))))

(defun write-texinfo-doc (symbol package stream)
  (write-incudine-doc symbol package #'format-texinfo stream))

(defun write-unlinked-symbols (outfile list)
  (when (and outfile list)
    (let ((*package* (find-package "INCUDINE.DOC")))
      (with-open-file (f outfile :direction :output :if-exists :supersede)
        (format f "(require :incudine)~%(in-package :incudine)~2%~
                   (defparameter *symbols-out-of-dictionary*~%  ~
                     '(~(~{~S~^~%    ~}~)))~%"
                list)
        outfile))))

(defun filter-doc-function (type)
  (or (cdr (assoc type (list (cons "texinfo" #'write-texinfo-doc))
                  :test #'string-equal))
      (error "Unknown doc function for type ~A" type)))

(defun org-headline-p (line)
  (and (> (length line) 1)
       (char= (char line 0) #\*)
       (every (lambda (c) (char= c #\*))
              (subseq line 0 (position #\Space line)))))

(defun incudine-symbol (line)
  (let* ((*package* (find-package "INCUDINE"))
         (colon-pos (position #\: line))
         (sym (read-from-string line)))
    (values sym (if colon-pos
                    (find-package (string-upcase (subseq line 0 colon-pos)))
                    *package*))))

(defun filter-doc (infile outfile &key (type "texinfo") (if-exists :append)
                   (out-of-dictionary-file "out-of-dict.lisp"))
  (with-open-file (in infile)
    (with-open-file (out outfile :direction :output :if-exists if-exists
                     :if-does-not-exist :create)
      (let ((doc-function (filter-doc-function type))
            (symbol-list nil))
        (loop for l = (read-line in nil nil)
              while l do
                (if (substringp ":::" l)
                    (multiple-value-bind (sym pkg)
                        (incudine-symbol (subseq l 4))
                      (when (external-symbol-p (symbol-name sym) pkg)
                        (cond ((deprecated-p sym)
                               (warn "~A is deprecated." sym))
                              (t
                               (pushnew sym symbol-list)
                               (funcall doc-function sym pkg out)))))
                    (write-line l out))
              finally (write-unlinked-symbols
                        out-of-dictionary-file
                        (set-difference (all-incudine-symbols) symbol-list))))
      outfile)))

(defun has-features-p (lst)
  (= (length lst) (length (intersection lst *features*))))

(defun use-cached-dictionary-p (dict)
  (let ((prop (cdr (assoc dict *dictionary-dependence* :test #'string=))))
    (when prop
      (let* ((cached (make-pathname :defaults dict :type "org"))
             (cache-exists-p (probe-file cached)))
        (or (and cache-exists-p
                 (< (reduce #'max
                            (mapcar #'file-write-date (getf prop :source-files)))
                    (file-write-date dict)
                    (file-write-date cached)))
            (flet ((cached-file-not-found ()
                     (error "cached file ~S not found." cached)))
              (let ((system (getf prop :system))
                    (required (getf prop :required-features)))
                (cond (system
                       (handler-case
                           (progn
                             (asdf:load-system system :verbose nil)
                             (not (has-features-p required)))
                         (error (c)
                           (declare (ignore c))
                           (cond (cache-exists-p
                                  (format t "~&[IGNORABLE COMPILER ERROR] ~
                                             Using cached dictionary for ~A.~%"
                                          system)
                                  t)
                                 (t (cached-file-not-found))))))
                      ((has-features-p required) nil)
                      (cache-exists-p t)
                      (t (cached-file-not-found))))))))))

(defun write-doc (infile outfile &key (type "texinfo") (if-exists :append)
                  out-of-dictionary-file)
  (unless (use-cached-dictionary-p infile)
    (filter-doc infile outfile :type type :if-exists if-exists
                :out-of-dictionary-file out-of-dictionary-file)))

(defun write-undocumented-symbols (orgfile outfile)
  (with-open-file (in orgfile)
    (with-open-file (out outfile :direction :output :if-exists :supersede)
      (loop for line = (read-line in nil nil)
            with acc = nil
            with count = 0
            while line do
              (cond ((org-headline-p line)
                     (write-line line out))
                    ((substringp "#+attr_texinfo:" line)
                     (push line acc)
                     (setf line (read-line in nil nil))
                     (when (and line (substringp "#+begin_" line))
                       (push line acc)
                       (setf line (read-line in nil nil))
                       (when (and line (substringp "#+end_" line))
                         (incf count)
                         (dolist (str (nreverse (cons line acc)))
                           (write-line str out))))
                     (setf acc nil)))
           finally (format t "[INFO] Written ~D undocumented functions in ~S.~%"
                           count outfile)))))
