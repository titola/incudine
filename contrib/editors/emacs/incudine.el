;;; incudine.el --- major mode for editing Incudine sources

;; Copyright (c) 2013-2022 Tito Latini

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;; incudine.el

;; Installation:

;; Add the following lines to your `.emacs' file
;;
;;   (push "/path/to/incudine/contrib/editors/emacs" load-path)
;;   (require 'incudine)

;;; Code:
(require 'slime)

(defgroup incudine nil
  "Major mode for editing incudine code."
  :group 'languages)

(defvar incudine-mode-hook nil
  "Hook called when a buffer enters Incudine mode.")

(defvar incudine-rego-mode-hook nil
  "Hook called when a buffer enters Incudine Rego mode.")

(defcustom incudine-scratch-message nil
  "Initial message displayed in *incudine-scratch* buffer.
If this is nil, no message will be displayed."
  :type '(choice (text :tag "Message")
                 (const :tag "none" nil))
  :group 'incudine)

(defcustom incudine-save-buffer-before-play t
  "Save the modified rego file before playback."
  :type 'boolean
  :group 'incudine)

(defun incudine-buffer-name (string)
  (concat "*incudine-" string "*"))

(defvar incudine-scratch-buffer (incudine-buffer-name "scratch"))

(defvar incudine-rego-files-walk nil
  "Association list where the CAR of each element is the buffer related
to an included rego file and CDR is the buffer of the parent.
Used during meta-dotting from a rego file to an included rego file and
vice versa.")

(defun incudine-scratch ()
  "Switch to Incudine scratch buffer."
  (interactive)
  (let ((buffer (get-buffer incudine-scratch-buffer)))
    (unless buffer
      (setq buffer (get-buffer-create incudine-scratch-buffer))
      (with-current-buffer buffer
        (incudine-mode)
        (slime-mode t)
        (if incudine-scratch-message
            (insert incudine-scratch-message))))
    (switch-to-buffer buffer)))

(defun incudine-eval (string &rest args)
  (slime-eval-with-transcript
   `(swank:interactive-eval ,(if args
                                 (apply #'format string args)
                                 string))))

(defun incudine-eval-defun ()
  "Evaluate the current toplevel form."
  (interactive)
  (slime-eval-defun))

(defun incudine-show-repl ()
  "Show REPL in other window."
  (interactive)
  (switch-to-buffer-other-window (slime-output-buffer)))

(defun incudine-repl-clear-buffer ()
  "Clear the REPL buffer."
  (interactive)
  (save-window-excursion
    (with-current-buffer (slime-repl-buffer)
      (slime-repl-insert-prompt)
      (slime-repl-clear-buffer))))

(defun incudine-prev-defun (&optional n)
  "Jump at the end of the previous defun.

If N is non-NIL, do it that many times."
  (interactive "p")
  (end-of-defun)
  (beginning-of-defun)
  (if n
      (dotimes (i n) (beginning-of-defun))
      (beginning-of-defun))
  (forward-sexp))

(defun incudine-next-defun (&optional n)
  "Jump at the end of the next defun.

If N is non-NIL, do it that many times."
  (interactive "p")
  (end-of-defun)
  (if n
      (dotimes (i n) (end-of-defun))
      (forward-sexp))
  (beginning-of-defun)
  (forward-sexp))

(defun incudine-eval-and-next-fn ()
  "Eval the function and jump to the next defun."
  (interactive)
  (slime-eval-defun)
  (incudine-next-defun))

(defun incudine-eval-and-prev-fn ()
  "Eval the function and jump to the previous defun."
  (interactive)
  (slime-eval-defun)
  (incudine-prev-defun))

(defun incudine-eval-print-last-expression (string &optional save-point-p)
  (interactive (list (slime-last-expression) t))
  (if save-point-p
      ;; A version of `slime-eval-print' with `save-excursion'.
      (slime-eval-async `(swank:eval-and-grab-output ,string)
        (lambda (result)
          (save-excursion
            (cl-destructuring-bind (output value) result
              (insert "\n" output value)))))
      (progn
        (insert "\n")
        (slime-eval-print string))))

(defun prefix-numeric-value0 (n)
  (if n (prefix-numeric-value n) 0))

(defun incudine-rt-start (&optional block-size)
  "Start the real-time thread.

If BLOCK-SIZE is positive, set the new block size before starting."
  (interactive "P")
  (let ((n (prefix-numeric-value0 block-size)))
    (if (> n 0)
        (incudine-eval
          "(progn (incudine:set-rt-block-size %d) (values (incudine:rt-start) (incudine:block-size)))"
          n)
        (incudine-eval "(incudine:rt-start)"))))

(defun incudine-rt-stop ()
  "Stop the real-time thread."
  (interactive)
  (incudine-eval "(incudine:rt-stop)"))

(defun incudine-free-node (&optional id)
  "Stop to play a node of the graph.

If ID is negative, call `incudine:stop' instead of `incudine:free'.

If ID is zero, call `incudine:flush-pending' before `incudine:free'."
  (interactive "P")
  (let ((n (prefix-numeric-value0 id)))
    (incudine-eval (cond ((= n 0) "(progn (incudine:flush-pending) (incudine:free 0))")
                         ((< n 0) "(incudine:stop %d)")
                         (t "(incudine:free %d)"))
                   (abs n))))

(defun incudine-pause-node (&optional id)
  "Pause node.

If ID is non-NIL, pause the node with identifier ID."
  (interactive "P")
  (incudine-eval "(incudine:pause %d)"
                 (prefix-numeric-value0 id)))

(defun incudine-unpause-node (&optional id)
  "Unpause node.

If ID is non-NIL, unpause the node with identifier ID."
  (interactive "P")
  (incudine-eval "(incudine:unpause %d)"
                 (prefix-numeric-value0 id)))

(defun incudine-dump-graph (&optional id)
  "Print informations about the graph of nodes.

If ID is non-NIL, print info about the group with identifier ID."
  (interactive "P")
  (let ((form (format "(incudine:dump (incudine:node %d) *standard-output*)"
                      (prefix-numeric-value0 id))))
    (slime-eval-async `(swank:pprint-eval ,form)
      (lambda (result)
        (slime-with-popup-buffer ("*incudine-node-tree*")
          (insert result)
          (kill-line -1))))))

(defun incudine-live-nodes ()
  "Print the number of the live nodes."
  (interactive)
  (incudine-eval "(incudine:live-nodes)"))

(defun incudine-scheduled-events ()
  "Print the number of the scheduled events."
  (interactive)
  (incudine-eval
    "(incudine.util:rt-eval (:return-value-p t) (incudine.edf:heap-count))"))

(defun incudine-gc ()
  "Initiate a garbage collection."
  (interactive)
  (incudine-eval "(incudine.util::gc :full t)"))

(defun incudine-bytes-consed-in (&optional time)
  "Rough estimate of the bytes consed in TIME seconds."
  (interactive "p")
  (incudine-eval "(incudine.util:get-bytes-consed-in %d)"
                 (prefix-numeric-value time)))

(defun incudine-rt-memory-free-size ()
  "Display the free realtime memory."
  (interactive)
  (incudine-eval
   "(values (incudine.util:get-foreign-sample-free-size)
            (incudine.util:get-rt-memory-free-size))"))

(defun incudine-peak-info (&optional ch)
  "Display the peak info of the channel CH (0 by default).

If CH is a negative number, reset the meters."
  (interactive "P")
  (let ((value (prefix-numeric-value0 ch)))
    (if (< value 0)
        (incudine-eval "(incudine:reset-peak-meters)")
        (incudine-eval "(incudine:peak-info %d)" value))))

(defun incudine-xruns-info (&optional reset-p)
  "Display the number of the occurred xruns and the time in samples of
the last xrun.

If RESET-P is non-NIL, set the number of xruns to zero."
  (interactive "P")
  (incudine-eval "(incudine:rt-xruns %S)" (and reset-p t)))

(defun incudine-set-logger-level (value)
  (incudine-eval "(setf (incudine.util:logger-level) %s)" value))

(defun incudine-logger-level-choice (c)
  "Set Logger Level from a single character."
  (interactive "cLogger level? (e)rror, (w)arn, (i)nfo or (d)ebug")
  (when (member c '(?e ?w ?i ?d))
    (incudine-set-logger-level
      (pcase c
        (?e ":ERROR")
        (?w ":WARN")
        (?i ":INFO")
        (?d ":DEBUG")))))

(defun incudine-set-logger-stream (value)
  (incudine-eval "(setf incudine.util:*logger-stream* %s)" value))

(defun incudine-logger-stream-choice (c)
  "Set Logger Stream from a single character."
  (interactive "cLogger stream? (e)rror-output, (s)tandard-output or (d)ebug-io")
  (when (member c '(?e ?s ?d))
    (incudine-set-logger-stream
      (pcase c
        (?e "CL:*ERROR-OUTPUT*")
        (?s "CL:*STANDARD-OUTPUT*")
        (?d "CL:*DEBUG-IO*")))))

(defun incudine-set-logger-time (value)
  (incudine-eval "(setf (incudine.util:logger-time) %s)" value))

(defun incudine-logger-time-choice (c)
  "Set Logger Time from a single character."
  (interactive "cLogger time? (S)amples, (s)econds or (n)il")
  (when (member c '(?S ?s ?n))
    (incudine-set-logger-time
      (pcase c
        (?n "NIL")
        (?s ":SEC")
        (?S ":SAMP")))))

(defun incudine-dsp/ugen-expand ()
  "If the form at point starts with DSP! or DEFINE-UGEN, prompt the
arguments and display the generated code."
  (interactive)
  (let ((string (slime-sexp-at-point)))
    (when (string-match "^[ \t]*(\\([^ \t:]+:\\)?\\(dsp!\\|define-ugen\\) " string)
      (let* ((type (if (char-equal (aref string (1+ (match-beginning 2))) ?s)
                       "dsp"
                       "ugen"))
             (args (read-from-minibuffer (concat type " args: ")))
             (form (format "(funcall (incudine.vug:%s-debug %s %s)"
                           type (substring string (match-end 0)) args)))
        (slime-eval-async `(swank:interactive-eval ,form)
          #'slime-initialize-macroexpansion-buffer)))))

(defun incudine-play-regofile ()
  "Eval the edited rego file and schedule the obtained events."
  (interactive)
  (when (and incudine-save-buffer-before-play (buffer-modified-p))
    (save-buffer))
  (incudine-eval
    "(progn (incudine:rt-start) (funcall (incudine:regofile->function %S)))"
    (buffer-file-name)))

(defun incudine-fix-rego-files-walk ()
  "Remove the killed buffers from incudine-rego-files-walk."
  (setq incudine-rego-files-walk
        (delete-if-not (lambda (x)
                         (and (buffer-live-p (car x))
                              (buffer-live-p (cdr x))))
                       incudine-rego-files-walk)))

(defun incudine-find-name (&optional name where)
  "If the current line is an `include' statement, edit the included
file name, otherwise edit a lisp definition or call `find-tag'."
  (interactive (list (and (not current-prefix-arg) (slime-symbol-at-point))))
  (unless (save-excursion
            (beginning-of-line)
            (when (re-search-forward "^[; \t]*include +\"\\(.*\\)\""
                                     (point-at-eol) t)
              (let ((parent (current-buffer)))
                (incudine-fix-rego-files-walk)
                (find-file (match-string-no-properties 1))
                (pushnew (cons (current-buffer) parent)
                         incudine-rego-files-walk :key 'car))))
    (let ((name (or name (slime-read-symbol-name "Edit Definition of: "))))
      (if (and (slime-connected-p) (slime-find-definitions name))
          (slime-edit-definition name where)
          (find-tag name)))))

(defun incudine-find-name-retract ()
  "If we are editing a rego file, goto the location of the parent
rego file or call `tags-loop-continue'."
  (interactive)
  (let* ((from (current-buffer))
         (entry (assoc from incudine-rego-files-walk)))
    (if entry
        (let ((to (cdr entry)))
          (incudine-fix-rego-files-walk)
          (when (buffer-live-p to)
            (setq incudine-rego-files-walk
                  (delete entry incudine-rego-files-walk))
            (switch-to-buffer to)))
        (tags-loop-continue))))

(defun incudine-regofile-to-sexp ()
  "Display the expansion of the edited rego file."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (let ((form (format "(incudine:regofile->sexp %S)" (buffer-file-name))))
    (slime-eval-async `(swank:interactive-eval ,form)
      #'slime-initialize-macroexpansion-buffer)))

(defun incudine-regofile-to-list ()
  "Display the list of events obtained from the edited rego file."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (let ((form (format "(incudine:regofile->list %S)" (buffer-file-name))))
    (slime-eval-async `(swank:interactive-eval ,form)
      #'slime-initialize-macroexpansion-buffer)))

(defun incudine-regofile-to-scheduled-events ()
  "Display the scheduled events obtained from the edited rego file."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (let ((form (format "`(with-schedule ,@(mapcar (lambda (x) `(at (* ,(car x) *sample-rate*) #',(cadr x) ,@(cddr x))) (incudine:regofile->list %S)))" (buffer-file-name))))
    (slime-eval-async `(swank:interactive-eval ,form)
      #'slime-initialize-macroexpansion-buffer)))

(font-lock-add-keywords
  'incudine-mode
  '((;; define-vug define-vug-macro define-ugen define-ugen-control-setter
     ;; defscore-statement defun* defmacro* lambda* dsp!
     "\\(d\\(?:ef\\(?:ine-\\(?:ugen\\(?:-control-setter\\)?\\|vug\\(?:-macro\\)?\\)\\|macro\\*\\|score-statement\\|un\\*\\)\\|sp!\\)\\|lambda\\*\\)\\_>\\s *\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"
     (1 font-lock-keyword-face nil t)
     (2 font-lock-function-name-face nil t))
    ("(\\(vuglet\\)\\_>"
     (1 font-lock-keyword-face nil t))))

(put 'define-vug 'common-lisp-indent-function '(4 &lambda &body))
(put 'define-ugen 'common-lisp-indent-function '(4 4 &lambda &body))
(put 'dsp! 'common-lisp-indent-function '(4 &lambda &body))
(put 'vuglet 'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 &lambda &body)) &body))
(put 'initialize 'common-lisp-indent-function 0)

(defun incudine-mode-common-map (map)
  (define-key map [C-M-return] 'incudine-free-node)
  (define-key map "\C-cv" 'incudine-show-repl)
  (define-key map "\C-cs" 'incudine-scratch)
  (define-key map "\C-c\M-o" 'incudine-repl-clear-buffer)
  (define-key map "\C-crs" 'incudine-rt-start)
  (define-key map "\C-crq" 'incudine-rt-stop)
  (define-key map "\C-cp" 'incudine-pause-node)
  (define-key map "\C-cu" 'incudine-unpause-node)
  (define-key map "\C-cgc" 'incudine-gc)
  (define-key map "\C-cgb" 'incudine-bytes-consed-in)
  (define-key map "\C-cid" 'incudine-dsp/ugen-expand)
  (define-key map "\C-cie" 'incudine-scheduled-events)
  (define-key map "\C-cig" 'incudine-dump-graph)
  (define-key map "\C-cim" 'incudine-rt-memory-free-size)
  (define-key map "\C-cin" 'incudine-live-nodes)
  (define-key map "\C-cip" 'incudine-peak-info)
  (define-key map "\C-cix" 'incudine-xruns-info)
  (define-key map "\C-cll" 'incudine-logger-level-choice)
  (define-key map "\C-cls" 'incudine-logger-stream-choice)
  (define-key map "\C-clt" 'incudine-logger-time-choice))

(defvar incudine-mode-map
  (let ((map (make-sparse-keymap "Incudine")))
    (incudine-mode-common-map map)
    (define-key map [C-return] 'incudine-eval-and-next-fn)
    (define-key map [C-S-return] 'incudine-eval-and-prev-fn)
    (define-key map [M-return] 'incudine-eval-defun)
    (define-key map [prior] 'incudine-prev-defun)
    (define-key map [next] 'incudine-next-defun)
    map)
  "Keymap for Incudine mode.")

(defvar incudine-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map incudine-mode-map)
    map))

(slime-define-keys incudine-scratch-mode-map
  ("\C-j" 'slime-eval-print-last-expression)
  ((kbd "C-S-j") 'incudine-eval-print-last-expression))

(defvar incudine-rego-mode-map
  (let ((map (make-sparse-keymap "Incudine Rego")))
    (incudine-mode-common-map map)
    (define-key map [f9] 'incudine-play-regofile)
    (define-key map "\C-cRe" 'incudine-regofile-to-scheduled-events)
    (define-key map "\C-cRl" 'incudine-regofile-to-list)
    (define-key map "\C-cRs" 'incudine-regofile-to-sexp)
    (define-key map "\M-." 'incudine-find-name)
    (define-key map "\M-," 'incudine-find-name-retract)
    map)
  "Keymap for Incudine Rego mode.")

(defvar incudine-mode-menu-list
  (list "Incudine"
        (list "REPL"
              ["Show REPL" incudine-show-repl t]
              ["REPL Clear Buffer" incudine-repl-clear-buffer t])
        (list "Realtime"
              ["RT Start" incudine-rt-start t]
              ["RT Stop" incudine-rt-stop t]
              ["Number of Scheduled Events" incudine-scheduled-events t]
              ["Peak Info"
               (incudine-peak-info
                 (string-to-number (read-from-minibuffer "Channel: " "0")))
               :keys "C-c i p"]
              ["Xruns Info" incudine-xruns-info t]
              ["Reset Peak Meters" (incudine-peak-info -1) t]
              ["Reset Xruns" (incudine-xruns-info t) t])
        (list "Graph"
              ["Stop Playing" incudine-free-node t]
              ["Pause" incudine-pause-node t]
              ["Unpause" incudine-unpause-node t]
              ["Number of Live Nodes" incudine-live-nodes t]
              ["Print Graph" incudine-dump-graph t])
        (list "Memory"
              ["Garbage Collection" incudine-gc t]
              ["RT Memory Free Size" incudine-rt-memory-free-size t])
        (list "Debugging"
              ["DSP/UGEN Generated Code" incudine-dsp/ugen-expand t])
        (list "Logger"
              ["Log Level" incudine-logger-level-choice t]
              ["Log Stream" incudine-logger-stream-choice t]
              ["Log Time"  incudine-logger-time-choice t])
        ["Scratch buffer" incudine-scratch t]))

(easy-menu-define incudine-mode-menu incudine-mode-map
  "Menu used in Incudine mode."
  incudine-mode-menu-list)

(easy-menu-define incudine-rego-mode-menu incudine-rego-mode-map
  "Menu used in Incudine Rego mode."
  (let* ((l (copy-sequence incudine-mode-menu-list))
         (rt-entry (assoc "Realtime" (cdr l)))
         (debug-entry (assoc "Debugging" (cdr l))))
    (setf (nthcdr 3 rt-entry)
          (cons ["Play Rego File" incudine-play-regofile t]
                (nthcdr 3 rt-entry)))
    (setf (cdr debug-entry)
          (list ["Rego File to List of Events" incudine-regofile-to-list t]
                ["Rego File to Scheduled Events"
                 incudine-regofile-to-scheduled-events t]
                ["Rego File to Sexp" incudine-regofile-to-sexp t]))
    l))

(defvar incudine-rego-font-lock-keywords
  '(("^;.*$" . font-lock-comment-face)))

(add-to-list 'auto-mode-alist '("\\.\\(cudo\\|rexi\\)$" . incudine-mode))
(add-to-list 'auto-mode-alist '("\\.rego$" . incudine-rego-mode))

(define-derived-mode incudine-mode lisp-mode "Incudine"
  "Major mode for incudine.

\\{incudine-mode-map}"
  (use-local-map
    (if (buffer-file-name)
        incudine-mode-map
        incudine-scratch-mode-map))
  (easy-menu-add incudine-mode-menu)
  (run-hooks 'incudine-mode-hook))

(define-derived-mode incudine-rego-mode org-mode "Incudine Rego"
    "Major mode for incudine rego.

\\{incudine-rego-mode-map}"
  (use-local-map incudine-rego-mode-map)
  (easy-menu-add incudine-rego-mode-menu)
  (push '(";.*$" . font-lock-comment-face) org-font-lock-keywords)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1)
  (setq-local comment-column 40)
  (setq-local comment-region-function 'comment-region-default)
  (setq-local uncomment-region-function 'uncomment-region-default)
  (run-hooks 'incudine-rego-mode-hook))

(provide 'incudine)
