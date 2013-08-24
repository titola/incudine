;;; incudine.el --- major mode for editing Incudine sources

;; Copyright (c) 2013 Tito Latini

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

(defcustom incudine-scratch-message nil
  "Initial message displayed in *incudine-scratch* buffer.
If this is nil, no message will be displayed."
  :type '(choice (text :tag "Message")
                 (const :tag "none" nil))
  :group 'incudine)

(defvar incudine-mode-map nil
  "Incudine keymap.")

(defun incudine-buffer-name (string)
  (concat "*incudine-" string "*"))

(defvar incudine-scratch-buffer (incudine-buffer-name "scratch"))

(defun incudine-scratch ()
  "Switch to Incudine scratch buffer."
  (interactive)
  (let ((buffer (get-buffer incudine-scratch-buffer)))
    (unless buffer
      (setq buffer (get-buffer-create incudine-scratch-buffer))
      (with-current-buffer buffer
	(incudine-mode)
        (if incudine-scratch-message
            (insert incudine-scratch-message))))
    (switch-to-buffer buffer)))

(defun incudine-eval (string)
  (slime-interactive-eval string))

(defun incudine-eval-defun ()
  (interactive)
  (slime-eval-defun))

(defun incudine-show-repl ()
  "Show REPL in other window."
  (interactive)
  (switch-to-buffer-other-window (slime-output-buffer)))

(defun incudine-repl-clear-buffer ()
  (interactive)
  (save-window-excursion
    (with-current-buffer (slime-repl-buffer)
      (slime-repl-insert-prompt)
      (slime-repl-clear-buffer))))

(defun incudine-prev-defun (&optional n)
  "Jump at the end of the previous defun."
  (interactive "p")
  (end-of-defun)
  (beginning-of-defun)
  (if n
      (loop repeat n do (beginning-of-defun))
    (beginning-of-defun))
  (forward-sexp))

(defun incudine-next-defun (&optional n)
  "Jump at the end of the next defun."
  (interactive "p")
  (end-of-defun)
  (if n (loop repeat n do (end-of-defun))
    (forward-sexp))
  (beginning-of-defun)
  (forward-sexp))

(defun incudine-eval-and-next-fn ()
  "Eval the function and jump to the next."
  (interactive)
  (slime-eval-defun)
  (incudine-next-defun))

(defun incudine-eval-and-prev-fn ()
  "Eval the function and jump to the previous."
  (interactive)
  (slime-eval-defun)
  (incudine-prev-defun))

(defun incudine-stop-graph ()
  "Stop Playing."
  (interactive)
  (incudine-eval "(incudine:free incudine:*node-root*)"))

(defun incudine-mode-keybindings (map)
  "Incudine keybindings."
  (define-key map [C-return] 'incudine-eval-and-next-fn)
  (define-key map [C-S-return] 'incudine-eval-and-prev-fn)
  (define-key map [M-return] 'incudine-eval-defun)
  (define-key map [C-M-return] 'incudine-stop-graph)
  (define-key map [prior] 'incudine-prev-defun)
  (define-key map [next] 'incudine-next-defun)
  (define-key map "\C-cv" 'incudine-show-repl)
  (define-key map "\C-cs" 'incudine-scratch)
  (define-key map "\C-c\M-o" 'incudine-repl-clear-buffer))

(defun incudine-mode-menu (map)
  "Incudine menu."
  (define-key map [menu-bar incudine]
    (cons "Incudine" (make-sparse-keymap "incudine")))
  (define-key map [menu-bar incudine stop-graph]
    '("Stop Playing" . incudine-stop-graph))
  (define-key map [menu-bar incudine clear-repl]
    '("REPL Clear Buffer" . incudine-repl-clear-buffer))
  (define-key map [menu-bar incudine show-repl]
    '("Show REPL" . incudine-show-repl))
  (define-key map [menu-bar incudine scratch]
    '("Scratch buffer" . incudine-scratch)))

(if incudine-mode-map
    nil
  (let ((map (make-sparse-keymap "Incudine")))
    (incudine-mode-keybindings map)
    (incudine-mode-menu map)
    (setq incudine-mode-map map)))

(define-derived-mode incudine-mode lisp-mode "Incudine"
  "Major mode for incudine.

\\{incudine-mode-map}")

(add-to-list 'auto-mode-alist '("\\.cudo$" . incudine-mode))
	     
(provide 'incudine)