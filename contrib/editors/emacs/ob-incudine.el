(require 'ob-lisp)

(add-to-list 'org-babel-tangle-lang-exts '("incudine" . "cudo"))

(defalias 'org-babel-expand-body:incudine 'org-babel-expand-body:lisp)

(defalias 'org-babel-execute:incudine 'org-babel-execute:lisp)

(defun load-incudine-rego-library ()
  "Load the collection of code blocks for incudine-rego-mode."
  (interactive)
  (org-babel-lob-ingest (locate-library "incudine-rego-library.org")))

(load-incudine-rego-library)

(provide 'ob-incudine)
