(require 'ob-lisp)

(add-to-list 'org-babel-tangle-lang-exts '("incudine" . "cudo"))

(defalias 'org-babel-expand-body:incudine 'org-babel-expand-body:lisp)

(defalias 'org-babel-execute:incudine 'org-babel-execute:lisp)

(provide 'ob-incudine)
