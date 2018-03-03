(in-package :incudine-tests)

(deftest group.1
  (progn
    (make-group 100)
    (make-group 200 :after 100)
    (incudine::node-funcons (node 200)))
  nil)
