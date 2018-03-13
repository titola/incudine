(in-package :incudine-tests)

(defmacro node-test-loop ((index min max) &body body)
  `(loop for ,index from ,min to ,max do ,@body))

(defun node-test-list ()
  (let ((acc nil))
    (dograph (n *node-root* (cdr (nreverse acc)))
      (push (list (node-id n) (node-name n) (group-p n)) acc))))

(defun check-empty-node-hash-items ()
  (assert
    (every #'null-node-p
           (incudine::int-hash-table-items incudine::*node-hash*)))
  t)

(deftest node.1
    (let ((res nil))
      (bounce-to-buffer (*buffer-test-c1*)
        (node-test-loop (i 1 *max-number-of-nodes*)
          (play (lambda ()) :id i))
        (push (live-nodes) res)
        (let ((node-list (node-test-list)))
          (push (length node-list) res)
          (push (equal (loop for i from *max-number-of-nodes* downto 1
                             collect (node-id (node i)))
                       (mapcar 'first node-list))
                res)
          (node-test-loop (i 1 *max-number-of-nodes*) (free i))
          (check-empty-node-hash-items)
          (push (node-test-list) res)
          (push (live-nodes) res)))
      res)
  (0 NIL T 1024 1024))

(deftest node.2
    (let ((res nil))
      (bounce-to-buffer (*buffer-test-c1*)
        (handler-case
            (node-test-loop (i 1 (* *max-number-of-nodes* 2))
              (play (lambda ()) :id i))
          (incudine-node-error (c) c (push t res)))
        (push (live-nodes) res)
        (node-test-loop (i 1 *max-number-of-nodes*) (free i))
        (push (live-nodes) res))
      res)
  (0 1024 T))

(deftest group.1
  (progn
    (make-group 100)
    (make-group 200 :after 100)
    (values (incudine::node-funcons (node 200))
            (live-nodes)
            (progn (free 0) (live-nodes))))
  nil 2 0)

(deftest replace.1
    (let ((res nil))
      (bounce-to-buffer (*buffer-test-c1*)
        (flet ((node-test (max-nodes)
                 (node-test-loop (i 1 max-nodes)
                   (play (lambda ()) :id i :tail *node-root* :name "foo"))))
          (node-test (1- *max-number-of-nodes*))
          (let ((n (ash *max-number-of-nodes* -1)))
            (play (lambda ()) :replace n :name "bar")
            (push (equal (node-test-list)
                         (loop for i from 1 to (1- *max-number-of-nodes*)
                               collect (list i (if (= i n) "bar" "foo") nil)))
                  res)
            (free 0)
            (check-empty-node-hash-items)
            (push (node-test-list) res)
            ;; Replacing requires one freed node.
            (node-test *max-number-of-nodes*)
            (handler-case
                (play (lambda ()) :replace n :name "bar")
              (incudine-node-error (c)
                (push 'free-node-required res)))
            (free 0)
            (check-empty-node-hash-items)
            (push (live-nodes) res))))
      res)
  (0 FREE-NODE-REQUIRED NIL T))

(deftest replace.2
    (let ((res nil))
      (bounce-to-buffer (*buffer-test-c1*)
        (flet ((update-result ()
                 (push (node-test-list) res)))
          (let ((g0 100))
            (make-group g0)
            (node-test-loop (i 1000 1005)
              (play (lambda ()) :id i :tail g0))
            (update-result)
            (play (lambda ()) :replace 100)
            (update-result)
            (free 100)
            (update-result)
            (let ((groups (list g0 200 300)))
              (mapc #'make-group groups)
              (loop for g in groups do
                      (node-test-loop (i 1000 1005)
                        (play (lambda ()) :id (+ g i) :head g)))
              (update-result)
              (play (lambda ()) :replace (second groups))
              (update-result)))))
      (nreverse res))
  (;; Before replacing.
   ((100 NIL T)
    (1000 NIL NIL) (1001 NIL NIL) (1002 NIL NIL)
    (1003 NIL NIL) (1004 NIL NIL) (1005 NIL NIL))
   ;; After replacing group 100 -> node.
   ((100 NIL NIL))
   NIL
   ;; Before replacing.
   ;; group 0
   ;;     group 300
   ;;         node 1305
   ;;         node 1304
   ;;         node 1303
   ;;         node 1302
   ;;         node 1301
   ;;         node 1300
   ;;     group 200
   ;;         node 1205
   ;;         node 1204
   ;;         node 1203
   ;;         node 1202
   ;;         node 1201
   ;;         node 1200
   ;;     group 100
   ;;         node 1105
   ;;         node 1104
   ;;         node 1103
   ;;         node 1102
   ;;         node 1101
   ;;         node 1100
   ;;
   ((300 NIL T)
    (1305 NIL NIL) (1304 NIL NIL) (1303 NIL NIL)
    (1302 NIL NIL) (1301 NIL NIL) (1300 NIL NIL)
    (200 NIL T)
    (1205 NIL NIL) (1204 NIL NIL) (1203 NIL NIL)
    (1202 NIL NIL) (1201 NIL NIL) (1200 NIL NIL)
    (100 NIL T)
    (1105 NIL NIL) (1104 NIL NIL) (1103 NIL NIL)
    (1102 NIL NIL) (1101 NIL NIL) (1100 NIL NIL))
   ;; After replacing group 200 -> node.
   ;; group 0
   ;;     group 300
   ;;         node 1305
   ;;         node 1304
   ;;         node 1303
   ;;         node 1302
   ;;         node 1301
   ;;         node 1300
   ;;     node 200
   ;;     group 100
   ;;         node 1105
   ;;         node 1104
   ;;         node 1103
   ;;         node 1102
   ;;         node 1101
   ;;         node 1100
   ;;
   ((300 NIL T)
    (1305 NIL NIL) (1304 NIL NIL) (1303 NIL NIL)
    (1302 NIL NIL) (1301 NIL NIL) (1300 NIL NIL)
    (200 NIL NIL)
    (100 NIL T)
    (1105 NIL NIL) (1104 NIL NIL) (1103 NIL NIL)
    (1102 NIL NIL) (1101 NIL NIL) (1100 NIL NIL))))
