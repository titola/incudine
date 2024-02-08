(in-package :incudine-tests)

(defmacro node-test-loop ((index min max) &body body)
  `(loop for ,index from ,min to ,max do ,@body))

(defun node-test-list ()
  (let ((acc nil))
    (dograph (n *root-node* (cdr (nreverse acc)))
      (push (list (node-id n) (node-name n) (group-p n)) acc))))

(defun node-test-list-id ()
  (mapcar 'first (node-test-list)))

(defun move-node-init-test ()
  (let ((groups '(400 300 200 100)))
    (mapc #'make-group groups)
    (dolist (g groups)
      (loop repeat 4 for i from (1+ g) do
              (play (lambda ()) :id i :tail g)))))

(defun check-empty-node-hash-items ()
  (assert
    (every #'null-node-p
           (incudine::int-hash-table-items incudine::*node-hash*)))
  t)

(defun check-node-tree (&optional (compare nil compare-p) (group *root-node*))
  (flet ((node-tree-error (expected actual &key type)
           (multiple-value-bind (control args)
               (if (eq type 'funcons)
                   (values "Corrupted node tree:~%  ~A~%Funcons test failed."
                           (list expected))
                   (values "Corrupted node tree.~%Expected tree: ~A~%Actual tree: ~A"
                           (list expected actual)))
             (error 'incudine-node-error
               :format-control control :format-arguments args))))
    (let (groups funcons)
      (dograph (n group)
        (cond ((group-p n)
               (setf (getf groups n)
                     `((g ,(node-id n) ,@(and (pause-p n) '(p)))))
               (unless (eq n group)
                 (push (getf groups n) (rest (getf groups (group n))))))
              (t (push (if (pause-p n)
                           (list 'p (node-id n))
                           (node-id n))
                       (rest (getf groups (group n))))
                 (unless (incudine::paused-node-p n)
                   (push (incudine::node-function n) funcons)))))
      (loop for x on (cdr groups) by 'cddr do
            (setf (cdar x) (nreverse (cdar x))))
      (let ((res (getf groups group)))
        (when compare-p
          (unless (tree-equal res compare)
            (node-tree-error compare res)))
        (unless (equal (incudine::node-funcons group) (nreverse funcons))
          (node-tree-error res nil :type 'funcons))
        (or compare-p res)))))

(deftest node.1
    (let ((res nil)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
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
    (let ((res nil)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
        (handler-case
            (node-test-loop (i 1 (* *max-number-of-nodes* 2))
              (play (lambda ()) :id i))
          (incudine-node-error (c) c (push t res)))
        (push (live-nodes) res)
        (node-test-loop (i 1 *max-number-of-nodes*) (free i))
        (push (live-nodes) res))
      res)
  (0 1024 T))

;; Double free of a temporary node.
(deftest temp-node.1
    (flet ((pool-size ()
             (let ((data (incudine.util::cons-pool-data incudine::*node-pool*)))
               (if (alexandria:circular-list-p data)
                   -123
                   (length data)))))
      (let ((n (incudine::make-temp-node))
            (initial-pool-size (pool-size)))
        (free n)
        (free n)
        (= (pool-size) (1+ initial-pool-size))))
  T)

(deftest group.1
    (progn
      (free 0)
      (make-group 100)
      (make-group 200 :after 100)
      (values (incudine::node-funcons (node 200))
              (live-nodes)
              (progn (free 0) (live-nodes))))
  nil 2 0)

(deftest group.2
    (let ((n 100)
          (res nil))
      (free 0)
      (push `(start with ,(live-nodes) nodes) res)
      (loop repeat n
            do (make-group :free-hook (list (lambda (x) x (decf n)))))
      (push `(create ,(live-nodes) groups) res)
      (free 0)
      (push `(end with ,(live-nodes) nodes) res)
      (push `(= n ,n) res)
      (nreverse res))
  ((start with 0 nodes)
   (create 100 groups)
   (end with 0 nodes)
   (= n 0)))

(deftest move-node.1
    (let ((res nil)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
        (flet ((add-ids ()
                 (push (node-test-list-id) res))
               (after-p (n g)
                 (eq (incudine::node-next (incudine::node-last (node g)))
                     (node n)))
               (last-p (n g)
                 (= (node-id (incudine::node-last (node g))) n)))
          (move-node-init-test)  (add-ids)
          (move 300 :before 200) (add-ids)
          (move 200 :before 100) (add-ids)
          (move 103 :before 102) (add-ids)
          (move 104 :before 101) (add-ids)
          (move 100 :after  300) (add-ids)
          (move 200 :after  400) (add-ids)
          (move 402 :after  403) (add-ids)
          (move 104 :after  102) (add-ids)
          (move 400 :head   300) (add-ids)
          (push (after-p 301 400) res)
          (move 200 :head 400) (add-ids)
          (push (after-p 401 200) res)
          (move 200 :tail 100) (add-ids)
          (push (last-p 200 100) res)
          (move 300 :tail 200) (add-ids)
          (push (last-p 300 200) res)))
      (nreverse res))
  ((100 101 102 103 104 200 201 202 203 204 300 301 302 303 304 400 401 402 403 404)
   (100 101 102 103 104 300 301 302 303 304 200 201 202 203 204 400 401 402 403 404)
   (200 201 202 203 204 100 101 102 103 104 300 301 302 303 304 400 401 402 403 404)
   (200 201 202 203 204 100 101 103 102 104 300 301 302 303 304 400 401 402 403 404)
   (200 201 202 203 204 100 104 101 103 102 300 301 302 303 304 400 401 402 403 404)
   (200 201 202 203 204 300 301 302 303 304 100 104 101 103 102 400 401 402 403 404)
   (300 301 302 303 304 100 104 101 103 102 400 401 402 403 404 200 201 202 203 204)
   (300 301 302 303 304 100 104 101 103 102 400 401 403 402 404 200 201 202 203 204)
   (300 301 302 303 304 100 101 103 102 104 400 401 403 402 404 200 201 202 203 204)
   (300 400 401 403 402 404 301 302 303 304 100 101 103 102 104 200 201 202 203 204)
   T
   (300 400 200 201 202 203 204 401 403 402 404 301 302 303 304 100 101 103 102 104)
   T
   (300 400 401 403 402 404 301 302 303 304 100 101 103 102 104 200 201 202 203 204)
   T
   (100 101 103 102 104 200 201 202 203 204 300 400 401 403 402 404 301 302 303 304)
   T))

(deftest move-node.2
    (let ((res nil)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
        (move-node-init-test)
        (let ((start-ids (node-test-list-id)))
          (flet ((moved-p ()
                   (push (not (equal (node-test-list-id) start-ids)) res)))
            (move 200 :after 100) (moved-p)
            (move 103 :after 102) (moved-p)
            (move 300 :before 400) (moved-p)
            (move 403 :before 404) (moved-p)
            (move 201 :head 200) (moved-p)
            (move 304 :tail 300) (moved-p)
            (move 200 :head 100)
            (setf start-ids (node-test-list-id))
            (move 200 :head 100) (moved-p)
            (move 200 :tail 100)
            (setf start-ids (node-test-list-id))
            (move 200 :tail 100) (moved-p))))
      (every #'null res))
  T)

(deftest move-node.3
    (let ((initially nil)
          (finally nil))
      (with-logger (*null-output*)
        (bounce-to-buffer (*buffer-test-c1* :frames 1)
          (make-group 1)
          (play (lambda ()) :id 2 :head 1)
          (dograph (n) (push (node-id n) initially))
          (move 1 :head 1)
          (move 1 :tail 1)
          (move 1 :before 1)
          (move 1 :after 1)
          (move 2 :before 2)
          (move 2 :after 2)
          (dograph (n) (push (node-id n) finally)))
        (values initially finally)))
  (2 1 0)
  (2 1 0))

(deftest move-node.4
    (let ((res nil))
      (flet ((dump-node-list ()
               (let ((acc nil))
                 (dograph (n (node 0) (push (nreverse acc) res))
                   (push (if (eq n *root-node*)
                             0
                             (list (node-id (group n)) (node-id n)))
                         acc)))))
        (with-logger (*null-output*)
          (bounce-to-buffer (*buffer-test-c1* :frames 1)
            (make-group 100)
            (dotimes (i 4)
              (play #'* :id (+ i 1) :head 100)
              (play #'* :id (+ i 5) :tail 0))
            (dump-node-list)
            (move 5 :after 1)
            (dump-node-list)
            (move 4 :before 100)
            (move 5 :before 6)
            (dump-node-list))))
      (nreverse res))
  ((0 (0 100) (100 4) (100 3) (100 2) (100 1) (0 5) (0 6) (0 7) (0 8))
   (0 (0 100) (100 4) (100 3) (100 2) (100 1) (100 5) (0 6) (0 7) (0 8))
   (0 (0 4) (0 100) (100 3) (100 2) (100 1) (0 5) (0 6) (0 7) (0 8))))

(deftest move-node-loop-error.1
    (let ((loop-errors 0)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
        (macrolet ((loop-test (&body body)
                      `(handler-case (progn ,@body)
                         (incudine-node-error (c) c (incf loop-errors)))))
          (make-group 100)
          (make-group 200 :head 100)
          (make-group 300 :head 200)
          (loop-test (move 100 :head 200))
          (loop-test (move 100 :head 300))
          (loop-test (move 100 :tail 200))
          (loop-test (move 100 :tail 300))
          (loop-test (move 100 :before 300))
          (loop-test (move 100 :after 300))))
      (= loop-errors 6))
  T)

(deftest replace.1
    (let ((res nil)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
        (flet ((node-test (max-nodes)
                 (node-test-loop (i 1 max-nodes)
                   (play (lambda ()) :id i :tail *root-node* :name "foo"))))
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
    (let ((res nil)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
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

(deftest stop-group.1
    (let ((res nil)
          (g0 100)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
        (flet ((update-result ()
                 (push (node-test-list-id) res))
               (fill-group (group)
                 (node-test-loop (i 1000 1005)
                   (play (lambda ()) :id i :tail group))))
          (make-group 100)
          (fill-group g0)
          (update-result)
          (stop 100)
          (update-result)
          (fill-group g0)
          (update-result)
          (stop 0)
          (update-result)
          (fill-group g0)
          (move 1001 :head 0)
          (move 1002 :tail 0)
          (update-result)
          (stop 100)
          (update-result)
          (stop 0)
          (update-result)
          (fill-group g0)
          (move 1001 :head 0)
          (move 1002 :tail 0)
          (update-result)
          (stop 0)
          (update-result)))
      (nreverse res))
  (;; Fill group 100.
   (100 1000 1001 1002 1003 1004 1005)
   ;; STOP 100
   (100)
   ;; Refill.
   (100 1000 1001 1002 1003 1004 1005)
   ;; STOP 0
   (100)
   ;; Refill, then move 1001 and 1002.
   (1001 100 1000 1003 1004 1005 1002)
   ;; STOP 100
   (1001 100 1002)
   ;; STOP 0
   (100)
   ;; Refill, then move 1001 and 1002.
   (1001 100 1000 1003 1004 1005 1002)
   ;; STOP 0
   (100)))

(deftest stop-group.2
    (let ((id 123)
          (res nil)
          (*logger-stream* *null-output*))
      (bounce-to-buffer (*buffer-test-c1* :frames 10)
        (make-group id
          :action (lambda (n) (push `(start group ,(node-id n)) res))
          :stop-hook (list (lambda (n) (push `(stop group ,(node-id n)) res)))
          :free-hook (list (lambda (n)
                             (if (null-node-p n) (push '(end of group) res)))))
        (play (lambda ()) :head id
          :action (lambda (n)
                    (push `(start playing from group ,(node-id (group n))) res))
          :stop-hook (list (lambda (n)
                             (push `(stop playing from group ,(node-id (group n))) res)))
          :free-hook (list (lambda (n)
                             (if (null-node-p n) (push '(end of play) res)))))
        (at 7 #'stop id)
        (at 8 #'free id)
        (at 9 #'free *root-node*))
      (nreverse res))
  ((start group 123)
   (start playing from group 123)
   (stop playing from group 123)
   (stop group 123)
   (end of play)
   (end of group)))

(deftest dump-group.1
    (labels ((groups (obj parent)
               (if (atom obj)
                   (play (lambda ()) :id obj :tail parent
                         :name (format nil "~R" obj))
                   (multiple-value-bind (childs id group-p)
                       (if (atom (first obj))
                           (values (rest obj) (first obj) t)
                           (values obj parent))
                     (when group-p
                       (make-group id :tail parent
                                   :name (format nil "~R" (first obj))))
                     (dolist (c childs) (groups c id))))))
      (with-output-to-string (s)
        (let ((*logger-stream* *null-output*))
          (bounce-to-buffer (*buffer-test-c1* :frames 1)
            (groups '((100 (200 1 2 (300 4 5)) (400 6 (500 7 8)) 9)
                      (600 10 11 (700 12 13))
                      14 15 16)
                    0)
            (terpri s)
            (dump *root-node* s)))))
  "
group 0
    group 100 one hundred
        group 200 two hundred
            node 1
              one
            node 2
              two
            group 300 three hundred
                node 4
                  four
                node 5
                  five
        group 400 four hundred
            node 6
              six
            group 500 five hundred
                node 7
                  seven
                node 8
                  eight
        node 9
          nine
    group 600 six hundred
        node 10
          ten
        node 11
          eleven
        group 700 seven hundred
            node 12
              twelve
            node 13
              thirteen
    node 14
      fourteen
    node 15
      fifteen
    node 16
      sixteen
")

(deftest node-tree.1
    (let (failed)
      (bounce-to-buffer (*buffer-test-c1* :frames 1)
        (handler-case
            (progn
              ;; 1
              (pause 0) (play #'*) (play #'*) (play #'*)
              (check-node-tree '((G 0 P) 3 2 1))
              (unpause 0)
              (check-node-tree '((G 0) 3 2 1))
              ;; 2
              (free 0)
              (pause 0)
              (play #'* :id 1)
              (play #'* :id 2 :after 1)
              (play #'* :id 3 :before 1)
              (check-node-tree '((G 0 P) 3 1 2))
              (unpause 0)
              (check-node-tree '((G 0) 3 1 2))
              ;; 3
              (free 0)
              (make-group 200)
              (make-group 300 :action (lambda (n) (pause n)))
              (play #'* :tail 200)
              (check-node-tree '((G 0) ((G 300 P)) ((G 200) 1)))
              ;; 4
              (free 0)
              (play #'*)
              (make-group 300 :action (lambda (n) (pause n)))
              (play #'* :tail 300)
              (play #'* :head 300)
              (check-node-tree '((G 0) ((G 300 P) 3 2) 1))
              (unpause 300)
              (check-node-tree '((G 0) ((G 300) 3 2) 1))
              ;; 5
              (free 0)
              (make-group 300)
              (play #'* :tail 300)
              (pause 300)
              (make-group 200 :after 300)
              (play #'* :tail 200)
              (check-node-tree '((G 0) ((G 300 P) 1) ((G 200) 2)))
              (unpause 300)
              (check-node-tree '((G 0) ((G 300) 1) ((G 200) 2)))
              ;; 6
              (free 0)
              (make-group 300 :action (lambda (n) (pause n)))
              (make-group 200 :after 300)
              (play #'* :tail 300)
              (play #'* :tail 200)
              (check-node-tree '((G 0) ((G 300 P) 1) ((G 200) 2)))
              ;; 7
              (free 0)
              (play #'* :id 1)
              (play #'* :id 2)
              (play #'* :id 3)
              (play #'* :id 4)
              (pause 3)
              (pause 2)
              (check-node-tree '((G 0) 4 (P 3) (P 2) 1))
              (make-group 100 :after 3)
              (make-group 200 :before 100)
              (play #'* :id 5 :head 100)
              (check-node-tree '((G 0) 4 (P 3) ((G 200)) ((G 100) 5) (P 2) 1))
              (pause 100)
              (pause 200)
              (check-node-tree '((G 0) 4 (P 3) ((G 200 P)) ((G 100 P) 5) (P 2) 1))
              (unpause 100)
              (unpause 200)
              (unpause 2)
              (unpause 3)
              (check-node-tree '((G 0) 4 3 ((G 200)) ((G 100) 5) 2 1))
              ;; 8
              (free 0)
              (play #'* :id 1)
              (play #'* :id 2)
              (make-group 100 :after 2)
              (play #'* :id 3 :head 100)
              (play #'* :id 4 :head 100)
              (pause 100)
              (make-group 200 :head 100)
              (pause 200)
              (pause 4)
              (check-node-tree '((G 0) 2 ((G 100 P) ((G 200 P)) (P 4) 3) 1))
              (unpause 100)
              (check-node-tree '((G 0) 2 ((G 100) ((G 200 P)) (P 4) 3) 1))
              (unpause 4)
              (unpause 200)
              (check-node-tree '((G 0) 2 ((G 100) ((G 200)) 4 3) 1))
              ;; 9
              (free 0)
              (play #'* :id 1)
              (make-group 100)
              (play #'* :id 2 :tail 100)
              (play #'* :id 3 :tail 100)
              (play #'* :id 11 :after 100)
              (pause 2)
              (pause 100)
              (check-node-tree '((G 0) ((G 100 P) (P 2) 3) 11 1))
              (unpause 100)
              (check-node-tree '((G 0) ((G 100) (P 2) 3) 11 1))
              ;; 10
              (free 0)
              (play #'* :id 1)
              (play #'* :id 2)
              (play #'* :id 3)
              (play #'* :id 4)
              (move 2 :before 3)
              (move 1 :before 2)
              (make-group 100 :before 1)
              (move 1 :head 100)
              (move 2 :before 1)
              (pause 100)
              (move 3 :before 1)
              (check-node-tree '((G 0) 4 ((G 100 P) 2 3 1)))
              (unpause 100)
              (check-node-tree '((G 0) 4 ((G 100) 2 3 1)))
              ;; 11
              (free 0)
              (make-group 100 :action (lambda (n) (pause n)))
              (make-group 200 :head 100)
              (make-group 300 :head 100)
              (play #'* :id 1 :head 300)
              (play #'* :id 2 :tail 300)
              (play #'* :id 3 :head 200)
              (play #'* :id 4 :after 3)
              (make-group 42 :before 3)
              (move 3 :head 42)
              (make-group 49 :tail 300)
              (play #'* :head 49)
              (check-node-tree
                '((G 0) ((G 100 P) ((G 300) 1 2 ((G 49) 5)) ((G 200) ((G 42) 3) 4))))
              (unpause 100)
              (check-node-tree
                '((G 0) ((G 100) ((G 300) 1 2 ((G 49) 5)) ((G 200) ((G 42) 3) 4))))
              ;; 12
              (free 0)
              (make-group 100 :action (lambda (n) (pause n)))
              (make-group 300 :head 100)
              (play #'* :id 1 :head 300)
              (play #'* :id 2 :tail 300)
              (check-node-tree '((G 0) ((G 100 P) ((G 300) 1 2))))
              (unpause 100)
              (check-node-tree '((G 0) ((G 100) ((G 300) 1 2)))))
          (incudine-node-error (c) (setf failed t) (error c))))
      failed)
  NIL)
