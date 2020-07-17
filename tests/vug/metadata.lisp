(in-package :incudine-tests)

(define-ugen ugen-metadata-test-1 nil ((buffer buffer) pan)
  (:defaults (incudine-missing-arg "BUFFER") 0.5)
  (:metadata "Test" "UGEN metadata test 1")
  (:metadata :inputs 1)
  (:metadata :outputs 2)
  (:metadata test-function (lambda () "UGEN metadata function test 1"))
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha)))
    (loop for i below (buffer-size buffer) by 2 do
         (setf (buffer-value buffer (1+ i))
               (* right (buffer-value buffer i)))
         (setf (buffer-value buffer i)
               (* left (buffer-value buffer i))))))

(define-vug-macro vug-macro-metadata-test-1 (obj)
  (:metadata "Test" "VUG Macro metadata test 1")
  (:metadata test-function (lambda () "VUG Macro metadata function test 1"))
  `(progn ,obj))

(dsp! dsp-metadata-test-1 ((buffer buffer) pan)
  (:defaults (incudine-missing-arg "BUFFER") 0.5)
  (:metadata "Test" "DSP metadata test 1")
  (:metadata :inputs 1)
  (:metadata :outputs 2)
  (:metadata test-function (lambda () "DSP metadata function test 1"))
  (vug-macro-metadata-test-1
   (ugen-metadata-test-1 buffer pan)))

(deftest dsp-metadata.1
    (flet ((test (obj)
             (list
               (mapcar (lambda (x)
                         (if (functionp (cdr x))
                             (cons (car x) 'function)
                             x))
                       (metadata obj))
               (metadata obj "Test")
               (metadata obj :inputs)
               (metadata obj :outputs)
               (funcall (metadata obj 'test-function)))))
      (values
        (test 'ugen-metadata-test-1)
        (test 'vug-macro-metadata-test-1)
        (test 'ugen-metadata-test-1)
        (test 'dsp-metadata-test-1)))
  ;; VUG
  ((("Test" . "UGEN metadata test 1") (:INPUTS . 1) (:OUTPUTS . 2)
    (TEST-FUNCTION . FUNCTION))
   "UGEN metadata test 1" 1 2 "UGEN metadata function test 1")
  ;; VUG-MACRO
  ((("Test" . "VUG Macro metadata test 1") (TEST-FUNCTION . FUNCTION))
   "VUG Macro metadata test 1" NIL NIL "VUG Macro metadata function test 1")
  ;; Compiled VUG
  ((("Test" . "UGEN metadata test 1") (:INPUTS . 1) (:OUTPUTS . 2)
    (TEST-FUNCTION . FUNCTION))
   "UGEN metadata test 1" 1 2 "UGEN metadata function test 1")
  ;; DSP
  ((("Test" . "DSP metadata test 1") (:INPUTS . 1) (:OUTPUTS . 2)
    (TEST-FUNCTION . FUNCTION))
   "DSP metadata test 1" 1 2 "DSP metadata function test 1"))
