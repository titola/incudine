(in-package :incudine-tests)

(deftest open-sound-control.1
    (osc:with-stream (oscout :direction :output)
      (list (osc:input-stream-p oscout)
            (osc:output-stream-p oscout)
            (osc:open-p oscout)
            (osc:block-p oscout)
            (osc:host oscout)
            (osc:port oscout)
            (osc:protocolp oscout :udp)
            (osc:protocolp oscout :tcp)
            (cffi:null-pointer-p (osc:buffer-pointer oscout))
            (= (osc:buffer-size oscout)
               (max 1500 (* (osc:max-values oscout) 20) osc:*buffer-size*))))
  (NIL T T T "localhost" #36ROSE T NIL NIL T))

(deftest open-sound-control.2
    (osc:with-stream (oscin)
      ;; OSC:START-MESSAGE is usually used with a OSC:OUTPUT-STREAM,
      ;; but it is a test.
      (let ((mlen (osc:start-message oscin "/osc/test" "iifs")))
        (multiple-value-bind (address typetag)
            (osc:address-pattern oscin t)
          (list address typetag mlen (osc:required-values oscin)
                (osc:check-pattern oscin "/osc/test" "iifs")
                (osc:check-pattern oscin "/osc/test" "iif")
                (osc:check-pattern oscin "/osc/tes" "iifs")
                (= (osc:message-length oscin) mlen)
                (loop for i below (osc:required-values oscin)
                      collect (osc:value oscin i))
                (cffi:mem-ref (osc:value-pointer oscin 0) :int32)
                (cffi:mem-ref (osc:value-pointer oscin 1) :int32)
                (cffi:mem-ref (osc:value-pointer oscin 2) :float)
                (cffi:foreign-string-to-lisp (osc:value-pointer oscin 3))))))
  ("/osc/test" "iifs" 36 4 T NIL NIL T (0 0 0.0 "") 0 0 0.0 ""))

(deftest open-sound-control.3
    (osc:with-stream (oscin)
      (osc:start-message oscin "/osc/test" "isf")
      ;; Again, it's only a test, we don't have to fill a OSC:INPUT-STREAM.
      (setf (osc:value oscin 0) 123)
      (setf (osc:value oscin 1) "rate")
      (setf (osc:value oscin 2) 0.75)
      (osc:index-values oscin t t)
      (values (loop for i below 3 collect (osc:value oscin i))
              (osc:with-values (node ctl val) (oscin "isf")
                (list node ctl val))
              (osc:buffer-to-octets oscin)))
  (123 "rate" 0.75)
  (123 "rate" 0.75)
  #(47 111 115 99 47 116 101 115 116 0 0 0 44 105 115 102 0 0 0 0
    123 0 0 0 114 97 116 101 0 0 0 0 0 0 64 63))

;;; Buffer to octets to buffer.
(deftest open-sound-control.4
    (osc:with-stream (oscout :direction :output)
      (osc:message oscout "/osc/test1" "fii" 123.456 -123 456)
      (let ((o1 (osc:buffer-to-octets oscout)))
        (osc:message oscout "/osc/test2" "sf" "frequency" 440.0)
        (let ((o2 (osc:buffer-to-octets oscout)))
          (osc:message oscout "/osc/test3" "b" #(60 64 67 71))
          (let ((o3 (osc:buffer-to-octets oscout))
                (acc nil))
            (flet ((test-octets (o values)
                     (osc:octets-to-buffer o oscout)
                     (osc:index-values oscout)
                     (push (loop for i below values
                                 collect (osc:value oscout i))
                           acc)))
              (test-octets o1 3)
              (test-octets o2 2)
              (test-octets o3 1))
            (nreverse acc)))))
  ((123.456 -123 456) ("frequency" 440.0) (#(60 64 67 71))))


;;; OSC MIDI.
(deftest open-sound-control.5
    (osc:with-stream (oscout :direction :output)
      (osc:start-message oscout "/osc/midi" "m")
      (setf (osc:value oscout 0) (osc:midi 3 144 60 96))
      (osc:value oscout 0))
  3 144 60 96)

(deftest open-sound-control-bundle.1
    (osc:with-stream (oscout :direction :output)
      (osc:simple-bundle oscout 0 "/osc/test/bundle" "isf" 1 "two" 3.0)
      (values
        (loop for i below (+ (osc:message-length oscout) 20)
              collect (cffi:mem-aref (osc::stream-bundle-pointer oscout)
                                     :unsigned-char i))
        (osc:buffer-to-octets oscout)))
  (35 98 117 110 100 108 101 0  ; "#bundle"
   0 0 0 0 0 0 0 1              ; timestamp
   0 0 0 40                     ; first element length
   47 111 115 99 47 116 101 115 116 47 98 117 110 100 108 101 0 0 0 0
   44 105 115 102 0 0 0 0 0 0 0 1 116 119 111 0 64 64 0 0)
  #(47 111 115 99 47 116 101 115 116 47 98 117 110 100 108 101 0 0 0 0
    44 105 115 102 0 0 0 0 0 0 0 1 116 119 111 0 64 64 0 0))

(defun osc-bundle-time-fields (stream)
  (flet ((field32 (index)
           (swap-bytes:ntohl (cffi:mem-aref (osc::stream-bundle-pointer stream)
                                            :uint32 (+ index 2)))))
    (values (field32 0) (field32 1))))

(deftest open-sound-control-bundle.2
    (osc:with-stream (oscout :direction :output :latency .25)
      (osc:simple-bundle oscout 3704513491 "/incudine/commit" "s" "e978fcc8")
      (osc-bundle-time-fields oscout))
  3704513491
  ;; Latency 0.25
  #x40000000)

(deftest open-sound-control-bundle.3
    (osc:with-stream (oscout :direction :output)
      (let ((time 3704516247))
        (osc:simple-bundle oscout time "/test/bundle" "isf" 1 "two" 3.0)
        (let ((msg (osc:buffer-to-octets oscout)))
          (setf (osc:value oscout 1) "ottocentottantotto")
          (osc:send-bundle oscout (+ time 3/2))
          (multiple-value-bind (sec frac) (osc-bundle-time-fields oscout)
            (values sec frac msg (osc:buffer-to-octets oscout))))))
  3704516248
  #x80000000
  #(47 116 101 115 116 47 98 117 110 100 108 101 0 0 0 0 44 105
    115 102 0 0 0 0 0 0 0 1 116 119 111 0 64 64 0 0)
  #(47 116 101 115 116 47 98 117 110 100 108 101 0 0 0 0 44 105
    115 102 0 0 0 0 0 0 0 1 111 116 116 111 99 101 110 116 111 116
    116 97 110 116 111 116 116 111 0 0 64 64 0 0))

;;; SLIP encoding/decoding.
(deftest open-sound-control-slip-enc.1
    (osc:with-stream (oscout :direction :output :protocol :tcp
                             :message-encoding :slip)
      (osc:start-message oscout "/osc/slip/test" "iibi")
      (setf (osc:value oscout 0) 12345)
      (setf (osc:value oscout 1) 192) ; SLIP END
      (setf (osc:value oscout 2) #(216 217 218 219 220 219 221 222))
      (setf (osc:value oscout 3) 219) ; SLIP ESC
      (let* ((slip-msg-len (osc:slip-encode oscout))
             (msg (osc:buffer-to-octets oscout))
             (slip-msg (loop for i below slip-msg-len
                             collect (cffi:mem-ref
                                       (osc::stream-aux-buffer-pointer oscout)
                                       :unsigned-char i))))
        (setf (osc::stream-message-length oscout) slip-msg-len)
        (incudine.external:foreign-copy (osc:message-pointer oscout)
                                        (osc::stream-aux-buffer-pointer oscout)
                                        (osc:message-length oscout))
        (osc:slip-decode oscout)
        (values msg slip-msg (osc:buffer-to-octets oscout)
                (osc:message-length oscout) slip-msg-len)))
  #(47 111 115 99 47 115 108 105 112 47 116 101 115 116 0 0
    44 105 105 98 105 0 0 0 0 0 48 57 0 0 0 192 0 0 0 8
    216 217 218 219 220 219 221 222 0 0 0 219)
  (192 47 111 115 99 47 115 108 105 112 47 116 101 115 116 0 0
   44 105 105 98 105 0 0 0 0 0 48 57 0 0 0 219 220 0 0 0 8
   216 217 218 219 221 220 219 221 221 222 0 0 0 219 221 192)
  #(47 111 115 99 47 115 108 105 112 47 116 101 115 116 0 0
    44 105 105 98 105 0 0 0 0 0 48 57 0 0 0 192 0 0 0 8
    216 217 218 219 220 219 221 222 0 0 0 219)
  48 54)

(deftest open-sound-control-slip-enc.2
    (osc:with-stream (oscout :direction :output :protocol :tcp
                             :message-encoding :slip)
      (osc:simple-bundle oscout 3704513491 "/test/bundle/slip/" "iii" 1 2 3)
      (loop for i below (+ (osc::stream-bundle-length oscout) 2)
            collect (cffi:mem-aref (osc::stream-aux-buffer-pointer oscout)
                                   :unsigned-char i)))
  (192 35 98 117 110 100 108 101 0 220 206 99 211 0 0 0 0 0 0 0 40
   47 116 101 115 116 47 98 117 110 100 108 101 47 115 108 105 112
   47 0 0 44 105 105 105 0 0 0 0 0 0 0 1 0 0 0 2 0 0 0 3 192))
