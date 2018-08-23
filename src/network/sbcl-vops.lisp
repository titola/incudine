(in-package :incudine.osc)

;;; The result-type of the VOPs for x86oids in SWAP-BYTES is
;;; SB-VM::UNSIGNED-NUM, so it is necessary to coerce to signed-num
;;; if we use OSC:VALUE with a OSC:OUTPUT-STREAM. On the contrary,
;;; the portable functions in `swap-bytes/portable.lisp' are optimized
;;; with (SAFETY 0) and if the input is a signed number, the output
;;; should also be a signed number.
(defknown swap-bytes-i32 ((signed-byte 32)) (signed-byte 32)
    (movable foldable flushable)
  :overwrite-fndb-silently t)

#+x86-64
(defknown swap-bytes-i64 ((signed-byte 64)) (signed-byte 64)
    (movable foldable flushable)
  :overwrite-fndb-silently t)

#+x86-64
(define-vop (signed-32bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-i32)
  (:note "inline 32-bit swap bytes")
  (:args (integer :scs (sb-vm::signed-reg) :target res))
  (:arg-types sb-vm::signed-num)
  (:results (res :scs (sb-vm::signed-reg)))
  (:result-types sb-vm::signed-num)
  (:generator 2
   (move res integer)
   #.(if (incudine.util::old-movsxd-p)
         '(let ((res-dword (sb-vm::reg-in-size res :dword)))
            (inst bswap res-dword)
            (inst movsxd res res-dword))
         '(progn
            (inst bswap (sb-vm::reg-in-size res :dword))
            (inst movsx '(:dword :qword) res res)))))

#+x86
(define-vop (signed-32bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-i32)
  (:note "inline 32-bit swap bytes")
  (:args (integer :scs (sb-vm::signed-reg) :target res))
  (:arg-types sb-vm::signed-num)
  (:results (res :scs (sb-vm::signed-reg)))
  (:result-types sb-vm::signed-num)
  (:generator 2
   (move res integer)
   (inst bswap res)))

#+x86-64
(define-vop (signed-64bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-i64)
  (:note "inline 64-bit swap bytes")
  (:args (integer :scs (sb-vm::signed-reg) :target res))
  (:arg-types sb-vm::signed-num)
  (:results (res :scs (sb-vm::signed-reg)))
  (:result-types sb-vm::signed-num)
  (:generator 2
   (move res integer)
   (inst bswap res)))
