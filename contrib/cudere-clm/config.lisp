(in-package :cudere-clm)

#+sbcl
(declaim (sb-ext:disable-package-locks double-float))

(define-constant two-pi (* pi 2))

(define-constant mus-unsupported 0)
(define-constant mus-next 1)
(define-constant mus-aifc 2)
(define-constant mus-riff 3)
(define-constant mus-rf64 4)
(define-constant mus-bicsf 5)
(define-constant mus-nist 6)
(define-constant mus-inrs 7)
(define-constant mus-esps 8)
(define-constant mus-svx 9)
(define-constant mus-voc 10)
(define-constant mus-sndt 11)
(define-constant mus-raw 12)
(define-constant mus-smp 13)
(define-constant mus-avr 14)
(define-constant mus-ircam 15)
(define-constant mus-sd1 16)
(define-constant mus-sppack 17)
(define-constant mus-mus10 18)
(define-constant mus-hcom 19)
(define-constant mus-psion 20)
(define-constant mus-maud 21)
(define-constant mus-ieee 22)
(define-constant mus-matlab 23)
(define-constant mus-adc 24)
(define-constant mus-midi 25)
(define-constant mus-soundfont 26)
(define-constant mus-gravis 27)
(define-constant mus-comdisco 28)
(define-constant mus-goldwave 29)
(define-constant mus-srfs 30)
(define-constant mus-midi-sample-dump 31)
(define-constant mus-diamondware 32)
(define-constant mus-adf 33)
(define-constant mus-sbstudioii 34)
(define-constant mus-delusion 35)
(define-constant mus-farandole 36)
(define-constant mus-sample-dump 37)
(define-constant mus-ultratracker 38)
(define-constant mus-yamaha-sy85 39)
(define-constant mus-yamaha-tx16 40)
(define-constant mus-digiplayer 41)
(define-constant mus-covox 42)
(define-constant mus-avi 43)
(define-constant mus-omf 44)
(define-constant mus-quicktime 45)
(define-constant mus-asf 46)
(define-constant mus-yamaha-sy99 47)
(define-constant mus-kurzweil-2000 48)
(define-constant mus-aiff 49)
(define-constant mus-paf 50)
(define-constant mus-csl 51)
(define-constant mus-file-samp 52)
(define-constant mus-pvf 53)
(define-constant mus-soundforge 54)
(define-constant mus-twinvq 55)
(define-constant mus-akai4 56)
(define-constant mus-impulsetracker 57)
(define-constant mus-korg 58)
(define-constant mus-nvf 59)
(define-constant mus-caff 60)
(define-constant mus-maui 61)
(define-constant mus-sdif 62)
(define-constant mus-ogg 63)
(define-constant mus-flac 64)
(define-constant mus-speex 65)
(define-constant mus-mpeg 66)
(define-constant mus-shorten 67)
(define-constant mus-tta 68)
(define-constant mus-wavpack 69)

(let ((al `((,mus-next . "au") (,mus-aifc . "aiff") (,mus-riff . "wav")
            (,mus-rf64 . "rf64") (,mus-nist "nist") (,mus-raw . "raw")
            (,mus-ircam . "ircam") (,mus-aiff . "aiff") (,mus-caff . "caf"))))

  (defun mus-header-type-ok (n)
    (find n al :key #'car :test #'=))

  (defun mus-to-sf-header-type (n)
    (or (cdr (assoc n al)) "unsupported"))

  (defun sf-to-mus-header-type (ht)
    (or (car (rassoc ht al :test 'string=)) 0)))

(define-constant mus-unknown 0)
(define-constant mus-bshort 1)
(define-constant mus-mulaw 2)
(define-constant mus-byte 3)
(define-constant mus-bfloat 4)
(define-constant mus-bint 5)
(define-constant mus-alaw 6)
(define-constant mus-ubyte 7)
(define-constant mus-b24int 8)
(define-constant mus-bdouble 9)
(define-constant mus-lshort 10)
(define-constant mus-lint 11)
(define-constant mus-lfloat 12)
(define-constant mus-ldouble 13)
(define-constant mus-ubshort 14)
(define-constant mus-ulshort 15)
(define-constant mus-l24int 16)
(define-constant mus-bintn 17)
(define-constant mus-lintn 18)
(define-constant mus-blfoatu 19)
(define-constant mus-lfloatu 20)
(define-constant mus-bdoubleu 21)
(define-constant mus-ldoubleu 22)

(let ((al `((,mus-bshort . "pcm-16") (,mus-lshort . "pcm-16")
            (,mus-mulaw . "ulaw") (,mus-alaw . "alaw")
            (,mus-byte . "pcm-s8") (,mus-ubyte . "pcm-u8")
            (,mus-bfloat . "float") (,mus-lfloat . "float")
            (,mus-bint . "pcm-32") (,mus-lint . "pcm-32")
            (,mus-b24int . "pcm-24") (,mus-l24int . "pcm-24")
            (,mus-bdouble . "double") (,mus-ldouble . "double"))))

  (defun mus-data-format-ok (n)
    (find n al :key #'car :test #'=))

  (defun mus-to-sf-data-format (n)
    (or (cdr (assoc n al)) "unsupported"))

  (defun sf-to-mus-data-format (fmt)
    (or (car (rassoc fmt al :test 'string=)) 0))

  (defun bytes-per-sample (n)
    (if (<= mus-bshort n mus-l24int)
        (svref #(0 2 1 1 4 4 1 1 3 8 2 4 4 8 2 2 3) n)
        0)))

(define-constant mus-audio-default 0)

(define-constant mus-interp-none 0)
(define-constant mus-interp-linear 1)
(define-constant mus-interp-sinusoidal 2)
(define-constant mus-interp-all-pass 3)
(define-constant mus-interp-lagrange 4)
(define-constant mus-interp-bezier 5)
(define-constant mus-interp-hermite 6)

;;; Backwards compatibility.
(define-constant mus-linear 0)
(define-constant mus-sinusoidal 1)

(deftype mus-interp () `(mod 7))
(deftype mus-locsig-interp () `(member ,mus-interp-linear ,mus-interp-sinusoidal))

(define-constant mus-chebyshev-first-kind 1)
(define-constant mus-chebyshev-second-kind 2)

(define-constant rectangular-window 0)
(define-constant hann-window 1)
(define-constant hanning-window 1)
(define-constant welch-window 2)
(define-constant parzen-window 3)
(define-constant bartlett-window 4)
(define-constant hamming-window 5)
(define-constant blackman2-window 6)
(define-constant blackman3-window 7)
(define-constant blackman4-window 8)
(define-constant exponential-window 9)
(define-constant riemann-window 10)
(define-constant kaiser-window 11)
(define-constant cauchy-window 12)
(define-constant poisson-window 13)
(define-constant gaussian-window 14)
(define-constant tukey-window 15)
(define-constant dolph-chebyshev-window 16)
(define-constant hann-poisson-window 17)
(define-constant connes-window 18)
(define-constant samaraki-window 19)
(define-constant ultraspherical-window 20)
(define-constant bartlett-hann-window 21)
(define-constant bohman-window 22)
(define-constant flat-top-window 23)
(define-constant blackman5-window 24)
(define-constant blackman6-window 25)
(define-constant blackman7-window 26)
(define-constant blackman8-window 27)
(define-constant blackman9-window 28)
(define-constant blackman10-window 29)
(define-constant rv2-window 30)
(define-constant rv3-window 31)
(define-constant rv4-window 32)

(defvar *output* nil)
(declaim (type (or null soundfile:output-stream) *output*))

(defvar *reverb* nil)
(declaim (type (or null soundfile:stream) *reverb*))

(defvar *clm-srate* *sample-rate*)
(defvar *srate* *clm-srate*)
(declaim (type real *clm-srate* *srate*))

(defvar *clm-channels* 1)
(defvar *channels* *clm-channels*)
(declaim (type non-negative-fixnum *clm-channels* *channels*))

(defvar *clm-file-buffer-size* (* 64 1024))
(declaim (type non-negative-fixnum *clm-file-buffer-size*))

(defvar *clm-file-name* #+little-endian "test.wav"
                        #-little-endian "test.aiff")
(declaim (type (or string null) *clm-file-name*))

(defvar *clm-header-type* #+little-endian mus-riff
                          #-little-endian mus-aiff)

(defvar *header-type* *clm-header-type*)
(declaim (type non-negative-fixnum *clm-header-type* *header-type*))

(defvar *clm-data-format* #+little-endian mus-ldouble
                          #-little-endian mus-bdouble)

(defvar *data-format* *clm-data-format*)
(declaim (type non-negative-fixnum *clm-data-format* *data-format*))

(defvar *clm-tempfile-header-type* mus-next)
(declaim (type non-negative-fixnum *clm-tempfile-header-type*))

(defvar *clm-tempfile-data-format* #+little-endian mus-ldouble
                                   #-little-endian mus-bdouble)
(declaim (type non-negative-fixnum *clm-tempfile-data-format*))

(defvar *clm-verbose* nil)
(defvar *verbose* *clm-verbose*)

(defvar *clm-play* t)
(declaim (type boolean *clm-play*))

(defvar *clm-player* nil)

(defvar *clm-table-size* 512)
(declaim (type non-negative-fixnum *clm-table-size*))

(defvar *clm-safety* 0)
(defvar *safety* *clm-safety*)

(defvar *clm-array-print-length* 10)

(defvar *clm-init* nil)

(defvar *clm-search-list* (list ""))

(defvar *clm-notehook* nil)
(defvar *notehook* *clm-notehook*)

(defvar *clm-clipped* t)
(defvar *clipped* *clm-clipped*)

(defvar *clm-src-width* 10)
(declaim (type positive-fixnum *clm-src-width*))

(defvar *clm-delete-reverb* nil)

(defvar *clm-reverb-channels* 1)

(defvar *clm-statistics* nil)
(defvar *statistics* nil)

(defvar *clm-default-frequency* 0.0)
(declaim (type real *clm-default-frequency*))

(defvar *clm-debug* nil)
(defvar *debug* *clm-debug*)

(defvar *clm-ins* nil)

(defvar *clm-locsig-type* mus-interp-linear)
(declaim (type mus-locsig-interp *clm-locsig-type*))

(defvar *interrupted* 0)

(defvar *offset* 0)
(declaim (type non-negative-fixnum64 *offset*))

(defvar *clm-linked* nil)

(defvar *clm-with-sound-depth* 0)
(defvar *clm-within-with-sound* nil)
(defvar *clm-with-sound-body* nil)

(defvar *clm-mix-calls* nil)
(defvar *clm-mix-options* nil)

(defvar *open-input-explicit-output* nil)
(defvar *open-input-explicit-reverb* nil)
(defvar *open-input-verbose* nil)
(defvar *open-input-pathname* nil)
(defvar *open-input-truename* nil)

(defvar *clm-scaled-amp* nil)

(defvar *clm-dac-wait-default* nil)

(defvar *definstrument-hook* nil)

(defvar *force-recomputation* nil)

(defvar *ws-reverb-file* nil)

(defvar clm-start-time nil)

(defvar clm-last-begin-time 0)
(declaim (type non-negative-fixnum64 clm-last-begin-time))

(defvar clm-outfile-name nil)

(defvar clm-revfile-name nil)

(defvar last-open-input-file-name nil)

(defvar *dac-pid* nil)

(defvar last-dac-filename *clm-file-name*)

;;; From clm.c
(define-constant +max-clm-sinc-width+ 65536)
(define-constant +max-clm-src+ 65536d0)

;;; Defined only in cudere-clm (unrelated to the original CLM).

(defvar *to-snd* nil)

(defvar *clm-ugens-package* "CUDERE-CLM.UGENS")

(defvar *clm-optimize-settings* *standard-optimize-settings*)

(defvar *clm-logger-stream* incudine.util:*logger-stream*)
(declaim (type stream *clm-logger-stream*))
