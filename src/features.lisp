(declaim
  (special *audio-driver* *enable-jack-midi* *sample-type* *c-compiler*
    *foreign-library-directories* *tlsf-block-align* *use-foreign-sample-p*
    *max-buffer-size* *frames-per-buffer* *portaudio-input-device*
    *portaudio-output-device* *client-name* *max-number-of-channels*
    *number-of-input-bus-channels* *number-of-output-bus-channels*
    *number-of-bus-channels* *rt-edf-heap-size* *nrt-edf-heap-size*
    *edf-heap-pool-size* *edf-heap-pool-grow* *sched-policy*
    *rt-priority* *nrt-priority* *receiver-default-priority*
    *max-number-of-nodes* *default-table-size* *default-bpm*
    *fade-curve* *standard-optimize-settings* *foreign-sample-pool-size*
    *foreign-rt-memory-pool-size* *foreign-nrt-memory-pool-size*
    *sndfile-buffer-size* *bounce-to-disk-guard-size* *default-header-type*
    *default-data-format* *osc-buffer-size* *osc-max-values*))

(let ((init-file (merge-pathnames ".incudinerc" (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)
    (if (boundp *audio-driver*)
        (case *audio-driver*
          (:jack (when (and (boundp *enable-jack-midi*) *enable-jack-midi*)
                   (pushnew :jack-midi *features*))
                 (pushnew :jack-audio *features*))
          ((:portaudio :portaudio-jack) (pushnew :portaudio *features*))
          (:dummy (pushnew :dummy-audio *features*)))
        (when (and (boundp *enable-jack-midi*) *enable-jack-midi*)
          (pushnew :jack-midi *features*)
          (pushnew :jack-audio *features*)))))
