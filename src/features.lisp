(defpackage "INCUDINE.FEATURES" (:use :cl))
(in-package "INCUDINE.FEATURES")

(load #.(load-time-value
          (merge-pathnames "config-specials.lisp" *load-pathname*)))

(let ((init-file (merge-pathnames ".incudinerc" (user-homedir-pathname))))
  (cond ((probe-file init-file)
         (load init-file)
         (let ((jack-midi-p (and (boundp *enable-jack-midi*) *enable-jack-midi*)))
           (if (boundp *audio-driver*)
               (case *audio-driver*
                 (:jack (when jack-midi-p
                          (pushnew :jack-midi *features*))
                        (pushnew :jack-audio *features*))
                 ((:portaudio :portaudio-jack) (pushnew :portaudio *features*))
                 (:dummy (pushnew :dummy-audio *features*)))
               (when jack-midi-p
                 (pushnew :jack-midi *features*)
                 (pushnew :jack-audio *features*)))))
        (t (pushnew #+linux :jack-audio
                    #-linux :portaudio
                    *features*))))

(in-package :cl-user)
(delete-package "INCUDINE.FEATURES")
