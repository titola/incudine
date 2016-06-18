(defpackage "INCUDINE.FEATURES" (:use :cl))
(in-package "INCUDINE.FEATURES")

(load #.(load-time-value
          (merge-pathnames "config-specials.lisp" *load-pathname*)))

(let ((init-file (merge-pathnames ".incudinerc" (user-homedir-pathname))))
  (flet ((default-features ()
           (pushnew #+linux :jack-audio
                    #-linux :portaudio
                    *features*)))
    (cond ((probe-file init-file)
           (load init-file)
           (let ((jack-midi-p (and (boundp '*enable-jack-midi*) *enable-jack-midi*)))
             (cond ((boundp '*audio-driver*)
                    (case *audio-driver*
                      (:jack (when jack-midi-p
                               (pushnew :jack-midi *features*))
                             (pushnew :jack-audio *features*))
                      ((:portaudio :portaudio-jack) (pushnew :portaudio *features*))
                      (:dummy (pushnew :dummy-audio *features*))))
                   (jack-midi-p
                    (pushnew :jack-midi *features*)
                    (pushnew :jack-audio *features*))
                   (t (default-features)))))
          (t (default-features)))))

(in-package :cl-user)
(delete-package "INCUDINE.FEATURES")
