(in-package :story)

(defparameter *welcome-text*
         "
__      __   _                    _____      ___ _                _
\\ \\    / /__| |__ ___ _ __  ___  |_   _|__  / __| |_ ___ _ _ _  _| |
 \\ \\/\\/ / -_) / _/ _ \\ '  \\/ -_)   | |/ _ \\ \\__ \\  _/ _ \\ '_| || |_|
  \\_/\\_/\\___|_\\__\\___/_|_|_\\___|   |_|\\___/ |___/\\__\\___/_|  \\_, (_)
                                                             |__/
")

(defun initialize-story (&optional quiet)
  (require 'story-modules)
  (unless quiet (format t "~A~%" (blue *welcome-text* :effect :bright)))
  (initialize-magic)
  (start-server))

(defun initialize ()
  (format t "Welcome to Story!~%")
  (initialize-story))

