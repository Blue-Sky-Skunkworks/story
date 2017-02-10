(in-package :story)

(define-story-module suncalc
  :scripts ("suncalc/suncalc.js"
            ("suncalc.js" suncalc)))

(in-package :story-js)

(define-script suncalc
  (defvar *sc-lat* 46.3894) ;; Standing Rock Indian Reservation
  (defvar *sc-lng* -100.5940)
  (defun set-suncalc-lat-lng (lat lng) (setf *sc-lat* lat *sc-lng* lng))
  (defun sun-times (&optional (date (new (*date))))
    ((@ *sun-calc get-times) date *sc-lat* *sc-lng*))
  (defun sun-position (date) ((@ *sun-calc get-position) date *sc-lat* *sc-lng*))
  (defun moon-times (&optional (date (new (*date))))
    ((@ *sun-calc get-moon-times) date *sc-lat* *sc-lng*))
  (defun moon-position (&optional (date (new (*date))))
    ((@ *sun-calc get-moon-position) date *sc-lat* *sc-lng*))
  (defun moon-illumination (date) ((@ *sun-calc get-moon-illumination) date))
  (defun rad-to-deg (rad) (/ (* rad 180) (@ *math *p-i)))
  (defun format-time (time) (+ ((@ time get-hours)) ":" ((@ time get-minutes)))))
