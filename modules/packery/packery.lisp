(in-package :story)

(define-story-module packery
    :scripts ("packery.pkgd.min.js"
              ("packery.js" packery)))

(in-package :story-js)

(define-script packery
  (defun pack (container-id &key (item "pack") (gutter 20))
    (let ((container (if (stringp container-id) (id container-id) container-id)))
      (if (@ container pack-object)
          ((@ container pack-object layout))
          (setf (@ container pack-object) (new (*packery container
                                                         (create :item-selector (+ "." item)
                                                                 :gutter gutter))))))))
