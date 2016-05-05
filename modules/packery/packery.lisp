(in-package :story)

(define-story-module packery
    :scripts ("packery.pkgd.min.js"
              ("packery.js" packery)))

(in-package :story-js)

(define-script packery
  (defun setup-packing (container-id item &optional (gutter 20))
    (let ((container (id container-id)))
      (if (@ container pack)
          ((@ container pack layout))
          (setf (@ container pack) (new (*packery container
                                                  (create :item-selector (+ "." item)
                                                          :gutter gutter))))))))
