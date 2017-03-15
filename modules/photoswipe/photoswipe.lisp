(in-package :story)

(define-story-module photoswipe
    :directories ("ps")
    :stylesheets ("ps/photoswipe.css" "ps/default-skin/default-skin.css")
    :scripts ("/ps/photoswipe.js" "photoswipe-ui-default-modified.js" ("ps.js" photoswipe))
    :prefixes ("photoswipe.html"))

(in-package :story-js)

(define-script photoswipe
  (defun show-image-gallery (items &optional (index 0))
    (let ((gallery
           (new (*photo-swipe (id "kspswp")
                              *photo-swipe-u-i-_default
                              ;; KLUDGE had to alter the js to rename the default variable
                              ;; couldn't get parenscript to output the capital "D"
                              items (create :index index))))))
    ((@ gallery init))))





