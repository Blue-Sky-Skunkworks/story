(in-package :story)

(define-demo photoswipe ((:photoswipe :sample-images))
  (:style (str ".pswp__bg {background:white;}"))
  (:button :onclick "showPhotoswipeDemo();" "Again")
  (script*
    `(progn
       (defun show-photoswipe-demo ()
         (show-image-gallery
          (ps:array
           ,@(with-words (word "Photoswipe is working!")
                         (collect `(create :src ,(format nil "/sample-images/~A.png" word) :w 400 :h 200))))))
       (show-photoswipe-demo))))
