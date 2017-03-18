(in-package :story)

(define-demo sample-images ((:sample-images))
  (with-words (word "This is a set of test images for your reading pleasure!")
    (image :alt word :src (f "sample-images/~A.png" word))))
