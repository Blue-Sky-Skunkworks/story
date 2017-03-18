(in-package :story)

(define-demo echo ((:echo :sample-images))
  (with-words (word "This is a set of test images to test echo, which
  should be lazily loading them for your pleasure and enjoyment! ...")
    (image :alt word :src (f "sample-images/~A.png" word)))
  (dotimes (x 100)
    (image :alt "random" :src (f "sample-images-random/~A.png" x))))

