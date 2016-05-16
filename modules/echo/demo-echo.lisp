(in-package :story)

(define-demo echo (:echo :sample-images)
  (with-words (word "This is a set of test images to test echo, which
  should be lazily loading them for your pleasure and enjoyment!")
    (htm (:img :alt word :src "blank.png" :data-echo (format nil "sample-images/~A.png" word))))
  (dotimes (x 100)
    (htm (:img :alt "random" :src "blank.png" :data-echo (format nil "sample-images-random/~A.png" x))))
  (script (initialize-echo)))

