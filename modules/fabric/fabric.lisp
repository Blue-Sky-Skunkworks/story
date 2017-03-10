(in-package :story)

(define-story-module fabric
  :scripts ("fabric.min.js"))

(in-package :story-js)

(defpsmacro random (max) `(* ((@ *math random)) ,max))





