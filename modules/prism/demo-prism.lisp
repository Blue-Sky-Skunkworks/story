(in-package :story)

(define-demo prism ((:prism) :scripts ("prism/components/prism-bash.js"))
  (:pre (:code :class "language-css" "p { color: red }"))
  (:pre (:code :class "language-js" "random(1+2);"))
  (:pre (:code :class "language-bash" "echo 1+2")))
