(in-package :story)

(define-demo emoji ((:emoji))
  (iter (for code in (list-emoji-codes))
    (let ((char (code-char code)))
      (html
        (:span :style "display:inline-block;margin:10px;"
               (:i :class "emoji"
                   :title (f "~(~A~)" (subseq (prin1-to-string char) 2))
                   :style (f "background-image:url(\"/emoji/~(~X~).svg\");" code))
               (:br)
               (:span (fmt "~A ~X" char code)))))))
