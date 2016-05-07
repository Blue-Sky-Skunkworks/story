(in-package :story)

(define-demo page (:page)
  (:div :id "id" "home")
  (:ul (:li (:span :onclick "page(\"/\")\;" "home"))
       (:li (:span :onclick "page(\"/page1\");" "page 1"))
       (:li (:span :onclick "page(\"/page2\");" "page 2")))
  (script
    (page "/" (lambda () (set-html "id" "home a")))
    (page "/page1" (lambda () (set-html "id" "page one")))
    (page "/page2" (lambda () (set-html "id" "page two")))
    (page "*" (lambda () (set-html "id" "unknown")))
    (page (create :hashbang t))))
