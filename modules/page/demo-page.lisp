(in-package :story)

(define-demo page (:page)
  (:div :id "id" "home")
  (:br)
  (:button :onclick "page(\"/\")\;" "home") " "
  (:button :onclick "page(\"/page1\");" "page 1") " "
  (:button :onclick "page(\"/page2\");" "page 2")
  (script
    (page "/" (lambda () (set-html "id" "home")))
    (page "/page1" (lambda () (set-html "id" "page one")))
    (page "/page2" (lambda () (set-html "id" "page two")))
    (page "*" (lambda () (set-html "id" "unknown")))
    (page (create :hashbang t))))
