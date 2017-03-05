(in-package :story)

(defparameter *sample-wiki-directory* (story-modules-file "wiki/sample-wiki/"))

(define-demo wiki ((:wiki :page) :dispatches ((:folder "/sample-wiki/" *sample-wiki-directory*)))
  (:div :id "wiki-title")
  (:br)
  (:div :id "wiki-controls"
        (:button :onclick (ps (view-wiki-home)) "Home") " "
        (:button :onclick (ps (reload-wiki)) "Reload") " "
        ;; (:button :onclick (ps (edit-wiki)) "Edit") " "
        ;; (:button :onclick (ps (view-wiki-source)) "View Source")
        )
  (:br)
  (:div :id "wiki-tree" (render-wiki-tree stream))
  (:div :id "wiki-body")
  (script
    (setup-wiki :title "Story Sample Wiki"
                :url "/sample-wiki/"
                :title-id "wiki-title"
                :body-id "wiki-body")
    (page "/:page" (lambda (ctx) (fetch-wiki-page (@ ctx params page))))
    (page "/" (lambda () (page "/Home")))
    (page (create :hashbang t))))

