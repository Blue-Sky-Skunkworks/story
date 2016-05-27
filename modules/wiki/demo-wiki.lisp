(in-package :story)

(defparameter *sample-wiki-directory* (story-modules-file "wiki/sample-wiki/"))


    ;; (toolbar :class "wiki-controls"
    ;;          (:span :class "title")
    ;;          (icon-button :id "wiki-view-toggle" :icon "list" :onclick "toggleWikiView();")
    ;;          (icon-button :icon "cloud" :onclick "viewWikiSource();")
    ;;          (icon-button :icon "editor:mode-edit" :onclick "editWiki();")
    ;;          (icon-button :icon "refresh" :onclick "refreshWiki();"))


(define-demo wiki ((:wiki) :dispatches ((:folder "/sample-wiki/" *sample-wiki-directory*)))
  (:div :id "wiki-title")
  (:br)
  (:div :id "wiki-controls"
        (:button :onclick (ps (view-wiki-home)) "Home") " "
        (:button :onclick (ps (reload-wiki)) "Reload") " "
        (:button :onclick (ps (edit-wiki)) "Edit") " "
        (:button :onclick (ps (view-wiki-source)) "View Source"))
  (:div :id "wiki-body")
  (script
    (setup-wiki :title "Story Sample Wiki"
                :url "/sample-wiki/"
                :title-id "wiki-title"
                :body-id "wiki-body")
    (page "/:page" (lambda (ctx) (fetch-wiki-page (@ ctx params page))))
    (page "/" (lambda () (page "/Home")))
    (page (create :hashbang t))))

