(in-package :story)

(define-story-module files
  :stylesheets (("files.css" files-css))
  :scripts (("files.js" files-js) "marked.js")
  :depends-on (:iron-request :images :prism :emoji))

(defun file-icon (mime)
  (let ((name (string-case (mime :default "black-question-mark-ornament")
                ("application/pdf" "closed-book")
                ("application/gzip" "compression")
                ("text/html" "page-facing-up")
                ("text/plain" "page-facing-up")
                ("image/png" "frame-with-picture")
                ("image/jpeg" "frame-with-picture")
                ("inode/directory" "file-folder"))))
    (f "~(~X~)" (emoji-code name))))

(defun create-file-listing (directory)
  (let* ((files (nconc
                 (remove-if #L(string= (pathname-name %) ".file-listing") (directory-files directory))
                 (subdirectories directory)))
         (count (length files)))
    (iter (for file in files)
      (for index from 1)
      (multiple-value-bind (description mime) (magic file)
        (let ((info (nconc (list (cons :name (if (string= mime "inode/directory")
                                                 (last1 (pathname-directory file))
                                                 (pathname-name file)))
                                 (cons :type (pathname-type file))
                                 (cons :mime mime)
                                 (cons :description description)
                                 (cons :size (ql-util:file-size file))
                                 (cons :icon (file-icon mime)))
                           (additional-file-information (ksymb (string-upcase mime)) file))))
          (collect info)
          (note "[~A/~A] ~28T ~10A ~@[~A~]" index count (assoc-value info :name)
                (assoc-value info :comment)))))))

(defun save-file-listing (directory)
  (let ((filename (f "~A.file-listing" directory)))
    (with-output-to-file (stream filename
                                 :if-does-not-exist :create :if-exists :supersede)
      (json:encode-json (create-file-listing directory) stream)
      (note "Wrote ~S." filename))))

(export 'save-file-listing)

(defun render-directory-listing (query path)
  (setf (content-type*) "text/html")
  (unless (probe-file (f "~A.file-listing" path))
    (save-file-listing path))
  (html-to-string
    (:html
      (:head
       (:title (fmt "listing for ~S." query))
       (:link :rel "import" :href "/polymer/polymer/polymer.html")
       (:link :rel "import" :href "/polymer/iron-ajax/iron-request.html")
       (:script :type "text/javascript" :src "/js.js")
       (:script :type "text/javascript" :src "/webcomponentsjs/webcomponents-lite.js")
       (:script :type "text/javascript" :src "/polymer/iron-request.js")
       (:script :type "text/javascript" :src "/files/files.js")
       (:link :rel "stylesheet" :type "text/css" :href "/emoji.css")
       (:link :rel "stylesheet" :type "text/css" :href "/files.css"))
      (:body
       (:div :id "files")))
    (script*
      `(render-file-listing "files" ,query))))

(setf *directory-listing-fn* 'render-directory-listing
      *file-argument-handler* 'render-file-viewer)

(in-package :story-js)

(defpsmacro on (event-name el &body body)
  `(setf (getprop ,el ,(format nil "~(on~A~)" event-name))
         (lambda (event) ,@body)))

(define-script files-js
  (defun fetch-json (url callback)
    (request url (lambda (val) (funcall callback (eval (+ "(" (@ val response) ")"))))))

  (defun fetch-file-listing (url callback)
    (fetch-json (+ url ".file-listing") callback))

  (defun create-headings (parent)
    (create-el-html* ("tr" parent)
               (:th)
               (when *show-images* (htm (:th "thumbnail")))
               (:th "name") (:th "type") (:th "size") (:th "width") (:th "height")
               (when *show-comments* (htm (:th "comment")))
               (when *show-descriptions* (htm (:th "description")))))

  (defun row-url (row)
    (+ *file-listing-url* (@ row name)
       (if (@ row type) "." "")
       (if (@ row type) (@ row type) "")))

  (defun select-row (row)
    (visit-url (+ (row-url row)
                  (if (eql (@ row mime) "inode/directory")
                      "/"
                      "?view=t"))))

  (defvar *select-row-fn* (lambda (row) (select-row row)))

  (defun file-icon (icon)
    (htm (:i :class "emoji"
             :style (+ "background-image:url('/emoji/" icon ".svg');"))))

  (defun create-row (parent data &optional index)
    (on "click"
        (create-el-html*
         ("tr" parent)
         (:td (when (@ data icon) (file-icon (@ data icon))))
         (when *show-images*
           (htm (:td (when (@ data thumbnail)
                       (htm (:img :src (+ "data:image/png;base64," (@ data thumbnail))))))))
         (:td :nowrap t (@ data name))
         (:td (@ data type))
         (:td (@ data size))
         (:td (@ data width))
         (:td (@ data height))
         (when *show-comments* (htm (:td (@ data comment))))
         (when *show-descriptions* (htm (:td (@ data description)))))
        (funcall *select-row-fn* data)))

  (defun create-grid-el (parent data &optional index)
    (on "click"
        (create-el-html* ("div" parent :class "grid-el")
                   (when *show-images*
                     (when (@ data thumbnail)
                       (htm (:img :src (+ "data:image/png;base64," (@ data thumbnail))))))
                   (:div :class "name"
                         (when (@ data icon) (file-icon (@ data icon)))
                         " "
                         (@ data name))
                   (when *show-comments*
                     (htm (:div :class "comment" (@ data comment))))
                   (when *show-descriptions*
                     (htm (:div :class "desc" (@ data description)))))
        (funcall *select-row-fn* data)))

  (defun create-controls (parent &optional class-prefix)
    (set-html* (create-element "tr" parent "controls")
               (:td :colspan 5
                    (:button :style "margin-right:20px;" :onclick "toggleShowAsGrid()"
                      (if *show-as-grid* "show as list" "show as grid"))
                    (:button :style "margin-right:20px;" :onclick "toggleShowImages()"
                      (if *show-images* "hide thumbnails" "show thumbnails"))
                    (:button :style "margin-right:20px;" :onclick "toggleShowComments()"
                      (if *show-comments* "hide comments" "show comments"))
                    (:button :onclick "toggleShowDescriptions()"
                      (if *show-descriptions* "hide descriptions" "show descriptions")))))

  (defvar *show-descriptions* nil)
  (defvar *show-comments* t)
  (defvar *show-images* t)
  (defvar *show-as-grid* t)
  (defvar *show-controls* t)

  (defvar *file-listing*)
  (defvar *file-listing-url*)
  (defvar *create-headings-fn* (lambda (parent) (create-headings parent)))
  (defvar *create-row-fn* (lambda (parent row) (create-row parent row)))
  (defvar *create-grid-el-fn* (lambda (parent row) (create-grid-el parent row)))
  (defvar *create-controls-fn* (lambda (parent row) (create-controls parent)))

  (defun identity (el) el)

  (defun render-file-listing (container url &key (rerender-from-cache t)
                                              parent-id
                                              (parent-type "table") (class-name "files")
                                              (create-row-fn *create-row-fn*)
                                              (create-grid-el-fn *create-grid-el-fn*)
                                              (create-controls-fn *create-controls-fn*)
                                              (create-headings-fn *create-headings-fn*)
                                              (row-filter identity)
                                              continuation)
    (let ((div (id container)))
      (when (@ div first-child) (remove-node (@ div first-child)))
      (let ((parent (create-element (if *show-as-grid* "div" parent-type) div class-name)))
        (when parent-id (setf (@ parent id) parent-id))
        (setf *file-listing* parent *file-listing-url* url)
        (when (and create-controls-fn *show-controls*) (funcall create-controls-fn parent))
        (when (and (not *show-as-grid*) create-headings-fn) (funcall create-headings-fn parent))
        (let ((fn
                (lambda (rows)
                  (setf (@ div rows) (mapcar row-filter rows))
                  (loop for row in rows
                        for index from 0
                        do (funcall (if *show-as-grid* create-grid-el-fn create-row-fn)
                                    parent row index))
                  (when continuation (funcall continuation div)))))
          (if (and (@ div rows) rerender-from-cache)
              (funcall fn (@ div rows))
              (fetch-file-listing url fn))))))

  (defun rerender-listing ()
    (let ((container (@ *file-listing* parent-node id)))
      (remove-node *file-listing*)
      (render-file-listing container *file-listing-url* :rerender t)))

  (defun toggle-show-descriptions ()
    (setf *show-descriptions* (not *show-descriptions*))
    (rerender-listing))

  (defun toggle-show-comments ()
    (setf *show-comments* (not *show-comments*))
    (rerender-listing))

  (defun toggle-show-images ()
    (setf *show-images* (not *show-images*))
    (rerender-listing))

  (defun toggle-show-as-grid ()
    (setf *show-as-grid* (not *show-as-grid*))
    (rerender-listing))

  (defun render-markdown (el query)
    (request query
             (lambda (val)
               (set-html (id el) (marked (@ val response)))))))

(in-package :story-css)

(defun files-css ()
  (css
   '((".files td" :padding 5px 20px 5px 0px)
     (".files tr" :cursor pointer)
     (".files tr:hover" :background-color "#888")
     ("div.grid-el" :width 200 :background-color "#DDD" :color "#222"
                    :margin 10px :padding 20px :display "inline-block")
     ("div.grid-el .name" :margin "10px 0 10px 0")
     ("div.grid-el .comment" :margin "10px 0 10px 0")
     ("div.grid-el .desc" :margin "10px 0 10px 0"))))
