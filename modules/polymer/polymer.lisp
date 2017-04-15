(in-package :story)

(defmacro define-polymer-macros (prefix &rest names)
  `(progn
     ,@(iter (for name in names)
             (appending
              `((defmacro ,name (&body body)
                  `(html (,,(ksymb prefix '- name ) ,@body)))
                (export ',name))))))

(define-story-module polymer
    :directories (("imports" "polymer") "webcomponentsjs")
    :scripts ("/webcomponentsjs/webcomponents-lite.js")
    :imports ("polymer/polymer"))

(define-story-module font-roboto :extends :polymer :imports ("font-roboto/roboto"))

(defmacro define-polymer-module (name &key (helpers t) depends-on init)
  (let*  ((raw (symbol-name name))
          (pos (position #\- raw))
          (module (subseq raw 0 pos))
          (rest (subseq raw (1+ pos))))
    `(progn
       (define-story-module ,name
         :extends :polymer
         :depends-on ,depends-on
         :imports (,(f "~(~A/~A~)" name name))
         :init ,init)
       ,@(when helpers `((define-polymer-macros ,(symb module) ,(symb rest)))))))

(define-polymer-module iron-meta)
(define-story-module iron-flex-layout :extends :polymer :imports ("iron-flex-layout/iron-flex-layout-classes"))

(define-polymer-module iron-icons)
(defmacro define-iron-icons (&rest  names)
  `(progn
     ,@(iter (for name in names)
         (collect
             `(define-story-module ,(symb 'iron-icons- name)
                :extends :polymer :imports (,(f "iron-icons/~(~A~)-icons" name)))))))
(define-iron-icons communication device editor hardware image maps notification places social)

(define-polymer-module iron-icon)
(define-polymer-module iron-pages)
(define-polymer-module iron-collapse)
(define-polymer-module iron-image :depends-on (:images)
  :init ((register-image-arguments :sizing :position)))

(defmacro image (&rest args)
  `(render-iron-image stream ,@args))

(defun render-iron-image (stream &rest args)
  (format stream "<iron-image ")
  (iter (for (k v) on (process-image-args args) by 'cddr)
    (format stream "~(~A~)=~S " k v))
  (format stream "></iron-image>"))

(define-polymer-module iron-ajax)
(define-story-module iron-request :extends :polymer
  :imports ("iron-ajax/iron-request")
  :scripts (("iron-request.js" iron-request)))

(define-polymer-module paper-button)
(define-polymer-module paper-icon-button)
(define-polymer-module paper-material)
(define-polymer-module paper-card)
(define-polymer-module paper-ripple)
(define-polymer-module paper-fab)
(define-polymer-module paper-item)
(define-polymer-module paper-header-panel)
(define-polymer-module paper-toolbar)
(define-polymer-module paper-listbox)
(define-polymer-module paper-dropdown-menu)
(define-polymer-module paper-drawer-panel)
(define-polymer-module paper-input)
(define-polymer-module paper-menu)
(define-polymer-module paper-menu-button)

(define-story-module paper-textarea :extends :polymer :imports ("paper-input/paper-textarea"))
(define-polymer-macros paper textarea)




(define-story-module neon-animated-pages :extends :polymer :imports ("neon-animation/neon-animated-pages"))
(define-story-module neon-animatable :extends :polymer :imports ("neon-animation/neon-animatable")
  ;;:production-import-fix neon-animation-import-fix
  )

(defun neon-animation-import-fix (text stream)
  (multiple-value-bind (ms me) (scan (create-scanner "\\.\\./web-animations-js" :single-line-mode t) text)
    (princ (subseq text 0 ms) stream)
    (princ "polymer/web-animations-js" stream)
    (princ (subseq text me) stream)))

(define-polymer-macros neon animatable animated-pages)

(defmacro define-neon-animations (&rest names)
  `(progn
     ,@ (iter (for name in names)
              (collect `(define-story-module ,(symb name '-animation)
                            :extends :polymer
                            :imports (,(f "neon-animation/animations/~(~A~)-animation" name)))))))

(define-neon-animations fade-in fade-out scale-down scale-up slide-down slide-up
                        slide-from-top slide-from-bottom slide-left slide-right
                        slide-from-left slide-from-right transform hero
                        ripple reverse-ripple cascase opaque)

(define-story-module google-map :extends :polymer :imports ("google-map/google-map"))
(defmacro google-map (&body body) `(html (:google-map ,@body)))
(defmacro map-marker (&body body) `(html (:google-map-marker ,@body)))
(export '(google-map map-marker))

(ps:defpsmacro _dom (el) `((@ *polymer dom) ,el))
(ps:defpsmacro parent-node (node) `(@ (_dom ,node) parent-node))
(ps:defpsmacro first-child (node) `(@ (_dom ,node) first-child))
(ps:defpsmacro last-child (node) `(@ (_dom ,node) last-child))
(ps:defpsmacro child-nodes (parent) `(@ (_dom ,parent) child-nodes))
(ps:defpsmacro append-child (parent node) `((@ (_dom ,parent) append-child) ,node))
(ps:defpsmacro insert-before (parent node before-node)
  `((@ (_dom ,parent) insert-before) ,node ,before-node))
(ps:defpsmacro remove-child (parent node) `((@ (_dom ,parent) remove-child) ,node))
(ps:defpsmacro with-content (ids &body body) `(with-slots ,ids (@ this $) ,@body))
(ps:defpsmacro flush-dom () `((@ *polymer dom flush)))

(export '(with-content))

(in-package :story-js)

(define-script iron-request
  (defun request (url response-handler &optional (error-handler default-request-error-handler))
    (let* ((req (create-element "iron-request"))
           (promise ((@ req send) (create :url url))))
      ((@ promise then) response-handler error-handler)))

  (defun default-request-error-handler (val)
    (console "error in request" val)))

(in-package :story-css)

(defmacro var (name)
  (format nil "var(~(~A~))" name))

;; red pink purple deep-purple indigo blue light-blue cyan teal green
;; light-green lime yellow amber orange deep-orange brown grey blue-grey

(defparameter *polymer-color-indexes* '(50 100 200 300 400 500 600 700 800 900))
(defparameter *polymer-saturated-color-indexes* '(100 200 400 700))

(defun %color (name number &optional saturated)
  (unless (member number *polymer-color-indexes*)
    (error "Unvalid color index ~A." number))
  (when (and saturated (not  (member number *polymer-saturated-color-indexes*)))
    (error "Unvalid saturated color index ~A." number))
  (assert (or (null saturated) (member number '(100 200 400 700))))
  (format nil "var(--paper-~(~A~)-~@[~A~]~A)" name (and saturated "a") number))

(defmacro color (name number &optional saturated)
  (%color name number saturated))

(defmacro app (what)
  (format nil "{ @apply(~(~A~)); }" what))

