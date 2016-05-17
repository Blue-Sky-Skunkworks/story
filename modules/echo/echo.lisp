(in-package :story)

(define-story-module echo
  :scripts ("echo.js" ("init-echo.js" init-echo))
  :files ("blank.png")
  :init ((register-image-processor 'echo-image-processor))
  :script-init ((initialize-echo)))

(defun echo-image-processor (args)
  (let (src)
    (append
     (iter (for (k v) on args by 'cddr)
       (cond
         ((eq k :src) (setf src v))
         (t (appending (list k v)))))
     (list
      :src "blank.png"
      :data-echo src))))

(in-package :story-js)

(define-script init-echo
  (defvar *echo-initialized*)

  (defun initialize-echo ()
    (when *echo-initialized* (console "Re-initializing echo."))
    (setf *echo-initialized* t)
    ((@ echo init)
     (create :offset 100
             :throttle 250
             :unload nil
             ;; :callback (lambda (el op) (console el op))
             )))

  (defun echo-watch-scrolling (container-id)
    ((@ (id container-id) add-event-listener) "content-scroll" echo-handle-scroll))

  (defun echo-handle-scroll ()
    ((@ echo render)))

  )


