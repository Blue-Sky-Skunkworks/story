(in-package :story)

(define-story-module emoji
  :directories (("svg" "emoji"))
  :stylesheets (("emoji.css" emoji-css)))

(defvar *emoji*)

(defparameter *emoji-include-regional-indicator-range* nil)

(defun normalize-unicode-name (name) (substitute #\- #\_ (string-downcase name)))
(defun normalize-unicode-code (code) (normalize-unicode-name (subseq (prin1-to-string (code-char code)) 2)))

(defun emoji-code (name)
  (or (gethash (normalize-unicode-name name) *emoji*)
      (progn
        (warn "Unknown emoji ~S." name)
        (char-code #\no_entry_sign))))

(defun load-emoji ()
  (setf *emoji* (make-hash-table :test 'equal))
  (mapc #L(setf (gethash (normalize-unicode-code %) *emoji*) %)
        (sort
         (iter (for path in (directory-files (story-modules-file "emoji/svg/")))
           (when (equal (pathname-type path) "svg")
             (let ((name (pathname-name path)))
               (when (> (length name) 3)
                 (if-let (pos (position #\- name))
                   (when (and *emoji-include-regional-indicator-range* (= (length name) 11))
                     (appending (iter (for i from (parse-integer name :end pos :radix 16) to
                                           (parse-integer name :start (1+ pos) :radix 16))
                                  (collect i))))
                   (collect (parse-integer name :radix 16)))))))
         '<))
  (setup-emoji-aliases))

(defparameter *emoji-aliases*
  '(("smile" #\smiling_face_with_open_mouth_and_smiling_eyes)))

(defun setup-emoji-aliases ()
  (iter (for (name char) in *emoji-aliases*)
    (setf (gethash name *emoji*) (char-code char))))

(load-emoji)

(defun list-emoji-codes ()
  (sort (iter (for (k v) in-hashtable *emoji*) (collect v)) '<))

(defun show-emoji-table (&key with-hex (columns 5 columns-set) (max-text-size 20))
  (when (and with-hex (not columns-set)) (setf columns 4))
  (print-table
   (group
    (iter (for code in (list-emoji-codes))
      (let ((char (code-char code)))
        (appending
         (nconc
          (list (f "~C " char))
          (when with-hex (list (f "~(~X~)" code)))
          (list (as-string (print-with-ellipses (subseq (f "~(~S~)" char) 2) :max max-text-size)))))))
    (* columns (if with-hex 3 2)))))

(defun save-emoji-table ()
  (with-output-to-file (*standard-output* (story-modules-file "emoji/emoji-table.txt")
                                          :if-exists :supersede :if-does-not-exist :create)
    (show-emoji-table :with-hex t)))

(defmacro emoji (name)
  `(html (:i :class "emoji"
             :style ,(f "background-image:url(\"/emoji/~(~X~).svg\");" (emoji-code name)))))

(in-package :story-css)

(defun emoji-css ()
  (css
   `((".emoji" :display inline-block
               :height 3em
               :width 3em
               :margin "0 0.15em 0 0.3em"
               :vertical-align -0.3em
               :background-repeat no-repeat
               :background-position "center center"
               :background-size "3em 3em"))))
