(in-package :story)

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

(ps:defpsmacro when-enter-or-tap (&body body)
  `(when (or (and (eql (@ event type) "keypress")
                  (eql (@ event key) "Enter"))
             (eql (@ event type) "tap"))
     (let ((el (@ event target)))
       ,@body)))

(export '(with-content parent-node first-child last-child child-nodes append-child
          insert-before remove-child with-content flush-dom when-enter-or-tap))


