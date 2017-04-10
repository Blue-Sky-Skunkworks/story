(in-package :story)

(defvar *object-presentors* nil)

(defmacro define-object-presentor (type &body body)
  `(progn
     (setf *object-presentors* (remove ',type *object-presentors* :key #'car :test #'equal))
     (push (list ',type ',(if (second body) `(progn ,@body) (first body))) *object-presentors*)))

(defvar *object-name-aliases* nil)

(defmacro define-object-name-alias (from to)
  `(pushnew (list ',from ',to) *object-name-aliases* :key #'car :test #'equal))

(define-object-presentor "#text" (@ el text-content))
(define-object-presentor "title" (@ el text-content))
(define-object-presentor "h1" (@ el text-content))
(define-object-presentor "link" (shref (@ el href)))

(define-object-presentor "Promise"
  (let* ((value (dom :span "unresolved"))
         (fn (lambda (arg)
               (setf (@ value text-content) "")
               ((@ value append-child) ((@ interface present) arg)))))
    ((@ el then) fn fn)
    value))

(define-object-presentor "script"
  ((@ (list
       (case (@ el type)
         ("text/javascript" "")
         (otherwise (@ el type)))
       (shref (@ el src)))
      join) " "))

(define-object-presentor "BatteryManager"
  (mkstr (* (@ el level) 100) "%" (when (@ el charging) " charging")))

(define-object-name-alias "CSSStyleDeclaration" "css")
(define-object-presentor "CSSStyleDeclaration" (text (@ el css-text)))

(define-template-method debugger-interface _present-obj (type el)
  `(let* ((shref (@ this _shorten-href))
          (interface this)
          (info (case type ,@*object-presentors*))
          (id (ignore-errors (@ el id))))
     (list (when info (dom (:span "info") info))
           (when ((@ type ends-with) "Error") (_present-error el))
           (when (and (stringp id) (plusp (@ id length))) (dom (:span "id") id)))))

(define-template-method debugger-interface present (element &optional fn-this)
  `(let ((type (type-of element))
         (aliases (create ,@(iter (for (from to) in *object-name-aliases*)
                              (appending (list from to))))))
     (cond
       ((eql type "number") element)
       ((eql type "boolean") (if element "true" "false"))
       ((eql type "function") type
        (dom (:span "desc" (((on-tap on-keypress) "_handlePresentEvent")
                            (presenting element) (tab-index 1)
                            (_fn-this fn-this)))
             (mkstr "[" (@ element name) "]")))
       ((eql type "object")
        (if (eql element nil)
            "null"
            (let* ((type (or (try ((@ element node-name to-lower-case)) (:catch (error) nil))
                             ((@ ((@ *object prototype to-string call) element) slice) 8 -1))))
              (dom (:span (+ "desc" (if (eql type "Error") " error" ""))
                          (((on-tap on-keypress) "_handlePresentEvent")
                           (presenting element) (tab-index 1)))
                   (text "<")
                   (text (case type
                           ("Error" (@ element name))
                           (otherwise (or (aref aliases type) type))))
                   ((@ this _present-obj call) this type element)
                   (text ">")))))
       ((eql type "string")
        (dom (:span "prestring" (((on-tap on-keypress) "_handleTextExpansion")))
             (+ "\"" element "\"")))
       ((eql type "array") (+ "[" ((@ element to-string)) "]"))
       ((eql type "undefined") type)
       (t (+ "UNHANDLED: " type)))))


