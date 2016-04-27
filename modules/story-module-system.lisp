(in-package :asdf/user)

(defclass story-module-system (asdf:system)
  ((category :accessor system-category :initarg :category :initform nil)
   (icon :accessor system-icon :initarg :icon :initform nil)))

(export '(system-category system-icon))
