(in-package :story)

(defmethod acceptor-status-message ((acceptor web-acceptor) http-status-code &rest args &key &allow-other-keys)
  (call-next-method))

(defmethod acceptor-status-message ((acceptor web-acceptor) (http-status-code (eql +http-not-found+)) &rest args &key &allow-other-keys)
  (call-next-method))
