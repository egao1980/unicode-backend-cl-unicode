(in-package #:unicode-backend-cl-unicode)

(defclass cl-unicode-backend (unicode-backend) ()
  (:documentation "unicode-protocol backend over cl-unicode."))

(defvar *cl-unicode-backend* nil)

(defmethod backend-capabilities ((backend cl-unicode-backend))
  '(:properties :normalize :casefold :idna :script :char-name))

(defun use-cl-unicode-backend (&optional (backend (or *cl-unicode-backend*
                                                      (setf *cl-unicode-backend*
                                                            (make-instance 'cl-unicode-backend)))))
  (use-unicode-backend backend)
  backend)

;;; Install on load
(use-cl-unicode-backend)
