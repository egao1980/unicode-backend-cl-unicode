(in-package #:unicode-backend-cl-unicode)

(defmethod backend-normalize ((backend cl-unicode-backend) string form)
  (declare (ignore backend))
  (let* ((cps (%cps-from-string string))
         (out (ecase form
                (:nfc (cl-unicode:normalization-form-c cps))
                (:nfd (cl-unicode:normalization-form-d cps))
                (:nfkc (cl-unicode:normalization-form-k-c cps))
                (:nfkd (cl-unicode:normalization-form-k-d cps))
                (:nfkc-casefold
                 (error 'unicode-unsupported
                        :capability :nfkc-casefold
                        :message "cl-unicode backend has no :nfkc-casefold")))))
    (%string-from-cps out)))

(defmethod backend-normalized-p ((backend cl-unicode-backend) string form)
  (string= string (backend-normalize backend string form)))

(defmethod backend-quick-check ((backend cl-unicode-backend) string form)
  (if (backend-normalized-p backend string form) :yes :no))

(defmethod backend-normalization-boundary-before-p ((backend cl-unicode-backend) code-point form)
  (declare (ignore backend code-point form))
  nil)

(defmethod backend-normalization-boundary-after-p ((backend cl-unicode-backend) code-point form)
  (declare (ignore backend code-point form))
  nil)

(defmethod backend-raw-decomposition ((backend cl-unicode-backend) code-point form)
  (declare (ignore backend form))
  (let ((d (cl-unicode:canonical-decomposition code-point)))
    (when d (%string-from-cps (if (listp d) d (list d))))))
