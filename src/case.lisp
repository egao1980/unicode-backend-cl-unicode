(in-package #:unicode-backend-cl-unicode)

(defun %mapping-cps (result)
  "Normalize cl-unicode mapping return (integer | list | nil) → list of code points."
  (cond ((null result) nil)
        ((integerp result) (list result))
        ((listp result) result)
        (t nil)))

(defun %map-string (string mapper)
  (with-output-to-string (out)
    (loop for c across (string string)
          for cps = (or (%mapping-cps (funcall mapper c)) (list (char-code c)))
          do (dolist (cp cps)
               (write-char (code-char cp) out)))))

(defmethod backend-casefold ((backend cl-unicode-backend) string &key)
  (declare (ignore backend))
  (%map-string string (lambda (c) (cl-unicode:case-fold-mapping c :want-code-point-p t))))

(defmethod backend-downcase ((backend cl-unicode-backend) string &key)
  (declare (ignore backend))
  (%map-string string (lambda (c) (cl-unicode:lowercase-mapping c :want-code-point-p t))))

(defmethod backend-upcase ((backend cl-unicode-backend) string &key)
  (declare (ignore backend))
  (%map-string string (lambda (c) (cl-unicode:uppercase-mapping c :want-code-point-p t))))

(defmethod backend-titlecase ((backend cl-unicode-backend) string &key)
  (declare (ignore backend))
  (%map-string string (lambda (c) (cl-unicode:titlecase-mapping c :want-code-point-p t))))

(defmethod backend-simple-casefold ((backend cl-unicode-backend) code-point)
  (declare (ignore backend))
  (or (first (%mapping-cps (cl-unicode:case-fold-mapping code-point :want-code-point-p t)))
      code-point))

(defmethod backend-simple-downcase ((backend cl-unicode-backend) code-point)
  (declare (ignore backend))
  (or (let ((r (cl-unicode:lowercase-mapping code-point :want-code-point-p t)))
        (if (listp r) (first r) r))
      code-point))

(defmethod backend-simple-upcase ((backend cl-unicode-backend) code-point)
  (declare (ignore backend))
  (or (let ((r (cl-unicode:uppercase-mapping code-point :want-code-point-p t)))
        (if (listp r) (first r) r))
      code-point))

(defmethod backend-simple-titlecase ((backend cl-unicode-backend) code-point)
  (declare (ignore backend))
  (or (let ((r (cl-unicode:titlecase-mapping code-point :want-code-point-p t)))
        (if (listp r) (first r) r))
      code-point))
