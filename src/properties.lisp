(in-package #:unicode-backend-cl-unicode)

(defun %property-alias (property)
  "Map protocol keyword → cl-unicode property name string."
  (case property
    (:alphabetic "Alphabetic")
    (:white-space "White_Space")
    (:lowercase "Lowercase")
    (:uppercase "Uppercase")
    (:hex-digit "Hex_Digit")
    (:id-start "ID_Start")
    (:id-continue "ID_Continue")
    (:xid-start "XID_Start")
    (:xid-continue "XID_Continue")
    (:ideographic "Ideographic")
    (:bidi-mirrored "Bidi_Mirrored")
    (:emoji "Emoji")
    (:emoji-presentation "Emoji_Presentation")
    (:extended-pictographic "Extended_Pictographic")
    (:posix-blank "Blank")
    (otherwise
     ;; :foo-bar → "Foo_Bar"
     (with-output-to-string (out)
       (let ((s (symbol-name property))
             (cap t))
         (loop for ch across s
               do (cond ((char= ch #\-)
                         (write-char #\_ out)
                         (setf cap t))
                        (cap
                         (write-char (char-upcase ch) out)
                         (setf cap nil))
                        (t (write-char (char-downcase ch) out)))))))))

(defmethod backend-binary-property-p ((backend cl-unicode-backend) code-point property)
  (declare (ignore backend))
  (let ((name (%property-alias property)))
    (or (cl-unicode:has-property code-point name)
        (cl-unicode:has-binary-property code-point name)
        ;; try without underscores e.g. Whitespace
        (cl-unicode:has-property code-point (remove #\_ name)))))

(defmethod backend-int-property ((backend cl-unicode-backend) code-point property)
  (declare (ignore backend))
  (ecase property
    (:general-category (%kw (cl-unicode:general-category code-point)))
    (:bidi-class (%kw (cl-unicode:bidi-class code-point)))
    (:canonical-combining-class (cl-unicode:combining-class code-point))
    (:block (%kw (cl-unicode:code-block code-point)))
    (:script (%kw (cl-unicode:script code-point)))
    (:east-asian-width nil)
    (:word-break (%kw (cl-unicode:word-break code-point)))))

(defmethod backend-script-extensions ((backend cl-unicode-backend) code-point)
  (list (backend-int-property backend code-point :script)))

(defmethod backend-char-name ((backend cl-unicode-backend) code-point &key (choice :unicode))
  (declare (ignore backend))
  (ecase choice
    (:unicode (cl-unicode:unicode-name code-point))
    (:alias nil)
    (:extended (or (cl-unicode:unicode-name code-point)
                   (cl-unicode:unicode1-name code-point)))))

(defmethod backend-lookup-name ((backend cl-unicode-backend) name)
  (declare (ignore backend))
  (cl-unicode:character-named name :want-code-point-p t))

(defmethod backend-numeric-value ((backend cl-unicode-backend) code-point)
  (declare (ignore backend))
  (cl-unicode:numeric-value code-point))

(defmethod backend-digit-value ((backend cl-unicode-backend) code-point &key (radix 10))
  (declare (ignore backend))
  (let ((v (cl-unicode:numeric-value code-point)))
    (when (and (integerp v) (< v radix))
      v)))

(defmethod backend-mirror-char ((backend cl-unicode-backend) code-point)
  (declare (ignore backend))
  (or (cl-unicode:bidi-mirroring-glyph code-point :want-code-point-p t)
      code-point))

(defmethod backend-age ((backend cl-unicode-backend) code-point)
  (declare (ignore backend))
  (cl-unicode:age code-point))

(defmethod backend-property-value-name ((backend cl-unicode-backend) property value &key short)
  (declare (ignore backend property short))
  (if (keywordp value)
      (string-downcase (symbol-name value))
      (princ-to-string value)))
