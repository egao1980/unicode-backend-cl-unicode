(in-package #:unicode-backend-cl-unicode)

(defun %kw (string)
  "\"Lu\" → :lu ; NIL → NIL."
  (when string
    (intern (string-upcase string) :keyword)))

(defun %cps-from-string (string)
  (map 'list #'char-code (string string)))

(defun %string-from-cps (cps)
  (map 'string #'code-char cps))

(defun %optionp (options key)
  (or (find key options :test #'eq)
      (getf options key)))
