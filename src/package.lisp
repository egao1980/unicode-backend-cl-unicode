(defpackage #:unicode-backend-cl-unicode
  (:use #:cl #:unicode-protocol)
  (:export #:cl-unicode-backend
           #:use-cl-unicode-backend
           #:*cl-unicode-backend*))

(in-package #:unicode-backend-cl-unicode)
