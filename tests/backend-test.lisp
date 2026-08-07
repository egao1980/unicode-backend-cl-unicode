(in-package #:unicode-backend-cl-unicode/tests)

(deftest backend-installed
  (ok (typep *unicode-backend* 'cl-unicode-backend))
  (ok (member :idna (backend-capabilities *unicode-backend*))))

(deftest general-category-and-normalize
  (ok (eq (general-category #\A) :lu))
  (ok (string= (normalize (map 'string #'code-char '(#x65 #x301)) :form :nfc)
               (string (code-char #x00E9)))))

(deftest casefold-eszett
  (ok (string= (casefold "ß") "ss")))

(deftest idna-buecher
  (ok (string= (idna-name-to-ascii "bücher.de") "xn--bcher-kva.de"))
  (ok (string= (idna-name-to-unicode "xn--bcher-kva.de") "bücher.de")))
