(defpackage #:unicode-backend-cl-unicode/tests
  (:use #:cl #:rove #:unicode-protocol #:unicode-backend-cl-unicode))

(in-package #:unicode-backend-cl-unicode/tests)

(defun %s (&rest cps)
  (map 'string #'code-char cps))

(deftest backend-installed
  (ok (typep *unicode-backend* 'cl-unicode-backend))
  (dolist (cap '(:properties :normalize :casefold :idna :script :char-name))
    (ok (member cap (backend-capabilities *unicode-backend*))
        (format nil "capability ~s" cap)))
  (ok (not (member :breaks (backend-capabilities *unicode-backend*))))
  (ok (not (member :uset (backend-capabilities *unicode-backend*)))))

(deftest general-category-ascii
  (ok (eq (general-category #\A) :lu))
  (ok (eq (general-category #\a) :ll))
  (ok (eq (general-category #\1) :nd))
  (ok (eq (general-category #\Space) :zs)))

(deftest binary-predicates
  (ok (alphabetic-p #\A))
  (ok (not (alphabetic-p #\1)))
  (ok (uppercase-p #\A))
  (ok (lowercase-p #\a))
  (ok (digit-p #\7))
  (ok (whitespace-p #\Space))
  (ok (letter-p #\Z))
  (ok (letter-or-digit-p #\9)))

(deftest numeric-and-names
  (ok (= (numeric-value #\5) 5))
  (ok (search "LATIN CAPITAL LETTER A" (unicode-name #\A)))
  (ok (= (lookup-name "LATIN CAPITAL LETTER A") #x0041)))

(deftest script-keywords
  (ok (eq (script #\A) :latin)))

(deftest nfc-nfd
  (let* ((decomp (%s #x65 #x301))
         (comp (string (code-char #x00E9))))
    (ok (string= (normalize decomp :form :nfc) comp))
    (ok (string= (normalize comp :form :nfd) decomp))
    (ok (normalized-p comp :form :nfc))))

(deftest nfkc-ligature
  (ok (string= (normalize (string (code-char #xFB01)) :form :nfkc) "fi")))

(deftest casefold-and-string-case
  (ok (string= (casefold "ß") "ss"))
  (ok (string= (casefold "Straße") "strasse"))
  (ok (string= (downcase "AbC") "abc"))
  (ok (string= (upcase "AbC") "ABC")))

(deftest idna-roundtrip
  (ok (string= (idna-name-to-ascii "bücher.de") "xn--bcher-kva.de"))
  (ok (string= (idna-name-to-unicode "xn--bcher-kva.de") "bücher.de"))
  (ok (string= (idna-name-to-ascii "example.com") "example.com"))
  (ok (string= (idna-label-to-ascii "bücher") "xn--bcher-kva")))
