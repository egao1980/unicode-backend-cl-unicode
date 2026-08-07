(in-package #:unicode-backend-cl-unicode)

;;; UTS #46 / IDNA2008 — algorithm ported from egao1980/cl-idna (Ultralisp pin untouched).
;;; Uses cl-unicode:idna-mapping + NFC.

(defconstant +delimiter+ (code-char #x2d))
(defconstant +initial-n+ #x80)
(defconstant +initial-bias+ 72)
(defconstant +base+ 36)
(defconstant +maxint+ #x7FFFFFFF)
(defconstant +tmin+ 1)
(defconstant +tmax+ 26)
(defconstant +damp+ 700)
(defconstant +skew+ 38)
(defconstant +zero+ #x30)
(defconstant +small-a+ #x61)

(defun %adapt (delta numpoints first-time)
  (setf delta (if first-time (truncate delta +damp+) (truncate delta 2)))
  (incf delta (truncate delta numpoints))
  (+ (do ((k 0 (+ k +base+)))
         ((<= delta (truncate (* (- +base+ +tmin+) +tmax+) 2)) k)
       (setf delta (truncate delta (- +base+ +tmin+))))
     (truncate (* (1+ (- +base+ +tmin+)) delta) (+ delta +skew+))))

(defun %basic-p (c) (< c +initial-n+))

(defun %encode-digit (digit)
  (code-char (+ digit (if (< digit 26) +small-a+ (- +zero+ 26)))))

(defun %decode-digit (cp)
  (cond ((< (- cp 48) 10) (- cp 22))
        ((< (- cp 65) 26) (- cp 65))
        ((< (- cp 97) 26) (- cp 97))
        (t +base+)))

(defun %punycode-encode (code-points)
  (let* ((input-length (length code-points))
         (h 0) (b 0) (m +maxint+) (n +initial-n+) (delta 0)
         (bias +initial-bias+) (encodedp nil))
    (values
     (with-output-to-string (output)
       (loop for c-code in code-points
             do (when (%basic-p c-code)
                  (write-char (code-char c-code) output)
                  (incf b) (incf h)))
       (when (and (> b 0) (< h input-length))
         (write-char +delimiter+ output))
       (loop
         (unless (< h input-length) (return))
         (setf encodedp t m +maxint+)
         (loop for c-code in code-points
               when (and (>= c-code n) (> m c-code))
                 do (setf m c-code))
         (when (> (- m n) (truncate (- +maxint+ delta) (1+ h)))
           (error 'unicode-idna-error :message "punycode overflow"))
         (incf delta (* (- m n) (1+ h)))
         (setf n m)
         (loop for c-code in code-points
               do (when (and (< c-code n) (> (incf delta) +maxint+))
                    (error 'unicode-idna-error :message "punycode overflow"))
                  (when (= c-code n)
                    (let ((q (do* ((q delta (truncate (- q tee) (- +base+ tee)))
                                   (k +base+ (+ k +base+))
                                   (tt (- k bias) (- k bias))
                                   (tee (cond ((< tt +tmin+) +tmin+)
                                              ((>= k (+ bias +tmax+)) +tmax+)
                                              (t tt))
                                        (cond ((< tt +tmin+) +tmin+)
                                              ((>= k (+ bias +tmax+)) +tmax+)
                                              (t tt))))
                                 ((< q tee) q)
                               (write-char (%encode-digit (+ tee (rem (- q tee) (- +base+ tee))))
                                           output))))
                      (write-char (%encode-digit q) output))
                    (setf bias (%adapt delta (1+ h) (= h b))
                          delta 0)
                    (incf h)))
         (incf delta)
         (incf n)))
     encodedp)))

(defun %punycode-decode (code-points)
  (let ((output nil) (output-length 0) (n +initial-n+) (i 0)
        (bias +initial-bias+)
        (basic (or (position #x2d code-points :from-end t) 0)))
    (loop for char-code in (subseq code-points 0 basic)
          do (when (>= char-code #x80)
               (error 'unicode-idna-error :message "invalid punycode input"))
             (setf output (nconc output (list char-code)))
             (incf output-length))
    (loop with input = (if (zerop basic) code-points (subseq code-points (1+ basic)))
          with out = 0
          while input
          do (let ((oldi i))
               (loop with w = 1 with digit with tee
                     for k from +base+ by +base+
                     do (unless input
                          (error 'unicode-idna-error :message "punycode bad input"))
                        (setf digit (%decode-digit (car input))
                              input (rest input)
                              tee (cond ((<= k bias) +tmin+)
                                        ((>= k (+ bias +tmax+)) +tmax+)
                                        (t (- k bias))))
                        (when (>= digit +base+)
                          (error 'unicode-idna-error :message "punycode bad input"))
                        (incf i (* digit w))
                        (when (< digit tee) (return))
                        (setf w (* w (- +base+ tee))))
               (setf out (1+ output-length)
                     bias (%adapt (- i oldi) out (zerop oldi)))
               (incf n (truncate i out))
               (setf i (rem i out)
                     output (nconc (subseq output 0 i) (list n) (subseq output i)))
               (incf output-length)
               (incf i)))
    output))

(defun %split-dots (cps)
  (loop for start = 0 then (1+ pos)
        for pos = (position (char-code #\.) cps :start start)
        collect (subseq cps start (or pos (length cps)))
        while pos))

(defun %idna-map-cps (cps &key transitional std3)
  (let ((+ignored+ (cl-unicode:property-symbol "ignored"))
        (+deviation+ (cl-unicode:property-symbol "deviation"))
        (+valid+ (cl-unicode:property-symbol "valid"))
        (+mapped+ (cl-unicode:property-symbol "mapped"))
        (+disallowed-std3-valid+ (cl-unicode:property-symbol "disallowed_STD3_valid"))
        (+disallowed-std3-mapped+ (cl-unicode:property-symbol "disallowed_STD3_mapped")))
    (loop for code-point in cps
          for mapping = (cl-unicode:idna-mapping code-point)
          for status = (car mapping)
          unless (eql status +ignored+)
            nconc (cond
                    ((or (eql status +valid+)
                         (and (not transitional) (eql status +deviation+))
                         (and (not std3) (eql status +disallowed-std3-valid+)))
                     (list code-point))
                    ((or (and transitional (eql status +deviation+))
                         (eql status +mapped+)
                         (and (not std3) (eql status +disallowed-std3-mapped+)))
                     (let ((mapped (cadr mapping)))
                       (cond ((null mapped) nil)
                             ((listp mapped) mapped)
                             (t (list mapped)))))
                    (t (error 'unicode-idna-error
                              :message (format nil "disallowed code point U+~X" code-point)
                              :code-point code-point))))))

(defun %mark-p (cp)
  (member (cl-unicode:general-category cp) '("Mn" "Mc" "Me") :test #'string=))

(defun %check-label (code-points &key transitional std3 check-hyphens)
  (let* ((punycoded-p (and (> (length code-points) 3)
                           (equal '(#x78 #x6e #x2d #x2d) (subseq code-points 0 4))))
         (label (if punycoded-p
                    (%punycode-decode (subseq code-points 4))
                    code-points)))
    (unless (equal label (cl-unicode:normalization-form-c label))
      (error 'unicode-idna-error :message "label must be NFC"))
    (when check-hyphens
      (let ((minus (char-code #\-)))
        (when (or (eql (first label) minus)
                  (eql (car (last label)) minus)
                  (eql (third label) minus)
                  (eql (fourth label) minus))
          (error 'unicode-idna-error :message "hyphen in forbidden position"))))
    (when (find (char-code #\.) label)
      (error 'unicode-idna-error :message "label contains FULL STOP"))
    (when (and label (%mark-p (first label)))
      (error 'unicode-idna-error :message "label begins with combining mark"))
    (let ((+deviation+ (cl-unicode:property-symbol "deviation"))
          (+valid+ (cl-unicode:property-symbol "valid"))
          (+disallowed-std3-valid+ (cl-unicode:property-symbol "disallowed_STD3_valid")))
      (when (find-if
             (lambda (c)
               (let ((status (car (cl-unicode:idna-mapping c))))
                 (not (or (and (not std3) (eql status +disallowed-std3-valid+))
                          (if (or (not transitional) punycoded-p)
                              (or (eql status +valid+) (eql status +deviation+))
                              (eql status +valid+))))))
             label)
        (error 'unicode-idna-error :message "label has invalid IDNA status")))
    (values code-points label)))

(defun %prepare (name &key transitional std3)
  (cl-unicode:normalization-form-c
   (%idna-map-cps (%cps-from-string name) :transitional transitional :std3 std3)))

(defun %parse-idna-options (options)
  (values (find :transitional options)
          (not (find :no-std3 options))
          (not (find :no-check-hyphens options))))

(defun %domain-to-ascii (name &key transitional (std3 t) (check-hyphens t))
  (with-output-to-string (output)
    (loop for (component . rest) on (%split-dots (%prepare name :transitional transitional :std3 std3))
          do (multiple-value-bind (_ label)
                 (%check-label component
                               :transitional transitional
                               :std3 std3
                               :check-hyphens check-hyphens)
               (declare (ignore _))
               (multiple-value-bind (punycode encodedp) (%punycode-encode label)
                 (cond (encodedp
                        (write-string "xn--" output)
                        (write-string punycode output))
                       (t (write-sequence (mapcar #'code-char label) output)))
                 (when rest (write-char #\. output)))))))

(defun %domain-to-unicode (name &key transitional (std3 t) (check-hyphens t))
  (with-output-to-string (output)
    (loop for (component . rest) on (%split-dots (%prepare name :transitional transitional :std3 std3))
          do (multiple-value-bind (_ label)
                 (%check-label component
                               :transitional transitional
                               :std3 std3
                               :check-hyphens check-hyphens)
               (declare (ignore _))
               (write-sequence (mapcar #'code-char label) output)
               (when rest (write-char #\. output))))))

(defmethod backend-idna-map ((backend cl-unicode-backend) string &key std3 transitional)
  (declare (ignore backend))
  (%string-from-cps
   (%idna-map-cps (%cps-from-string string)
                  :transitional transitional
                  :std3 (if (eq std3 nil) nil t))))

(defmethod backend-idna-name-to-ascii ((backend cl-unicode-backend) name &key options)
  (declare (ignore backend))
  (multiple-value-bind (transitional std3 check-hyphens) (%parse-idna-options options)
    (%domain-to-ascii name :transitional transitional :std3 std3 :check-hyphens check-hyphens)))

(defmethod backend-idna-name-to-unicode ((backend cl-unicode-backend) name &key options)
  (declare (ignore backend))
  (multiple-value-bind (transitional std3 check-hyphens) (%parse-idna-options options)
    (%domain-to-unicode name :transitional transitional :std3 std3 :check-hyphens check-hyphens)))

(defmethod backend-idna-label-to-ascii ((backend cl-unicode-backend) label &key options)
  (backend-idna-name-to-ascii backend label :options options))

(defmethod backend-idna-label-to-unicode ((backend cl-unicode-backend) label &key options)
  (backend-idna-name-to-unicode backend label :options options))
