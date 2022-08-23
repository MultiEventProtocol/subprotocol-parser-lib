(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :anaphora :optima :fare-quasiquote-extras
                  :fare-quasiquote-optima)))

(use-package '(:alexandria :anaphora :optima))


(defparameter *sexp* (read-from-string (read-file-into-string "mep.sexp")))


(defun maptree-transform (predicate-transformer tree)
  (multiple-value-bind (t-tree control)
      (aif (funcall predicate-transformer tree)
           it                       ;; replace returned value
           (values tree #'mapcar))  ;; if return nil - return
    (if (and (consp t-tree) ;; if t-tree is tree
             control)       ;; and control is exists
        (let ((result))     ;; then result will be result of recurse funcall
          (setf result
                (funcall control
                         #'(lambda (x)
                             (maptree-transform predicate-transformer x))
                         t-tree))
          result) ;; return result
        ;; else - just return t-tree
        t-tree)))

(defmacro -> (x &optional form &rest more)
  "Thread the expr through the forms. Insert X as the second item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
second item in second form, etc."
  (cond
    ((null form) x)
    ((null more) (if (listp form)
                     `(,(car form) ,x ,@(cdr form))
                     (list form x)))
    (:else `(-> (-> ,x ,form) ,@more))))

;; (sb-cltl2:macroexpand-all '(-> 'first (cons 'second) (cons 'third)))
;; => (CONS (CONS 'FIRST 'SECOND) 'THIRD)

(defmacro ->> (x &optional form &rest more)
  "Thread the expr through the forms. Insert X as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, insert the first form as the
last item in second form, etc."
  (cond
    ((null form) x)
    ((null more) (if (listp form)
                     `(,@form ,x)
                     (list form x)))
    (:else `(->> (->> ,x ,form) ,@more))))

;; (sb-cltl2:macroexpand-all '(->> 'first (cons 'second) (cons 'third)))
;; => (CONS 'THIRD (CONS 'SECOND 'FIRST))


(defun transform-struct-member (param)
  (maptree-transform
   #'(lambda (par)
       (match par
         ((and (property :STRUCT-NAME name)
               (property :TYPE type))
          `(:member-struct-name ,name
            :member-struct-type ,(match type
                                   ((property :ELT-TYPE-NAME elt-type)
                                    (cadr elt-type))
                                   ((property :IDENTIFIER-PATH id-path)
                                    (cadr (car (cadr id-path))))
                                   (otherwise :not-implemented))))))
   param))

(defun transform-uplift-members (param)
  (maptree-transform
   #'(lambda (par)
       (match par
         ((property :MEMBER members)  `(:struct-members ,members))))
   param))

(defun transform-uplift-struct-def (param)
  (maptree-transform
   #'(lambda (par)
       (match par
         ((property :STRUCT-DEF struct-def)
          (match struct-def
            ((and (property :STRUCT struct-name)
                  (property :CONTENTS contents))
             `(:struct-def ,struct-name :contents ,contents))))))
   param))


(defparameter *new*
  (->> '(:CTRACT-BODY-ELT
         ((:STRUCT-DEF
           (:STRUCT "ContractId"
            :CONTENTS (:MEMBER
                       ((:STRUCT-NAME "network"
                         :TYPE (:ELT-TYPE-NAME (:STRING STRING)))
                        (:STRUCT-NAME "contractAddr"
                         :TYPE (:ELT-TYPE-NAME (:STRING STRING)))))))
          (:STRUCT-DEF
           (:STRUCT "ContractEvent"
            :CONTENTS (:MEMBER
                       ((:STRUCT-NAME "contractId"
                         :TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "ContractId")))))
                        (:STRUCT-NAME "EventId"
                         :TYPE (:ELT-TYPE-NAME (:STRING STRING)))))))))
       (transform-struct-member)
       (transform-uplift-members)
       (transform-uplift-struct-def)))


(defparameter *indent* 1)

(defun ind ()
  (make-string (* 2 *indent*) :initial-element #\Space))

(defmacro with-indent (&body body)
  `(progn
     (incf *indent*)
     (prog1 ,@body
       (decf *indent*))))

;; (macroexpand-1
;;  '(with-indent (mapcar #'member-struct-name (getf par :struct-members))))

(defun member-struct-name (par)
  (format nil "~A~A: ~A"
          (ind)
          (getf par :member-struct-name)
          (getf par :member-struct-type)))

;; (member-struct-name
;;  '(:MEMBER-STRUCT-NAME "EventId" :MEMBER-STRUCT-TYPE "String"))

(defun struct-members (par)
  (format nil "~{~a~^,~%~}"
          (with-indent
            (mapcar #'member-struct-name (getf par :struct-members)))))

;; (struct-members
;;  '(:STRUCT-MEMBERS
;;    ((:MEMBER-STRUCT-NAME "First" :MEMBER-STRUCT-TYPE "String")
;;     (:MEMBER-STRUCT-NAME "Second" :MEMBER-STRUCT-TYPE "String"))))


(defun %ctract-body-elt (par)
  (format nil "~A// contract body elt comment ~%~{~A~}~%"
          (ind)
          (with-indent
            (mapcar #'outer (cadr par)))))

(defun %struct-def (par)
  (format nil "~Astruct ~A {~%~A~%~A}~%"
          (ind)
          (getf par :struct-def)
          (with-indent
            (struct-members (getf par :contents)))
          (ind)))

;; (%struct-def
;;  `(:STRUCT-DEF "ContractId"
;;    :CONTENTS (:STRUCT-MEMBERS
;;               ((:MEMBER-STRUCT-NAME "First" :MEMBER-STRUCT-TYPE "String")
;;                (:MEMBER-STRUCT-NAME "Second" :MEMBER-STRUCT-TYPE "String")))))


(defun outer (param)
  (maptree-transform
   #'(lambda (par)
       (if (not (and (listp par) (keywordp (car par))))
           nil
           ;; else
           (let ((fn (intern (concatenate 'string "%" (symbol-name (car par))))))
             (if (not (fboundp fn))
                 (prog1 nil
                   (format t "ERR[:unimplemented gen:] ~A~%" fn))
                 ;; else
                 (funcall fn par)))))
   param))

(print
 (outer *new*))
