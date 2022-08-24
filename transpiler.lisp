;; [[file:transpiler.org::*Frame][Frame:1]]
;; ;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::copyright][copyright]]][copyright]]
;; Copyright © 2021-2022 Glukhov Mikhail. All rights reserved. Licensed
;; under the GNU AGPLv3
;; ;; copyright ends here
(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :anaphora :optima
                  :fare-quasiquote
                  :fare-quasiquote-extras
                  :fare-quasiquote-optima)))

(use-package '(:anaphora :optima :fare-quasiquote))

(named-readtables:in-readtable :fare-quasiquote)

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]
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
;; ends here

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]

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

;; ends here

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]
(defun transform-struct-member (param)
  (maptree-transform
   #'(lambda (par)
       (match par
              ((and (property :STRUCT-NAME name)
                    (property :TYPE type))
               `(:member-struct-name
                 ,name
                 :member-struct-type
                 ,(match type
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
;; ends here

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]
(defun transform-uplift-event-def (param)
  (maptree-transform
   #'(lambda (par)
       (match par
              ((property :EVENT-DEF event-def)
               (match event-def
                      ((and (property :EVENT event-name)
                            (property :PARAMS params))
                       `(:event-def ,event-name :params ,params))))))
   param))
;; ends here

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]
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
;; ends here

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]
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

;; ends here

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]
(defun event-param (par)
  (format nil "~A: ~A"
          (getf par :id)
          (caadr (getf par :type-name))))

;; (event-param
;;  `(:TYPE-NAME (:ELT-TYPE-NAME (:ADDRESS AD)) :ID "_from"))

(defun event-params (par)
  (format nil "~{~a~^, ~}"
          (with-indent
              (mapcar #'event-param (getf par :event-param)))))

;; (event-params
;;  `(:EVENT-PARAM
;;    ((:TYPE-NAME (:ELT-TYPE-NAME (:ADDRESS AD)) :ID "_from")
;;     (:TYPE-NAME (:ELT-TYPE-NAME (:UNSIGNED-INTEGER-TYPE UINT)) :ID "_value"))))

(defun %event-def (par)
  (format nil "~Afn ~A(~A) {~%~A}" (ind) (getf par :event-def)
          (with-indent
              (event-params (getf par :params)))
          (ind)))

;; (%event-def
;;  `(:EVENT-DEF "SomeEvent" :PARAMS
;;               (:EVENT-PARAM
;;                ((:TYPE-NAME (:ELT-TYPE-NAME (:ADDRESS AD)) :ID "_from")
;;                 (:TYPE-NAME (:ELT-TYPE-NAME (:UNSIGNED-INTEGER-TYPE UINT)) :ID
;;                             "_value")))))

;; ends here

;; [[[[file:~/src/subprotocol-parser-lib/transpiler.org::*Frame][Frame]]][]]
;; (defun %ctract-body-elt (par)
;;   (format nil "~A// contract body elt comment ~%~{~A~}~%"
;;           (ind)
;;           (with-indent
;;             (mapcar #'outer (cadr par)))))
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

(defun transform-uplift-params (param)
  (maptree-transform
   #'(lambda (par)
       (match par
         ((list :PARAM-LIST (list :PAR param-list))  `(:param-list ,param-list))))
   param))

;; (transform-uplift-params
;;  '(:PARAM-LIST
;;    (:PAR
;;     ((:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeContractEvent"))))
;;       :NAME "e")))))


(defun transpile-file (param)
  (print
   (outer (->> (read-from-string (alexandria:read-file-into-string param))
               (transform-struct-member)
               (transform-uplift-members)
               (transform-uplift-struct-def)
               (transform-uplift-event-def)
               (transform-uplift-params)
               ))))

;; (transpile-file "mep.sexp")

(defun func-param (par)
  (match par
    ((list :PAR-TYPE
           (list :IDENTIFIER-PATH
                 (list :IDENT (list (list :ID param-type))))
           :NAME param-name)
     (format nil "~A: ~A"  param-name param-type))
    (otherwise (format t "ERR:[otherwise-unimplemented]:-func-param(\"~A\")~%"
                       (cadr par)))))

;; (func-param
;;  `(:PAR-TYPE2 (:IDENTIFIER-PATH (:IDENT ((:ID "SomeContractEvent")))) :NAME "e"))

(defun func-params (par)
  (match par
    ((list :PARAM-LIST param-list)
     (format nil "~{~a~^, ~}"
             (with-indent
               (mapcar #'func-param param-list))))))

;; (func-params
;;  '(:PARAM-LIST
;;    ((:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeContractEvent"))))
;;      :NAME "e1")
;;     (:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeContractEvent"))))
;;      :NAME "e2"))))

(defun %func-def (par)
  (match par
    ((list :FUNC-DEF fn)
     (let* ((fname  (getf fn :fun))
            (parlist (getf fn :parlist))
            (fmeta (getf fn :fmeta))
            (retlist (getf fn :retlist))
            (blk (getf fn :block)))
       (format nil "~Afn ~A(~A)~A {~%~A~A}~%"
               (ind)
               fname
               (func-params parlist)
               (let ((result-retlist "!"))
                 (when retlist
                   (match retlist
                     ((list :RETLIST (list :PAR returns))
                      (match (car returns)
                        ((list :PAR-TYPE
                               (list :IDENTIFIER-PATH
                                     (list :IDENT (list (list :ID param-type)))))
                         (setf result-retlist (format nil "-> ~A"  param-type)))
                        (otherwise (format t "ERR:[unimplemented in func-def]:~%"
                                           (car returns)))))))
                 result-retlist)
               (with-indent
                 (format nil "~A~%" (bprint blk)))
               (ind))))))

;; (print
;; (%func-def
;; '(:FUNC-DEF
;;   (:FUN "SomeEventHandler"
;;    :PARLIST (:PARAM-LIST
;;              ((:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeContractEvent"))))
;;                :NAME "e")
;;               (:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeContractEvent"))))
;;                :NAME "e2")))
;;    :FMETA (:VISIBILITY PUBLIC :STATE-MUTABILITY (:STATE-MUTABILITY VIEW))
;;    :RETLIST
;;         (:RETLIST
;;          (:PAR
;;           ((:PAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeMEPEvent"))))))))
;;    :BLOCK
;;         (:STMNT
;;          (:VAR-DECL-STMNT
;;           ((:VAR-DECL
;;             (:VAR-TYPE (:IDENTIFIER-PATH (:IDENT ((:ID "SomeMEPEvent")))) :NAME
;;                        "mep_e")
;;             :INIT (:EXPR-TN (:IDENTIFIER-PATH (:IDENT ((:ID "e"))))))
;;            (:RETURN
;;              (:EXPR-TN (:IDENTIFIER-PATH (:IDENT ((:ID "e")))))))))))))
;; ends here
;; Frame:1 ends here
