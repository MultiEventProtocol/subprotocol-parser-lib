;; [[file:doc.org::defsystem][defsystem]]
;; Copyright © 2021-2022 Glukhov Mikhail. All rights reserved. Licensed
;; under the GNU AGPLv3
(asdf:defsystem #:solipsism
  :version      "0.0.1"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "AGPLv3"
  :description  "solipsism"
  :depends-on   (#:yacc #:cl-lex #:alexandria #:anaphora)
  :serial       t
  :components   ((:file "result.lisp")))
;; defsystem ends here
