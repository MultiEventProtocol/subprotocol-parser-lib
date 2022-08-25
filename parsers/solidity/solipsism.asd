;; Copyright © 2021-2022 Glukhov Mikhail. All rights reserved. Licensed
;; under the GNU AGPLv3
(asdf:defsystem #:solipsism
  :version      "0.0.1"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "AGPLv3"
  :description  "solipsism"
  :depends-on   (#:yacc #:cl-lex #:alexandria #:anaphora)
  :serial       t
  :pathname     "src"
  :components   ((:file "packages")
                 (:file "utils")
                 (:file "delcomments")
                 (:file "lexer")
                 (:file "parser")
                 (:file "tests")
                 (:file "run")))
