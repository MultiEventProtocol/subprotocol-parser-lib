;; Copyright © 2021-2022 Glukhov Mikhail. All rights reserved. Licensed
;; under the GNU AGPLv3
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:yacc :cl-lex :alexandria :anaphora)))

;; (use-package '(:cl :yacc :cl-lex :alexandria :anaphora))

(defpackage #:solipsism
  (:use #:cl :yacc :cl-lex :alexandria :anaphora)
  (:export #:sol-lexer
           #:parse-with-lexer
           #:test-all-contracts
           #:test-contract-file))
