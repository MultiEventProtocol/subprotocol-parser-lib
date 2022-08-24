(defparameter *call-path*
  (pathname (concatenate 'string (sb-posix:getcwd) "/")))

(defparameter *true-dir*
  (let* ((truename  (or *compile-file-truename* *load-truename*))
         (directory (if (null truename)
                        *call-path*
                        (pathname (directory-namestring truename)))))
    directory))

(load (merge-pathnames (make-pathname :name "result" :type "lisp")
                       *true-dir*))

(load (merge-pathnames (make-pathname :directory '(:relative "mw-diff-sexp")
                                      :name "packages" :type "lisp")
                       *true-dir*))

(load (merge-pathnames (make-pathname :directory '(:relative "mw-diff-sexp")
                                      :name "diff-sexp" :type "lisp")
                       *true-dir*))


(ql:quickload "unix-opts")
(use-package :mw-diff-sexp)

(defun parse-filename (str)
  str)

(opts:define-opts
  (:name :usage
   :description "Usage solidity parser."
   :short #\u
   :long "usage")
  (:name :parse
   :description "Parse solidity file"
   :short #\p
   :long "parse"
   ;; :required t
   :arg-parser #'parse-filename
   :meta-var "<filename>")
  (:name :test
   :description "Parse all files in test directory"
   :short #\t
   :long "test"
   ;; :required t
   :arg-parser #'parse-filename
   :meta-var "<directory>")
  (:name :output
   :description "Output file"
   :short #\o
   :long "output"
   ;; :required t
   :arg-parser #'parse-filename
   :meta-var "<filename>")
  ;; (:name :compare
  ;;  :description "Compare contract"
  ;;  :short #\c
  ;;  :long "compare"
  ;;  ;; :required t
  ;;  :arg-parser #'parse-filename
  ;;  :meta-var "<filename>")
  ;; -------------------------------------
  ;; (:name :check
  ;;  :description "Contract for checking"
  ;;  :short #\c
  ;;  :long "check"
  ;;  ;; :required t
  ;;  :arg-parser #'parse-filename
  ;;  :meta-var "<filename>")
  )

(defun unknown-option (condition)
  (format t "WARNING: ~s option is unknown!~%~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "FATAL: option ~s needs an argument!~%~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "FATAL: cannot parse ~s as argument of ~s~%~%"
                  (opts:raw-arg condition)
                  (opts:option condition)))
        (opts:missing-required-option (con)
          (format t "FATAL: ~a~%%" con)
          (opts:exit 1)))
    (if (null options)
        (setf options '(:usage t)))
    (let ((parsed)
          (overbox))
      (when-option (options :usage)
        (opts:describe
         :prefix "Solidity parser. Usage:"
         :suffix "To find differences between the base contract and the checked one, use: solparser -p base.sol -c checked.sol"
         :usage-of "./solparser"
         ;; :args "[keywords]"
         ))
      (when-option (options :parse)
        (format t "Solidity parser. Parse file: ~A ~%" it)
        (setf parsed (test-contract-file it)))
      (when-option (options :test)
        (format t "Solidity parser. Test in directory: ~A ~%" it)
        (setf parsed (test-all-contracts it)))
      (when-option (options :output)
        (format t "Solidity parser. Output: ~A ~%" it)
        (setf overbox it))
      ;; (when-option (options :compare)
      ;;   (format t "Solidity parser. Compare with: ~A ~%" it)
      ;;   (setf parsed
      ;;         (diff-sexp parsed (test-contract-file it))))
      ;; output
      (if overbox
          (alexandria:write-string-into-file (bprint parsed) overbox
                                             :if-exists :supersede)
          ;; else
          (format t "~%~A~%" (bprint parsed)))
      )))

(export 'main)
(sb-ext:save-lisp-and-die
 (merge-pathnames (make-pathname :name "solparser")
                  *call-path*)
 :toplevel #'main :executable t)
