(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:solipsism)))

(load "transpiler.lisp")

(ql:quickload "unix-opts")

(defun transpile-filename (str)
  str)

(opts:define-opts
  (:name :usage
   :description "Usage transpiler."
   :short #\u
   :long "usage")
  (:name :transpile
   :description "Transpile file"
   :short #\t
   :long "transpile"
   ;; :required t
   :arg-parser #'transpile-filename
   :meta-var "<filename>")
  (:name :output
   :description "Output file"
   :short #\o
   :long "output"
   ;; :required t
   :arg-parser #'transpile-filename
   :meta-var "<filename>")
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
    (let ((transpiled)
          (overbox))
      (when-option (options :usage)
        (opts:describe
         :prefix "Transpiler. Usage:"
         :suffix "use: transpiler -p example.sexp"
         :usage-of "./transpiler"
         ;; :args "[keywords]"
         ))
      (when-option (options :transpile)
        (format t "Transpiler. Transpile file: ~A ~%" it)
        (setf transpiled (transpile-file it)))
      (when-option (options :output)
        (format t "Transpiler. Output: ~A ~%" it)
        (setf overbox it))
      ;; output
      (if overbox
          (alexandria:write-string-into-file (bprint transpiled)
                                             overbox
                                             :if-exists :supersede)
          ;; else
          (format t "~%~A~%" (bprint transpiled)))
      )))

(export 'main)
(sb-ext:save-lisp-and-die #P"transpiler" :toplevel #'main :executable t)
