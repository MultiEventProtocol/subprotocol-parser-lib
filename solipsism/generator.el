(require 'cl)

(defun accommodations (in)
  (if (equal 1 (length in))
      (list in)
    (loop
     for item in in
     append (let* ((res)
                   (rest (remove item in))
                   (rets (accommodations rest)))
              (loop for ret in rets do
                    (push ret res)
                    (push (append (list item) ret) res))
              res))))

(defun gen-accmd (in)
  (let ((res (remove-duplicates (accommodations in) :test #'equal)))
    ;; (length res))
    (loop for item in res do
          (let ((short-list (mapcar #'(lambda (item)
                                        (substring (symbol-name item) 1 4))
                                    item)))
            (princ (format "(%s\n  #'(lambda %s\n      %s\n"
                           (mapconcat #'(lambda (it)
                                          (symbol-name it))
                                      item " ")
                           short-list
                           (concat "`("
                                   (mapconcat #'(lambda (item)
                                                  (concat " :"
                                                          (substring (symbol-name item) 1) " ,"
                                                          (substring (symbol-name item) 1 4) " "))
                                              item "")
                                   ")))")))))))

(gen-accmd
 '(%visibility %state-mutability %modifier-invocation %virtual %override-specifier))
