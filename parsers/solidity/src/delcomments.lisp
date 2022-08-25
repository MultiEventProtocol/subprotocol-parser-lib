;; Copyright © 2021-2022 Glukhov Mikhail. All rights reserved. Licensed
;; under the GNU AGPLv3

(defun get-filtered-file-content (file filter)
  (let ((output (make-string-output-stream)))
    (with-open-file (input file)
      (loop :for line = (read-line input nil)
            :while line
            :do (write-line (funcall filter line) output)))
    (get-output-stream-string output)))

(define-condition get-filtered-file-content-error (error)
  ((text :initarg :text :reader text)))

(let ((state :no-comment))
  (defun clear-filter-comments ()
    (setf state :no-comment))
  (defun filter-comments (line)
    (macrolet ((add-acc () `(setf acc (concatenate 'string acc (coerce (list char) 'string))))
               (restore () `(setf acc (concatenate 'string acc "/"))))
      (loop :for char :across line
            :with acc = ""
            :finally
               (progn (when (equal state :candidate) ;; last symbol before end of line
                        (restore)
                        (setf state :no-comment)) ;; clear for next call
                      (return acc))
            :do
               (cond ((equal state :no-comment)
                      (if (char= #\/ char)
                          (setf state :candidate) ;; then
                          (add-acc)))
                     ((equal state :candidate)
                      (cond ((char= #\/ char) ;; single-line comment
                             (progn
                               (setf state :no-comment)
                               (loop-finish))) ;; cut acc
                            ((char= #\* char) ;; multi-line comment
                             (progn
                               (setf state :multi-line-comment)))
                            (t (progn
                                 (setf state :no-comment)
                                 (restore)
                                 (add-acc)))))
                     ((equal state :multi-line-comment)
                      (cond ((char= #\* char)
                             (setf state :maybe-fin))))
                     ((equal state :maybe-fin)
                      (cond ((char= #\/ char)
                             (setf state :no-comment))
                            (t (setf state :multi-line-comment))))
                     (t (error 'get-filtered-file-content-error :text (symbol-name state))))))))

(defun get-filtered-comments-file-contents (file)
  (clear-filter-comments)
  (get-filtered-file-content file #'filter-comments))

;; (get-filtered-comments-file-contents #P"./test_contract.sol")
