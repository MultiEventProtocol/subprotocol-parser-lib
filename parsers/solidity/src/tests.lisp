;; Copyright © 2021-2022 Glukhov Mikhail. All rights reserved. Licensed
;; under the GNU AGPLv3

(defun map-subdir (test-dir test-fn)
  (let ((test-subdirs))
    (uiop:collect-sub*directories
     test-dir
     (constantly t)
     (constantly t)
     (lambda (it) (push it test-subdirs)))
    (mapcar test-fn
            test-subdirs)))

(defun map-subdir-files (test-dir test-fn)
  (let ((test-files))
    (map-subdir test-dir
                #'(lambda (subdir)
                    (mapcar #'(lambda (x)
                                (push x test-files))
                            (uiop:directory-files
                             (car (uiop:directory* subdir))))))
    (mapcar test-fn
            (reverse test-files))))

;; run-tests
(defun test-all (dir)
  (map-subdir-files
   dir
   #'(lambda (file)
       (print file)
       (let* ((clj-lex (sol-lexer (get-filtered-comments-file-contents file))))
         (print (parse-with-lexer clj-lex *sol-parser*))))))

(defun test-all-contracts (dir)
  (let ((result))
    (map-subdir-files
     dir
     #'(lambda (file)
         (push `(:file ,file) result)
         (let* ((clj-lex (sol-lexer (get-filtered-comments-file-contents file))))
           (push (parse-with-lexer clj-lex *sol-parser*) result))))
    (reverse result)))

;; Вариант для тестирования конкретного файла
(defun test-file (pathname)
  (funcall #'(lambda (file)
               (print file)
               (let* ((clj-lex (sol-lexer (get-filtered-comments-file-contents file))))
                 (print (parse-with-lexer clj-lex *sol-parser*))))
           pathname))

;; Вариант для тестирования тестового контракта
(defun test-contract-file (pathname)
  (funcall #'(lambda (file)
               (let* ((clj-lex (sol-lexer (get-filtered-comments-file-contents file))))
                 (parse-with-lexer clj-lex *sol-parser*)))
           pathname))

;; (test-all "./tests")
;; (test-file #P"./tests/test_lib_def.sol")
;; (test-file #P"./tests/test_ctract_def.sol")
;; (test-file #P"./tests/test_ctract_body_elt.sol")
;; (test-file #P"./tests/test_simple_func_def.sol")
;; (test-file #P"./tests/test_func_with_param.sol")
;; (test-file #P"./tests/test_func_with_params.sol")
;; (test-file #P"./tests/test_fn_prm_type_id.sol")
;; (test-file #P"./tests/test_fn_prms_type_id.sol")
;; (test-file #P"./tests/test_fn_prms_type_loc.sol")
;; (test-file #P"./tests/test_fn_prms_type_loc_id.sol")
;; (test-file #P"./tests/test_fn_returns.sol")
;; (test-file #P"./tests/test_fn_visibility.sol")
;; (test-file #P"./tests/test_fn_state_mutability.sol")
;; (test-file #P"./tests/test_fn_virtual.sol")
;; (test-file #P"./tests/test_fn_mod_inv.sol")
;; (test-file #P"./tests/test_fn_override.sol")
;; (test-file #P"./tests/test_fn_block.sol")
;; (test-file #P"./tests/test_state_var_decl.sol")
;; (test-file #P"./tests/test_non_empty_str.sol")
;; (test-file #P"./tests/test_fn_type_name.sol")
;; (test-file #P"./tests/test_constructor.sol")
;; (test-file #P"./tests/test_expr_idx_stmnt.sol")
;; (test-file #P"./tests/test_modifier.sol")
;; (test-file #P"./tests/test_hexnum.sol")
;; (test-file #P"./tests/test_bool_lit.sol")
;; (test-file #P"./tests/test_mapping.sol")
;; (test-file #P"./tests/test_iface_def.sol")
;; (test-file #P"./tests/test_revert_stmnt.sol")
;; (test-file #P"./tests/test_fallback.sol")
;; (test-file #P"./tests/test_receive.sol")
;; (test-file #P"./tests/test_struct.sol")
;; (test-file #P"./tests/test_enum.sol")
;; (test-file #P"./tests/test_user_def_type.sol")
;; (test-file #P"./tests/test_error_def.sol")
;; (test-file #P"./tests/test_event_def.sol")
;; (test-file #P"./tests/test_try_catch_stmnt.sol")
;; (test-file #P"./tests/test_inline_arr_expr.sol")
;; (test-file #P"./tests/test_using_def.sol")
;; (test-file #P"./tests/test_const_def.sol")
;; (test-file #P"./tests/test_break_stmnt.sol")
;; (test-file #P"./tests/test_continue_stmnt.sol")C
;; (test-file #P"./tests/test_yul_block.sol")
;; (test-file #P"./tests/test_yul_var_decl.sol")
;; (test-file #P"./tests/test_yul_funccall.sol")
;; (test-file #P"./tests/test_yul_bool_lit.sol")
;; (test-file #P"./tests/test_yul_hexnum.sol")
;; (test-file #P"./tests/test_yul_dec_lit.sol")
;; (test-file #P"./tests/test_yul_str_lit.sol")
;; (test-file #P"./tests/test_yul_assignmt.sol")
;; (test-file #P"./tests/test_yul_if.sol")
;; (test-file #P"./tests/test_yul_func_def.sol")
;; (test-file #P"./tests/test_yul_for.sol")
;; (test-file #P"./tests/test_yul_switch.sol")
;; (test-file #P"./tests/test_yul_leave_break_continue.sol")

;; (test-contract-file #P"./test_contract.sol")
;; (test-contract-file #P"./test_dbg.sol")
;; (test-contract-file #P"./tests/test_dbg2.sol")

;; (test-all "./contracts")

;;; Option to get a list of all tokens of a particular file
;; (let* ((file #P"./tests/test_fn_mod_inv.sol")
;;        (contents (read-file-into-string file))
;;        (clj (sol-lexer contents)))
;;   (print contents)
;;   (tagbody
;;    repeat
;;      (multiple-value-bind (one two)
;;          (funcall clj)
;;        (format t "~%~A : ~A" one (bprint two))
;;        (when one
;;          (go repeat)))
;;      'fin))
