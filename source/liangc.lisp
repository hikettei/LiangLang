
(defpackage #:liang.compiler

  (:use #:cl)

  (:import-from #:cl-lex)
  (:import-from #:yacc
                #:define-parser
                #:parse-with-lexer)

  (:import-from #:liang.lvm
                #:*MNEMONIC*
                #:mnemonic)
   
  (:export
   #:gentree
   #:compile-to-lvm
   #:*variable-names*))



(in-package #:liang.compiler)


(defun compile-to-lvm (source &optional (use-library? T))
  (with-generate-iseq i
    (when use-library?
      (dolist (n (gentree (read-file-sequence "source/lib.liang")))
        (generate-tree-to-iseq i n)))
    (dolist (n (gentree source)) (generate-tree-to-iseq i n))
    (generate-iseq i :RETURN)))

