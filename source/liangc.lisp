
(in-package #:liang)

(defpackage #:liang.compiler

  (:use #:cl)

  (:import-from #:cl-lex)
  (:import-from #:yacc
                #:define-parser
                #:parse-with-lexer)

  (:import-from #:liang
                #:*MNEMONIC*
                #:mnemonic)
   
  (:export
   #:gentree
   #:compile-to-lvm))



(in-package #:liang.compiler)


(defun compile-to-lvm (source)
  (let ((tree (gentree source)))
    (loop for i from 0 to (1- (length tree))
          append (tree2iseq (elt tree i)))))

