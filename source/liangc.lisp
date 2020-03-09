
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
  (compile-body-to-lvm (gentree source)))

