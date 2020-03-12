
(in-package :cl-user)


(defpackage #:liang

  (:use :cl)

  (:import-from
    #:yacc
    #:define-parser
    #:parse-with-lexer)

  (:import-from #:cl-lex)

  (:import-from #:cl-ppcre)

  (:import-from #:alexandria
                #:make-hash-table)

  (:import-from #:liang.lvm
                #:initvm
                #:vmrun)
  
  (:import-from #:liang.compiler
                #:compile-to-lvm
                #:*variable-names*)
  
  (:export #:liang-execute-code))

