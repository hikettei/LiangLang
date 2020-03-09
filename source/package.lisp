
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
  
  (:export #:liang-compile-program #:*MNEMONIC* #:mnemonic #:*BuiltInMethods*))

