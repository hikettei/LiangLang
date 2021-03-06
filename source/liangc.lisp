
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
   #:compile-to-lvm-byfile
   #:*variable-names*
   #:*static-heap*
   #:*using-files*
   #:compile-to-lvm-simply))



(in-package #:liang.compiler)


