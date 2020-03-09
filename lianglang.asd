

(asdf:defsystem :lianglang
  :author "RuliaChan <a href='RuliaChan0317@Gmail.com'>Gmail</a> <a href='https://twitter.com/no_coffeenolife'>Twitter</a>"
  :license "MIT"
  :version "0.1"
  :description "Liang Programming Language"
  :depends-on (#:yacc #:cl-lex #:alexandria #:cl-ppcre)
  :serial t
  :components ( 
	       (:file "source/package")
           (:file "source/function")
           (:file "source/liang")
           (:file "source/lvm")
           (:file "source/utils")
           (:file "source/variable")
           (:file "source/builtin")
           (:file "source/liangc")
           (:file "source/gencode")
           (:file "source/lex")
           (:file "source/tree")))
