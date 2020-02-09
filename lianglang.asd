

(asdf:defsystem :lianglang
  :author "RuliaChan <a href='RuliaChan0317@Gmail.com'>Gmail</a> <a href='https://twitter.com/no_coffeenolife'>Twitter</a>"
  :license "MIT"
  :version "0.1"
  :description "Liang Programming Language"
  :depends-on (#:yacc #:cl-lex #:cl-ansi-text)
  :serial t
  :components ( 
	       (:file "source/package")
	       (:file "source/lex")
           (:file "source/tree")
           (:file "source/lvm")
           (:file "source/liangc")))
