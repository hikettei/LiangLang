

(in-package :asdf-user)


(asdf:defsystem :lianglang
  :author "RuliaChan <a href='RuliaChan0317@Gmail.com'>Gmail</a> <a href='https://twitter.com/no_coffeenolife'>Twitter</a>"
  :license "MIT"
  :version "0.1"
  :description "Liang Programming Language"
  :depends-on (#:yacc #:cl-lex #:alexandria #:cl-pack)
  :pathname "source"
  :components ((:file "liang" :depends-on ("package"))
               (:file "package" :depends-on ("lvm" "liangc"))
               (:file "lvmfile" :depends-on ("lvm"))
               (:file "lvm")
               (:file "variable" :depends-on ("lvm" "function"))
               (:file "function" :depends-on ("lvm"))
               (:file "liangc" :depends-on  ("lvm"))
               (:file "lex"    :depends-on  ("liangc"))
               (:file "tree"   :depends-on  ("liangc"))
               (:file "gencode" :depends-on ("liangc" "lvm"))
               (:file "compiler" :depends-on ("liangc"))
               (:file "utils" :depends-on   ("liangc"))))
