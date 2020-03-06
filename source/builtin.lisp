
(in-package #:liang)



(defparameter *built-in-methods* `("=" "+" "-" "*" "/"
                                             "PRINT"
                                             "VALUE_IF"
                                             "EQUALS"
                                             "OR"))

(defstruct LVMBuiltInFunction
  (args)
  (body))

(defun liang-register-builtin-methods (compile-data)
  (dolist (i *built-in-methods*)
    (liang-name2id compile-data (slot-value compile-data 'name2id) i))
  
  compile-data)


(defun lvm-implements-builtin-function (vm index args lambda)

  (lvm-set-local-variable vm index index (make-LVMBuiltInFunction
                                               :args args
                                               :body lambda)))

(defun lvm-implements-default-functions (vm)

  (lvm-implements-builtin-function vm 0 2 NIL)

  (lvm-implements-builtin-function vm 1 2 #'(lambda (x y) (+ x y)))
  (lvm-implements-builtin-function vm 2 2 #'(lambda (x y) (- x y)))
  (lvm-implements-builtin-function vm 3 2 #'(lambda (x y) (* x y)))
  (lvm-implements-builtin-function vm 4 2 #'(lambda (x y) (/ x y))) 

  (lvm-implements-builtin-function vm 5 1 #'(lambda (x) (print x)))
  (lvm-implements-builtin-function vm 6 3 #'(lambda (cond then else) (if cond then else)))
  (lvm-implements-builtin-function vm 7 2 #'(lambda (x y) (equal x y)))
  (lvm-implements-builtin-function vm 8 2 #'(lambda (x y) (or x y))))
