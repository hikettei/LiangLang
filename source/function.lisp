
(in-package #:liang.lvm)

(defstruct LVMFunction
  (index NIL :type fixnum)
  (args)
  (content-at)
  (content-size))

(defstruct LVMLambda
  (content-at NIL :type fixnum)
  (content-size NIL :type fixnum)
  (args))

(defstruct LVMBuiltIn
  (body)
  (args-size))

(defun send (vm index arg-size &optional func)
  (with-slots (pc variable-size) vm
    (let ((fargs (genlist-withpop vm arg-size T))
          (func  (if func func (findvariable vm index))))
      (cond
        ((or (typep func 'LVMFunction) (typep func 'LVMLambda))
         (with-slots (content-at content-size args) func
           (setself vm (1+ pc))
           (dotimes (i (length args)) (set-variable vm (elt args i) (elt fargs i)))
           (- content-at pc)))
        ((typep func 'LVMBuiltIn)
         (stack-push vm (apply (slot-value func 'body) fargs)) '1)
        (T (error "The called object isn't a function"))))))

(defun set-builtinmethod (vm index args-size lambda)
  (set-variable vm (make-VMVariableIndex :i index)
                (make-LVMBuiltIn :body lambda
                                :args-size args-size)))
                          
(defun set-defaultbuiltinmethods (vm)
  (set-builtinmethod vm 0 2 NIL)
  
  (set-builtinmethod vm 1 2 #'+)
  (set-builtinmethod vm 2 2 #'-)
  (set-builtinmethod vm 3 2 #'*)
  (set-builtinmethod vm 4 2 #'/)

  (set-builtinmethod vm 5 1 #'print)
  (set-builtinmethod vm 6 3 #'(lambda (cond then else) (if cond then else)))
  (set-builtinmethod vm 7 2 #'(lambda (x y) (equal x y)))
  (set-builtinmethod vm 8 2 #'(lambda (x y) (or x y)))
  (set-builtinmethod vm 9 2 #'(lambda (x y) (set-variable vm x y)))
  (set-builtinmethod vm 10 1 #'(lambda (list) (1- (lvmarray-lengthof list))))
  (set-builtinmethod vm 11 3 #'(lambda (list n value) (lvmarray-setof list n value) n))
  (set-builtinmethod vm 12 2 #'(lambda (list n) (lvmarray-getof list n)))
  (set-builtinmethod vm 13 0 NIL)
  NIL)
