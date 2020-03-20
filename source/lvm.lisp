
(defpackage #:liang.lvm
  (:use #:cl)
  
  (:export #:initvm
           #:runvm
           #:mnemonic
           #:*MNEMONIC*))

(in-package #:liang.lvm)

(declaim (inline stack-push))

(defparameter *MNEMONIC*
  (alexandria:plist-hash-table
  `(:PUSHNUMBER 0
   :PUSHSTRING 1
   :PUSHNAME   2
   :PUSHDEF    3
   :PUSHLAMBDA 4
   :SENDEXP 10
   :SENDFN 11
   :SETQ 12
    :RETURN 30)))

(defmacro mnemonic (mnemonic)
  `(gethash ,mnemonic *MNEMONIC*))

(defstruct LVM
  (iseq)
  (ep 0 :type fixnum)
  (pc 0 :type fixnum)
  (variable-size NIL :type fixnum)
  (fp 0 :type fixnum)
  (stack))

(defstruct Self
  (returnp NIL :type fixnum)
  (callerep NIL :type fixnum)
  (stacksize NIL :type fixnum))

(defparameter *STACKSIZE* 3000)

(defun initvm (iseq variable-size)
  (let ((vm (make-LVM :iseq iseq :variable-size variable-size
                      :stack (make-array 3000 :initial-element NIL))))

    ;initialize main

    (setself vm (1+ (length iseq)))

    ;implement builtin functions

    (set-defaultbuiltinmethods vm)
    vm))

(defun setself (vm returnp)
  (with-slots (stack variable-size ep fp) vm
    (stack-push vm (make-Self :returnp returnp
                             :callerep ep
                             :stacksize fp))
    
    (setf ep fp)
    (setf fp (+ fp variable-size))
  NIL))

(defun returnself (vm)
  (with-slots (stack variable-size ep pc fp) vm
    (let* ((self (aref stack (1- ep)))
           (ssize (Self-stacksize self))
           (rval (stack-pop vm T)))
      (setf fp ssize)
      (with-slots (returnp callerep) self
        (setf ep callerep)
        (setf pc returnp)
        (stack-push vm rval)
        NIL))))

(defun stack-push (vm value)
  (let ((fp (LVM-fp vm)))
    (setf (LVM-fp vm) (1+ fp))
    (setf (aref (LVM-stack vm) fp) value)
    fp))

(defun stack-pop (vm &optional (use NIL))
  (setf (LVM-fp vm) (1- (LVM-fp vm)))
  (let ((value (aref (LVM-stack vm) (LVM-fp vm))))
    (if use
        (if (typep value 'VMVariableIndex)
            (findvariable vm value)
            value)
        value)))

(defun genlist-withpop (vm size &optional (use NIL))
  (reverse (loop for i from 1 to size
                 append `(,(stack-pop vm use)))))

(eval
`(defun executevm (vm)
   (let* ((i (elt (LVM-iseq vm) (LVM-pc vm)))
          (opecode (car i))
          (operand (cdr i)))
     
    (case opecode
      (,(mnemonic :PUSHNUMBER) (stack-push vm (car operand)) '1)
      (,(mnemonic :PUSHSTRING) (stack-push vm (car operand)) '1)
      (,(mnemonic :PUSHNAME)   (stack-push vm (make-VMVariableIndex :i
                                                                    (car operand)))
       '1)
      (,(mnemonic :PUSHDEF) (destructuring-bind (name args) operand
                              (stack-push vm (make-LVMFunction
                                              :index name
                                              :args (genlist-withpop vm args))))
       '1)
      
      (,(mnemonic :PUSHLAMBDA) (destructuring-bind (size args) operand
                                 (stack-push vm (make-LVMLambda
                                                 :content-at (1+ (slot-value vm 'pc))
                                                 :content-size size
                                                 :args (genlist-withpop vm args)))
                                 (1+ size)))
      
      (,(mnemonic :SENDEXP) (send vm (car operand) 2))
      (,(mnemonic :SENDFN)  (send vm (car operand) (second operand)))
      (,(mnemonic :SETQ)    (destructuring-bind (x y) (genlist-withpop vm 2)
                              (set-variable vm x y))
       '1)
      
      (,(mnemonic :RETURN) (returnself vm) '0)
      (T (print "Unimplemented opecode")
       (print opecode) '1)))))

(defun vmrun (vm)
  (with-slots (ep pc iseq) vm
    (let ((iseqsize (length iseq)))
      (setq pc 0)
      (setq ep 1)
      (loop :while (< pc iseqsize)
            :do (let ((x (executevm vm)))
                  (setq pc (+ pc x)))))))
