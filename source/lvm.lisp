
(defpackage #:liang.lvm
  (:use #:cl)

  (:import-from :cl-pack
   :pack :unpack)
  
  (:export #:initvm
           #:runvm
           #:write-lvm-iseq
           #:load-lvm-structure
           #:write-lvm-iseq
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
   :PUSHNIL 5
   :SENDEXP 10
   :SENDFN 11
   :SENDPOP 12
   :SETQ 22
   :RETURN 30
   :MAKE_SYMBOLS 41
   :MAKE_SIMPLE_ARRAY 42 
   :MAKE_ADJUSTABLE_ARRAY 43)))

(defun mnemonic (mnemonic)
  (gethash mnemonic *MNEMONIC*))

(defstruct LVM
  (iseq)
  (ep 0)
  (pc 0)
  (variable-size NIL :type fixnum)
  (static-strings)
  (stack))

(defstruct Self
  (returnp NIL :type fixnum)
  (callerep NIL :type fixnum)
  (stacksize NIL :type fixnum))

(defparameter *STACKSIZE* 3000)

(defun initvm (iseq variable-size &optional static-strings)
  (let ((vm (make-LVM :iseq iseq :variable-size variable-size
                      :stack (make-array *STACKSIZE* :fill-pointer 0 :initial-element NIL)
                      :static-strings static-strings)))

    ;initialize main 

    (setself vm (1+ (length iseq)))
    
    ;implement builtin functions

    (set-defaultbuiltinmethods vm)
    vm))

(defun setself (vm returnp)
  (with-slots (stack variable-size ep) vm
    (vector-push (make-Self :returnp returnp
                            :callerep ep
                            :stacksize (fill-pointer stack))
                 stack)
    (setf ep (fill-pointer stack))
    (setf (fill-pointer stack) (+ (fill-pointer stack)
                                  variable-size))
  NIL))

(defun returnself (vm)
  (with-slots (stack variable-size ep pc) vm
    (let* ((self (aref stack (1- ep)))
           (ssize (Self-stacksize self))
           (rval (stack-pop vm T)))
      (dotimes (i (- (fill-pointer stack) ssize))
        (setf (aref stack (+ i ssize)) NIL))
      (setf (fill-pointer stack) ssize)
      (with-slots (returnp callerep) self
        (setf ep callerep)
        (setf pc returnp)
        (stack-push vm rval)
        NIL))))

(defun stack-push (vm value)
  (vector-push value (slot-value vm 'stack)))

(defun stack-pop (vm &optional (use NIL))
  (if use
      (let ((val (vector-pop (slot-value vm 'stack))))
        (if (typep val 'VMVariableIndex)
            (findvariable vm val)
            val))
      (vector-pop (slot-value vm 'stack))))

(declaim (ftype (function (LVM fixnum &optional boolean)) genlist-withpop))
(defun genlist-withpop (vm size &optional (use NIL))
  (the list (reverse (loop for i from 1 to size
                           append `(,(stack-pop vm use))))))

(defmacro in-processing-system (name &body body)
  `(defun ,name (vm)
     (let* ((i (elt (LVM-iseq vm) (LVM-pc vm)))
	    (opecode (car i))
	    (operand (cdr i))
	    (cases (make-array 50 :initial-element NIL
				  :fill-pointer 0)))
       ,@body
       (funcall (aref cases opecode) vm operand opecode))))

(defmacro defprocess (opename args &body body)
  `(setf (aref cases (mnemonic ,opename)) (lambda (,@args &rest _)
					    (declare (ignore _))
					    ,@body)))

(in-processing-system LiangVM
  (defprocess :PUSHNUMBER (vm operand)
    (stack-push vm (car operand)) '1)
  
  (defprocess :PUSHSTRING (vm operand)
    (stack-push vm (aref (LVM-static-strings vm) (car operand))) '1)

  (defprocess :PUSHNAME (vm operand)
    (stack-push vm (make-VMVariableIndex :i (car operand))) '1)

  (defprocess :PUSHDEF (vm operand)
    (destructuring-bind (name args &rest _) operand
      (declare (ignore _))
	(stack-push vm (make-LVMFunction
			:index name
			:args (genlist-withpop vm args)))))
  
  (defprocess :PUSHLAMBDA (vm operand)
    (destructuring-bind (size args &rest _) operand
      (declare (ignore _))
      (stack-push vm (make-LVMLambda
		      :content-at (1+ (slot-value vm 'pc))
		      :content-size size
		      :args (genlist-withpop vm args)))) '1)
  (defprocess :SENDPOP (vm operand) (send vm NIL (car operand) (stack-pop vm)))
  (defprocess :SENDEXP (vm operand) (send vm (car operand) 2))
  (defprocess :SENDFN (vm operand) (send vm (car operand) (second operand)))
  (defprocess :SETQ (vm)
    (destructuring-bind (x y &rest _) (genlist-withpop vm 2)
      (declare (ignore _))
      (set-variable vm x y)))
  (defprocess :MAKE_SYMBOLS (vm operand)
    (stack-push vm (init-lvm-array
		    (genlist-withpop vm (first operand))
		    (first operand))))
  (defprocess :RETURN (vm) (returnself vm) '0)
  (defprocess :PUSHNIL (vm) (stack-push vm NIL) '1))

 (eval
`(defun executevm (vm)
   (let* ((i (elt (LVM-iseq vm) (LVM-pc vm)))
          (opecode (car i))
          (operand (cdr i)))
     (declare (ignore _))
     
    (case opecode
      (,(mnemonic :PUSHNUMBER) (stack-push vm (car operand)) '1)
      (,(mnemonic :PUSHSTRING) (stack-push vm (aref (LVM-static-strings vm) (car operand))) '1)
      (,(mnemonic :PUSHNAME)   (stack-push vm (make-VMVariableIndex :i
                                                                    (car operand)))
       '1)
      (,(mnemonic :PUSHDEF) (destructuring-bind (name args &rest _) operand
                              (stack-push vm (make-LVMFunction
                                              :index name
                                              :args (genlist-withpop vm args))))
       '1)
      (,(mnemonic :PUSHLAMBDA) (destructuring-bind (size args &rest _) operand
                                 (stack-push vm (make-LVMLambda
                                                 :content-at (1+ (slot-value vm 'pc))
                                                 :content-size size
                                                 :args (genlist-withpop vm args)))
                                 (1+ size)))
      (,(mnemonic :SENDPOP) (send vm NIL (car operand) (stack-pop vm)))
      (,(mnemonic :SENDEXP) (send vm (car operand) 2))
      (,(mnemonic :SENDFN)  (send vm (car operand) (second operand)))
      (,(mnemonic :SETQ)    (destructuring-bind (x y &rest _)
                                (genlist-withpop vm 2)
                              (set-variable vm x y))
       '1)
      (,(mnemonic :MAKE_SYMBOLS) (stack-push vm (init-lvm-array
                                                 (genlist-withpop vm (first operand))
                                                 (first operand)))
       '1)
      (,(mnemonic :MAKE_ADJUSTABLE_ARRAY) (stack-push vm (init-lvm-array-adjustable
                                                          (genlist-withpop vm (first operand) T)
                                                          (first operand)))
       '1)
      (,(mnemonic :RETURN) (returnself vm) '0)
      (,(mnemonic :PUSHNIL) (stack-push vm NIL) '1)
      (T (print "Unimplemented opecode")
       (print opecode) '1)))))

(defun vmrun (vm &optional args)
  (setlocalvariable vm 13 ; 13 = sys_args
                (init-lvm-array args (length args)))
  (with-slots (ep pc iseq) vm
    (let ((iseqsize (length iseq)))
      (setq pc 0)
      (setq ep 1)

      (loop :while (< pc iseqsize)
            :do (let ((x (executevm vm)))
                  (setq pc (+ pc x)))))))

