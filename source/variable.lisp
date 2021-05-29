
(in-package #:liang.lvm)

(declaim (inline setlocalvariable
                 getlocalvariable
                 set-shortcut
		 findvariable))

(defstruct VMVariable
  (value))
(defstruct VMVariableIndex
  (i NIL :type fixnum))

(defun setlocalvariable (vm index value)
  (setf (aref (LVM-stack vm) (+ (LVM-ep vm) index)) (make-VMVariable :value value))
  NIL)

(defun getlocalvariable (vm index)
  (declare (fixnum index))
  (aref (LVM-stack vm) (+ (LVM-ep vm) index)))

(defun findvariable (vm fixnum)
  (with-slots (stack ep) vm
    (labels ((check_before (i sep)
               (declare (fixnum i sep))
               (setf ep i)
               (if (eq i 0) NIL
                   (let ((variable (getlocalvariable vm index))
                         (beforeself (aref stack (1- i))))
                     (if variable variable
                     (check_before (Self-callerep beforeself) sep))))))
      (let* ((eptmp ep)
             (result (check_before 1 1)))
        (setf ep eptmp)
        (setq result (if result result
                         (check_before ep eptmp)))
        (setf ep eptmp)
        (VMVariable-value result)))))

(defmethod findvariable (vm (index VMVariableIndex))
  (findvariable vm (VMVariableIndex-i index)))

; x=y
(defmethod set-variable (vm (x VMVariableIndex) (y T))
  (setlocalvariable vm (VMVariableIndex-i x) y)
  NIL)

(defmethod set-variable (vm (x LVMFunction) (y LVMLambda))
  (with-slots (args content-at content-size) y
    (setlocalvariable vm (LVMFunction-index x) (make-LVMFunction
                                                :index (LVMFunction-index x)
                                                :content-at content-at
                                                :content-size content-size
                                                :args (or (LVMFunction-args x)
                                                          (LVMLambda-args y)))))
  NIL)

