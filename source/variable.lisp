
(in-package #:liang.lvm)


(defstruct VMUndefinedVariable)
(defstruct VMVariable
  (value))
(defstruct VMVariableIndex
  (i NIL :type fixnum))


(defun setlocalvariable (vm index value)
  (with-slots (stack ep) vm
    (setf (aref stack (+ ep index)) (make-VMVariable :value value)))
  NIL)

(defun getlocalvariable (vm index)
  (declare (fixnum index))
  (with-slots (stack ep) vm
    (aref stack (+ ep index))))

(defmethod findvariable (vm (index number))
  (with-slots (stack ep) vm
    (labels ((check_before (i)
               (setf ep i)
               (if (eq i -1) (error "Undefined Variable"))
               (let ((variable (getlocalvariable vm index))
                     (beforeself (aref stack (1- i))))
                 (cond
                   ((typep variable 'VMUndefinedVariable)
                    (check_before (slot-value beforeself 'callerep)))
                   ((typep variable 'VMVariable)
                    variable)
                   (T (error "VMError"))))))
      (let* ((eptmp ep)
             (result (check_before ep)))
        (setf ep eptmp)
        (VMVariable-value result)))))

(defmethod findvariable (vm (index VMVariableIndex))
  (findvariable vm (slot-value index 'i)))

; x=y
(defmethod set-variable (vm (x VMVariableIndex) (y T))
  (setlocalvariable vm (VMVariableIndex-i x) y)
  NIL)

(defmethod set-variable (vm (x LVMFunction) (y LVMLambda))
  (with-slots (args content-at content-size) y
    (setlocalvariable vm (slot-value x 'index) (make-LVMFunction
                                                :index (slot-value x 'index)
                                                :content-at content-at
                                                :content-size content-size
                                                :args (or (slot-value x 'args)
                                                          (slot-value y 'args)))))
  NIL)

