
(in-package #:liang)


(defstruct LVMSymbol
  (name NIL :type fixnum)
  (nameofid NIL :type fixnum))

(defstruct LVMVariable
  (value)
  (nameofid NIL :type fixnum))


(defun lvm-set-local-variable (vm index name value)
  (with-slots (stack ep) vm
    (setf (aref stack (+ ep index)) (make-LVMVariable
                                     :value value
                                     :nameofid name))))


(defun lvm-get-local-variable (vm index)
  (with-slots (stack ep) vm
    (slot-value (aref stack (+ ep index)) 'value)))


(defmethod lvm-set-variable ((vm LVM) (index LVMSymbol) (value T))
  (lvm-set-local-variable vm (slot-value index 'name)
                             (slot-value index 'nameofid) value))

(defmethod lvm-set-variable ((vm LVM) (index LVMFunctionObject) (value LVMFunction))
  (setf (slot-value value 'args) (slot-value index 'args))
  (lvm-set-local-variable vm (slot-value index 'name)
                             (slot-value index 'nameofid) value))
                          

(defun lvm-find-variable (vm target-symbol &optional (next-self NIL))  
  (with-slots (stack ep) vm

    (let ((target-self (aref stack (if next-self
                                       next-self
                                       (1- ep))))
          (target-var-id (slot-value target-symbol 'name))
          (target-var-name (slot-value target-symbol 'nameofid)))


      (with-slots (caller-ep field-size) target-self
        
        (let ((result (if (<= target-var-id field-size)

                          (let ((var (aref stack (+ (if next-self
                                                        caller-ep
                                                        ep)
                                                    target-var-id))))
                       
                          (if (typep var 'LVMVariable)
                                (if (eq (slot-value var 'nameofid)
                                        target-var-name)
                                    var))))))

          (if result
              (slot-value result 'value)
              (if (eq next-self 0)
                  (error "undefined variable")
                  (lvm-find-variable vm target-symbol (1- caller-ep)))))))))

      
