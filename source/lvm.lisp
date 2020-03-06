
(in-package #:liang)


(defparameter *LVMSTACKSIZE* 3000)


(defstruct LVM
  (program-register)
  (pc 0)
  (min-var-id 0)
  (ep 0)
  
  (stack)
  (heap)

  (now-recursive? NIL))

(defstruct LVMSelf
  (return-addres NIL :type fixnum)
  (caller-ep NIL :type fixnum)
  (field-size NIL :type fixnum)
  (min-var-id))


(defun lvm-get-variable-addres (vm name)
  (declare (ignore vm))
  name)

(defun lvm-init-vm   (iseq main-field-size)
  (let ((vm (make-LVM :program-register iseq
                      :stack (make-array *LVMSTACKSIZE* :fill-pointer 0))))

    (lvm-init-self vm main-field-size (1+ (length iseq)) 1)
    
    (lvm-implements-default-functions vm)
    
    vm))

(defun lvm-init-vm-script (compile-data)
  (lvm-init-vm (slot-value compile-data 'iseq)
               (slot-value compile-data 'self-field-size)))

(defun lvm-push-stack (vm value)
  (vector-push value (slot-value vm 'stack)))

(defun lvm-pop-stack  (vm &optional (use NIL))
  (let ((value (vector-pop (slot-value vm 'stack))))
    (if use
        (if (typep value 'LVMSymbol)
            (lvm-find-variable vm value)
            value)
        value)))

(defun lvm-init-self (vm field-size &optional (return-addres 0) (caller-ep 0))

  (with-slots (stack ep min-var-id) vm

    (vector-push (make-LVMSelf :return-addres return-addres
                               :caller-ep caller-ep
                               :field-size field-size
                               :min-var-id (+ min-var-id field-size))
                 stack)

    (setf ep (fill-pointer stack))
    
    (dotimes (i field-size)
      (lvm-set-local-variable vm i (+ i min-var-id 1) 'no-value))

    (setf min-var-id (+ min-var-id field-size))

    (setf (fill-pointer stack)
          (+ (fill-pointer stack)
             field-size))))

(defun lvm-return (vm)
  (with-slots (stack ep pc min-var-id) vm

    (let ((self (aref stack (1- ep)))
          (x    (lvm-pop-stack vm)))
      
      (dotimes (i (- (fill-pointer stack) ep -1))
        (vector-pop stack))

      
      (lvm-push-stack vm x)
      (setf min-var-id (slot-value self 'min-var-id))
      (setf pc (slot-value self 'return-addres))
      (setf ep (slot-value self 'caller-ep)))))


(defun lvm-push-def (vm operand)
  (lvm-push-stack vm (make-LVMFunctionObject
                      :name (first operand)
                      :nameofid (second operand)
                      :args (reverse (loop for i from 1 to (third operand)
                                 append `(,(lvm-pop-stack vm)))))))


(defun lvm-push-lambda (vm operand)
  (let ((arg-size   (elt operand 0))
        (body-size  (elt operand 1))
        (field-size (elt operand 2)))

    (lvm-push-stack vm (lvm-make-function vm body-size arg-size field-size))
    
     (+ 2 body-size)))

(defun lvm-send-exp (vm operand)

  (if (eq (second operand) 0)
      (let ((x (lvm-pop-stack vm T))
            (y (lvm-pop-stack vm)))

        (lvm-set-variable vm y x))

      (lvm-send vm (first operand) (second operand)
                (reverse `(,(lvm-pop-stack vm T)
                            ,(lvm-pop-stack vm T))))))


(defun lvm-send-function (vm operand)
  (let ((args (loop for i from 1 to (third operand)
                   append `(,(lvm-pop-stack vm T)))))

    (lvm-send vm (first operand) (second operand) (reverse args))))


(defun lvm-execute-vm (vm)

  (with-slots (program-register pc) vm

    (setq pc 0)
    
    (let ((program-size (length program-register)))

      (loop while (< pc program-size)
         do (setq pc (+ (lvm-advance-pc vm) pc))))))


(defun lvm-advance-pc (vm)

  (with-slots (program-register pc min-var-id) vm

    (let* ((i (elt program-register pc))
           (opecode (car i))
           (operand (cdr i)))


      (case opecode
        (:PUSHNAME (lvm-push-stack vm (make-LVMSymbol
                                       :name     (first operand)
                                       :nameofid (second operand)))
                   '1)
        
        (:PUSHNUMBER (lvm-push-stack vm (first operand)) '1)
        (:PUSHSTRING (lvm-push-stack vm (first operand)) '1)
        
        (:PUSHDEF    (lvm-push-def vm operand) '1)
        (:PUSHLAMBDA (lvm-push-lambda vm operand))
        
        (:SENDEXP    (lvm-send-exp vm operand) '1)
        (:SENDFN     (lvm-send-function vm operand) '1)

        (:RETURN (lvm-return vm) '1)
        
        (T
         (print "Caught unimplemented opecode")
         (print opecode)
         '1)))))

