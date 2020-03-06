
(in-package #:liang)


(defstruct LVMFunction
  (args NIL)
  (function-from 0)
  (function-size 0)
  (field-size    0))

(defstruct LVMFunctionObject
  (args NIL :type list)
  (name NIL :type fixnum)
  (nameofid NIL :type fixnum))

(defun lvm-make-function (vm fn-size args field-size)
  (with-slots (pc) vm

    (make-LVMFunction
     :args args
     :function-from pc
     :function-size (1+ fn-size)
     :field-size field-size)))

(defun lvm-send (vm id name args)

  (let ((fn (lvm-find-variable vm (make-LVMSymbol
                                   :name id
                                   :nameofid name))))
    
    (if (typep fn 'LVMBuiltInFunction)

        (lvm-push-stack vm (apply
                            (slot-value fn 'body)
                            args))

        (let ((fu-args (slot-value fn 'args)))
          (lvm-init-self vm (slot-value fn 'field-size)
                         (+ (slot-value vm 'pc)
                            (slot-value fn 'function-size))
                         (slot-value vm 'ep))

          (dotimes (i (length args))
            (lvm-set-local-variable
             vm
             (slot-value (elt fu-args i) 'name)
             (slot-value (elt fu-args i) 'nameofid)
             (elt args i)))

          (setf (slot-value vm 'pc)
                (slot-value fn 'function-from))))))
