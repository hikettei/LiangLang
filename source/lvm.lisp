
(in-package #:liang)


(defparameter *LVMVersion* "0.1")



(defstruct LiangVMInfomation
  (sp 0)
  (started-at  "")
  (lvm-version "")
  (scopes (make-array 0 :adjustable t :fill-pointer 0)))



(defstruct LVMScope
  (variable-table (make-hash-table))
  (stack (make-array 0 :adjustable t :fill-pointer 0)))


(defstruct LVMFunction
  (function-body)
  (arg-symbols))


(defmethod lvm-register-variable (symbol-name symbol-value self)
  (with-slots (variable-table) self
    (setf (gethash symbol-name variable-table) symbol-value)))


(defmethod lvm-pushvalue (operand stack)
  (vector-push-extend (first operand) stack))


(defun lvm-call-function (function args self)
  (with-slots (function-body arg-symbols) function


    ; Set arguments

    (dotimes (i (length args))
      (lvm-register-variable (nth i arg-symbols)
                         (nth i args) self)
    
    (dolist (i function-body)
      (lvm-exec-instruction i (make-LVMScope))))))

(defun lvm-calldef (operand self)

  (with-slots (variable-table stack) self

    (let* ((name (car operand))
           (argssize (second operand))

           (args (loop for i from 1 to argssize
                    append `(,(vector-pop stack))))

           (function (if (or (gethash name variable-table)
                             (equal name '=))
                         (gethash name variable-table)
                         (error "The variable doesn't exist"))))

      
    (cond
      ((equal name '=)
       (lvm-register-variable (first args) (second args) self))
      
      ((equal (slot-value function 'function-body) NIL)
       (error "The called function doesn't have implements."))

      (T (lvm-call-function function args self))))))


(defun lvm-pushdef (operand self)

  (with-slots (stack) self

    (let* ((args (loop for i from 0 to (second operand)
                    append (vector-pop stack))))

      (lvm-register-variable (first operand)
                             (make-LVMFunction :arg-symbols args) self))))


(defun lvm-execute (iseq &optional (lvm-info (make-LiangVMInfomation)))


  (with-slots (sp scopes) lvm-info


      (vector-push-extend (make-LVMScope) scopes)

       ; initialize main


      (dolist (i iseq)
        (lvm-exec-instruction i (aref scopes 0)))))



(defun lvm-exec-instruction (iseq self)

  (let* ((opecode (first iseq))
           (operand (cdr iseq))
           (stack (slot-value self 'stack)))
      
      (case opecode

        (:PUSHNAME
         (lvm-pushvalue operand stack))
        
        (:PUSHNUMBER
         (lvm-pushvalue operand stack))

        
        (:CALLDEF
         (lvm-calldef operand self))


        (:PUSHDEF
         (lvm-pushdef operand self))


        (T (error "Recieved unimplemented operand")))))
