
(in-package #:liang)


(defparameter *LVMVersion* "0.1")



(defstruct LiangVMInfomation
  (sp 0)
  (started-at  "")
  (lvm-version "")
  (scopes (make-array 0 :adjustable t :fill-pointer 0)))


(defstruct LVMScope
  (global-variable-table (make-hash-table))
  (variable-table (make-hash-table))
  (stack (make-array 0 :adjustable t :fill-pointer 0)))


(defstruct LVMFunction
  (function-body)
  (arg-symbols)
  (name))


(defstruct LVMArray
  (length)
  (values)
  (array-type nil))


(defun lvm-make-newscope ()
  (let ((scope (make-LVMScope)))

    (lvm-define-function scope 'calllisp `(fname args) `(((:CALLLISP))))

    scope))


(defun lvm-make-newscope-withextending (base)
  (make-LVMScope :global-variable-table
                 (slot-value base 'global-variable-table)
                 :variable-table
                 (copy-hash-table
                  (slot-value base 'variable-table))))


(defun lvm-implements-function (x y self)

  (let* ((fname (slot-value x 'name))
         (function (make-LVMFunction
                    :name fname
                    :function-body (slot-value y 'function-body)
                    :arg-symbols  (slot-value  x 'arg-symbols))))

    (setf (gethash fname (slot-value self 'global-variable-table))
          function)))


(defun lvm-register-variable (symbol-name symbol-value self)
  (with-slots (variable-table) self

    (cond
      
      ((and (typep symbol-name 'LVMFunction)
            (typep symbol-value 'LVMFunction))
       
       (lvm-implements-function symbol-name symbol-value self))
      
      (T (setf (gethash symbol-name (slot-value self 'variable-table)) symbol-value)))))


(defmethod lvm-pushvalue (operand stack)
  (vector-push-extend (first operand) stack))


(defun lvm-stack-pop (self &optional (use NIL))

  (when (= (length (slot-value self 'stack)) 0)
    (error "StackUnderflow"))

  (let* ((popped-value (vector-pop (slot-value self 'stack)))
         (value (if use
                    (if (typep popped-value 'symbol)
                        (has-variable? popped-value self)
                        popped-value)
                    popped-value)))
    value))

(defun lvm-pushobject (obj self)
  (vector-push-extend obj (slot-value self 'stack)))


(defun lvm-define-function (self name args iseq)

  (with-slots (global-variable-table) self

    (lvm-register-variable
     (make-LVMFunction :name name
                       :arg-symbols args)
     (make-LVMFunction :function-body iseq)
     self)))

(defmacro lvm-evaluation-args (arg self)
  `(if (typep ,arg 'symbol)
       (lvm-getlocal-variable ,arg ,self)
       ,arg))

(defun lvm-call-function (function args self)
  (with-slots (function-body arg-symbols) function


    (let* ((newscope (lvm-make-newscope-withextending self))
           (newscopestack (slot-value newscope 'stack))
           (basestack     (slot-value self     'stack)))

      
      ; Set arguments

      (dotimes (i (length args))
        (lvm-register-variable (nth i arg-symbols)
                               (lvm-evaluation-args
                               (nth i args)
                                self)
                               newscope))

      
      (dolist (i (first function-body))
          (lvm-exec-instruction i newscope))


      ; return value


      (vector-push-extend (vector-pop newscopestack)
                          basestack)
      NIl)))

(defun lvm-calllisp (self)

  (let* ((args  (lvm-getlocal-variable 'args  self))
        (fname (lvm-getlocal-variable 'fname self))
        (result (apply (if (typep fname 'string)
                               (read-from-string fname)
                               fname)
                           
                           (if (typep args 'LVMArray)
                               (slot-value args 'values)
                               (error "Calllisp: Arguments must be an array.")))))

    (lvm-pushobject (if (eq result T) 1 result) self)))


(defun lvm-getlocal-variable (name self)

;  (with-slots (variable-table) self
;    (if (gethash name variable-table)
;        (gethash name variable-table)
;        (error "The variable doesn't exist")))

(has-variable? name self))


(defun has-variable? (name self)
  (with-slots (global-variable-table variable-table) self
    (or (gethash name variable-table)
        (gethash name global-variable-table))))


(defun lvm-calldef (operand self)

  (with-slots (stack) self
    
    (let* ((name (car operand))
           (argssize (second operand))

           (args (reverse (loop for i from 1 to argssize
                    append `(,(lvm-stack-pop self)))))

           (function (if (or (has-variable? name self)
                             (equal name '=))
                         (has-variable? name self)
                         (error "The variable doesn't exist"))))

    (cond
      ((equal name '=)
       (lvm-register-variable (first args) (second args) self))
       
      ((equal (slot-value function 'function-body) NIL)
       (error "The called function doesn't have implements."))

      (T (lvm-call-function function args self))))))


(defun lvm-pushdef (operand self)

  (with-slots (stack) self

    (let* ((args (reverse (loop for i from 1 to (second operand)
                    append `(,(lvm-stack-pop self))))))

      (lvm-pushobject (make-LVMFunction :arg-symbols args
                                        :name (first operand))
                      self))))

(defun lvm-pushprog (operand self)
  (lvm-pushobject (make-LVMFunction :function-body operand) self))


(defun lvm-make-array (values)
  (make-LVMArray
   :length (length values)
   :values values))


(defun lvm-pushlist (operand self)

  (with-slots (stack) self
    (lvm-pushobject
     (lvm-make-array (reverse (loop for i from 1 to (car operand)
                       append `(,(lvm-stack-pop self T)))))
     self)))


(defun lvm-loadfile-and-execute (filepath)
  (with-open-file (in (concatenate 'string filepath ".lvm")
                      :direction :input)
    (let ((buf (make-string (file-length in))))
      (read-sequence buf in)
      
      (lvm-execute (read-from-string buf)))))


(defun lvm-execute (iseq &optional (lvm-info (make-LiangVMInfomation)))


  (with-slots (sp scopes) lvm-info


      (vector-push-extend (lvm-make-newscope) scopes)

       ; initialize main

      (dolist (i iseq)
        (lvm-exec-instruction i (aref scopes 0))))
  lvm-info)

(defun lvm-exec-instruction (iseq self)

  (let* ((opecode (first iseq))
           (operand (cdr iseq))
           (stack (slot-value self 'stack)))


      (case opecode

        (:PUSHNAME
         (lvm-pushvalue operand stack))
        
        (:PUSHNUMBER
         (lvm-pushvalue operand stack))

        (:PUSHSTRING
         (lvm-pushvalue operand stack))
        
        (:PUSHPROG
         (lvm-pushprog operand self))

        (:PUSHLIST
         (lvm-pushlist operand self))
        
        (:CALLDEF
         (lvm-calldef operand self))

        (:PUSHDEF
         (lvm-pushdef operand self))

        (:CALLLISP
         (lvm-calllisp self))
        
        (T (error "Recieved unimplemented operand")))))
