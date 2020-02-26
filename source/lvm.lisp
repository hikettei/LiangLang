
(in-package #:liang)


(defparameter *LVMVersion* "0.1")


(declaim (inline lvm-make-array
                 lvm-exec-instruction
                 has-variable?
                 lvm-pushlist
                 lvm-call-function
                 lvm-stack-pop
                 lvm-pushobject
                 lvm-getlocal-variable
                 lvm-pushprog
                 lvm-make-newscope-withextending
                 copy-hash-table
                 lvm-register-variable))


(defstruct LiangVMInfomation
  (iseq)
  (scopes (make-array 1 :adjustable T :fill-pointer 0)))


(defstruct LVMScope
  (global-variable-table (make-hash-table :test 'eq))
  (variable-table (make-hash-table :test 'eq))
  (stack (make-array 64 :initial-element NIL :fill-pointer 0)))


(defstruct LVMFunction
  (function-body NIL)
  (arg-symbols NIL :type list)
  (name NIL :type symbol))


(defstruct LVMArray
  (length 0 :type fixnum)
  (values NIL :type list)
  ;(array-type nil)
  )


(defmacro each-iseq (self iseq)
  `(dolist (i ,iseq)
    (lvm-exec-instruction i ,self)))

(defun lvm-implement-default-function (self)

  (lvm-include-lib "source/lib" self)
  
  (lvm-define-function self 'calllisp `(fname args) `(((:CALLLISP))))

  (lvm-define-function self '+ `(x y) #'+)
  (lvm-define-function self '- `(x y) #'-)
  (lvm-define-function self '* `(x y) #'*)
  (lvm-define-function self '/ `(x y) #'/)

  (lvm-define-function self 'print `(x) #'print)
  (lvm-define-function self 'equals `(x y) #'=)

  (lvm-define-function self 'or `(x y) #'(lambda (x y) (or x y)))
  (lvm-define-function self 'value_if `(x y z) #'(lambda (x y z)
                                                   (if x y z))))



(defun lvm-make-newscope ()
  (let ((scope (make-LVMScope)))
    (lvm-implement-default-function scope)
    scope))


(defun lvm-make-newscope-withextending (base)
  (declare (optimize (speed 3) (space 0) (safety 0))
           (type LVMScope base))
  
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

(defmethod lvm-register-variable ((symbol-name LVMFunction)
                                  (symbol-value LVMFunction)
                                  (self LVMScope))
  
  (lvm-implements-function symbol-name symbol-value self))


(defmethod lvm-register-variable ((symbol-name symbol)
                                  (symbol-value T)
                                  (self LVMScope))
  (setf (gethash symbol-name (slot-value self 'variable-table)) symbol-value))


(defun lvm-pushvalue (operand stack)
  (declare (type list operand)
           (type vector stack))
  (vector-push (first operand) stack))


(defmacro lvm-stack-pop-args (self size &optional (use NIL))
  `(nreverse (loop for i from 1 to ,size
                 append (list (lvm-stack-pop ,self ,use)))))


(defun lvm-stack-pop (self &optional (use NIL))
  (declare (optimize (speed 3) (safety 1))
           (type LVMScope self))
  
  (let* ((popped-value (vector-pop (slot-value self 'stack)))
         (value (if use
                    (if (typep popped-value 'symbol)
                        (has-variable? popped-value self)
                        popped-value)
                    popped-value)))
    value))

(defun lvm-pushobject (obj self)
  (declare (optimize (speed 3) (safety 0))
           (type LVMScope self))
  (vector-push obj (slot-value self 'stack)))


(defun lvm-define-function (self name args iseq)
  (lvm-register-variable
     name
     (make-LVMFunction :name name :arg-symbols args :function-body iseq)
     self))


(defmacro lvm-evaluation-args (arg self)
  `(if (typep ,arg 'symbol)
       (lvm-getlocal-variable ,arg ,self)
       ,arg))

(defun lvm-call-function (function args self)
  
  (with-slots (function-body arg-symbols) function

    (if (typep function-body 'function)
      (vector-push
       (let* ((result (apply function-body (mapcar #'(lambda (x)
                                        (lvm-evaluation-args x self))
                                                   args))))
         (if (eq result T) 1 result))
       (slot-value self 'stack))
    
    (let* ((newscope (lvm-make-newscope-withextending self))
           (newscopestack (slot-value newscope 'stack))
           (basestack     (slot-value self     'stack)))

      
      ; Set arguments

      (dotimes (i (length args))
        (lvm-register-variable (elt arg-symbols i)
                               (lvm-evaluation-args
                               (elt args i)
                                self)
                               newscope))


      (each-iseq newscope (first function-body))

      ; return value


      (vector-push (vector-pop newscopestack) basestack))))
  NIL)


(defun lvm-calllisp (self)

  (declare (optimize (speed 3) (space 0) (safety 0)))
  
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
  (has-variable? name self))


(defun has-variable? (name self)
  (declare (type symbol name) (type LVMScope self))
  
  (with-slots (global-variable-table variable-table) self
    (or (gethash name variable-table)
        (gethash name global-variable-table))))


(defun lvm-calldef (operand self)

  (declare (type list operand)
           (type LVMScope self))
  
  (with-slots (stack) self
    
    (let* ((name (car operand))
           (argssize (second operand))

           (args (lvm-stack-pop-args self argssize))
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

  (declare (type list operand)
           (type LVMScope self))
  
  (with-slots (stack) self

    (let* ((args (lvm-stack-pop-args self (second operand))))
      (lvm-pushobject (make-LVMFunction :arg-symbols args
                                        :name (first operand))
                      self))))

(defun lvm-pushprog (operand self)
  (lvm-pushobject (make-LVMFunction :function-body operand) self))


(defun lvm-make-array (values)
  (make-LVMArray
   :length 0 ;(length values)
   :values values))


(defun lvm-pushlist (operand self) 
  (with-slots (stack) self
    (lvm-pushobject
     (lvm-make-array (lvm-stack-pop-args self (first operand) T))
     self)))


(defun lvm-loadfile (filepath)
  (with-open-file (in (concatenate 'string filepath ".lvm")
                      :direction :input)
    (let ((buf (make-string (file-length in))))
      (read-sequence buf in)
      
      (make-LiangVMInfomation :iseq (read-from-string buf)))))

(defun lvm-loadfile-and-execute (filepath)
  (lvm-run (lvm-loadfile filepath)))

(defun lvm-include-lib (filepath self)
  (with-open-file (in (concatenate 'string filepath ".lvm")
                      :direction :input)

    (let ((buf (make-string (file-length in))))
      (read-sequence buf in)
      (each-iseq self (read-from-string buf)))))

(defun lvm-run (lvm-info) (lvm-execute (slot-value lvm-info 'iseq) lvm-info))

(defun lvm-execute (iseq &optional (lvm-info (make-LiangVMInfomation)))

  (with-slots (scopes) lvm-info

      ; initialize main
    
      (vector-push-extend (lvm-make-newscope) scopes)

      (each-iseq (aref scopes 0) iseq)
      
  lvm-info))


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
