

(in-package #:liang)




(defstruct lvmiseq-compile-infomation

  (iseq (make-array 0 :adjustable T :fill-pointer 0))

  
  )




(defun liang-compile-program (code &optional (compile-info (make-lvmiseq-compile-infomation)))
  (dolist (i code)
    (lvmiseq-encode i compile-info))

  compile-info)


(defmacro gen-iseq (iseq instruction-name operand)
  `(vector-push-extend (append (list ,instruction-name) ,operand) ,iseq))

(defmacro compile-for-prog (ast)
  `(slot-value (liang-compile-program ,ast) 'iseq))


(defun lvmiseq-encode (ast compile-info)

  (with-slots (iseq) compile-info

    (let ((branchtype (car ast))
          (branchbody (cdr ast)))


      (case branchtype


        (:EXP
         
         (lvmiseq-encode (second branchbody) compile-info)
         (lvmiseq-encode (third  branchbody) compile-info)
         
         (gen-iseq iseq :CALLEXP `(,(first branchbody))))


        (:DEF
         (gen-iseq iseq :PUSHDEF branchbody))


        (:PROG
         (gen-iseq iseq :PUSHPROG `(,(compile-for-prog (car branchbody)))))
        

        (:NUMBER
         (gen-iseq iseq :PUSHNUMBER branchbody))
        
        
        (:NAME
         (gen-iseq iseq :PUSHNAME branchbody))

        
        (T (print "Unknown Tree"))))))
