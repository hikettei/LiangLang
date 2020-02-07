

(in-package #:liang)




(defstruct lvmiseq-compile-infomation

  (iseq (make-array 0 :adjustable T :fill-pointer 0))

  (compiled-lvm-version "")
  
  )




(defun liang-compile-program
    (code &optional (compile-info (make-lvmiseq-compile-infomation)))
  (dolist (i code)
    (lvmiseq-encode i compile-info))
  compile-info)


(defun liang-compile-file ())


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

         (when (and (equal (car (second branchbody))
                       :CALLDEF)
                    (equal (car branchbody) '=))
           
           ; When calling the operator "=",
           ; if operator's target is a function then Replace a tree to :def

           (setf (car (second branchbody)) :DEF))
         
         (lvmiseq-encode (second branchbody) compile-info)
         (lvmiseq-encode (third  branchbody) compile-info)
         
         (gen-iseq iseq :CALLDEF `(,(first branchbody) 2)))


        (:CALLDEF
         
         (dolist (i (cdr branchbody))
           (lvmiseq-encode (car i) compile-info))


         (gen-iseq iseq :CALLDEF `(,(if (equal (caar branchbody) :NAME)
                                   (second (car branchbody))
                                   (error "function's name is not proper"))
                               ,(length (cdr branchbody)))))
        
        (:DEF
         
         (dolist (i (cdr branchbody))
           (lvmiseq-encode (car i) compile-info)
           
           (let* ((treetype-fname (caar branchbody))
                  (fname (if (equal treetype-fname :NAME)
                             (second (car branchbody))
                             (error "Function's name is not proper"))))


             (gen-iseq iseq :PUSHDEF `(,fname ,(length (cdr branchbody)))))))


        (:PROG
         (gen-iseq iseq :PUSHPROG `(,(compile-for-prog (car branchbody)))))
        

        (:NUMBER
         (gen-iseq iseq :PUSHNUMBER branchbody))
        
        
        (:NAME
         (gen-iseq iseq :PUSHNAME branchbody))

        
        (T (print "Undefined Tree ->")
           (print branchtype))))))
