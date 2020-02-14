

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


(defun liang-compile-file (filepath)

  (with-open-file (in (concatenate 'string filepath ".liang")
                      :direction :input)
    (let ((buf (make-string (file-length in))))
      (read-sequence buf in)

    (with-open-file (target (concatenate 'string filepath ".lvm")
                    :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)

      (format target (write-to-string
                      (coerce (slot-value (liangc buf) 'iseq) 'list)))))))


(defun liangc (code)
  (liang-compile-program (gentree code)))

(defmacro gen-iseq (iseq instruction-name operand)
  `(vector-push-extend (append (list ,instruction-name) ,operand) ,iseq))

(defmacro compile-for-prog (ast)
  `(coerce (slot-value (liang-compile-program ,ast) 'iseq) 'list))




(defun lvmiseq-encode (ast compile-info)

  (with-slots (iseq) compile-info

    (let ((branchtype (car ast))
          (branchbody (cdr ast)))


      (case branchtype


        (:EXP

         (when (and (eq (car (second branchbody))
                       :CALLDEF)
                    (eq (car branchbody) '=))
           
           ; When calling the operator "=",
           ; if operator's target is a function then Replace a tree to :def

           (setf (car (second branchbody)) :DEF))
         
         (lvmiseq-encode (second branchbody) compile-info)
         (lvmiseq-encode (third  branchbody) compile-info)

         (gen-iseq iseq :CALLDEF `(,(let* ((name (first branchbody))
                                           (result (if (listp name)
                                                       (second name)
                                                       name)))
                                      
                                      result)
                                    2)))


        (:CALLDEF

         (dolist (i (car (cdr branchbody)))
           (lvmiseq-encode i compile-info))


         (gen-iseq iseq :CALLDEF `(,(if (equal (caar branchbody) :NAME)
                                   (second (car branchbody))
                                   (error "function's name is not proper"))
                               ,(length (car (cdr branchbody))))))
        
        (:DEF
         
         (dolist (i (car (cdr branchbody)))
           (lvmiseq-encode i compile-info))
           
           (let* ((treetype-fname (caar branchbody))
                  (fname (if (equal treetype-fname :NAME)
                             (second (first branchbody))
                             (error "Function's name is not proper"))))


             (gen-iseq iseq :PUSHDEF `(,fname ,(length (car (cdr branchbody)))))))


        (:PROG
         (gen-iseq iseq :PUSHPROG `(,(compile-for-prog (car branchbody)))))
 

        (:NUMBER
         (gen-iseq iseq :PUSHNUMBER branchbody))
        

        (:STRING
         (gen-iseq iseq :PUSHSTRING branchbody))

        
        (:NAME
         (gen-iseq iseq :PUSHNAME branchbody))

        (:INSTANT-STRUCTURE

         (dolist (i (car branchbody))
           (lvmiseq-encode i compile-info))

         (gen-iseq iseq :PUSHLIST `(,(length (car branchbody)))))
        
        
        (T (print "Undefined Tree ->")
           (print branchtype))))))
