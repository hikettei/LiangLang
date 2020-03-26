

(in-package #:liang.compiler)



(defun gentree (source)
  (parse-with-lexer (*liang-lexer* source) *liang-parser*))

(defvar *liang-macros* (make-hash-table :test 'equal))

(defmacro liang-macros (&optional name)
  `(gethash ,name *liang-macros*))

(defun (setf liang-macros) (body &optional name)
  (setf (gethash name *liang-macros*) body))

(yacc:define-parser *liang-parser*
  (:muffle-conflicts t)

  (:start-symbol program)

  (:terminals (:string
              :number

              :|(|
              :|)|

              :{{
              :{
              :}

              :[
              :]


              :+
              :-
              :*
              :/
              :=
              :<
              :>
              
              :COMMA
              :DOT
              :END

              :at-mark
              :macro
              :macro-body
              :funame
              :name))


  (:precedence ((:left :* :/)
                (:left :+ :- :funame :name)
                (:left :=)))

  
  (program
   (liang :END program #'(lambda (x y z)
          (declare (ignore y))
          (cons x z)))
   (liang #'list))


  (liang
   liangsyntax
   liangsymbol)

  (liangsymbol
   liangnames

   (:number #'(lambda (x) `(:NUMBER ,x)))
   (:string #'(lambda (x) `(:STRING ,x))))


  (liangnames
   (liangnamelist #'(lambda (x) `(:NAME ,x))))

  (liangnamelist :+ :- :* :/ :funame :name :=)
  
  (liangexp
   (liang liangnames liang #'(lambda (x y z) `(:EXP ,y ,x ,z)))
   (liang :+ liang #'(lambda (x y z) `(:EXP (:NAME ,y) ,x ,z)))
   (liang :- liang #'(lambda (x y z) `(:EXP (:NAME ,y) ,x ,z)))
   (liang :* liang #'(lambda (x y z) `(:EXP (:NAME ,y) ,x ,z)))
   (liang :/ liang #'(lambda (x y z) `(:EXP (:NAME ,y) ,x ,z))))

  (lianglists
   (:< args1 :> #'(lambda (x y z)
                    (declare (ignore x z))
                    `(:SYMBOLLIST NIL ,(length y) ,y)))

   (:funame :[ parse-args :] #'(lambda (type x y z)
                               (declare (ignore x z))
                               `(:VECTOR ,type ,(length y) ,y)))

   (:[ parse-args :] #'(lambda (x y z)
                         (declare (ignore x z))
                         `(:VECTOR ,NIL ,(length y) ,y))))
       
  
  (lambdas
   (:{ program :} #'(lambda (x y z)
                      (declare (ignore x z))
                      `(:LAMBDA NIL ,y)))
   (:|(| liangnames :{{ program :} #'(lambda (x args y body _)
                                       (declare (ignore x y _))
                                       `(:LAMBDA ,args ,body)))
   
   (:|(| args1 :{{ program :} #'(lambda (x args y body _)
                                           (declare (ignore x y _))
                                           `(:LAMBDA ,args ,body))))
  
  (liangsyntax
   (liang := liang #'(lambda (x y z) `(:SETQ ,y ,x ,z)))
   lianglists
   liangexp
   
   (:at-mark liangnames parse-args2
               #'(lambda (n name name0)
                   (declare (ignore n))
                   `(:CALLDEF ,name ,name0)))

   (:macro liangnames :|(| args1 :|)| := :macro-body
           #'(lambda (n name x args y a body)
               (declare (ignore n x y a args))
               (setf (liang-macros name) (list (gentree body) args))
               name))

   (:macro liangnames :|(| :|)| := :macro-body
           #'(lambda (n name x y a body)
               (declare (ignore n x y a))
               (setf (liang-macros) (list (gentree body) args))
               name))
   
   (:macro liangnames parse-args2
           #'(lambda (n macroname args)
               (declare (ignore n))
               (let ((macro (liang-macros macroname)))
                 `(:LOCALLY NIL ( ,@(loop for i from 0 to (1- (length (second macro)))
                                         append `(,(list :SETQ := (elt (second macro) i)
                                                         (elt args i))))
                                  ,@(car macro))))))
   lambdas
      
   (:|(| liang :|)| #'(lambda (x y z)
                        (declare (ignore x z))
                        y))
   
   ; structure

   
   (liang :DOT liang #'(lambda (x y z) `(:exp ,y ,x ,z)))

   (liangnames :|(| :|)| #'(lambda (name x y )
                             (declare (ignore x y))

                             `(:CALLDEF ,name NIL)))
   
   (liangnames :|(| parse-args :|)| #'(lambda (name x args y)
                                        (declare (ignore x y))

                                        `(:CALLDEF ,name ,args))))

  (args
   parse-args)

  (parse-args2
   (liang #'list)
   (liang :COMMA parse-args2 #'(lambda (x y z)
          (declare (ignore y))
                   (cons x z))))
  
  (args1
   parse-args1)
  
  (parse-args1
   (liangnames #'list)
   (liangnames :COMMA parse-args1
               #'(lambda (x y z)
                   (declare (ignore y))
                   (cons x z))))
  
  (parse-args
   (liang #'list)
   (liang :COMMA parse-args
          #'(lambda (x y z)
              (declare (ignore y))
              (cons x z)))))
