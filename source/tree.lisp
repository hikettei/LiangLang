

(in-package #:liang.compiler)



(defun gentree (source)
  (parse-with-lexer (*liang-lexer* source) *liang-parser*))


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
              
              :COMMA
              :DOT
              :END

              :at-mark
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

  (lambdas
   (:{ program :} #'(lambda (x y z)
                      (declare (ignore x z))
                      `(:LAMBDA NIL ,y)))
   (:|(| parse-args :{{ program :} #'(lambda (x args y body _)
                                           (declare (ignore x y _))
                                           `(:LAMBDA ,args,body))))
  
  (liangsyntax
   
   (liang := liang #'(lambda (x y z) `(:SETQ ,y ,x ,z)))

   liangexp
   
   (:at-mark liangnames liangnames :|(| :|)| lambdas
               #'(lambda (n name name0 x y body)
                   (declare (ignore n x y))
                   `(:CALLDEF ,name ,name0 NIL ,body)))

   (:at-mark liangnames liangnames :|(| args :|)| lambdas
               #'(lambda (n name name0 x args z body)
                   (declare (ignore n x z))
                   `(:CALLDEF ,name ,name0 ,args ,body)))

   (:at-mark liangnames :|(| liang :|)| lambdas #'(lambda (n fname x args y body)
                                                      (declare (ignore n x y))
                                                      `(:CALLDEF ,fname ,args ,body)))

   lambdas
   (:|(| liang :|)| #'(lambda (x y z)
                        (declare (ignore x z))
                        y))
   
   ; structure


   (:funame :[ parse-args :] #'(lambda (name x y z)
                                (declare (ignore x z))

                                `(:ARRAYWITHNAME ,name ,y)))
   
   (:[ parse-args :] #'(lambda (x y z)
                         (declare (ignore x z))
                         `(:ARRAY ,y)))
   
   (liang :DOT liang #'(lambda (x y z) `(:exp ,y ,x ,z)))

   (liangnames :|(| :|)| #'(lambda (name x y )
                             (declare (ignore x y))

                             `(:CALLDEF ,name NIL)))
   
   (liangnames :|(| parse-args :|)| #'(lambda (name x args y)
                                        (declare (ignore x y))

                                        `(:CALLDEF ,name ,args))))

  (args
   parse-args)
  
  (parse-args
   (liang #'list)
   (liang :COMMA parse-args
          #'(lambda (x y z)
              (declare (ignore y))
              (cons x z)))))
