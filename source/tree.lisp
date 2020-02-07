

(in-package #:liang)



(defun gentree (source)
  (parse-with-lexer (*liang-lexer* source) *liang-parser*))


(yacc:define-parser *liang-parser*


  (:muffle-conflicts t)

  (:start-symbol program)

  (:terminals (:string
              :number

              :|(|
              :|)|

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
              
              :macro-body
              :funame
              :name))


  (:precedence ((:left :funame :name :* :/)
                (:left :+ :-)
                (:left :=)))

  
  (program
   (liang program #'cons)
   (liang #'list))


  (liang
   liangsyntax
   
   liangsymbol)

  (liangsymbol
   liangnames

   (:number #'(lambda (x) `(:NUMBER ,x)))
   (:string #'(lambda (x) `(:STRING ,x))))


  (liangnames
   (:funame #'(lambda (x) `(:NAME ,x)))
   (:name   #'(lambda (x) `(:NAME ,x))))

  
  (liangsyntax


   (liang :+ liang #'(lambda (x y z) `(:EXP ,y ,x ,z)))
   (liang :- liang #'(lambda (x y z) `(:EXP ,y ,x ,z)))
   (liang :* liang #'(lambda (x y z) `(:EXP ,y ,x ,z)))
   (liang :/ liang #'(lambda (x y z) `(:EXP ,y ,x ,z)))

   (liang := liang #'(lambda (x y z) `(:EXP ,y ,x ,z)))

   
   (:{ program :} #'(lambda (x y z)
                      (declare (ignore x z))
                      `(:PROG ,y)))


   (:|(| liang :|)| #'(lambda (x y z)
                        (declare (ignore x z))
                        `(:PROG ,y)))
   
   ; structure


   (funame :[ parse-args :] #'(lambda (name x y z)
                                (declare (ignore x z))

                                `(:STRUCTURE ,name ,y)))

   
   (:[ parse-args :] #'(lambda (x y z)
                         (declare (ignore x z))

                         `(:INSTANT-STRUCTURE ,y)))
                      
   (liang liangnames liang
               #'(lambda (x y z) `(:exp ,y ,x ,z)))

   (liang :DOT liang #'(lambda (x y z) `(:exp ,y ,x ,z)))
   
   (liangnames :|(| parse-args :|)| #'(lambda (name x args y)
                                        (declare (ignore x y))

                                        `(:CALLDEF ,name ,args)))

   )


  (parse-args
   (liang #'list)
   (liang :COMMA parse-args
          #'(lambda (x y z)
              (declare (ignore y))
              (cons x z)))))
