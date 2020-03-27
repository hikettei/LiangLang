

(in-package #:liang.compiler)



(defun gentree (source)
  (parse-with-lexer (*liang-lexer* source) *liang-parser*))

(defvar *liang-macros* (make-hash-table :test 'equal))
(defvar *using-files* (make-array 1 :adjustable T :fill-pointer 1
                                    :initial-contents `("source/lib.liang")))

(defun append-using-file (filepath)
  (gentree (read-file-sequence filepath))
  (vector-push-extend filepath *using-files*))

(defmacro liang-macros (&optional name)
  `(gethash ,name *liang-macros*))

(defun (setf liang-macros) (body &optional name)
  (setf (gethash name *liang-macros*) body))


(defmacro lvmnil () `(list :NIL))

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
              :USING

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
                                       `(:LAMBDA (,args) ,body)))
   
   (:|(| args1 :{{ program :} #'(lambda (x args y body _)
                                           (declare (ignore x y _))
                                           `(:LAMBDA ,args ,body))))
  
  (liangsyntax
   (liang := liang #'(lambda (x y z) `(:SETQ ,y ,x ,z)))
   lianglists
   liangexp

   (:USING :STRING #'(lambda (x y)
                       (declare (ignore x))
                       (append-using-file y)
                       `(:STRING ,y)))
   
   (:at-mark liangnames parse-args2
               #'(lambda (n name name0)
                   (declare (ignore n))
                   `(:CALLDEF ,name ,name0)))

   (:macro liangnames :|(| liangnames :|)| := :macro-body
           #'(lambda (n name x arg y a body)
               (declare (ignore n x y a))
               (setf (liang-macros) (list (gentree body) (list args)))
               name))
   
   (:macro liangnames :|(| args1 :|)| := :macro-body
           #'(lambda (n name x arg y a body)
               (declare (ignore n x y a))
               (setf (liang-macros name) (list (gentree body) arg))
               name))

   (:macro liangnames :|(| :|)| := :macro-body
           #'(lambda (n name x y a body)
               (declare (ignore n x y a))
               (setf (liang-macros) (list (gentree body) NIL))
               name))

   (:macro liangnames parse-args2
           #'(lambda (n macroname args)
               (declare (ignore n))
               (let* ((macro (liang-macros macroname))
                     (macro-args (second macro)))
                 `(:PROG (,@(loop for i from 0 to (1- (length macro-args))
                                  append `((:SETQ := ,(elt macro-args i)
                                                  ,(elt args i))))
                          ,@(car macro)
                          ,@(loop for i from 0 to (1- (length macro-args))
                                  append `((:SETQ := ,(elt macro-args i)
                                                   ,(LVMNIL)))))))))
   
   (:macro liangnames #'(lambda (n macroname)
                          (declare (ignore n))
                          (let ((macro (liang-macros macroname)))
                            `(:PROG ,(car macro)))))
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
                                        `(:CALLDEF ,name ,args)))

   (liang :|(| :|)| #'(lambda (obj x y)
                        (declare (ignore x y))
                        `(:LOCALLY NIL (,obj))))
   
   (liang :|(| parse-args :|)| #'(lambda (obj x args y)
                                   (declare (ignore x y))
                                   `(:LOCALLY ,args (,obj)))))
  
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
